use std::collections::hash_map::HashMap;
use std::mem::size_of;

use baseline::fct::{JitFct, JitFctId};
use cpu::{Mem, FREG_PARAMS, REG_FP, REG_PARAMS, REG_RESULT, REG_SP};
use ctxt::{exception_get_and_clear, FctId, get_ctxt, SemContext};
use masm::MacroAssembler;
use mem;
use os::signal::Trap;
use exception::DoraToNativeInfo;
use ty::{BuiltinType, MachineMode};

pub struct NativeFcts {
    map: HashMap<*const u8, JitFctId>,
}

impl NativeFcts {
    pub fn new() -> NativeFcts {
        NativeFcts {
            map: HashMap::new(),
        }
    }

    pub fn find_fct(&self, ptr: *const u8) -> Option<JitFctId> {
        self.map
            .get(&ptr)
            .map(|&jit_fct_id| jit_fct_id)
    }

    pub fn insert_fct(&mut self, ptr: *const u8, fct: JitFctId) {
        self.map.entry(ptr).or_insert(fct);
    }
}

pub struct InternalFct<'a> {
    pub ptr: *const u8,
    pub args: &'a [BuiltinType],
    pub return_type: BuiltinType,
}

pub fn generate<'a, 'ast: 'a>(ctxt: &'a SemContext<'ast>, fct: InternalFct, dbg: bool) -> JitFctId {
    let ngen = NativeGen {
        ctxt: ctxt,
        masm: MacroAssembler::new(),
        fct: fct,
        dbg: dbg,
    };

    let jit_fct = ngen.generate();

    let jit_fct_id = ctxt.jit_fcts.len().into();
    ctxt.jit_fcts.push(jit_fct);

    jit_fct_id
}

struct NativeGen<'a, 'ast: 'a> {
    ctxt: &'a SemContext<'ast>,
    masm: MacroAssembler,

    fct: InternalFct<'a>,
    dbg: bool,
}

impl<'a, 'ast> NativeGen<'a, 'ast>
where
    'ast: 'a,
{
    pub fn generate(mut self) -> JitFct {
        let save_return = self.fct.return_type != BuiltinType::Unit;
        let args = self.fct.args.len();

        let framesize = size_of::<DoraToNativeInfo>() as i32 + if save_return { 8 } else { 0 } +
            (args * 8) as i32;

        let framesize = mem::align_i32(framesize, 16);

        let offset_return = 0;
        let offset_args = offset_return + if save_return { 8 } else { 0 };
        // let offset_dtn = offset_args + self.args * 8;

        if self.dbg {
            self.masm.debug();
        }

        self.masm.prolog(framesize);

        save_params(&mut self.masm, self.fct.args, offset_args);

        self.masm.copy_reg(MachineMode::Ptr, REG_PARAMS[0], REG_FP);
        self.masm.copy_pc(REG_PARAMS[1]);
        self.masm
            .direct_call_without_info(start_native_call as *const u8);

        restore_params(&mut self.masm, self.fct.args, offset_args);

        self.masm.direct_call_without_info(self.fct.ptr);

        if save_return {
            self.masm
                .store_mem(MachineMode::Ptr, Mem::Base(REG_SP, 0), REG_RESULT.into());
        }

        self.masm
            .direct_call_without_info(finish_native_call as *const u8);

        let lbl_exception = self.masm.test_if_not_nil(REG_RESULT);

        if save_return {
            self.masm
                .load_mem(MachineMode::Ptr, REG_RESULT.into(), Mem::Base(REG_SP, 0));
        }

        self.masm.epilog(framesize, self.ctxt.polling_page.addr());

        self.masm.bind_label(lbl_exception);
        self.masm.trap(Trap::THROW);

        self.masm.jit(self.ctxt, framesize, FctId(0), false)
    }
}

fn save_params(masm: &mut MacroAssembler, args: &[BuiltinType], offset_args: i32) {
    let mut reg_idx = 0;
    let mut freg_idx = 0;
    let mut idx = 0;

    for &ty in args {
        let mode = ty.mode();
        let is_float = mode.is_float();
        let offset = offset_args + idx as i32 * 8;

        if is_float && freg_idx < FREG_PARAMS.len() {
            let freg = FREG_PARAMS[freg_idx].into();
            masm.store_mem(mode, Mem::Base(REG_SP, offset), freg);
            freg_idx += 1;
        } else if !is_float && reg_idx < REG_PARAMS.len() {
            let reg = REG_PARAMS[reg_idx].into();
            masm.store_mem(mode, Mem::Base(REG_SP, offset), reg);
            reg_idx += 1;
        } else {
            panic!("parameter saved on stack");
        }

        idx += 1;
    }
}

fn restore_params(masm: &mut MacroAssembler, args: &[BuiltinType], offset_args: i32) {
    let mut reg_idx = 0;
    let mut freg_idx = 0;
    let mut idx = 0;

    for &ty in args {
        let mode = ty.mode();
        let is_float = mode.is_float();
        let offset = offset_args + idx as i32 * 8;

        if is_float && freg_idx < FREG_PARAMS.len() {
            let freg = FREG_PARAMS[freg_idx].into();
            masm.load_mem(mode, freg, Mem::Base(REG_SP, offset));
            freg_idx += 1;
        } else if !is_float && reg_idx < REG_PARAMS.len() {
            let reg = REG_PARAMS[reg_idx].into();
            masm.load_mem(mode, reg, Mem::Base(REG_SP, offset));
            reg_idx += 1;
        } else {
            panic!("parameter saved on stack");
        }

        idx += 1;
    }
}

fn start_native_call(fp: *const u8, pc: usize) {
    unsafe {
        // fp is framepointer of native stub

        let dtn_size = size_of::<DoraToNativeInfo>() as isize;
        let dtn: *mut DoraToNativeInfo = fp.offset(-dtn_size) as *mut DoraToNativeInfo;
        let dtn: &mut DoraToNativeInfo = &mut *dtn;

        dtn.fp = fp as usize;
        dtn.pc = pc;

        let ctxt = get_ctxt();

        ctxt.push_dtn(dtn);
        ctxt.handles.push_border();
    }
}

fn finish_native_call() -> *const u8 {
    let ctxt = get_ctxt();

    ctxt.handles.pop_border();
    ctxt.pop_dtn();

    exception_get_and_clear()
}
