use std::collections::hash_map::HashMap;
use std::mem::size_of;

use dora_parser::lexer::position::Position;

use crate::baseline::fct::{JitBaselineFct, JitDescriptor, JitFct, JitFctId};
use crate::baseline::map::CodeDescriptor;
use crate::cpu::{
    Mem, FREG_PARAMS, REG_FP, REG_PARAMS, REG_SP, REG_THREAD, REG_TMP1, REG_TMP_CALLEE,
};
use crate::exception::DoraToNativeInfo;
use crate::gc::Address;
use crate::masm::MacroAssembler;
use crate::mem;
use crate::threads::ThreadLocalData;
use crate::ty::{BuiltinType, MachineMode};
use crate::vm::FctId;
use crate::vm::VM;

pub struct NativeThunks {
    map: HashMap<Address, JitFctId>,
}

impl NativeThunks {
    pub fn new() -> NativeThunks {
        NativeThunks {
            map: HashMap::new(),
        }
    }

    pub fn find_fct(&self, ptr: Address) -> Option<JitFctId> {
        self.map.get(&ptr).map(|&jit_fct_id| jit_fct_id)
    }

    pub fn insert_fct(&mut self, ptr: Address, fct: JitFctId) {
        self.map.entry(ptr).or_insert(fct);
    }
}

#[derive(Clone)]
pub enum InternalFctDescriptor {
    NativeThunk(FctId),
    AllocThunk,
    VerifyThunk,
    TrapThunk,
}

pub struct InternalFct<'a> {
    pub ptr: Address,
    pub args: &'a [BuiltinType],
    pub return_type: BuiltinType,
    pub throws: bool,
    pub desc: InternalFctDescriptor,
}

pub fn generate<'a, 'ast: 'a>(vm: &'a VM<'ast>, fct: InternalFct, dbg: bool) -> JitFctId {
    let fct_desc = fct.desc.clone();

    let ngen = NativeGen {
        vm,
        masm: MacroAssembler::new(),
        fct,
        dbg,
    };

    let jit_fct = ngen.generate();
    let jit_start = jit_fct.ptr_start();
    let jit_end = jit_fct.ptr_end();
    let jit_fct_id: JitFctId = vm.jit_fcts.push(JitFct::Base(jit_fct)).into();

    let code_desc = match fct_desc {
        InternalFctDescriptor::NativeThunk(_) => CodeDescriptor::NativeThunk(jit_fct_id),
        InternalFctDescriptor::TrapThunk => CodeDescriptor::TrapThunk,
        InternalFctDescriptor::VerifyThunk => CodeDescriptor::VerifyThunk,
        InternalFctDescriptor::AllocThunk => CodeDescriptor::AllocThunk,
    };

    vm.insert_code_map(jit_start, jit_end, code_desc);

    jit_fct_id
}

struct NativeGen<'a, 'ast: 'a> {
    vm: &'a VM<'ast>,
    masm: MacroAssembler,

    fct: InternalFct<'a>,
    dbg: bool,
}

impl<'a, 'ast> NativeGen<'a, 'ast>
where
    'ast: 'a,
{
    pub fn generate(mut self) -> JitBaselineFct {
        let save_return = self.fct.return_type != BuiltinType::Unit;
        let args = self.fct.args.len();

        let dtn_size = size_of::<DoraToNativeInfo>() as i32;

        let offset_dtn = 0;
        let offset_return = dtn_size;
        let offset_args = offset_return + if save_return { mem::ptr_width() } else { 0 };
        let framesize = mem::align_i32(offset_args + args as i32 * mem::ptr_width(), 16);

        if self.dbg {
            self.masm.debug();
        }

        self.masm.prolog_size(framesize);

        self.masm.load_mem(
            MachineMode::Ptr,
            REG_TMP_CALLEE.into(),
            Mem::Base(REG_THREAD, ThreadLocalData::dtn_offset()),
        );

        self.masm.store_mem(
            MachineMode::Ptr,
            Mem::Base(REG_SP, offset_dtn + DoraToNativeInfo::last_offset()),
            REG_TMP_CALLEE.into(),
        );

        self.masm.store_mem(
            MachineMode::Ptr,
            Mem::Base(REG_SP, offset_dtn + DoraToNativeInfo::fp_offset()),
            REG_FP.into(),
        );

        self.masm.copy_pc(REG_TMP1);

        self.masm.store_mem(
            MachineMode::Ptr,
            Mem::Base(REG_SP, offset_dtn + DoraToNativeInfo::pc_offset()),
            REG_TMP1.into(),
        );

        self.masm.store_mem(
            MachineMode::Ptr,
            Mem::Base(REG_THREAD, ThreadLocalData::dtn_offset()),
            REG_SP.into(),
        );

        self.masm.raw_call(self.fct.ptr.to_ptr());

        self.masm.store_mem(
            MachineMode::Ptr,
            Mem::Base(REG_THREAD, ThreadLocalData::dtn_offset()),
            REG_TMP_CALLEE.into(),
        );

        self.masm.load_mem(
            MachineMode::Ptr,
            REG_TMP1.into(),
            Mem::Base(REG_THREAD, ThreadLocalData::exception_object_offset()),
        );

        let lbl_exception = self.masm.test_if_not_nil(REG_TMP1);

        self.masm.safepoint(self.vm.polling_page.addr());
        self.masm.epilog();

        self.masm.bind_label(lbl_exception);
        self.masm.throw(REG_TMP1, Position::new(1, 1));
        self.masm.nop();

        let desc = match self.fct.desc {
            InternalFctDescriptor::NativeThunk(fid) => JitDescriptor::NativeThunk(fid),
            InternalFctDescriptor::AllocThunk => JitDescriptor::AllocThunk,
            InternalFctDescriptor::VerifyThunk => JitDescriptor::VerifyThunk,
            InternalFctDescriptor::TrapThunk => JitDescriptor::TrapThunk,
        };

        self.masm.jit(self.vm, framesize, desc, self.fct.throws)
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
