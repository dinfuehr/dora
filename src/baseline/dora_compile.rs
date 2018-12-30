use std::mem::size_of;

use baseline;
use baseline::dora_native::{finish_native_call, start_native_call};
use baseline::fct::{BailoutInfo, JitBaselineFct, JitDescriptor, JitFct};
use baseline::map::CodeDescriptor;
use class::TypeParams;
use cpu::{Mem, FREG_PARAMS, REG_FP, REG_PARAMS, REG_RESULT, REG_SP, REG_THREAD, REG_TMP1};
use ctxt::FctId;
use ctxt::{get_vm, VM};
use exception::DoraToNativeInfo;
use gc::Address;
use masm::MacroAssembler;
use mem;
use object::Obj;
use ty::MachineMode;

// This code generates the compiler thunk, there should only be one instance
// of this function be used in Dora. It is necessary for lazy compilation, where
// functions are only compiled on their first invocation. The compiler can use
// the address of this thunk for invocations of functions that have not been compiled
// yet. The thunk compiles the function and patches the call site to invoke the
// now-compiled function directly on the next invocation. In the end the function is
// executed.

pub fn generate<'a, 'ast: 'a>(vm: &'a VM<'ast>) -> Address {
    let ngen = DoraCompileGen {
        vm: vm,
        masm: MacroAssembler::new(),
        dbg: vm.args.flag_emit_debug_compile,
    };

    let jit_fct = ngen.generate();
    let addr = Address::from_ptr(jit_fct.fct_ptr());
    vm.insert_code_map(
        jit_fct.ptr_start(),
        jit_fct.ptr_end(),
        CodeDescriptor::CompilerThunk,
    );
    vm.jit_fcts.push(JitFct::Base(jit_fct));

    addr
}

struct DoraCompileGen<'a, 'ast: 'a> {
    vm: &'a VM<'ast>,
    masm: MacroAssembler,
    dbg: bool,
}

impl<'a, 'ast> DoraCompileGen<'a, 'ast>
where
    'ast: 'a,
{
    pub fn generate(mut self) -> JitBaselineFct {
        let framesize = size_of::<DoraToNativeInfo>() as i32
            + (REG_PARAMS.len() + FREG_PARAMS.len() + 2) as i32 * mem::ptr_width();
        let framesize = mem::align_i32(framesize, 16) as i32;

        let offset_params = 0;
        let offset_tmp =
            offset_params + (FREG_PARAMS.len() + REG_PARAMS.len()) as i32 * mem::ptr_width();
        let offset_thread = offset_tmp + mem::ptr_width();

        if self.dbg {
            self.masm.debug();
        }

        // the return address is the call-site we need to patch
        self.masm.copy_ra(REG_TMP1);
        self.masm.prolog(framesize);

        self.masm.store_mem(
            MachineMode::Ptr,
            Mem::Base(REG_SP, offset_thread),
            REG_THREAD.into(),
        );

        self.masm.store_mem(
            MachineMode::Ptr,
            Mem::Base(REG_SP, offset_tmp),
            REG_TMP1.into(),
        );

        // store params passed in registers on the stack
        self.store_params(offset_params);

        // prepare the native call
        self.masm.copy_reg(MachineMode::Ptr, REG_PARAMS[0], REG_FP);
        self.masm.copy_pc(REG_PARAMS[1]);
        self.masm.raw_call(start_native_call as *const u8);

        // invoke the compiler for the call site
        self.masm.load_mem(
            MachineMode::Ptr,
            REG_PARAMS[0].into(),
            Mem::Base(REG_SP, offset_tmp),
        );
        self.masm.load_mem(
            MachineMode::Ptr,
            REG_PARAMS[1].into(),
            Mem::Base(REG_SP, offset_params),
        );
        self.masm.raw_call(compile_request as *const u8);
        self.masm.store_mem(
            MachineMode::Ptr,
            Mem::Base(REG_SP, offset_tmp),
            REG_RESULT.into(),
        );

        // undo the previous native call
        self.masm.raw_call(finish_native_call as *const u8);

        // load the address of the compiled function into REG_TMP1
        self.masm.load_mem(
            MachineMode::Ptr,
            REG_TMP1.into(),
            Mem::Base(REG_SP, offset_tmp),
        );

        // restore thread register
        self.masm.load_mem(
            MachineMode::Ptr,
            REG_THREAD.into(),
            Mem::Base(REG_SP, offset_thread),
        );

        // restore argument registers from the stack
        self.load_params(offset_params);

        // remove the stack frame
        self.masm.epilog_without_return(framesize);

        // jump to compiled function
        self.masm.jump_reg(REG_TMP1);

        self.masm
            .jit(self.vm, framesize, JitDescriptor::CompilerThunk, false)
    }

    fn store_params(&mut self, mut offset: i32) {
        for reg in &REG_PARAMS {
            self.masm
                .store_mem(MachineMode::Ptr, Mem::Base(REG_SP, offset), (*reg).into());
            offset += mem::ptr_width();
        }

        for reg in &FREG_PARAMS {
            self.masm.store_mem(
                MachineMode::Float64,
                Mem::Base(REG_SP, offset),
                (*reg).into(),
            );
            offset += mem::ptr_width();
        }
    }

    fn load_params(&mut self, mut offset: i32) {
        for reg in &REG_PARAMS {
            self.masm
                .load_mem(MachineMode::Ptr, (*reg).into(), Mem::Base(REG_SP, offset));
            offset += mem::ptr_width();
        }

        for reg in &FREG_PARAMS {
            self.masm.load_mem(
                MachineMode::Float64,
                (*reg).into(),
                Mem::Base(REG_SP, offset),
            );
            offset += mem::ptr_width();
        }
    }
}

fn compile_request(ra: usize, receiver: Address) -> Address {
    let vm = get_vm();

    let bailout = {
        let data = {
            let code_map = vm.code_map.lock();
            code_map
                .get(ra as *const u8)
                .expect("return address not found")
        };

        let fct_id = match data {
            CodeDescriptor::DoraFct(fct_id) => fct_id,
            _ => panic!("expected function for code"),
        };

        let jit_fct = vm.jit_fcts.idx(fct_id);

        let offset = ra - jit_fct.fct_ptr() as usize;
        let jit_fct = jit_fct.to_base().expect("baseline expected");
        jit_fct
            .bailouts
            .get(offset as i32)
            .expect("bailout info not found")
            .clone()
    };

    match bailout {
        BailoutInfo::Compile(fct_id, disp, ref cls_tps, ref fct_tps) => {
            patch_fct_call(vm, ra, fct_id, cls_tps, fct_tps, disp)
        }

        BailoutInfo::VirtCompile(vtable_index, ref fct_tps) => {
            patch_vtable_call(vm, receiver, vtable_index, fct_tps)
        }
    }
}

fn patch_vtable_call(
    vm: &VM,
    receiver: Address,
    vtable_index: u32,
    fct_tps: &TypeParams,
) -> Address {
    let obj = unsafe { &mut *receiver.to_mut_ptr::<Obj>() };
    let vtable = obj.header().vtbl();
    let cls_id = vtable.class().cls_id;
    let cls = vm.classes.idx(cls_id);
    let cls = cls.read();

    let mut fct_ptr = Address::null();

    for &fct_id in &cls.methods {
        let fct = vm.fcts.idx(fct_id);
        let fct = fct.read();

        if Some(vtable_index) == fct.vtable_index {
            let empty = TypeParams::empty();
            fct_ptr = Address::from_ptr(baseline::generate(vm, fct_id, &empty, fct_tps));
            break;
        }
    }

    let methodtable = vtable.table_mut();
    methodtable[vtable_index as usize] = fct_ptr.to_usize();

    fct_ptr
}

fn patch_fct_call(
    vm: &VM,
    ra: usize,
    fct_id: FctId,
    cls_tps: &TypeParams,
    fct_tps: &TypeParams,
    disp: i32,
) -> Address {
    let fct_ptr = baseline::generate(vm, fct_id, cls_tps, fct_tps);
    let fct_addr: *mut usize = (ra as isize - disp as isize) as *mut _;

    // update function pointer in data segment
    unsafe {
        *fct_addr = fct_ptr as usize;
    }

    Address::from_ptr(fct_ptr)
}
