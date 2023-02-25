use std::mem::size_of;
use std::sync::Arc;

use crate::bytecode::BytecodeTypeArray;
use crate::compiler;
use crate::cpu::{
    CCALL_REG_PARAMS, FREG_PARAMS, REG_FP, REG_PARAMS, REG_RESULT, REG_SP, REG_THREAD, REG_TMP1,
};
use crate::gc::Address;
use crate::language::generator::{bty_array_from_ty, bty_from_ty};
use crate::language::sem_analysis::FctDefinitionId;
use crate::masm::{MacroAssembler, Mem};
use crate::mem;
use crate::mode::MachineMode;
use crate::object::Obj;
use crate::os;
use crate::stack::DoraToNativeInfo;
use crate::threads::ThreadLocalData;
use crate::vm::{get_vm, install_code_stub, Code, CodeKind, LazyCompilationSite, ShapeKind, VM};

// This code generates the compiler stub, there should only be one instance
// of this function be used in Dora. It is necessary for lazy compilation, where
// functions are only compiled on their first invocation. The compiler can use
// the address of this stub for invocations of functions that have not been compiled
// yet. The stub compiles the function and patches the call site to invoke the
// now-compiled function directly on the next invocation. In the end the function is
// executed.

pub fn generate<'a>(vm: &'a VM) -> Arc<Code> {
    let ngen = DoraCompileGen {
        vm,
        masm: MacroAssembler::new(),
        dbg: vm.args.flag_emit_debug_compile,
    };

    ngen.generate()
}

struct DoraCompileGen<'a> {
    vm: &'a VM,
    masm: MacroAssembler,
    dbg: bool,
}

impl<'a> DoraCompileGen<'a> {
    pub fn generate(mut self) -> Arc<Code> {
        let offset_shadow_stack = 0;
        let offset_dtn = offset_shadow_stack
            + if cfg!(target_family = "windows") {
                32
            } else {
                0
            };
        let offset_params = offset_dtn + size_of::<DoraToNativeInfo>() as i32;
        let offset_thread =
            offset_params + (FREG_PARAMS.len() + REG_PARAMS.len()) as i32 * mem::ptr_width();
        let framesize = mem::align_i32(offset_thread + mem::ptr_width(), 16) as i32;

        if self.dbg {
            self.masm.debug();
        }

        // the return address is the call-site we need to patch
        self.masm.prolog(framesize);

        // store params passed in registers on the stack
        self.store_params(offset_params);

        // prepare the native call
        self.masm.load_mem(
            MachineMode::Ptr,
            REG_TMP1.into(),
            Mem::Base(REG_THREAD, ThreadLocalData::dtn_offset()),
        );

        self.masm.store_mem(
            MachineMode::Ptr,
            Mem::Base(REG_SP, offset_dtn + DoraToNativeInfo::last_offset()),
            REG_TMP1.into(),
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

        self.masm.copy_reg(MachineMode::Ptr, REG_TMP1, REG_SP);
        if offset_dtn != 0 {
            self.masm
                .int_add_imm(MachineMode::Ptr, REG_TMP1, REG_TMP1, offset_dtn as i64);
        }

        self.masm.store_mem(
            MachineMode::Ptr,
            Mem::Base(REG_THREAD, ThreadLocalData::dtn_offset()),
            REG_TMP1.into(),
        );

        // invoke the compiler for the call site
        self.masm.load_mem(
            MachineMode::Ptr,
            CCALL_REG_PARAMS[0].into(),
            Mem::Base(REG_FP, mem::ptr_width()),
        );
        self.masm.load_mem(
            MachineMode::Ptr,
            CCALL_REG_PARAMS[1].into(),
            Mem::Base(REG_SP, offset_params),
        );
        self.masm.load_mem(
            MachineMode::Ptr,
            CCALL_REG_PARAMS[2].into(),
            Mem::Base(REG_SP, offset_params + mem::ptr_width()),
        );
        self.masm
            .raw_call(Address::from_ptr(compile_request as *const u8));

        self.masm.load_mem(
            MachineMode::Ptr,
            REG_TMP1.into(),
            Mem::Base(REG_SP, offset_dtn + DoraToNativeInfo::last_offset()),
        );

        self.masm.store_mem(
            MachineMode::Ptr,
            Mem::Base(REG_THREAD, ThreadLocalData::dtn_offset()),
            REG_TMP1.into(),
        );

        self.masm.copy_reg(MachineMode::Ptr, REG_TMP1, REG_RESULT);

        // restore argument registers from the stack
        self.load_params(offset_params);

        // remove the stack frame
        self.masm.epilog_without_return();

        // jump to compiled function
        self.masm.jump_reg(REG_TMP1);

        let code_descriptor = self.masm.code();
        install_code_stub(self.vm, code_descriptor, CodeKind::CompileStub)
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

fn compile_request(ra: usize, receiver1: Address, receiver2: Address) -> Address {
    let vm = get_vm();

    let lazy_compilation_site = {
        let code_id = vm
            .code_map
            .get(ra.into())
            .expect("return address not found");

        let code = vm.code_objects.get(code_id);

        let offset = ra - code.instruction_start().to_usize();
        code.lazy_for_offset(offset as u32)
            .expect("lazy compilation site not found")
            .clone()
    };

    match lazy_compilation_site {
        LazyCompilationSite::Direct(fct_id, disp, ref type_params) => {
            patch_direct_call(vm, ra, fct_id, type_params, disp)
        }

        LazyCompilationSite::Virtual(receiver_is_first, fct_id, vtable_index, ref type_params) => {
            patch_virtual_call(
                vm,
                receiver_is_first,
                receiver1,
                receiver2,
                fct_id,
                vtable_index,
                type_params,
            )
        }

        LazyCompilationSite::Lambda(receiver_is_first) => {
            patch_lambda_call(vm, receiver_is_first, receiver1, receiver2)
        }
    }
}

fn patch_lambda_call(
    vm: &VM,
    receiver_is_first: bool,
    receiver1: Address,
    receiver2: Address,
) -> Address {
    let receiver = if receiver_is_first {
        receiver1
    } else {
        receiver2
    };

    let obj = unsafe { &mut *receiver.to_mut_ptr::<Obj>() };
    let vtable = obj.header().vtbl();
    let class_instance = vtable.class_instance();

    let (lambda_id, type_params) = match &class_instance.kind {
        ShapeKind::Lambda(lambda_id, type_params) => (*lambda_id, type_params.clone()),
        _ => unreachable!(),
    };

    let fct_ptr = compiler::generate(vm, lambda_id, &bty_array_from_ty(&type_params));

    let methodtable = vtable.table_mut();
    methodtable[0] = fct_ptr.to_usize();

    fct_ptr
}

fn patch_virtual_call(
    vm: &VM,
    receiver_is_first: bool,
    receiver1: Address,
    receiver2: Address,
    trait_fct_id: FctDefinitionId,
    vtable_index: u32,
    type_params: &BytecodeTypeArray,
) -> Address {
    let receiver = if receiver_is_first {
        receiver1
    } else {
        receiver2
    };

    let obj = unsafe { &mut *receiver.to_mut_ptr::<Obj>() };
    let vtable = obj.header().vtbl();
    let class_instance = vtable.class_instance();

    let fct_ptr = match &class_instance.kind {
        ShapeKind::TraitObject { object_ty, .. } => {
            let all_type_params = type_params.append(bty_from_ty(object_ty.clone()));
            let thunk_fct_id = compiler::trait_object_thunk::ensure(
                vm,
                trait_fct_id,
                type_params.clone(),
                bty_from_ty(object_ty.clone()),
            );

            compiler::generate(vm, thunk_fct_id, &all_type_params)
        }

        _ => unreachable!(),
    };

    let methodtable = vtable.table_mut();
    methodtable[vtable_index as usize] = fct_ptr.to_usize();

    fct_ptr
}

fn patch_direct_call(
    vm: &VM,
    ra: usize,
    fct_id: FctDefinitionId,
    type_params: &BytecodeTypeArray,
    disp: i32,
) -> Address {
    let fct_ptr = compiler::generate(vm, fct_id, type_params);
    let fct_addr: *mut usize = (ra as isize - disp as isize) as *mut _;

    // update function pointer in data segment
    os::jit_writable();
    unsafe {
        *fct_addr = fct_ptr.to_usize();
    }
    os::jit_executable();

    fct_ptr
}
