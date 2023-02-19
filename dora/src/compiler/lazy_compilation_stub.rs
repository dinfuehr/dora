use std::mem::size_of;
use std::sync::Arc;

use crate::bytecode::{BytecodeBuilder, BytecodeFunction, Register};
use crate::compiler;
use crate::cpu::{
    CCALL_REG_PARAMS, FREG_PARAMS, REG_FP, REG_PARAMS, REG_RESULT, REG_SP, REG_THREAD, REG_TMP1,
};
use crate::gc::Address;
use crate::language::generator::register_bty_from_ty;
use crate::language::sem_analysis::{
    AnalysisData, FctDefinition, FctDefinitionId, FctParent, TypeParamId,
};
use crate::language::ty::{SourceType, SourceTypeArray};
use crate::masm::{MacroAssembler, Mem};
use crate::mem;
use crate::mode::MachineMode;
use crate::object::Obj;
use crate::os;
use crate::stack::DoraToNativeInfo;
use crate::threads::ThreadLocalData;
use crate::vm::{
    find_trait_impl, get_vm, install_code_stub, ClassInstanceId, Code, CodeKind,
    LazyCompilationSite, ShapeKind, VM,
};

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

    let fct_ptr = compiler::generate(vm, lambda_id, &type_params);

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
    type_params: &SourceTypeArray,
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
            let all_type_params = type_params.connect_single(object_ty.clone());
            let thunk_fct_id = ensure_thunk(
                vm,
                class_instance.id(),
                trait_fct_id,
                type_params.clone(),
                object_ty.clone(),
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
    type_params: &SourceTypeArray,
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

fn ensure_thunk(
    vm: &VM,
    cls_def_id: ClassInstanceId,
    fct_id: FctDefinitionId,
    type_params: SourceTypeArray,
    actual_ty: SourceType,
) -> FctDefinitionId {
    let fct = vm.fcts.idx(fct_id);
    let fct = fct.read();

    let trait_id = fct.parent.trait_id().expect("expected trait");
    let trait_object_ty = SourceType::Trait(trait_id, type_params);

    let thunk_id = fct.thunk_id.write();

    if let Some(thunk_id) = thunk_id.clone() {
        return thunk_id;
    }

    let callee_id = find_trait_impl(vm, fct_id, trait_object_ty.clone(), actual_ty.clone());

    let mut thunk_fct = FctDefinition::new(
        fct.package_id,
        fct.module_id,
        fct.file_id,
        &fct.ast,
        FctParent::None,
    );
    thunk_fct.type_params = fct.type_params.clone();

    let tp_name = vm.interner.intern("new_self");
    let tp_id = thunk_fct.type_params.add_type_param(tp_name);
    thunk_fct
        .type_params
        .add_bound(tp_id, trait_object_ty.clone());
    thunk_fct.bytecode = Some(generate_bytecode_for_thunk(
        cls_def_id,
        &*fct,
        trait_object_ty.clone(),
        &mut thunk_fct,
        callee_id,
        actual_ty,
    ));
    thunk_fct.analysis = Some(AnalysisData::new());

    let mut param_types: Vec<SourceType> = vec![trait_object_ty];
    param_types.extend_from_slice(fct.params_without_self());
    thunk_fct.param_types = param_types;
    thunk_fct.return_type = fct.return_type.clone();
    let thunk_fct_id = vm.add_fct(thunk_fct);

    thunk_fct_id
}

fn generate_bytecode_for_thunk(
    cls_def_id: ClassInstanceId,
    trait_fct: &FctDefinition,
    trait_object_ty: SourceType,
    thunk_fct: &mut FctDefinition,
    _callee_id: FctDefinitionId,
    actual_ty: SourceType,
) -> BytecodeFunction {
    let mut gen = BytecodeBuilder::new();
    gen.push_scope();
    gen.alloc_var(register_bty_from_ty(trait_object_ty));

    for param_ty in trait_fct.params_without_self() {
        if !param_ty.is_unit() {
            let ty = register_bty_from_ty(param_ty.clone());
            gen.alloc_var(ty);
        }
    }

    gen.set_arguments(trait_fct.params_with_self().len() as u32);

    if !actual_ty.is_unit() {
        let ty = register_bty_from_ty(actual_ty.clone());
        let new_self_reg = gen.alloc_var(ty);
        let field_idx = gen.add_const_field_fixed(cls_def_id, 0.into());
        gen.emit_load_field(new_self_reg, Register(0), field_idx, trait_fct.pos);
        gen.emit_push_register(new_self_reg);
    }

    for (idx, _) in trait_fct.params_without_self().iter().enumerate() {
        gen.emit_push_register(Register(1 + idx));
    }

    let type_param_id = TypeParamId(thunk_fct.type_params.len() - 1);
    let target_fct_idx =
        gen.add_const_generic(type_param_id, trait_fct.id(), SourceTypeArray::empty());

    let ty = register_bty_from_ty(trait_fct.return_type.clone());
    let result_reg = gen.alloc_var(ty);
    gen.emit_invoke_generic_direct(result_reg, target_fct_idx, trait_fct.pos);
    gen.emit_ret(result_reg);

    gen.pop_scope();
    gen.generate()
}
