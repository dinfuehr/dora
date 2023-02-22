use crate::bytecode::{BytecodeBuilder, BytecodeFunction, Register};
use crate::compiler::asm::BaselineAssembler;
use crate::cpu::STACK_FRAME_ALIGNMENT;
use crate::language::generator::register_bty_from_ty;
use crate::language::sem_analysis::{
    AnalysisData, FctDefinition, FctDefinitionId, FctParent, TypeParamId,
};
use crate::language::ty::{SourceType, SourceTypeArray};
use crate::masm::CodeDescriptor;
use crate::mem::align_i32;
use crate::vm::{find_trait_impl, specialize_type, VM};

pub fn ensure(
    vm: &VM,
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

fn generate_asm_for_thunk(
    vm: &VM,
    trait_fct: &FctDefinition,
    trait_object_ty: SourceType,
) -> CodeDescriptor {
    let mut asm = BaselineAssembler::new(vm);
    let trait_object_type_params = trait_object_ty.type_params();
    let mut stacksize = 0;
    let mut offsets = Vec::new();

    {
        let size = trait_object_ty.size(vm);
        let alignment = trait_object_ty.align(vm);
        stacksize = align_i32(stacksize + size, alignment);
        offsets.push(-stacksize);
    }

    for param_ty in trait_fct.params_without_self() {
        let param_ty = specialize_type(vm, param_ty.clone(), &trait_object_type_params);
        if !param_ty.is_unit() {
            let size = param_ty.size(vm);
            let alignment = param_ty.align(vm);
            stacksize = align_i32(stacksize + size, alignment);
        }
        offsets.push(-stacksize);
    }

    stacksize = align_i32(stacksize, STACK_FRAME_ALIGNMENT as i32);

    asm.prolog(stacksize);

    asm.epilog();
    asm.code()
}

fn generate_bytecode_for_thunk(
    trait_fct: &FctDefinition,
    trait_object_ty: SourceType,
    thunk_fct: &FctDefinition,
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
        gen.emit_load_trait_object_value(new_self_reg, Register(0));
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
