use crate::vm::{find_trait_impl, VM};
use dora_bytecode::{
    BytecodeBuilder, BytecodeFunction, BytecodeType, BytecodeTypeArray, FunctionId, Register,
    TraitId,
};
use dora_frontend::language::generator::{register_bty_from_bty, ty_from_bty};
use dora_frontend::language::sem_analysis::{
    AnalysisData, FctDefinition, FctDefinitionId, FctParent, TypeParamId,
};
use dora_frontend::language::ty::SourceType;

pub fn ensure(
    vm: &VM,
    fct_id: FunctionId,
    type_params: BytecodeTypeArray,
    actual_ty: BytecodeType,
) -> FunctionId {
    let fct = vm.fcts.idx(FctDefinitionId(fct_id.0 as usize));
    let fct = fct.read();

    let trait_id = fct.parent.trait_id().expect("expected trait");
    let trait_object_ty = BytecodeType::Trait(TraitId(trait_id.0), type_params);

    let callee_id = find_trait_impl(
        vm,
        fct_id,
        ty_from_bty(trait_object_ty.clone()),
        ty_from_bty(actual_ty.clone()),
    );

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
        .add_bound(tp_id, ty_from_bty(trait_object_ty.clone()));
    let trait_object_type_param_id = TypeParamId(thunk_fct.type_params.len() - 1);
    thunk_fct.bytecode = Some(generate_bytecode_for_thunk(
        vm,
        fct_id,
        trait_object_ty.clone(),
        trait_object_type_param_id,
        callee_id,
        actual_ty,
    ));
    thunk_fct.analysis = Some(AnalysisData::new());

    let mut param_types: Vec<SourceType> = vec![ty_from_bty(trait_object_ty)];
    param_types.extend_from_slice(fct.params_without_self());
    thunk_fct.param_types = param_types;
    thunk_fct.return_type = fct.return_type.clone();
    let thunk_fct_id = vm.add_fct(thunk_fct);

    FunctionId(thunk_fct_id.0 as u32)
}

fn generate_bytecode_for_thunk(
    vm: &VM,
    fct_id: FunctionId,
    trait_object_ty: BytecodeType,
    trait_object_type_param_id: TypeParamId,
    _callee_id: FunctionId,
    actual_ty: BytecodeType,
) -> BytecodeFunction {
    let program_trait_fct = &vm.program.functions[fct_id.0 as usize];

    let mut gen = BytecodeBuilder::new();
    gen.push_scope();
    gen.alloc_var(register_bty_from_bty(trait_object_ty));

    for param_ty in program_trait_fct.params.iter().skip(1) {
        if !param_ty.is_unit() {
            let ty = register_bty_from_bty(param_ty.clone());
            gen.alloc_var(ty);
        }
    }

    gen.set_arguments(program_trait_fct.params.len() as u32);

    if !actual_ty.is_unit() {
        let ty = register_bty_from_bty(actual_ty.clone());
        let new_self_reg = gen.alloc_var(ty);
        gen.emit_load_trait_object_value(new_self_reg, Register(0));
        gen.emit_push_register(new_self_reg);
    }

    for (idx, _) in program_trait_fct.params.iter().enumerate().skip(1) {
        gen.emit_push_register(Register(idx));
    }

    let target_fct_idx = gen.add_const_generic(
        trait_object_type_param_id.0 as u32,
        fct_id,
        BytecodeTypeArray::empty(),
    );

    let ty = register_bty_from_bty(program_trait_fct.return_type.clone());
    let result_reg = gen.alloc_var(ty);
    gen.emit_invoke_generic_direct(result_reg, target_fct_idx, program_trait_fct.loc);
    gen.emit_ret(result_reg);

    gen.pop_scope();
    gen.generate()
}
