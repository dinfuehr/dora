use crate::compiler::register_ty;
use crate::vm::specialize_bty_for_trait_object;
use dora_bytecode::{
    BytecodeFunction, BytecodeTraitType, BytecodeType, BytecodeTypeArray, BytecodeWriter,
    ConstPoolEntry, FunctionId, Program, Register,
};

pub(super) fn generate_bytecode_for_thunk(
    program: &Program,
    fct_id: FunctionId,
    trait_object_ty: BytecodeType,
    trait_object_type_param_id: usize,
    actual_ty: BytecodeType,
) -> BytecodeFunction {
    let program_trait_fct = program.fct(fct_id);

    let (trait_id, trait_type_params, trait_assoc_types) = match &trait_object_ty {
        BytecodeType::TraitObject(trait_id, trait_type_params, trait_assoc_types) => {
            (*trait_id, trait_type_params, trait_assoc_types)
        }
        _ => unreachable!(),
    };

    let mut w = BytecodeWriter::new();
    w.add_register(register_ty(trait_object_ty.clone()));

    for param_ty in program_trait_fct.params.iter().skip(1) {
        let param_ty = specialize_bty_for_trait_object(
            program,
            param_ty.clone(),
            trait_id,
            trait_type_params,
            trait_assoc_types,
        );
        w.add_register(register_ty(param_ty));
    }

    w.set_arguments(program_trait_fct.params.len() as u32 + 1);

    let actual_ty = register_ty(actual_ty.clone());
    let new_self_reg = w.add_register(actual_ty);
    w.emit_load_trait_object_value(new_self_reg, Register(0));

    let mut arguments = vec![new_self_reg];
    for (idx, _) in program_trait_fct.params.iter().enumerate().skip(1) {
        arguments.push(Register(idx));
    }

    let target_fct_idx = w.add_const(ConstPoolEntry::Generic {
        object_type: BytecodeType::TypeParam(
            trait_object_type_param_id.try_into().expect("overflow"),
        ),
        trait_ty: BytecodeTraitType {
            trait_id,
            type_params: trait_type_params.clone(),
            bindings: Vec::new(),
        },
        fct_id,
        fct_type_params: BytecodeTypeArray::empty(),
    });

    let return_ty = specialize_bty_for_trait_object(
        program,
        program_trait_fct.return_type.clone(),
        trait_id,
        trait_type_params,
        trait_assoc_types,
    );
    let return_ty = register_ty(return_ty);
    w.set_return_type(return_ty.clone());
    let result_reg = w.add_register(return_ty);
    w.set_location(program_trait_fct.loc);
    w.emit_invoke_generic_direct(result_reg, target_fct_idx, &arguments);
    w.emit_ret(result_reg);

    w.generate()
}
