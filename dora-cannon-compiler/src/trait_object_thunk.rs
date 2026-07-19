use std::collections::HashMap;

use dora_bytecode::{
    BytecodeBody, BytecodeTraitType, BytecodeType, BytecodeTypeArray, BytecodeWriter,
    ConstPoolEntry, FunctionId, Register,
};
use dora_compiler::{
    CodeDescriptor, CompilationData, Intrinsic, TraitObjectThunkCompilationData, register_ty,
};

use crate::codegen::CannonCodeGen;

pub fn compile(
    compilation_data: TraitObjectThunkCompilationData<'_>,
    intrinsics: &HashMap<FunctionId, Intrinsic>,
) -> CodeDescriptor {
    let bytecode_body = generate_bytecode(&compilation_data);
    let compilation_data = into_bytecode_compilation_data(compilation_data, &bytecode_body);
    CannonCodeGen::new(compilation_data, intrinsics).generate()
}

fn generate_bytecode(compilation_data: &TraitObjectThunkCompilationData<'_>) -> BytecodeBody {
    let trait_object_type_param_id = compilation_data.signature.type_params.len() - 1;
    assert_eq!(
        &compilation_data.signature.type_params[trait_object_type_param_id],
        &compilation_data.actual_object_ty
    );

    let (trait_id, trait_type_params) = match &compilation_data.trait_object_ty {
        BytecodeType::TraitObject(trait_id, trait_type_params, _) => (*trait_id, trait_type_params),
        _ => unreachable!(),
    };

    let mut w = BytecodeWriter::new();
    for param_ty in compilation_data.signature.params.iter() {
        w.add_register(register_ty(param_ty));
    }

    let actual_ty = register_ty(compilation_data.actual_object_ty.clone());
    let new_self_reg = w.add_register(actual_ty);
    w.emit_load_trait_object_value(new_self_reg, Register(0));

    let mut arguments = vec![new_self_reg];
    for idx in 1..compilation_data.signature.params.len() {
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
        fct_id: compilation_data.trait_fct_id,
        fct_type_params: BytecodeTypeArray::empty(),
    });

    let return_ty = register_ty(compilation_data.signature.return_type.clone());
    let result_reg = w.add_register(return_ty);
    w.set_location(compilation_data.loc);
    w.emit_invoke_generic_direct(result_reg, target_fct_idx, &arguments);
    w.emit_ret(result_reg);

    w.generate()
}

fn into_bytecode_compilation_data<'a, 'b>(
    compilation_data: TraitObjectThunkCompilationData<'a>,
    bytecode_body: &'b BytecodeBody,
) -> CompilationData<'b>
where
    'a: 'b,
{
    CompilationData {
        program: compilation_data.program,
        bytecode_body,
        fct_id: compilation_data.trait_fct_id,
        signature: compilation_data.signature,
        loc: compilation_data.loc,
        options: compilation_data.options,
    }
}
