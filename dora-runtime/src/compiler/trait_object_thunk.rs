use crate::cannon::codegen::register_ty;
use crate::compiler;
use crate::gc::Address;
use crate::vm::VM;
use dora_bytecode::{
    BytecodeBuilder, BytecodeFunction, BytecodeType, BytecodeTypeArray, FunctionId, FunctionKind,
    Register,
};

pub fn ensure_compiled(
    vm: &VM,
    trait_fct_id: FunctionId,
    type_params: BytecodeTypeArray,
    actual_ty: BytecodeType,
) -> Address {
    let all_type_params = type_params.append(actual_ty.clone());
    let trait_fct = &vm.program.functions[trait_fct_id.0 as usize];
    let trait_object_type_param_id = all_type_params.len() - 1;

    let trait_id = match trait_fct.kind {
        FunctionKind::Trait(trait_id) => trait_id,
        _ => unreachable!(),
    };
    let trait_object_ty = BytecodeType::Trait(trait_id, type_params.clone());

    let bytecode = generate_bytecode_for_thunk(
        vm,
        trait_fct_id,
        trait_object_ty.clone(),
        trait_object_type_param_id,
        actual_ty.clone(),
    );

    compiler::codegen::generate_thunk(
        vm,
        trait_fct_id,
        trait_object_ty,
        &all_type_params,
        bytecode,
    )
}

fn generate_bytecode_for_thunk(
    vm: &VM,
    fct_id: FunctionId,
    trait_object_ty: BytecodeType,
    trait_object_type_param_id: usize,
    actual_ty: BytecodeType,
) -> BytecodeFunction {
    let program_trait_fct = &vm.program.functions[fct_id.0 as usize];

    let mut gen = BytecodeBuilder::new();
    gen.push_scope();
    gen.alloc_var(register_ty(trait_object_ty));

    for param_ty in program_trait_fct.params.iter().skip(1) {
        if !param_ty.is_unit() {
            let ty = register_ty(param_ty.clone());
            gen.alloc_var(ty);
        }
    }

    gen.set_arguments(program_trait_fct.params.len() as u32);

    if !actual_ty.is_unit() {
        let ty = register_ty(actual_ty.clone());
        let new_self_reg = gen.alloc_var(ty);
        gen.emit_load_trait_object_value(new_self_reg, Register(0));
        gen.emit_push_register(new_self_reg);
    }

    for (idx, _) in program_trait_fct.params.iter().enumerate().skip(1) {
        gen.emit_push_register(Register(idx));
    }

    let target_fct_idx = gen.add_const_generic(
        trait_object_type_param_id.try_into().expect("does not fit"),
        fct_id,
        BytecodeTypeArray::empty(),
    );

    let ty = register_ty(program_trait_fct.return_type.clone());
    let result_reg = gen.alloc_var(ty);
    gen.emit_invoke_generic_direct(result_reg, target_fct_idx, program_trait_fct.loc);
    gen.emit_ret(result_reg);

    gen.pop_scope();
    gen.generate()
}
