use std::sync::Arc;

use crate::cannon::codegen::register_ty;
use crate::compiler::codegen::{compile_fct_to_code, select_compiler};
use crate::compiler::CompilationMode;
use crate::gc::Address;
use crate::vm::{Code, CodeId, CompilerName, VM};
use dora_bytecode::{
    BytecodeBuilder, BytecodeFunction, BytecodeType, BytecodeTypeArray, FunctionId, FunctionKind,
    Register,
};

pub fn ensure_compiled_jit(
    vm: &VM,
    trait_fct_id: FunctionId,
    trait_type_params: BytecodeTypeArray,
    actual_ty: BytecodeType,
) -> Address {
    let trait_object_ty = trait_object_ty(vm, trait_fct_id, &trait_type_params);
    let all_type_params = trait_type_params.append(actual_ty.clone());

    // Block here if compilation is already in progress.
    if let Some(instruction_start) =
        vm.compilation_database
            .compilation_request(vm, trait_fct_id, all_type_params.clone())
    {
        return instruction_start;
    }

    let trait_fct = &vm.program.functions[trait_fct_id.0 as usize];
    let compiler = select_compiler(vm, trait_fct_id, trait_fct);

    let (code_id, code) = compile_thunk_to_code(
        vm,
        trait_fct_id,
        &all_type_params,
        trait_object_ty,
        actual_ty,
        compiler,
        vm.flags.emit_compiler,
        CompilationMode::Jit,
    );

    // Mark compilation as finished and resume threads waiting for compilation.
    vm.compilation_database
        .finish_compilation(trait_fct_id, all_type_params.clone(), code_id);

    code.instruction_start()
}

pub fn ensure_compiled_aot(
    vm: &VM,
    trait_fct_id: FunctionId,
    trait_type_params: BytecodeTypeArray,
    actual_ty: BytecodeType,
) -> (CodeId, Arc<Code>) {
    let trait_object_ty = trait_object_ty(vm, trait_fct_id, &trait_type_params);
    let all_type_params = trait_type_params.append(actual_ty.clone());
    let (code_id, code) = compile_thunk_to_code(
        vm,
        trait_fct_id,
        &all_type_params,
        trait_object_ty,
        actual_ty,
        CompilerName::Cannon,
        false,
        CompilationMode::Aot,
    );

    vm.compilation_database
        .compile_aot(trait_fct_id, all_type_params.clone(), code_id);

    (code_id, code)
}

fn trait_object_ty(
    vm: &VM,
    trait_fct_id: FunctionId,
    type_params: &BytecodeTypeArray,
) -> BytecodeType {
    let trait_fct = &vm.program.functions[trait_fct_id.0 as usize];

    let trait_id = match trait_fct.kind {
        FunctionKind::Trait(trait_id) => trait_id,
        _ => unreachable!(),
    };

    BytecodeType::Trait(trait_id, type_params.clone())
}

fn compile_thunk_to_code(
    vm: &VM,
    trait_fct_id: FunctionId,
    type_params: &BytecodeTypeArray,
    trait_object_ty: BytecodeType,
    actual_ty: BytecodeType,
    compiler: CompilerName,
    emit_compiler: bool,
    mode: CompilationMode,
) -> (CodeId, Arc<Code>) {
    assert!(type_params.iter().all(|ty| ty.is_concrete_type()));

    let trait_object_type_param_id = type_params.len() - 1;
    assert_eq!(type_params[trait_object_type_param_id], actual_ty);

    let bytecode_fct = generate_bytecode_for_thunk(
        vm,
        trait_fct_id,
        trait_object_ty.clone(),
        trait_object_type_param_id,
        actual_ty.clone(),
    );

    let trait_fct = &vm.program.functions[trait_fct_id.0 as usize];
    let params = {
        let mut params = trait_fct.params.clone();
        assert_eq!(params[0], BytecodeType::This);
        params[0] = trait_object_ty;
        BytecodeTypeArray::new(params)
    };

    compile_fct_to_code(
        vm,
        trait_fct_id,
        trait_fct,
        params,
        &bytecode_fct,
        type_params,
        compiler,
        emit_compiler,
        mode,
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
