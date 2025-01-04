use std::sync::Arc;

use crate::cannon::codegen::register_ty;
use crate::compiler::codegen::{compile_fct_to_code, select_compiler};
use crate::compiler::CompilationMode;
use crate::gc::Address;
use crate::vm::{specialize_bty_for_trait_object, BytecodeTypeExt, Code, CodeId, Compiler, VM};
use dora_bytecode::{
    BytecodeFunction, BytecodeType, BytecodeTypeArray, BytecodeWriter, ConstPoolEntry, FunctionId,
    FunctionKind, Register,
};

use super::codegen::CompilerInvocation;

pub fn ensure_compiled_jit(
    vm: &VM,
    trait_fct_id: FunctionId,
    trait_object_ty: BytecodeType,
    actual_ty: BytecodeType,
) -> Address {
    let trait_type_params_with_actual_ty = trait_object_ty.type_params().append(actual_ty.clone());

    // Block here if compilation is already in progress.
    if let Some(instruction_start) = vm.compilation_database.compilation_request(
        vm,
        trait_fct_id,
        trait_type_params_with_actual_ty.clone(),
    ) {
        return instruction_start;
    }

    let trait_fct = vm.fct(trait_fct_id);
    let compiler = select_compiler(vm, trait_fct_id, trait_fct);
    let compiler = match compiler {
        Compiler::Cannon => CompilerInvocation::Cannon,
        Compiler::Boots => {
            let compile_address = vm.known.boots_compile_fct_address();
            CompilerInvocation::Boots(compile_address)
        }
    };

    let (code_id, code) = compile_thunk_to_code(
        vm,
        trait_fct_id,
        &trait_type_params_with_actual_ty,
        trait_object_ty,
        actual_ty,
        compiler,
        vm.flags.emit_compiler,
        CompilationMode::Jit,
    );

    // Mark compilation as finished and resume threads waiting for compilation.
    vm.compilation_database.finish_compilation(
        trait_fct_id,
        trait_type_params_with_actual_ty.clone(),
        code_id,
    );

    code.instruction_start()
}

pub fn ensure_compiled_aot(
    vm: &VM,
    trait_fct_id: FunctionId,
    trait_type_params: BytecodeTypeArray,
    actual_ty: BytecodeType,
    compiler: CompilerInvocation,
    mode: CompilationMode,
) -> (CodeId, Arc<Code>) {
    let trait_object_ty = trait_object_ty(vm, trait_fct_id, &trait_type_params);
    let all_type_params = trait_type_params.append(actual_ty.clone());
    let (code_id, code) = compile_thunk_to_code(
        vm,
        trait_fct_id,
        &all_type_params,
        trait_object_ty,
        actual_ty,
        compiler,
        false,
        mode,
    );

    (code_id, code)
}

fn trait_object_ty(
    vm: &VM,
    trait_fct_id: FunctionId,
    type_params: &BytecodeTypeArray,
) -> BytecodeType {
    let trait_fct = vm.fct(trait_fct_id);

    let trait_id = match trait_fct.kind {
        FunctionKind::Trait(trait_id) => trait_id,
        _ => unreachable!(),
    };

    BytecodeType::TraitObject(trait_id, type_params.clone(), BytecodeTypeArray::empty())
}

fn compile_thunk_to_code(
    vm: &VM,
    trait_fct_id: FunctionId,
    type_params: &BytecodeTypeArray,
    trait_object_ty: BytecodeType,
    actual_ty: BytecodeType,
    compiler: CompilerInvocation,
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

    let trait_fct = vm.fct(trait_fct_id);
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
        bytecode_fct.return_type().clone(),
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
    let program_trait_fct = vm.fct(fct_id);

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
            &vm.program,
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
    w.emit_push_register(new_self_reg);

    for (idx, _) in program_trait_fct.params.iter().enumerate().skip(1) {
        w.emit_push_register(Register(idx));
    }

    let target_fct_idx = w.add_const(ConstPoolEntry::Generic(
        trait_object_type_param_id.try_into().expect("does not fit"),
        fct_id,
        trait_object_ty.type_params(),
    ));

    let return_ty = specialize_bty_for_trait_object(
        &vm.program,
        program_trait_fct.return_type.clone(),
        trait_id,
        trait_type_params,
        trait_assoc_types,
    );
    let return_ty = register_ty(return_ty);
    w.set_return_type(return_ty.clone());
    let result_reg = w.add_register(return_ty);
    w.set_location(program_trait_fct.loc);
    w.emit_invoke_generic_direct(result_reg, target_fct_idx);
    w.emit_ret(result_reg);

    w.generate()
}
