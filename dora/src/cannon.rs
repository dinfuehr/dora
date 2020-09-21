use self::codegen::CannonCodeGen;

use crate::bytecode::{self, BytecodeFunction};
use crate::compiler::codegen::should_emit_bytecode;
use crate::compiler::Code;
use crate::ty::TypeList;
use crate::vm::{Fct, FctSrc, VM};

mod codegen;

pub(super) fn compile<'a, 'ast: 'a>(
    vm: &'a VM<'ast>,
    fct: &Fct<'ast>,
    src: &'a FctSrc,
    type_params: &TypeList,
) -> Code {
    if vm.args.flag_generic_bytecode {
        let bytecode_fct = fct.bytecode.as_ref().expect("bytecode missing");
        compile_from_bytecode(vm, fct, src, type_params, &bytecode_fct)
    } else {
        let bytecode_fct = bytecode::generate(vm, fct, src, type_params);
        compile_from_bytecode(vm, fct, src, type_params, &bytecode_fct)
    }
}

fn compile_from_bytecode<'a, 'ast: 'a>(
    vm: &'a VM<'ast>,
    fct: &Fct<'ast>,
    src: &'a FctSrc,
    type_params: &TypeList,
    bytecode_fct: &BytecodeFunction,
) -> Code {
    if should_emit_bytecode(vm, fct) {
        bytecode::dump(vm, bytecode_fct);
    }

    CannonCodeGen::new(vm, fct, src, bytecode_fct, type_params).generate()
}
