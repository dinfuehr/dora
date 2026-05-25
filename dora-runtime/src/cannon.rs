use self::codegen::CannonCodeGen;

use crate::compiler::CompilationData;
use crate::vm::{CodeDescriptor, VM};

pub mod asm;
pub mod codegen;

pub(super) fn compile<'a>(vm: &'a VM, compilation_data: CompilationData<'a>) -> CodeDescriptor {
    CannonCodeGen::new(Some(vm), compilation_data).generate()
}

pub(super) fn compile_without_vm<'a>(compilation_data: CompilationData<'a>) -> CodeDescriptor {
    CannonCodeGen::new(None, compilation_data).generate()
}
