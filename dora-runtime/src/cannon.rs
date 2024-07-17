use self::codegen::CannonCodeGen;

use crate::compiler::{CompilationData, CompilationMode};
use crate::vm::{CodeDescriptor, VM};

pub mod asm;
pub mod codegen;

pub(super) fn compile<'a>(
    vm: &'a VM,
    compilation_data: CompilationData<'a>,
    mode: CompilationMode,
) -> CodeDescriptor {
    CannonCodeGen::new(vm, compilation_data, mode).generate()
}
