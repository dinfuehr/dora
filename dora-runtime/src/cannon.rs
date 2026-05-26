use self::codegen::CannonCodeGen;

use crate::compiler::CompilationData;
use crate::compiler::aot::AotCodegenContext;
use crate::vm::{CodeDescriptor, VM};

pub mod asm;
pub mod codegen;

pub(super) fn compile<'a>(vm: &'a VM, compilation_data: CompilationData<'a>) -> CodeDescriptor {
    CannonCodeGen::new(Some(vm), compilation_data, None).generate()
}

pub(super) fn compile_without_vm<'a>(
    ctx: &AotCodegenContext<'_>,
    compilation_data: CompilationData<'a>,
) -> CodeDescriptor {
    CannonCodeGen::new(None, compilation_data, Some(ctx.intrinsics())).generate()
}
