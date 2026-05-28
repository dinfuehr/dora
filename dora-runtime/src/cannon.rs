use self::codegen::CannonCodeGen;

use crate::compiler::CompilationData;
use crate::compiler::aot::AotCodegenContext;
use crate::vm::CodeDescriptor;

pub mod asm;
pub mod codegen;

pub(super) fn compile<'a>(
    ctx: &AotCodegenContext<'_>,
    compilation_data: CompilationData<'a>,
) -> CodeDescriptor {
    CannonCodeGen::new(compilation_data, ctx.intrinsics()).generate()
}
