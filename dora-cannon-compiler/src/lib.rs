#![allow(dead_code)]

use self::codegen::CannonCodeGen;

use std::collections::HashMap;

use dora_bytecode::FunctionId;
use dora_runtime::compiler::CompilationData;
use dora_runtime::vm::{CodeDescriptor, Intrinsic};

pub mod asm;
pub mod codegen;

pub fn compile<'a>(
    compilation_data: CompilationData<'a>,
    intrinsics: &HashMap<FunctionId, Intrinsic>,
) -> CodeDescriptor {
    CannonCodeGen::new(compilation_data, intrinsics).generate()
}
