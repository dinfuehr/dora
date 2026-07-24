use std::collections::HashMap;

use dora_bytecode::FunctionId;
use dora_compiler::{CodeDescriptor, Intrinsic, TraitObjectThunkCompilationData};

use crate::codegen::CannonCodeGen;

pub fn compile(
    compilation_data: TraitObjectThunkCompilationData<'_>,
    intrinsics: &HashMap<FunctionId, Intrinsic>,
) -> CodeDescriptor {
    CannonCodeGen::new_for_trait_object_thunk(compilation_data, intrinsics)
        .generate_trait_object_thunk()
}
