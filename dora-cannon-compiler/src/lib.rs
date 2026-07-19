#![allow(dead_code)]

use self::codegen::CannonCodeGen;

use std::cell::RefCell;
use std::collections::HashMap;

use dora_bytecode::FunctionId;
use dora_compiler::{
    AotBackend, AotCodegenContext, AotContextGuard, CodeDescriptor, CompilationData, Intrinsic,
    TraitObjectThunkCompilationData,
};

pub mod asm;
pub mod codegen;
mod masm;
mod trait_object_thunk;

pub struct CannonAotBackend {
    intrinsics: RefCell<Option<HashMap<FunctionId, Intrinsic>>>,
}

impl CannonAotBackend {
    pub fn new() -> CannonAotBackend {
        CannonAotBackend {
            intrinsics: RefCell::new(None),
        }
    }
}

struct CannonAotContextGuard;

impl AotContextGuard for CannonAotContextGuard {}

impl AotBackend for CannonAotBackend {
    fn enter_context<'ctx>(
        &self,
        ctx: &'ctx AotCodegenContext<'_>,
    ) -> Box<dyn AotContextGuard + 'ctx> {
        self.intrinsics.replace(Some(ctx.intrinsics().clone()));
        Box::new(CannonAotContextGuard)
    }

    fn compile<'a>(
        &self,
        compilation_data: CompilationData<'a>,
        ctx: &AotCodegenContext<'_>,
    ) -> CodeDescriptor {
        CannonCodeGen::new(compilation_data, ctx.intrinsics()).generate()
    }

    fn compile_trait_object_thunk<'a>(
        &self,
        compilation_data: TraitObjectThunkCompilationData<'a>,
    ) -> CodeDescriptor {
        let intrinsics = self.intrinsics.borrow();
        let intrinsics = intrinsics.as_ref().expect("AOT context not entered");
        trait_object_thunk::compile(compilation_data, intrinsics)
    }
}
