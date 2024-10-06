use crate::sema::{ModuleDefinitionId, PackageDefinitionId, SourceFileId, TypeParamDefinition};

pub trait Element {
    fn file_id(&self) -> SourceFileId;
    fn module_id(&self) -> ModuleDefinitionId;
    fn package_id(&self) -> PackageDefinitionId;
}

pub trait ElementWithTypeParams: Element {
    fn type_param_definition(&self) -> &TypeParamDefinition;
}
