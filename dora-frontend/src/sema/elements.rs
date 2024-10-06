use std::rc::Rc;

use crate::sema::{
    ModuleDefinitionId, PackageDefinitionId, Sema, SourceFileId, TypeParamDefinition,
};

pub trait Element {
    type Id;

    fn by_id(sa: &Sema, id: Self::Id) -> &Self;

    fn id(&self) -> Self::Id;
    fn file_id(&self) -> SourceFileId;
    fn module_id(&self) -> ModuleDefinitionId;
    fn package_id(&self) -> PackageDefinitionId;
}

pub trait ElementWithTypeParams: Element {
    fn type_param_definition(&self) -> &Rc<TypeParamDefinition>;
}
