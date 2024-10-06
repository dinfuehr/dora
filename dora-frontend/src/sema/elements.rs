use std::rc::Rc;

use crate::sema::{
    ModuleDefinitionId, PackageDefinitionId, Sema, SourceFileId, TypeParamDefinition,
};

use super::TraitDefinition;

pub trait Element {
    fn file_id(&self) -> SourceFileId;
    fn module_id(&self) -> ModuleDefinitionId;
    fn package_id(&self) -> PackageDefinitionId;
    fn type_param_definition(&self) -> Option<&Rc<TypeParamDefinition>>;

    fn is_trait(&self) -> bool {
        self.to_trait().is_some()
    }
    fn to_trait(&self) -> Option<&TraitDefinition> {
        None
    }
}

pub trait ElementAccess {
    type Id;

    fn by_id(sa: &Sema, id: Self::Id) -> &Self;
    fn id(&self) -> Self::Id;
}
