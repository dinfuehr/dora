use std::rc::Rc;

use crate::sema::{
    AliasDefinitionId, ClassDefinitionId, ConstDefinitionId, EnumDefinitionId,
    ExtensionDefinitionId, FctDefinition, FctDefinitionId, GlobalDefinitionId, ImplDefinition,
    ImplDefinitionId, ModuleDefinitionId, PackageDefinitionId, Sema, SourceFileId,
    StructDefinitionId, TraitDefinition, TraitDefinitionId, TypeParamDefinition, UseDefinitionId,
};

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum ElementId {
    Alias(AliasDefinitionId),
    Const(ConstDefinitionId),
    Class(ClassDefinitionId),
    Struct(StructDefinitionId),
    Global(GlobalDefinitionId),
    Use(UseDefinitionId),
    Impl(ImplDefinitionId),
    Extension(ExtensionDefinitionId),
    Fct(FctDefinitionId),
    Enum(EnumDefinitionId),
    Trait(TraitDefinitionId),
}

pub trait Element {
    fn element_id(&self) -> ElementId;

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

    fn is_fct(&self) -> bool {
        self.to_trait().is_some()
    }
    fn to_fct(&self) -> Option<&FctDefinition> {
        None
    }

    fn is_impl(&self) -> bool {
        self.to_impl().is_some()
    }
    fn to_impl(&self) -> Option<&ImplDefinition> {
        None
    }
}

pub trait ElementAccess {
    type Id;

    fn by_id(sa: &Sema, id: Self::Id) -> &Self;
    fn id(&self) -> Self::Id;
}
