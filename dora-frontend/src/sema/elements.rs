use std::rc::Rc;

use dora_parser::ast;

use crate::sema::{
    AliasDefinition, AliasDefinitionId, AliasParent, ClassDefinitionId, ConstDefinitionId,
    EnumDefinitionId, ExtensionDefinitionId, FctDefinition, FctDefinitionId, FctParent,
    GlobalDefinitionId, ImplDefinition, ImplDefinitionId, ModuleDefinitionId, PackageDefinitionId,
    Sema, SourceFileId, StructDefinitionId, TraitDefinition, TraitDefinitionId,
    TypeParamDefinition, UseDefinitionId, Visibility,
};
use crate::{Name, SourceType, Span};

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
    fn span(&self) -> Span;
    fn module_id(&self) -> ModuleDefinitionId;
    fn package_id(&self) -> PackageDefinitionId;
    fn type_param_definition(&self) -> &Rc<TypeParamDefinition>;
    fn visibility(&self) -> Visibility;

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

    fn is_alias(&self) -> bool {
        self.to_alias().is_some()
    }
    fn to_alias(&self) -> Option<&AliasDefinition> {
        None
    }

    fn self_ty(&self, sa: &Sema) -> Option<SourceType>;
}

pub trait ElementAccess: Element {
    type Id;

    fn by_id(sa: &Sema, id: Self::Id) -> &Self;
    fn id(&self) -> Self::Id;
}

pub trait ElementWithFields {
    fn field_name_style(&self) -> ast::FieldNameStyle;
    fn fields_len(&self) -> usize;
    fn fields<'a>(&'a self) -> Box<dyn DoubleEndedIterator<Item = ElementField> + 'a>;
    fn field_name(&self, idx: usize) -> Option<Name>;
}

pub struct ElementField {
    pub id: usize,
    pub name: Option<Name>,
    pub ty: SourceType,
}

pub fn parent_element_or_self<'a>(sa: &'a Sema, element: &'a dyn Element) -> &'a dyn Element {
    if let Some(fct) = element.to_fct() {
        match fct.parent {
            FctParent::Extension(id) => sa.extension(id),
            FctParent::Impl(id) => sa.impl_(id),
            FctParent::Trait(id) => sa.trait_(id),
            FctParent::Function => unreachable!(),
            FctParent::None => element,
        }
    } else if let Some(alias) = element.to_alias() {
        match alias.parent {
            AliasParent::Impl(id) => sa.impl_(id),
            AliasParent::Trait(id) => sa.trait_(id),
            AliasParent::None => element,
        }
    } else {
        element
    }
}
