use std::cell::{OnceCell, RefCell};
use std::collections::HashMap;
use std::rc::Rc;
use std::sync::Arc;

use crate::interner::Name;
use crate::program_parser::ParsedModifierList;
use crate::ParsedType;
use dora_parser::ast;
use dora_parser::Span;

use id_arena::Id;

use crate::sema::{
    module_path, Element, ElementAccess, ElementId, ExtensionDefinitionId, ModuleDefinitionId,
    PackageDefinitionId, Sema, SourceFileId, TypeParamDefinition, Visibility,
};
use crate::{SourceType, SourceTypeArray};

use super::new_identity_type_params;

pub type EnumDefinitionId = Id<EnumDefinition>;

#[derive(Debug)]
pub struct EnumDefinition {
    pub id: Option<EnumDefinitionId>,
    pub package_id: PackageDefinitionId,
    pub module_id: ModuleDefinitionId,
    pub file_id: SourceFileId,
    pub ast: Arc<ast::Enum>,
    pub span: Span,
    pub name: Name,
    pub visibility: Visibility,
    pub type_param_definition: Rc<TypeParamDefinition>,
    pub variants: Vec<EnumVariant>,
    pub extensions: RefCell<Vec<ExtensionDefinitionId>>,
    pub simple_enumeration: OnceCell<bool>,
    pub name_to_value: HashMap<Name, u32>,
}

impl EnumDefinition {
    pub(crate) fn new(
        package_id: PackageDefinitionId,
        module_id: ModuleDefinitionId,
        file_id: SourceFileId,
        node: &Arc<ast::Enum>,
        modifiers: ParsedModifierList,
        name: Name,
        type_param_definition: Rc<TypeParamDefinition>,
        variants: Vec<EnumVariant>,
        name_to_value: HashMap<Name, u32>,
    ) -> EnumDefinition {
        EnumDefinition {
            id: None,
            package_id,
            module_id,
            file_id,
            ast: node.clone(),
            span: node.span,
            name,
            type_param_definition,
            visibility: modifiers.visibility(),
            variants,
            extensions: RefCell::new(Vec::new()),
            simple_enumeration: OnceCell::new(),
            name_to_value,
        }
    }

    pub fn id(&self) -> EnumDefinitionId {
        self.id.expect("id missing")
    }

    pub fn type_param_definition(&self) -> &Rc<TypeParamDefinition> {
        &self.type_param_definition
    }

    pub fn name_to_value(&self) -> &HashMap<Name, u32> {
        &self.name_to_value
    }

    pub fn is_simple_enum(&self) -> bool {
        self.simple_enumeration
            .get()
            .expect("uninitialized")
            .clone()
    }

    pub fn name(&self, sa: &Sema) -> String {
        module_path(sa, self.module_id, self.name)
    }

    pub fn name_with_params(&self, sa: &Sema, type_list: &SourceTypeArray) -> String {
        let name = sa.interner.str(self.name);

        if type_list.len() > 0 {
            let type_list = type_list
                .iter()
                .map(|p| p.name(sa))
                .collect::<Vec<_>>()
                .join(", ");

            format!("{}[{}]", name, type_list)
        } else {
            name.to_string()
        }
    }

    pub fn variants(&self) -> &[EnumVariant] {
        &self.variants
    }
}

impl Element for EnumDefinition {
    fn element_id(&self) -> ElementId {
        ElementId::Enum(self.id())
    }

    fn file_id(&self) -> SourceFileId {
        self.file_id
    }

    fn span(&self) -> Span {
        self.span
    }

    fn module_id(&self) -> ModuleDefinitionId {
        self.module_id
    }

    fn package_id(&self) -> PackageDefinitionId {
        self.package_id
    }

    fn type_param_definition(&self) -> Option<&Rc<TypeParamDefinition>> {
        Some(&self.type_param_definition)
    }

    fn self_ty(&self, _sa: &Sema) -> Option<SourceType> {
        let type_params = self.type_param_definition().type_param_count();
        let type_params = new_identity_type_params(type_params);
        Some(SourceType::Enum(self.id(), type_params))
    }
}

impl ElementAccess for EnumDefinition {
    type Id = EnumDefinitionId;

    fn by_id(sa: &Sema, id: Self::Id) -> &Self {
        sa.enum_(id)
    }

    fn id(&self) -> Self::Id {
        self.id.expect("missing id")
    }
}

#[derive(Debug)]
pub struct EnumVariant {
    pub id: u32,
    pub name: Name,
    pub parsed_types: Vec<ParsedType>,
}

impl EnumVariant {
    pub fn parsed_types(&self) -> &Vec<ParsedType> {
        &self.parsed_types
    }
}
