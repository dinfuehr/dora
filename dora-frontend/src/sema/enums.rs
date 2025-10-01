use std::cell::{OnceCell, RefCell};
use std::collections::HashMap;
use std::rc::Rc;

use crate::interner::Name;
use crate::program_parser::ParsedModifierList;
use dora_parser::Span;
use dora_parser::ast;

use id_arena::Id;

use crate::sema::{
    Element, ElementAccess, ElementId, ElementWithFields, ExtensionDefinitionId, FieldDefinitionId,
    FieldIndex, ModuleDefinitionId, PackageDefinitionId, Sema, SourceFileId, TypeParamDefinition,
    Visibility, module_path,
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
    pub ast_id: ast::AstId,
    pub span: Span,
    pub name: Name,
    pub visibility: Visibility,
    pub type_param_definition: Rc<TypeParamDefinition>,
    pub variants: OnceCell<Vec<VariantDefinitionId>>,
    pub extensions: RefCell<Vec<ExtensionDefinitionId>>,
    pub simple_enumeration: OnceCell<bool>,
    pub name_to_value: OnceCell<HashMap<Name, u32>>,
}

impl EnumDefinition {
    pub(crate) fn new(
        package_id: PackageDefinitionId,
        module_id: ModuleDefinitionId,
        file_id: SourceFileId,
        ast_id: ast::AstId,
        node: &ast::Enum,
        modifiers: ParsedModifierList,
        name: Name,
        type_param_definition: Rc<TypeParamDefinition>,
    ) -> EnumDefinition {
        EnumDefinition {
            id: None,
            package_id,
            module_id,
            file_id,
            ast_id,
            span: node.span,
            name,
            type_param_definition,
            visibility: modifiers.visibility(),
            variants: OnceCell::new(),
            extensions: RefCell::new(Vec::new()),
            simple_enumeration: OnceCell::new(),
            name_to_value: OnceCell::new(),
        }
    }

    pub fn id(&self) -> EnumDefinitionId {
        self.id.expect("id missing")
    }

    pub fn ast<'a>(&self, sa: &'a Sema) -> &'a ast::Enum {
        sa.file(self.file_id())
            .node(self.ast_id)
            .to_enum()
            .expect("class expected")
    }

    pub fn name_to_value(&self) -> &HashMap<Name, u32> {
        self.name_to_value.get().expect("missing name_to_value")
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

    pub fn variant_ids(&self) -> &[VariantDefinitionId] {
        self.variants.get().expect("missing variants")
    }

    pub fn variant_id_at(&self, idx: usize) -> VariantDefinitionId {
        self.variant_ids()[idx]
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

    fn type_param_definition(&self) -> &Rc<TypeParamDefinition> {
        &self.type_param_definition
    }

    fn self_ty(&self, _sa: &Sema) -> Option<SourceType> {
        let type_params = self.type_param_definition().type_param_count();
        let type_params = new_identity_type_params(0, type_params);
        Some(SourceType::Enum(self.id(), type_params))
    }

    fn visibility(&self) -> Visibility {
        self.visibility
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

pub type VariantDefinitionId = Id<VariantDefinition>;

#[derive(Debug)]
pub struct VariantDefinition {
    pub id: OnceCell<VariantDefinitionId>,
    pub index: u32,
    pub name: Name,
    pub field_name_style: ast::FieldNameStyle,
    pub field_ids: OnceCell<Vec<FieldDefinitionId>>,
}

impl VariantDefinition {
    pub fn field_ids(&self) -> &[FieldDefinitionId] {
        self.field_ids.get().expect("missing fields")
    }

    pub fn field_id(&self, idx: FieldIndex) -> FieldDefinitionId {
        self.field_ids()[idx.to_usize()]
    }
}

impl ElementWithFields for VariantDefinition {
    fn field_name_style(&self) -> ast::FieldNameStyle {
        self.field_name_style
    }

    fn field_ids(&self) -> &[FieldDefinitionId] {
        self.field_ids()
    }
}
