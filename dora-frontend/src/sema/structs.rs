use std::cell::{OnceCell, RefCell};
use std::collections::hash_map::HashMap;
use std::rc::Rc;

use id_arena::Id;

use crate::element_parser::ParsedModifierList;
use crate::interner::Name;
use dora_parser::Span;
use dora_parser::ast;

use crate::sema::{
    Element, ElementAccess, ElementId, ElementWithFields, ExtensionDefinitionId, FieldDefinitionId,
    FieldIndex, ModuleDefinitionId, PackageDefinitionId, Sema, SourceFileId, TypeParamDefinition,
    Visibility, module_path, new_identity_type_params,
};
use crate::{SourceType, SourceTypeArray};

pub type StructDefinitionId = Id<StructDefinition>;

#[derive(Debug)]
pub struct StructDefinition {
    pub id: Option<StructDefinitionId>,
    pub package_id: PackageDefinitionId,
    pub module_id: ModuleDefinitionId,
    pub file_id: SourceFileId,
    pub ast_id: ast::AstId,
    pub primitive_ty: Option<SourceType>,
    pub type_param_definition: Rc<TypeParamDefinition>,
    pub visibility: Visibility,
    pub is_internal: bool,
    pub internal_resolved: bool,
    pub span: Span,
    pub name: Name,
    pub field_ids: OnceCell<Vec<FieldDefinitionId>>,
    pub field_names: OnceCell<HashMap<Name, FieldIndex>>,
    pub extensions: RefCell<Vec<ExtensionDefinitionId>>,
    pub field_name_style: ast::FieldNameStyle,
}

impl StructDefinition {
    pub(crate) fn new(
        package_id: PackageDefinitionId,
        module_id: ModuleDefinitionId,
        file_id: SourceFileId,
        ast_id: ast::AstId,
        node: &ast::Struct,
        modifiers: ParsedModifierList,
        name: Name,
        type_param_definition: Rc<TypeParamDefinition>,
    ) -> StructDefinition {
        StructDefinition {
            id: None,
            package_id,
            module_id,
            file_id,
            ast_id,
            primitive_ty: None,
            visibility: modifiers.visibility(),
            span: node.span,
            name,
            is_internal: modifiers.is_internal,
            internal_resolved: false,
            type_param_definition,
            field_ids: OnceCell::new(),
            field_names: OnceCell::new(),
            extensions: RefCell::new(Vec::new()),
            field_name_style: node.field_style,
        }
    }

    pub fn id(&self) -> StructDefinitionId {
        self.id.expect("missing id")
    }

    pub fn ast<'a>(&self, sa: &'a Sema) -> &'a ast::Struct {
        sa.file(self.file_id())
            .node(self.ast_id)
            .to_struct()
            .expect("class expected")
    }

    pub fn name(&self, sa: &Sema) -> String {
        module_path(sa, self.module_id, self.name)
    }

    pub fn name_with_params(&self, sa: &Sema, type_params: &SourceTypeArray) -> String {
        let mut name = self.name(sa);

        if type_params.len() > 0 {
            let type_params = type_params
                .iter()
                .map(|p| p.name(sa))
                .collect::<Vec<_>>()
                .join(", ");

            name.push('[');
            name.push_str(&type_params);
            name.push(']');
        }

        name
    }

    pub fn ty(&self) -> SourceType {
        if let Some(ref primitive_ty) = self.primitive_ty {
            primitive_ty.clone()
        } else {
            SourceType::Struct(
                self.id(),
                new_identity_type_params(0, self.type_param_definition().type_param_count()),
            )
        }
    }

    pub fn field_names(&self) -> &HashMap<Name, FieldIndex> {
        self.field_names.get().expect("missing field_names")
    }

    pub fn field_ids(&self) -> &[FieldDefinitionId] {
        self.field_ids.get().expect("missing fields")
    }

    pub fn field_id(&self, idx: FieldIndex) -> FieldDefinitionId {
        self.field_ids()[idx.to_usize()]
    }

    pub fn all_fields_are_public(&self, sa: &Sema) -> bool {
        // "Internal" structs don't have any outside visible fields.
        if self.is_internal {
            return false;
        }

        for &field_id in self.field_ids() {
            let field = sa.field(field_id);
            if !field.visibility.is_public() {
                return false;
            }
        }

        true
    }
}

impl Element for StructDefinition {
    fn element_id(&self) -> ElementId {
        ElementId::Struct(self.id())
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
        Some(self.ty())
    }

    fn visibility(&self) -> Visibility {
        self.visibility
    }
}

impl ElementAccess for StructDefinition {
    type Id = StructDefinitionId;

    fn by_id(sa: &Sema, id: Self::Id) -> &Self {
        sa.struct_(id)
    }

    fn id(&self) -> Self::Id {
        self.id.expect("missing id")
    }
}

impl ElementWithFields for StructDefinition {
    fn field_name_style(&self) -> ast::FieldNameStyle {
        self.field_name_style
    }

    fn field_ids(&self) -> &[FieldDefinitionId] {
        self.field_ids()
    }
}
