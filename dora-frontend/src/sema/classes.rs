use std::cell::{OnceCell, RefCell};
use std::ops::{Index, IndexMut};
use std::sync::Arc;

use id_arena::Id;

use crate::interner::Name;
use crate::program_parser::ParsedModifierList;
use dora_parser::ast;
use dora_parser::Span;

use crate::sema::{
    extension_matches, impl_matches, module_path, ExtensionDefinitionId, FctDefinitionId,
    ModuleDefinitionId, PackageDefinitionId, Sema, SourceFileId,
};
use crate::{replace_type, AliasReplacement, SourceType, SourceTypeArray};

pub type ClassDefinitionId = Id<ClassDefinition>;

#[derive(Debug)]
pub struct ClassDefinition {
    pub id: Option<ClassDefinitionId>,
    pub package_id: PackageDefinitionId,
    pub module_id: ModuleDefinitionId,
    pub file_id: Option<SourceFileId>,
    pub ast: Option<Arc<ast::Class>>,
    pub span: Option<Span>,
    pub name: Name,
    pub ty: OnceCell<SourceType>,
    pub is_internal: bool,
    pub internal_resolved: bool,
    pub visibility: Visibility,

    pub fields: Vec<Field>,

    pub extensions: RefCell<Vec<ExtensionDefinitionId>>,

    pub type_params: OnceCell<TypeParamDefinition>,

    // true if this class is the generic Array class
    pub is_array: bool,
    pub is_str: bool,
}

impl ClassDefinition {
    pub(crate) fn new(
        package_id: PackageDefinitionId,
        module_id: ModuleDefinitionId,
        file_id: SourceFileId,
        ast: &Arc<ast::Class>,
        modifiers: ParsedModifierList,
        name: Name,
        fields: Vec<Field>,
    ) -> ClassDefinition {
        ClassDefinition {
            id: None,
            package_id,
            module_id,
            file_id: Some(file_id),
            ast: Some(ast.clone()),
            span: Some(ast.span),
            name,
            ty: OnceCell::new(),
            is_internal: modifiers.is_internal,
            internal_resolved: false,
            visibility: modifiers.visibility(),

            fields,

            extensions: RefCell::new(Vec::new()),

            type_params: OnceCell::new(),

            is_array: false,
            is_str: false,
        }
    }

    pub fn new_without_source(
        package_id: PackageDefinitionId,
        module_id: ModuleDefinitionId,
        file_id: Option<SourceFileId>,
        span: Option<Span>,
        name: Name,
        visibility: Visibility,
        fields: Vec<Field>,
    ) -> ClassDefinition {
        ClassDefinition {
            id: None,
            package_id,
            module_id,
            file_id,
            ast: None,
            span,
            name,
            ty: OnceCell::new(),
            is_internal: false,
            internal_resolved: false,
            visibility,

            fields,

            extensions: RefCell::new(Vec::new()),

            type_params: OnceCell::new(),

            is_array: false,
            is_str: false,
        }
    }

    pub fn id(&self) -> ClassDefinitionId {
        self.id.expect("missing id")
    }

    pub fn ast(&self) -> &Arc<ast::Class> {
        self.ast.as_ref().expect("ast expected")
    }

    pub fn file_id(&self) -> SourceFileId {
        self.file_id.expect("missing source file")
    }

    pub fn span(&self) -> Span {
        self.span.expect("missing position")
    }

    pub fn type_params(&self) -> &TypeParamDefinition {
        self.type_params.get().expect("uninitialized")
    }

    pub fn type_params_mut(&mut self) -> &mut TypeParamDefinition {
        self.type_params.get_mut().expect("uninitialized")
    }

    pub fn ty(&self) -> SourceType {
        self.ty.get().expect("not initialized").clone()
    }

    pub fn field_by_name(&self, name: Name) -> FieldId {
        for field in &self.fields {
            if field.name == name {
                return field.id;
            }
        }

        panic!("field not found!")
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

    pub fn all_fields_are_public(&self) -> bool {
        // "Internal" classes don't have any outside visible fields.
        if self.is_internal {
            return false;
        }

        for field in &self.fields {
            if !field.visibility.is_public() {
                return false;
            }
        }

        true
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct FieldId(pub usize);

impl FieldId {
    pub fn to_usize(self) -> usize {
        self.0
    }
}

impl From<usize> for FieldId {
    fn from(data: usize) -> FieldId {
        FieldId(data)
    }
}

#[derive(Debug)]
pub struct Field {
    pub id: FieldId,
    pub name: Name,
    pub ty: OnceCell<SourceType>,
    pub mutable: bool,
    pub visibility: Visibility,
}

impl Field {
    pub fn ty(&self) -> SourceType {
        self.ty.get().expect("uninitalized").clone()
    }
}

impl Index<FieldId> for Vec<Field> {
    type Output = Field;

    fn index(&self, index: FieldId) -> &Field {
        &self[index.0]
    }
}

impl IndexMut<FieldId> for Vec<Field> {
    fn index_mut(&mut self, index: FieldId) -> &mut Field {
        &mut self[index.0]
    }
}

pub fn find_field_in_class(
    sa: &Sema,
    class: SourceType,
    name: Name,
) -> Option<(SourceType, FieldId, SourceType)> {
    if class.cls_id().is_none() {
        return None;
    }

    let cls_id = class.cls_id().expect("no class");
    let cls = sa.class(cls_id);

    let type_list = class.type_params();

    for field in &cls.fields {
        if field.name == name {
            return Some((
                class,
                field.id,
                replace_type(
                    sa,
                    field.ty(),
                    Some(&type_list),
                    None,
                    AliasReplacement::None,
                ),
            ));
        }
    }

    None
}

pub struct Candidate {
    pub object_type: SourceType,
    pub container_type_params: SourceTypeArray,
    pub fct_id: FctDefinitionId,
}

pub fn find_methods_in_class(
    sa: &Sema,
    object_type: SourceType,
    type_param_defs: &TypeParamDefinition,
    name: Name,
    is_static: bool,
) -> Vec<Candidate> {
    let mut candidates = Vec::new();

    // Find extension methods
    for (_id, extension) in sa.extensions.iter() {
        if let Some(bindings) =
            extension_matches(sa, object_type.clone(), type_param_defs, extension.id())
        {
            let extension = sa.extension(extension.id());

            let table = if is_static {
                &extension.static_names
            } else {
                &extension.instance_names
            };

            if let Some(&fct_id) = table.borrow().get(&name) {
                return vec![Candidate {
                    object_type,
                    container_type_params: bindings,
                    fct_id: fct_id,
                }];
            }
        }
    }

    for (_id, impl_) in sa.impls.iter() {
        if let Some(bindings) = impl_matches(sa, object_type.clone(), type_param_defs, impl_.id()) {
            let impl_ = &sa.impl_(impl_.id());
            let trait_ = &sa.trait_(impl_.trait_id());

            if let Some(trait_method_id) = trait_.get_method(name, is_static) {
                candidates.push(Candidate {
                    object_type: object_type.clone(),
                    container_type_params: bindings.clone(),
                    fct_id: impl_
                        .get_method_for_trait_method_id(trait_method_id)
                        .expect("missing method"),
                });
            }
        }
    }

    candidates
}

#[derive(Clone, Debug)]
pub struct TypeParamDefinition {
    type_params: Vec<TypeParam>,
    bounds: Vec<Bound>,
}

impl TypeParamDefinition {
    pub fn new() -> TypeParamDefinition {
        TypeParamDefinition {
            type_params: Vec::new(),
            bounds: Vec::new(),
        }
    }

    pub fn len(&self) -> usize {
        self.type_params.len()
    }

    pub fn name(&self, id: TypeParamId) -> Name {
        self.type_params[id.to_usize()].name
    }

    pub fn add_type_param(&mut self, name: Name) -> TypeParamId {
        let id = TypeParamId(self.type_params.len());
        self.type_params.push(TypeParam { name });
        id
    }

    pub fn add_bound(&mut self, id: TypeParamId, trait_ty: SourceType) -> bool {
        assert!(trait_ty.is_trait());

        let bound = Bound {
            ty: SourceType::TypeParam(id),
            trait_ty: trait_ty.clone(),
        };

        let contains = self.bounds.contains(&bound);

        if contains {
            false
        } else {
            self.bounds.push(bound);

            true
        }
    }

    pub fn add_where_bound(&mut self, ty: SourceType, trait_ty: SourceType) {
        assert!(trait_ty.is_trait());
        self.bounds.push(Bound { ty, trait_ty });
    }

    pub fn implements_trait(&self, id: TypeParamId, trait_ty: SourceType) -> bool {
        self.bounds.contains(&Bound {
            ty: SourceType::TypeParam(id),
            trait_ty,
        })
    }

    pub fn bounds(&self) -> &[Bound] {
        &self.bounds
    }

    pub fn bounds_for_type_param(&self, id: TypeParamId) -> TypeParamBoundsIter {
        TypeParamBoundsIter {
            bounds: &self.bounds,
            current: 0,
            id,
        }
    }

    pub fn append(&mut self, other: &TypeParamDefinition) {
        assert_eq!(self.type_params.len(), 0);
        assert_eq!(self.bounds.len(), 0);

        self.type_params = other.type_params.clone();
        self.bounds = other.bounds.clone();
    }

    pub fn is_empty(&self) -> bool {
        self.type_params.is_empty()
    }

    pub fn names(&self) -> TypeParamNameIter {
        TypeParamNameIter {
            data: self,
            current: 0,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Bound {
    pub ty: SourceType,
    pub trait_ty: SourceType,
}

impl Bound {
    pub fn ty(&self) -> SourceType {
        self.ty.clone()
    }

    pub fn trait_ty(&self) -> SourceType {
        debug_assert!(self.trait_ty.is_trait());
        self.trait_ty.clone()
    }
}

pub struct TypeParamBoundsIter<'a> {
    bounds: &'a [Bound],
    current: usize,
    id: TypeParamId,
}

impl<'a> Iterator for TypeParamBoundsIter<'a> {
    type Item = SourceType;

    fn next(&mut self) -> Option<SourceType> {
        while self.current < self.bounds.len() {
            let bound = &self.bounds[self.current];
            if bound.ty() == SourceType::TypeParam(self.id) {
                self.current += 1;
                return Some(bound.trait_ty());
            }

            self.current += 1;
        }

        None
    }
}

pub struct TypeParamNameIter<'a> {
    data: &'a TypeParamDefinition,
    current: usize,
}

impl<'a> Iterator for TypeParamNameIter<'a> {
    type Item = (TypeParamId, Name);

    fn next(&mut self) -> Option<(TypeParamId, Name)> {
        if self.current < self.data.len() {
            let current = TypeParamId(self.current);
            self.current += 1;
            Some((current, self.data.name(current)))
        } else {
            None
        }
    }
}

#[derive(Clone, Debug)]
struct TypeParam {
    name: Name,
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub struct TypeParamId(pub usize);

impl TypeParamId {
    pub fn to_usize(self) -> usize {
        self.0
    }
}

#[derive(Copy, Clone, Debug)]
pub enum Visibility {
    Public,
    Module,
}

impl Visibility {
    pub fn is_public(&self) -> bool {
        match self {
            Visibility::Public => true,
            Visibility::Module => false,
        }
    }
}
