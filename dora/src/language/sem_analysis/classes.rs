use std::collections::HashSet;
use std::ops::{Index, IndexMut};
use std::slice::Iter;
use std::sync::Arc;

use dora_parser::ast;
use dora_parser::interner::Name;
use dora_parser::Position;

use crate::language::sem_analysis::{
    extension_matches, impl_matches, module_path, ExtensionDefinitionId, FctDefinitionId,
    ImplDefinitionId, ModuleDefinitionId, SemAnalysis, SourceFileId, TraitDefinitionId,
};
use crate::language::specialize::replace_type_param;
use crate::language::ty::{SourceType, SourceTypeArray};
use crate::utils::Id;

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub struct ClassDefinitionId(usize);

impl ClassDefinitionId {
    pub fn max() -> ClassDefinitionId {
        ClassDefinitionId(usize::max_value())
    }

    pub fn to_usize(self) -> usize {
        self.0
    }
}

impl Id for ClassDefinition {
    type IdType = ClassDefinitionId;

    fn id_to_usize(id: ClassDefinitionId) -> usize {
        id.0
    }

    fn usize_to_id(value: usize) -> ClassDefinitionId {
        ClassDefinitionId(value)
    }

    fn store_id(value: &mut ClassDefinition, id: ClassDefinitionId) {
        value.id = Some(id);
    }
}

#[derive(Debug)]
pub struct ClassDefinition {
    pub id: Option<ClassDefinitionId>,
    pub file_id: Option<SourceFileId>,
    pub ast: Option<Arc<ast::Class>>,
    pub module_id: ModuleDefinitionId,
    pub pos: Option<Position>,
    pub name: Name,
    pub ty: Option<SourceType>,
    pub internal: bool,
    pub internal_resolved: bool,
    pub is_pub: bool,

    pub fields: Vec<Field>,

    pub impls: Vec<ImplDefinitionId>,
    pub extensions: Vec<ExtensionDefinitionId>,

    pub type_params: TypeParamsDefinition,

    // true if this class is the generic Array class
    pub is_array: bool,
    pub is_str: bool,
}

impl ClassDefinition {
    pub fn new(
        file_id: SourceFileId,
        ast: &Arc<ast::Class>,
        module_id: ModuleDefinitionId,
    ) -> ClassDefinition {
        ClassDefinition {
            id: None,
            file_id: Some(file_id),
            ast: Some(ast.clone()),
            module_id,
            pos: Some(ast.pos),
            name: ast.name,
            ty: None,
            internal: ast.internal,
            internal_resolved: false,
            is_pub: ast.is_pub,

            fields: Vec::new(),

            impls: Vec::new(),
            extensions: Vec::new(),

            type_params: TypeParamsDefinition::new_ast(&ast.type_params),

            is_array: false,
            is_str: false,
        }
    }

    pub fn new_without_source(
        module_id: ModuleDefinitionId,
        file_id: Option<SourceFileId>,
        pos: Option<Position>,
        name: Name,
        is_pub: bool,
        fields: Vec<Field>,
    ) -> ClassDefinition {
        ClassDefinition {
            id: None,
            file_id,
            ast: None,
            module_id,
            pos,
            name,
            ty: None,
            internal: false,
            internal_resolved: false,
            is_pub,

            fields,

            impls: Vec::new(),
            extensions: Vec::new(),

            type_params: TypeParamsDefinition::new(),

            is_array: false,
            is_str: false,
        }
    }

    pub fn id(&self) -> ClassDefinitionId {
        self.id.expect("missing id")
    }

    pub fn file_id(&self) -> SourceFileId {
        self.file_id.expect("missing source file")
    }

    pub fn pos(&self) -> Position {
        self.pos.expect("missing position")
    }

    pub fn ty(&self) -> SourceType {
        self.ty.clone().expect("not initialized")
    }

    pub fn field_by_name(&self, name: Name) -> FieldId {
        for field in &self.fields {
            if field.name == name {
                return field.id;
            }
        }

        panic!("field not found!")
    }

    pub fn name(&self, sa: &SemAnalysis) -> String {
        module_path(sa, self.module_id, self.name)
    }

    pub fn name_with_params(&self, sa: &SemAnalysis, type_list: &SourceTypeArray) -> String {
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
        if self.internal {
            return false;
        }

        for field in &self.fields {
            if !field.is_pub {
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
    pub ty: SourceType,
    pub mutable: bool,
    pub is_pub: bool,
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
    sa: &SemAnalysis,
    class: SourceType,
    name: Name,
) -> Option<(SourceType, FieldId, SourceType)> {
    if class.cls_id().is_none() {
        return None;
    }

    let cls_id = class.cls_id().expect("no class");
    let cls = sa.classes.idx(cls_id);
    let cls = cls.read();

    let type_list = class.type_params();

    for field in &cls.fields {
        if field.name == name {
            return Some((
                class,
                field.id,
                replace_type_param(sa, field.ty.clone(), &type_list, None),
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
    sa: &SemAnalysis,
    object_type: SourceType,
    type_param_defs: &TypeParamsDefinition,
    name: Name,
    is_static: bool,
) -> Vec<Candidate> {
    let mut candidates = Vec::new();

    // Find extension methods
    {
        let cls_id = object_type.cls_id().expect("no class");
        let cls = sa.classes.idx(cls_id);
        let cls = cls.read();

        for &extension_id in &cls.extensions {
            if let Some(bindings) =
                extension_matches(sa, object_type.clone(), type_param_defs, extension_id)
            {
                let extension = sa.extensions[extension_id].read();

                let table = if is_static {
                    &extension.static_names
                } else {
                    &extension.instance_names
                };

                if let Some(&fct_id) = table.get(&name) {
                    return vec![Candidate {
                        object_type,
                        container_type_params: bindings,
                        fct_id: fct_id,
                    }];
                }
            }
        }
    }

    let cls_id = object_type.cls_id().expect("no class");
    let cls = sa.classes.idx(cls_id);
    let cls = cls.read();

    for &impl_id in &cls.impls {
        if let Some(bindings) = impl_matches(sa, object_type.clone(), type_param_defs, impl_id) {
            let impl_ = sa.impls[impl_id].read();

            let table = if is_static {
                &impl_.static_names
            } else {
                &impl_.instance_names
            };

            if let Some(&method_id) = table.get(&name) {
                candidates.push(Candidate {
                    object_type: object_type.clone(),
                    container_type_params: bindings.clone(),
                    fct_id: method_id,
                });
            }
        }
    }

    candidates
}

#[derive(Clone, Debug)]
pub struct TypeParamsDefinition {
    type_params: Vec<TypeParam>,
}

impl TypeParamsDefinition {
    pub fn new() -> TypeParamsDefinition {
        TypeParamsDefinition {
            type_params: Vec::new(),
        }
    }

    pub fn new_ast(type_params: &Option<Vec<ast::TypeParam>>) -> TypeParamsDefinition {
        let type_params = if let Some(ast_type_params) = type_params {
            ast_type_params
                .iter()
                .map(|type_param| TypeParam::new(type_param.name))
                .collect()
        } else {
            Vec::new()
        };

        TypeParamsDefinition { type_params }
    }

    pub fn len(&self) -> usize {
        self.type_params.len()
    }

    fn get(&self, idx: TypeParamId) -> &TypeParam {
        &self.type_params[idx.to_usize()]
    }

    pub fn at(&self, idx: TypeParamId) -> TypeParamDefinition {
        TypeParamDefinition {
            type_params: self,
            idx,
        }
    }

    pub fn name(&self, idx: TypeParamId) -> Name {
        self.type_params[idx.to_usize()].name
    }

    pub fn add_bound(&mut self, idx: TypeParamId, trait_it: TraitDefinitionId) -> bool {
        self.type_params[idx.to_usize()]
            .trait_bounds
            .insert(trait_it)
    }

    pub fn bounds(&self, idx: TypeParamId) -> &HashSet<TraitDefinitionId> {
        &self.type_params[idx.to_usize()].trait_bounds
    }

    pub fn push(&mut self, type_param: TypeParam) -> TypeParamId {
        let id = TypeParamId(self.type_params.len());
        self.type_params.push(type_param);
        id
    }

    pub fn is_empty(&self) -> bool {
        self.type_params.is_empty()
    }

    pub fn iter(&self) -> Iter<TypeParam> {
        self.type_params.iter()
    }
}

pub struct TypeParamDefinition<'a> {
    type_params: &'a TypeParamsDefinition,
    idx: TypeParamId,
}

impl<'a> TypeParamDefinition<'a> {
    pub fn name(&self) -> Name {
        self.type_params.get(self.idx).name
    }

    pub fn bounds(&self) -> &HashSet<TraitDefinitionId> {
        &self.type_params.get(self.idx).trait_bounds
    }
}

#[derive(Clone, Debug)]
pub struct TypeParam {
    pub name: Name,
    pub trait_bounds: HashSet<TraitDefinitionId>,
}

impl TypeParam {
    pub fn new(name: Name) -> TypeParam {
        TypeParam {
            name,
            trait_bounds: HashSet::new(),
        }
    }
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub struct TypeParamId(pub usize);

impl TypeParamId {
    pub fn to_usize(self) -> usize {
        self.0
    }
}
