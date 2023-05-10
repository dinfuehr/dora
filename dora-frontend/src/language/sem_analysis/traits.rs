use parking_lot::RwLock;
use std::collections::HashMap;
use std::convert::TryInto;
use std::ops::Index;
use std::sync::Arc;

use dora_parser::ast;
use dora_parser::interner::Name;
use dora_parser::Span;

use crate::language::sem_analysis::{
    module_path, FctDefinitionId, ModuleDefinitionId, PackageDefinitionId, SemAnalysis,
    SourceFileId, TypeParamDefinition, Visibility,
};
use crate::language::ty::{SourceType, SourceTypeArray};
use crate::Id;

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct TraitDefinitionId(pub u32);

impl TraitDefinitionId {
    pub fn to_usize(self) -> usize {
        self.0 as usize
    }
}

impl Id for TraitDefinition {
    type IdType = TraitDefinitionId;

    fn id_to_usize(id: TraitDefinitionId) -> usize {
        id.0 as usize
    }

    fn usize_to_id(value: usize) -> TraitDefinitionId {
        TraitDefinitionId(value.try_into().unwrap())
    }

    fn store_id(value: &mut TraitDefinition, id: TraitDefinitionId) {
        value.id = Some(id);
    }
}

#[derive(Debug)]
pub struct TraitDefinition {
    pub id: Option<TraitDefinitionId>,
    pub package_id: PackageDefinitionId,
    pub module_id: ModuleDefinitionId,
    pub file_id: SourceFileId,
    pub visibility: Visibility,
    pub ast: Arc<ast::Trait>,
    pub span: Span,
    pub name: Name,
    pub is_trait_object: bool,
    pub type_params: Option<TypeParamDefinition>,
    pub methods: Vec<FctDefinitionId>,
    pub instance_names: HashMap<Name, FctDefinitionId>,
    pub static_names: HashMap<Name, FctDefinitionId>,
}

impl TraitDefinition {
    pub fn new(
        package_id: PackageDefinitionId,
        module_id: ModuleDefinitionId,
        file_id: SourceFileId,
        node: &Arc<ast::Trait>,
        name: Name,
    ) -> TraitDefinition {
        TraitDefinition {
            id: None,
            package_id,
            module_id,
            file_id,
            ast: node.clone(),
            visibility: Visibility::from_ast(node.visibility),
            span: node.span,
            name,
            is_trait_object: false,
            type_params: None,
            methods: Vec::new(),
            instance_names: HashMap::new(),
            static_names: HashMap::new(),
        }
    }

    pub fn id(&self) -> TraitDefinitionId {
        self.id.expect("id missing")
    }

    pub fn type_params(&self) -> &TypeParamDefinition {
        self.type_params.as_ref().expect("uninitialized")
    }

    pub fn name(&self, sa: &SemAnalysis) -> String {
        module_path(sa, self.module_id, self.name)
    }

    pub fn name_with_params(&self, sa: &SemAnalysis, type_list: &SourceTypeArray) -> String {
        let name = module_path(sa, self.module_id, self.name);

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

    pub fn find_method(
        &self,
        sa: &SemAnalysis,
        name: Name,
        is_static: bool,
    ) -> Option<FctDefinitionId> {
        for &method in &self.methods {
            let method = sa.fcts.idx(method);
            let method = method.read();

            if method.name == name && method.is_static == is_static {
                return Some(method.id());
            }
        }

        None
    }

    pub fn find_method_with_replace(
        &self,
        sa: &SemAnalysis,
        is_static: bool,
        name: Name,
        replace: Option<SourceType>,
        args: &[SourceType],
    ) -> Option<FctDefinitionId> {
        for &method in &self.methods {
            let method = sa.fcts.idx(method);
            let method = method.read();

            if method.name == name
                && method.is_static == is_static
                && params_match(replace.clone(), method.params_without_self(), args)
            {
                return Some(method.id());
            }
        }

        None
    }
}

fn params_match(
    replace: Option<SourceType>,
    trait_args: &[SourceType],
    args: &[SourceType],
) -> bool {
    if trait_args.len() != args.len() {
        return false;
    }

    for (ind, ty) in trait_args.iter().enumerate() {
        let ty = ty.clone();
        let other = args[ind].clone();

        let found = if ty.is_self() {
            replace.is_none() || replace.clone().unwrap() == other
        } else {
            ty == other
        };

        if !found {
            return false;
        }
    }

    true
}

impl Index<TraitDefinitionId> for Vec<RwLock<TraitDefinition>> {
    type Output = RwLock<TraitDefinition>;

    fn index(&self, index: TraitDefinitionId) -> &RwLock<TraitDefinition> {
        &self[index.0 as usize]
    }
}
