use std::cell::{OnceCell, RefCell};
use std::collections::HashMap;
use std::sync::Arc;

use crate::interner::Name;
use crate::program_parser::ParsedModifierList;
use dora_parser::ast;
use dora_parser::Span;

use crate::sema::{
    module_path, FctDefinitionId, ModuleDefinitionId, PackageDefinitionId, Sema, SourceFileId,
    TypeParamDefinition, Visibility,
};
use crate::ty::{SourceType, SourceTypeArray};
use id_arena::Id;

pub type TraitDefinitionId = Id<TraitDefinition>;

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
    pub type_params: OnceCell<TypeParamDefinition>,
    pub methods: OnceCell<Vec<FctDefinitionId>>,
    pub instance_names: RefCell<HashMap<Name, FctDefinitionId>>,
    pub static_names: RefCell<HashMap<Name, FctDefinitionId>>,
}

impl TraitDefinition {
    pub(crate) fn new(
        package_id: PackageDefinitionId,
        module_id: ModuleDefinitionId,
        file_id: SourceFileId,
        node: &Arc<ast::Trait>,
        modifiers: ParsedModifierList,
        name: Name,
    ) -> TraitDefinition {
        TraitDefinition {
            id: None,
            package_id,
            module_id,
            file_id,
            ast: node.clone(),
            visibility: modifiers.visibility(),
            span: node.span,
            name,
            is_trait_object: false,
            type_params: OnceCell::new(),
            methods: OnceCell::new(),
            instance_names: RefCell::new(HashMap::new()),
            static_names: RefCell::new(HashMap::new()),
        }
    }

    pub fn id(&self) -> TraitDefinitionId {
        self.id.expect("id missing")
    }

    pub fn type_params(&self) -> &TypeParamDefinition {
        self.type_params.get().expect("uninitialized")
    }

    pub fn methods(&self) -> &[FctDefinitionId] {
        self.methods.get().expect("uninitialized")
    }

    pub fn name(&self, sa: &Sema) -> String {
        module_path(sa, self.module_id, self.name)
    }

    pub fn name_with_params(&self, sa: &Sema, type_list: &SourceTypeArray) -> String {
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

    pub fn find_method(&self, sa: &Sema, name: Name, is_static: bool) -> Option<FctDefinitionId> {
        for &method in self.methods() {
            let method = sa.fcts.idx(method);

            if method.name == name && method.is_static == is_static {
                return Some(method.id());
            }
        }

        None
    }

    pub fn find_method_with_replace(
        &self,
        sa: &Sema,
        is_static: bool,
        name: Name,
        replace: Option<SourceType>,
        args: &[SourceType],
    ) -> Option<FctDefinitionId> {
        for &method in self.methods() {
            let method = sa.fcts.idx(method);

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
