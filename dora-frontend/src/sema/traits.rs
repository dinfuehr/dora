use std::cell::OnceCell;
use std::collections::HashMap;
use std::sync::Arc;

use crate::interner::Name;
use crate::program_parser::ParsedModifierList;
use dora_parser::ast;
use dora_parser::Span;

use crate::sema::{
    module_path, AliasDefinitionId, FctDefinitionId, ModuleDefinitionId, PackageDefinitionId, Sema,
    SourceFileId, TypeParamDefinition, Visibility,
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
    pub aliases: OnceCell<Vec<AliasDefinitionId>>,
    pub instance_names: OnceCell<HashMap<Name, FctDefinitionId>>,
    pub static_names: OnceCell<HashMap<Name, FctDefinitionId>>,
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
            aliases: OnceCell::new(),
            instance_names: OnceCell::new(),
            static_names: OnceCell::new(),
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

    pub fn aliases(&self) -> &[AliasDefinitionId] {
        self.aliases.get().expect("uninitialized")
    }

    pub fn instance_names(&self) -> &HashMap<Name, FctDefinitionId> {
        self.instance_names.get().expect("uninitialized")
    }

    pub fn static_names(&self) -> &HashMap<Name, FctDefinitionId> {
        self.static_names.get().expect("uninitialized")
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

    pub fn get_method(&self, name: Name, is_static: bool) -> Option<FctDefinitionId> {
        let table = if is_static {
            self.static_names()
        } else {
            self.instance_names()
        };

        table.get(&name).cloned()
    }
}

pub fn params_match(
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
