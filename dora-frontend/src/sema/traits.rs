use std::cell::OnceCell;
use std::collections::HashMap;
use std::rc::Rc;
use std::sync::Arc;

use crate::interner::Name;
use crate::program_parser::ParsedModifierList;
use dora_parser::ast;
use dora_parser::Span;

use crate::sema::{
    module_path, AliasDefinitionId, Candidate, Element, ElementAccess, ElementId, FctDefinitionId,
    ModuleDefinitionId, PackageDefinitionId, Sema, SourceFileId, TypeParamDefinition, Visibility,
};
use crate::{contains_self, SourceType, SourceTypeArray};
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
    pub type_param_definition: Rc<TypeParamDefinition>,
    pub methods: OnceCell<Vec<FctDefinitionId>>,
    pub aliases: OnceCell<Vec<AliasDefinitionId>>,
    pub instance_names: OnceCell<HashMap<Name, FctDefinitionId>>,
    pub static_names: OnceCell<HashMap<Name, FctDefinitionId>>,
    pub alias_names: OnceCell<HashMap<Name, AliasDefinitionId>>,
}

impl TraitDefinition {
    pub(crate) fn new(
        package_id: PackageDefinitionId,
        module_id: ModuleDefinitionId,
        file_id: SourceFileId,
        node: &Arc<ast::Trait>,
        modifiers: ParsedModifierList,
        name: Name,
        type_params: Rc<TypeParamDefinition>,
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
            type_param_definition: type_params,
            methods: OnceCell::new(),
            aliases: OnceCell::new(),
            instance_names: OnceCell::new(),
            static_names: OnceCell::new(),
            alias_names: OnceCell::new(),
        }
    }

    pub fn id(&self) -> TraitDefinitionId {
        self.id.expect("id missing")
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

    pub fn alias_names(&self) -> &HashMap<Name, AliasDefinitionId> {
        self.alias_names.get().expect("uninitialized")
    }

    pub fn type_param_definition(&self) -> &Rc<TypeParamDefinition> {
        &self.type_param_definition
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

impl Element for TraitDefinition {
    fn element_id(&self) -> ElementId {
        ElementId::Trait(self.id())
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

    fn to_trait(&self) -> Option<&TraitDefinition> {
        Some(self)
    }

    fn self_ty(&self, _sa: &Sema) -> Option<SourceType> {
        Some(SourceType::This)
    }
}

impl ElementAccess for TraitDefinition {
    type Id = TraitDefinitionId;

    fn by_id(sa: &Sema, id: Self::Id) -> &Self {
        sa.trait_(id)
    }

    fn id(&self) -> Self::Id {
        self.id.expect("missing id")
    }
}

pub fn is_object_safe(sa: &Sema, trait_id: TraitDefinitionId) -> bool {
    let trait_ = sa.trait_(trait_id);

    for &alias_id in trait_.aliases() {
        let alias = sa.alias(alias_id);
        if alias.type_param_definition().has_own_type_params() {
            return false;
        }
    }

    for method_id in trait_.methods() {
        let method = sa.fct(*method_id);

        if method.type_param_definition().has_own_type_params() {
            return false;
        }

        if method.is_static {
            return false;
        }

        for param in method.params_without_self() {
            if contains_self(sa, param.ty()) {
                return false;
            }
        }

        if contains_self(sa, method.return_type()) {
            return false;
        }
    }

    let type_param_definition = trait_.type_param_definition();
    for bound in type_param_definition.bounds_for_self() {
        if !is_object_safe(sa, bound.trait_id) {
            return false;
        }
    }

    true
}

#[allow(unused)]
pub fn find_methods_in_trait(
    sa: &Sema,
    object_type: SourceType,
    type_param_defs: &TypeParamDefinition,
    name: Name,
    is_static: bool,
) -> Vec<Candidate> {
    unimplemented!()
}
