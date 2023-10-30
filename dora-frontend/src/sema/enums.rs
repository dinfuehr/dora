use std::cell::{OnceCell, RefCell};
use std::collections::HashMap;
use std::sync::Arc;

use crate::interner::Name;
use crate::program_parser::ParsedModifierList;
use dora_parser::ast;
use dora_parser::Span;

use id_arena::Id;

use crate::sema::{
    extension_matches, impl_matches, module_path, Candidate, ExtensionDefinitionId,
    ModuleDefinitionId, PackageDefinitionId, Sema, SourceFileId, TypeParamDefinition, Visibility,
};
use crate::ty::{SourceType, SourceTypeArray};

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
    pub type_params: OnceCell<TypeParamDefinition>,
    pub variants: OnceCell<Vec<EnumVariant>>,
    pub extensions: RefCell<Vec<ExtensionDefinitionId>>,
    pub simple_enumeration: OnceCell<bool>,
    pub name_to_value: OnceCell<HashMap<Name, u32>>,
}

impl EnumDefinition {
    pub(crate) fn new(
        package_id: PackageDefinitionId,
        module_id: ModuleDefinitionId,
        file_id: SourceFileId,
        node: &Arc<ast::Enum>,
        modifiers: ParsedModifierList,
        name: Name,
    ) -> EnumDefinition {
        EnumDefinition {
            id: None,
            package_id,
            module_id,
            file_id,
            ast: node.clone(),
            span: node.span,
            name,
            type_params: OnceCell::new(),
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

    pub fn type_params(&self) -> &TypeParamDefinition {
        self.type_params.get().expect("uninitialized")
    }

    pub fn name_to_value(&self) -> &HashMap<Name, u32> {
        self.name_to_value.get().expect("uninitialized")
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
        self.variants.get().expect("missing variants")
    }
}

#[derive(Debug)]
pub struct EnumVariant {
    pub id: u32,
    pub name: Name,
    pub types: OnceCell<Vec<SourceType>>,
}

impl EnumVariant {
    pub fn types(&self) -> &[SourceType] {
        self.types.get().expect("missing types")
    }
}

pub fn find_methods_in_enum(
    sa: &Sema,
    object_type: SourceType,
    type_param_defs: &TypeParamDefinition,
    name: Name,
    is_static: bool,
) -> Vec<Candidate> {
    for (_id, extension) in sa.extensions.iter() {
        if let Some(bindings) =
            extension_matches(sa, object_type.clone(), type_param_defs, extension.id())
        {
            let table = if is_static {
                &extension.static_names
            } else {
                &extension.instance_names
            };

            if let Some(&fct_id) = table.borrow().get(&name) {
                return vec![Candidate {
                    object_type: object_type.clone(),
                    container_type_params: bindings,
                    fct_id,
                }];
            }
        }
    }

    let mut candidates = Vec::new();

    for (_id, impl_) in sa.impls.iter() {
        if let Some(bindings) = impl_matches(sa, object_type.clone(), type_param_defs, impl_.id()) {
            let trait_ = &sa.trait_(impl_.trait_id());

            if let Some(trait_method_id) = trait_.get_method(name, is_static) {
                candidates.push(Candidate {
                    object_type: object_type.clone(),
                    container_type_params: bindings.clone(),
                    fct_id: impl_
                        .get_method_for_trait_method_id(trait_method_id)
                        .expect("missing fct"),
                });
            }
        }
    }

    candidates
}
