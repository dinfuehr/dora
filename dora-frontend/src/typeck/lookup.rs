use crate::interner::Name;
use crate::sema::{
    extension_matches, impl_matches, Candidate, Sema, TraitDefinition, TypeParamDefinition,
};
use crate::sym::ModuleSymTable;
use crate::SourceType;

pub fn find_method_call_candidates(
    sa: &Sema,
    table: &ModuleSymTable,
    object_type: SourceType,
    type_param_defs: &TypeParamDefinition,
    name: Name,
    is_static: bool,
) -> Vec<Candidate> {
    let mut candidates = Vec::with_capacity(1);
    let package_id = sa.module(table.module_id()).package_id();

    if let SourceType::TraitObject(trait_id, trait_type_params, _bindings) = object_type.clone() {
        let trait_ = sa.trait_(trait_id);
        if let Some(fct_id) = trait_.get_method(name, false) {
            candidates.push(Candidate {
                object_type: object_type.clone(),
                container_type_params: trait_type_params,
                fct_id,
            });
            return candidates;
        }

        find_super_trait_methods_on_trait_object(
            sa,
            object_type.clone(),
            trait_,
            name,
            &mut candidates,
        );
    }

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
                candidates.push(Candidate {
                    object_type: object_type.clone(),
                    container_type_params: bindings,
                    fct_id,
                });
                return candidates;
            }
        }
    }

    for (_id, impl_) in sa.impls.iter() {
        if let Some(trait_ty) = impl_.trait_ty() {
            let trait_ = &sa.trait_(trait_ty.trait_id);

            let is_impl_foreign = impl_.package_id != package_id;

            if is_impl_foreign && !table.contains_trait(trait_.id()) {
                continue;
            }

            if let Some(bindings) =
                impl_matches(sa, object_type.clone(), type_param_defs, impl_.id())
            {
                if let Some(trait_method_id) = trait_.get_method(name, is_static) {
                    if let Some(fct_id) = impl_.get_method_for_trait_method_id(trait_method_id) {
                        candidates.push(Candidate {
                            object_type: object_type.clone(),
                            container_type_params: bindings.clone(),
                            fct_id,
                        });
                    }
                }
            }
        }
    }

    candidates
}

fn find_super_trait_methods_on_trait_object(
    sa: &Sema,
    object_type: SourceType,
    trait_: &TraitDefinition,
    name: Name,
    candidates: &mut Vec<Candidate>,
) {
    for bound in trait_.type_param_definition().bounds_for_self() {
        let super_trait = sa.trait_(bound.trait_id);
        if let Some(fct_id) = super_trait.get_method(name, false) {
            candidates.push(Candidate {
                object_type: object_type.clone(),
                container_type_params: ().into(),
                fct_id,
            });
        }

        find_super_trait_methods_on_trait_object(
            sa,
            object_type.clone(),
            super_trait,
            name,
            candidates,
        );
    }
}
