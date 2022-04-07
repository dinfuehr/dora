use crate::language::sem_analysis::{
    ClassDefinitionId, ConstDefinitionId, EnumDefinitionId, FctDefinitionId, FctParent,
    GlobalDefinitionId, NamespaceId, StructDefinitionFieldId, StructDefinitionId,
    TraitDefinitionId,
};
use crate::vm::{FieldId, SemAnalysis};

pub fn global_accessible_from(
    sa: &SemAnalysis,
    global_id: GlobalDefinitionId,
    namespace_id: NamespaceId,
) -> bool {
    let global = sa.globals.idx(global_id);
    let global = global.read();

    accessible_from(sa, global.namespace_id, global.is_pub, namespace_id)
}

pub fn class_accessible_from(
    sa: &SemAnalysis,
    cls_id: ClassDefinitionId,
    namespace_id: NamespaceId,
) -> bool {
    let cls = sa.classes.idx(cls_id);
    let cls = cls.read();

    accessible_from(sa, cls.namespace_id, cls.is_pub, namespace_id)
}

pub fn class_field_accessible_from(
    sa: &SemAnalysis,
    cls_id: ClassDefinitionId,
    field_id: FieldId,
    namespace_id: NamespaceId,
) -> bool {
    let cls = sa.classes.idx(cls_id);
    let cls = cls.read();

    let field = &cls.fields[field_id];

    accessible_from(
        sa,
        cls.namespace_id,
        cls.is_pub && field.is_pub,
        namespace_id,
    )
}

pub fn method_accessible_from(
    sa: &SemAnalysis,
    fct_id: FctDefinitionId,
    namespace_id: NamespaceId,
) -> bool {
    let fct = sa.fcts.idx(fct_id);
    let fct = fct.read();

    let element_pub = match fct.parent {
        FctParent::Class(cls_id) => {
            let cls = sa.classes.idx(cls_id);
            let cls = cls.read();

            cls.is_pub && fct.is_pub
        }

        FctParent::Extension(_) => fct.is_pub,
        FctParent::Impl(_) | FctParent::Trait(_) => {
            // TODO: This should probably be limited
            return true;
        }

        FctParent::None => unreachable!(),
    };

    accessible_from(sa, fct.namespace_id, element_pub, namespace_id)
}

pub fn fct_accessible_from(
    sa: &SemAnalysis,
    fct_id: FctDefinitionId,
    namespace_id: NamespaceId,
) -> bool {
    let fct = sa.fcts.idx(fct_id);
    let fct = fct.read();

    accessible_from(sa, fct.namespace_id, fct.is_pub, namespace_id)
}

pub fn enum_accessible_from(
    sa: &SemAnalysis,
    enum_id: EnumDefinitionId,
    namespace_id: NamespaceId,
) -> bool {
    let xenum = sa.enums[enum_id].read();

    accessible_from(sa, xenum.namespace_id, xenum.is_pub, namespace_id)
}

pub fn struct_accessible_from(
    sa: &SemAnalysis,
    struct_id: StructDefinitionId,
    namespace_id: NamespaceId,
) -> bool {
    let xstruct = sa.structs.idx(struct_id);
    let xstruct = xstruct.read();

    accessible_from(sa, xstruct.namespace_id, xstruct.is_pub, namespace_id)
}

pub fn struct_field_accessible_from(
    sa: &SemAnalysis,
    struct_id: StructDefinitionId,
    field_id: StructDefinitionFieldId,
    namespace_id: NamespaceId,
) -> bool {
    let xstruct = sa.structs.idx(struct_id);
    let xstruct = xstruct.read();

    let field = &xstruct.fields[field_id.to_usize()];

    accessible_from(
        sa,
        xstruct.namespace_id,
        xstruct.is_pub && field.is_pub,
        namespace_id,
    )
}

pub fn namespace_accessible_from(
    sa: &SemAnalysis,
    target_id: NamespaceId,
    from_id: NamespaceId,
) -> bool {
    accessible_from(sa, target_id, true, from_id)
}

pub fn trait_accessible_from(
    sa: &SemAnalysis,
    trait_id: TraitDefinitionId,
    namespace_id: NamespaceId,
) -> bool {
    let xtrait = sa.traits[trait_id].read();

    accessible_from(sa, xtrait.namespace_id, xtrait.is_pub, namespace_id)
}

pub fn const_accessible_from(
    vm: &SemAnalysis,
    const_id: ConstDefinitionId,
    namespace_id: NamespaceId,
) -> bool {
    let xconst = vm.consts.idx(const_id);
    let xconst = xconst.read();

    accessible_from(vm, xconst.namespace_id, xconst.is_pub, namespace_id)
}

fn accessible_from(
    sa: &SemAnalysis,
    target_id: NamespaceId,
    element_pub: bool,
    from_id: NamespaceId,
) -> bool {
    // each namespace can access itself
    if target_id == from_id {
        return true;
    }

    // namespaces can access all their parents
    if namespace_contains(sa, target_id, from_id) {
        return true;
    }

    // find the common parent of both namespaces
    let common_parent_id = common_parent(sa, target_id, from_id);

    let target = &sa.namespaces[target_id.to_usize()].read();

    if let Some(common_parent_id) = common_parent_id {
        let common_parent_depth = sa.namespaces[common_parent_id.to_usize()].read().depth;

        if common_parent_depth + 1 == target.depth {
            // siblings are accessible
            element_pub
        } else {
            let start_depth = common_parent_depth + 2;
            for ns_id in &target.parents[start_depth..] {
                let ns = &sa.namespaces[ns_id.to_usize()].read();
                if !ns.is_pub {
                    return false;
                }
            }

            target.is_pub && element_pub
        }
    } else {
        // no common parent: means we try to access another package
        // the whole path needs to be public
        for ns_id in &target.parents {
            let ns = &sa.namespaces[ns_id.to_usize()].read();
            if !ns.is_pub {
                return false;
            }
        }

        target.is_pub && element_pub
    }
}

fn common_parent(
    sa: &SemAnalysis,
    lhs_id: NamespaceId,
    rhs_id: NamespaceId,
) -> Option<NamespaceId> {
    if lhs_id == rhs_id {
        return Some(lhs_id);
    }

    let lhs = &sa.namespaces[lhs_id.to_usize()].read();
    let rhs = &sa.namespaces[rhs_id.to_usize()].read();

    if lhs.depth > rhs.depth {
        if lhs.parents[rhs.depth] == rhs_id {
            return Some(rhs_id);
        } else {
            // do nothing
        }
    } else if rhs.depth > lhs.depth {
        if rhs.parents[lhs.depth] == lhs_id {
            return Some(lhs_id);
        } else {
            // do nothing
        }
    }

    let start = std::cmp::min(lhs.depth, rhs.depth);

    for depth in (0..start).rev() {
        if lhs.parents[depth] == rhs.parents[depth] {
            return Some(lhs.parents[depth]);
        }
    }

    None
}

pub fn namespace_contains(sa: &SemAnalysis, parent_id: NamespaceId, child_id: NamespaceId) -> bool {
    if parent_id == child_id {
        return true;
    }

    let namespace = &sa.namespaces[child_id.to_usize()].read();
    namespace.parents.contains(&parent_id)
}
