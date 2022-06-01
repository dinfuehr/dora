use crate::language::sem_analysis::{
    ClassDefinitionId, ConstDefinitionId, EnumDefinitionId, FctDefinitionId, FctParent, FieldId,
    GlobalDefinitionId, ModuleDefinitionId, SemAnalysis, StructDefinitionFieldId,
    StructDefinitionId, TraitDefinitionId,
};
use crate::language::sym::Sym;

pub fn sym_accessible_from(sa: &SemAnalysis, sym: Sym, module_id: ModuleDefinitionId) -> bool {
    match sym {
        Sym::Annotation(_) => unimplemented!(),
        Sym::Class(class_id) => class_accessible_from(sa, class_id, module_id),
        Sym::Const(const_id) => const_accessible_from(sa, const_id, module_id),
        Sym::Enum(enum_id) => enum_accessible_from(sa, enum_id, module_id),
        Sym::EnumVariant(_, _) => unreachable!(),
        Sym::Fct(fct_id) => fct_accessible_from(sa, fct_id, module_id),
        Sym::Field(_) => unreachable!(),
        Sym::Global(global_id) => global_accessible_from(sa, global_id, module_id),
        Sym::Module(sym_module_id) => module_accessible_from(sa, sym_module_id, module_id),
        Sym::Struct(struct_id) => struct_accessible_from(sa, struct_id, module_id),
        Sym::Trait(trait_id) => trait_accessible_from(sa, trait_id, module_id),
        Sym::TypeParam(_) => unreachable!(),
        Sym::Var(_) => unreachable!(),
    }
}

pub fn global_accessible_from(
    sa: &SemAnalysis,
    global_id: GlobalDefinitionId,
    module_id: ModuleDefinitionId,
) -> bool {
    let global = sa.globals.idx(global_id);
    let global = global.read();

    accessible_from(sa, global.module_id, global.is_pub, module_id)
}

pub fn class_accessible_from(
    sa: &SemAnalysis,
    cls_id: ClassDefinitionId,
    module_id: ModuleDefinitionId,
) -> bool {
    let cls = sa.classes.idx(cls_id);
    let cls = cls.read();

    accessible_from(sa, cls.module_id, cls.is_pub, module_id)
}

pub fn class_field_accessible_from(
    sa: &SemAnalysis,
    cls_id: ClassDefinitionId,
    field_id: FieldId,
    module_id: ModuleDefinitionId,
) -> bool {
    let cls = sa.classes.idx(cls_id);
    let cls = cls.read();

    let field = &cls.fields[field_id];

    accessible_from(sa, cls.module_id, cls.is_pub && field.is_pub, module_id)
}

pub fn method_accessible_from(
    sa: &SemAnalysis,
    fct_id: FctDefinitionId,
    module_id: ModuleDefinitionId,
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

        FctParent::Function(_) | FctParent::None => unreachable!(),
    };

    accessible_from(sa, fct.module_id, element_pub, module_id)
}

pub fn fct_accessible_from(
    sa: &SemAnalysis,
    fct_id: FctDefinitionId,
    module_id: ModuleDefinitionId,
) -> bool {
    let fct = sa.fcts.idx(fct_id);
    let fct = fct.read();

    accessible_from(sa, fct.module_id, fct.is_pub, module_id)
}

pub fn enum_accessible_from(
    sa: &SemAnalysis,
    enum_id: EnumDefinitionId,
    module_id: ModuleDefinitionId,
) -> bool {
    let enum_ = sa.enums[enum_id].read();

    accessible_from(sa, enum_.module_id, enum_.is_pub, module_id)
}

pub fn struct_accessible_from(
    sa: &SemAnalysis,
    struct_id: StructDefinitionId,
    module_id: ModuleDefinitionId,
) -> bool {
    let xstruct = sa.structs.idx(struct_id);
    let xstruct = xstruct.read();

    accessible_from(sa, xstruct.module_id, xstruct.is_pub, module_id)
}

pub fn struct_field_accessible_from(
    sa: &SemAnalysis,
    struct_id: StructDefinitionId,
    field_id: StructDefinitionFieldId,
    module_id: ModuleDefinitionId,
) -> bool {
    let xstruct = sa.structs.idx(struct_id);
    let xstruct = xstruct.read();

    let field = &xstruct.fields[field_id.to_usize()];

    accessible_from(
        sa,
        xstruct.module_id,
        xstruct.is_pub && field.is_pub,
        module_id,
    )
}

pub fn module_accessible_from(
    sa: &SemAnalysis,
    target_id: ModuleDefinitionId,
    from_id: ModuleDefinitionId,
) -> bool {
    accessible_from(sa, target_id, true, from_id)
}

pub fn trait_accessible_from(
    sa: &SemAnalysis,
    trait_id: TraitDefinitionId,
    module_id: ModuleDefinitionId,
) -> bool {
    let trait_ = sa.traits[trait_id].read();

    accessible_from(sa, trait_.module_id, trait_.is_pub, module_id)
}

pub fn const_accessible_from(
    sa: &SemAnalysis,
    const_id: ConstDefinitionId,
    module_id: ModuleDefinitionId,
) -> bool {
    let const_ = sa.consts.idx(const_id);
    let const_ = const_.read();

    accessible_from(sa, const_.module_id, const_.is_pub, module_id)
}

pub fn is_default_accessible(
    sa: &SemAnalysis,
    target_id: ModuleDefinitionId,
    from_id: ModuleDefinitionId,
) -> bool {
    // each module can access itself
    if target_id == from_id {
        return true;
    }

    // modules can access all their parents
    module_contains(sa, target_id, from_id)
}

fn accessible_from(
    sa: &SemAnalysis,
    target_id: ModuleDefinitionId,
    element_pub: bool,
    from_id: ModuleDefinitionId,
) -> bool {
    // each module can access itself
    if target_id == from_id {
        return true;
    }

    // modules can access all their parents
    if module_contains(sa, target_id, from_id) {
        return true;
    }

    // find the common parent of both modules
    let common_parent_id = common_parent(sa, target_id, from_id);

    let target = &sa.modules[target_id].read();

    if let Some(common_parent_id) = common_parent_id {
        let common_parent_depth = sa.modules[common_parent_id].read().depth;

        if common_parent_depth + 1 == target.depth {
            // siblings are accessible
            element_pub
        } else {
            let start_depth = common_parent_depth + 2;
            for &ns_id in &target.parents[start_depth..] {
                let ns = &sa.modules[ns_id].read();
                if !ns.is_pub {
                    return false;
                }
            }

            target.is_pub && element_pub
        }
    } else {
        // no common parent: means we try to access another package
        // the whole path needs to be public
        for &ns_id in &target.parents {
            let ns = &sa.modules[ns_id].read();
            if !ns.is_pub {
                return false;
            }
        }

        target.is_pub && element_pub
    }
}

fn common_parent(
    sa: &SemAnalysis,
    lhs_id: ModuleDefinitionId,
    rhs_id: ModuleDefinitionId,
) -> Option<ModuleDefinitionId> {
    if lhs_id == rhs_id {
        return Some(lhs_id);
    }

    let lhs = &sa.modules[lhs_id].read();
    let rhs = &sa.modules[rhs_id].read();

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

pub fn module_contains(
    sa: &SemAnalysis,
    parent_id: ModuleDefinitionId,
    child_id: ModuleDefinitionId,
) -> bool {
    if parent_id == child_id {
        return true;
    }

    let module = &sa.modules[child_id].read();
    module.parents.contains(&parent_id)
}
