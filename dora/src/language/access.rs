use crate::vm::{
    accessible_from, ClassDefinitionId, EnumDefinitionId, FctDefinitionId, FctParent, FieldId,
    GlobalDefinitionId, NamespaceId, SemAnalysis, StructDefinitionFieldId, StructDefinitionId,
};

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

        FctParent::Module(module_id) => {
            let module = sa.modules.idx(module_id);
            let module = module.read();

            module.is_pub && fct.is_pub
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
