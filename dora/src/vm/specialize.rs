use parking_lot::RwLock;
use std::cmp::max;

use crate::bytecode::{BytecodeType, BytecodeTypeArray};
use crate::language::generator::{bty_array_from_ty, ty_array_from_bty, ty_from_bty};
use crate::language::sem_analysis::{ClassDefinitionId, FctDefinitionId, TraitDefinitionId};
use crate::language::ty::{SourceType, SourceTypeArray};
use crate::mem;
use crate::object::Header;
use crate::size::InstanceSize;
use crate::vm::{
    create_class_instance_with_vtable, get_concrete_tuple_ty, ClassDefinition, ClassInstanceId,
    EnumDefinition, EnumDefinitionId, EnumInstance, EnumInstanceId, EnumLayout, FieldInstance,
    ShapeKind, StructDefinition, StructDefinitionId, StructInstance, StructInstanceField,
    StructInstanceId, TraitDefinition, VM,
};

pub fn specialize_type(vm: &VM, ty: SourceType, type_params: &SourceTypeArray) -> SourceType {
    replace_type_param(vm, ty, type_params, None)
}

pub fn specialize_type_list(
    vm: &VM,
    list: &SourceTypeArray,
    type_params: &SourceTypeArray,
) -> SourceTypeArray {
    let types = list.types();

    if types.is_empty() {
        return SourceTypeArray::empty();
    }

    let mut specialized_types = Vec::with_capacity(types.len());

    for ty in types {
        let ty = replace_type_param(vm, ty.clone(), type_params, None);
        specialized_types.push(ty);
    }

    SourceTypeArray::with(specialized_types)
}

pub fn specialize_struct_id_params(
    vm: &VM,
    struct_id: StructDefinitionId,
    type_params: BytecodeTypeArray,
) -> StructInstanceId {
    let struc = vm.structs.idx(struct_id);
    let struc = struc.read();
    specialize_struct(vm, &*struc, type_params)
}

pub fn specialize_struct(
    vm: &VM,
    struct_: &StructDefinition,
    type_params: BytecodeTypeArray,
) -> StructInstanceId {
    if let Some(&id) = vm
        .struct_specializations
        .read()
        .get(&(struct_.id(), type_params.clone()))
    {
        return id;
    }

    create_specialized_struct(vm, struct_, type_params)
}

fn create_specialized_struct(
    vm: &VM,
    struct_: &StructDefinition,
    type_params: BytecodeTypeArray,
) -> StructInstanceId {
    assert!(struct_.primitive_ty.is_none());

    let mut size = 0;
    let mut align = 0;
    let mut fields = Vec::with_capacity(struct_.fields.len());
    let mut ref_fields = Vec::new();

    for f in &struct_.fields {
        let ty = specialize_type(vm, f.ty.clone(), &ty_array_from_bty(&type_params));
        debug_assert!(ty.is_concrete_type());

        let field_size = ty.size(vm);
        let field_align = ty.align(vm);

        let offset = mem::align_i32(size, field_align);
        fields.push(StructInstanceField {
            offset,
            ty: ty.clone(),
        });

        size = offset + field_size;
        align = max(align, field_align);

        add_ref_fields(vm, &mut ref_fields, offset, ty);
    }

    size = mem::align_i32(size, align);

    let mut specializations = vm.struct_specializations.write();

    if let Some(&id) = specializations.get(&(struct_.id(), type_params.clone())) {
        return id;
    }

    let id = vm.struct_instances.push(StructInstance {
        size,
        align,
        fields,
        ref_fields,
    });

    let old = specializations.insert((struct_.id(), type_params.clone()), id);
    assert!(old.is_none());

    id
}

pub fn specialize_enum_id_params(
    vm: &VM,
    enum_id: EnumDefinitionId,
    type_params: SourceTypeArray,
) -> EnumInstanceId {
    let enum_ = &vm.enums[enum_id];
    let enum_ = enum_.read();
    specialize_enum(vm, &*enum_, type_params)
}

pub fn specialize_enum(
    vm: &VM,
    enum_: &EnumDefinition,
    type_params: SourceTypeArray,
) -> EnumInstanceId {
    if let Some(&id) = vm
        .enum_specializations
        .read()
        .get(&(enum_.id(), type_params.clone()))
    {
        return id;
    }

    create_specialized_enum(vm, enum_, type_params)
}

fn create_specialized_enum(
    vm: &VM,
    enum_: &EnumDefinition,
    type_params: SourceTypeArray,
) -> EnumInstanceId {
    let layout = if enum_is_simple_integer(enum_) {
        EnumLayout::Int
    } else if enum_is_ptr(vm, enum_, &type_params) {
        EnumLayout::Ptr
    } else {
        EnumLayout::Tagged
    };

    let mut specializations = vm.enum_specializations.write();

    if let Some(&id) = specializations.get(&(enum_.id(), type_params.clone())) {
        return id;
    }

    let variants = if let EnumLayout::Tagged = layout {
        vec![None; enum_.variants.len()]
    } else {
        Vec::new()
    };

    let id = vm.enum_instances.push(EnumInstance {
        enum_id: enum_.id(),
        type_params: type_params.clone(),
        layout,
        variants: RwLock::new(variants),
    });

    let old = specializations.insert((enum_.id(), type_params.clone()), id);
    assert!(old.is_none());

    id
}

fn enum_is_simple_integer(enum_: &EnumDefinition) -> bool {
    for variant in &enum_.variants {
        if !variant.types.is_empty() {
            return false;
        }
    }

    true
}

fn enum_is_ptr(vm: &VM, enum_: &EnumDefinition, type_params: &SourceTypeArray) -> bool {
    if enum_.variants.len() != 2 {
        return false;
    }

    let variant1 = enum_.variants.first().unwrap();
    let variant2 = enum_.variants.last().unwrap();

    let (none_variant, some_variant) = if variant1.types.is_empty() {
        (variant1, variant2)
    } else {
        (variant2, variant1)
    };

    none_variant.types.len() == 0
        && some_variant.types.len() == 1
        && specialize_type(vm, some_variant.types.first().unwrap().clone(), type_params)
            .reference_type()
}

pub fn specialize_enum_class(
    vm: &VM,
    edef: &EnumInstance,
    enum_: &EnumDefinition,
    variant_idx: usize,
) -> ClassInstanceId {
    let mut variants = edef.variants.write();
    let variant = variants[variant_idx];

    if let Some(cls_def_id) = variant {
        return cls_def_id;
    }

    let enum_variant = &enum_.variants[variant_idx];
    let mut csize = Header::size() + 4;
    let mut fields = vec![FieldInstance {
        offset: Header::size(),
        ty: SourceType::Int32,
    }];
    let mut ref_fields = Vec::new();

    for ty in &enum_variant.types {
        let ty = replace_type_param(vm, ty.clone(), &edef.type_params, None);
        assert!(ty.is_concrete_type());

        let field_size = ty.size(vm);
        let field_align = ty.align(vm);

        let offset = mem::align_i32(csize, field_align);
        fields.push(FieldInstance {
            offset,
            ty: ty.clone(),
        });

        csize = offset + field_size;

        add_ref_fields(vm, &mut ref_fields, offset, ty);
    }

    let instance_size = mem::align_i32(csize, mem::ptr_width());

    let class_instance_id = create_class_instance_with_vtable(
        vm,
        ShapeKind::Enum(edef.enum_id, edef.type_params.clone()),
        InstanceSize::Fixed(instance_size),
        fields,
        0,
    );

    variants[variant_idx] = Some(class_instance_id);

    class_instance_id
}

pub fn add_ref_fields(vm: &VM, ref_fields: &mut Vec<i32>, offset: i32, ty: SourceType) {
    assert!(ty.is_concrete_type());

    if ty.is_tuple() {
        let tuple = get_concrete_tuple_ty(vm, &ty);

        for &ref_offset in tuple.references() {
            ref_fields.push(offset + ref_offset);
        }
    } else if let SourceType::Enum(enum_id, type_params) = ty.clone() {
        let edef_id = specialize_enum_id_params(vm, enum_id, type_params);
        let edef = vm.enum_instances.idx(edef_id);

        match edef.layout {
            EnumLayout::Int => {}
            EnumLayout::Ptr | EnumLayout::Tagged => {
                ref_fields.push(offset);
            }
        }
    } else if let SourceType::Struct(struct_id, type_params) = ty.clone() {
        let sdef_id = specialize_struct_id_params(vm, struct_id, bty_array_from_ty(&type_params));
        let sdef = vm.struct_instances.idx(sdef_id);

        for &ref_offset in &sdef.ref_fields {
            ref_fields.push(offset + ref_offset);
        }
    } else if ty.reference_type() {
        ref_fields.push(offset);
    }
}

pub fn specialize_class_id(vm: &VM, cls_id: ClassDefinitionId) -> ClassInstanceId {
    let cls = vm.classes.idx(cls_id);
    let cls = cls.read();
    specialize_class(vm, &*cls, &BytecodeTypeArray::empty())
}

pub fn specialize_class_id_params(
    vm: &VM,
    cls_id: ClassDefinitionId,
    type_params: &BytecodeTypeArray,
) -> ClassInstanceId {
    let cls = vm.classes.idx(cls_id);
    let cls = cls.read();
    specialize_class(vm, &*cls, &type_params)
}

pub fn specialize_class(
    vm: &VM,
    cls: &ClassDefinition,
    type_params: &BytecodeTypeArray,
) -> ClassInstanceId {
    if let Some(&id) = vm
        .class_specializations
        .read()
        .get(&(cls.id(), type_params.clone()))
    {
        return id;
    }

    create_specialized_class(vm, cls, type_params)
}

fn create_specialized_class(
    vm: &VM,
    cls: &ClassDefinition,
    type_params: &BytecodeTypeArray,
) -> ClassInstanceId {
    debug_assert!(type_params.iter().all(|ty| ty.is_concrete_type()));

    if cls.is_array || cls.is_str {
        create_specialized_class_array(vm, cls, type_params)
    } else {
        create_specialized_class_regular(vm, cls, type_params)
    }
}

fn create_specialized_class_regular(
    vm: &VM,
    cls: &ClassDefinition,
    type_params: &BytecodeTypeArray,
) -> ClassInstanceId {
    let mut csize = Header::size();
    let mut fields = Vec::new();
    let mut ref_fields = Vec::new();

    for f in &cls.fields {
        let ty = specialize_type(vm, f.ty.clone(), &ty_array_from_bty(&type_params));
        debug_assert!(ty.is_concrete_type());

        let field_size = ty.size(vm);
        let field_align = ty.align(vm);

        let offset = mem::align_i32(csize, field_align);
        fields.push(FieldInstance {
            offset,
            ty: ty.clone(),
        });

        csize = offset + field_size;

        add_ref_fields(vm, &mut ref_fields, offset, ty);
    }

    let size = InstanceSize::Fixed(mem::align_i32(csize, mem::ptr_width()));

    let mut specializations = vm.class_specializations.write();

    if let Some(&id) = specializations.get(&(cls.id(), type_params.clone())) {
        return id;
    }

    let class_instance_id = create_class_instance_with_vtable(
        vm,
        ShapeKind::Class(cls.id(), ty_array_from_bty(type_params)),
        size,
        fields,
        0,
    );

    let old = specializations.insert((cls.id(), type_params.clone()), class_instance_id);
    assert!(old.is_none());

    class_instance_id
}

fn create_specialized_class_array(
    vm: &VM,
    cls: &ClassDefinition,
    type_params: &BytecodeTypeArray,
) -> ClassInstanceId {
    assert!(cls.is_array || cls.is_str);

    assert!(cls.fields.is_empty());

    let size = if cls.is_array {
        let element_ty = type_params[0].clone();

        match element_ty {
            BytecodeType::Unit => InstanceSize::UnitArray,
            BytecodeType::Ptr
            | BytecodeType::Class(_, _)
            | BytecodeType::Trait(_, _)
            | BytecodeType::Lambda(_, _) => InstanceSize::ObjArray,

            BytecodeType::Tuple(_) => {
                let tuple = get_concrete_tuple_ty(vm, &ty_from_bty(element_ty));
                InstanceSize::StructArray(tuple.size())
            }

            BytecodeType::Struct(struct_id, type_params) => {
                let sdef_id = specialize_struct_id_params(vm, struct_id, type_params);
                let sdef = vm.struct_instances.idx(sdef_id);

                InstanceSize::StructArray(sdef.size)
            }

            BytecodeType::Enum(enum_id, type_params) => {
                let edef_id =
                    specialize_enum_id_params(vm, enum_id, ty_array_from_bty(&type_params));
                let edef = vm.enum_instances.idx(edef_id);

                match edef.layout {
                    EnumLayout::Int => InstanceSize::PrimitiveArray(4),
                    EnumLayout::Ptr | EnumLayout::Tagged => InstanceSize::ObjArray,
                }
            }

            BytecodeType::Bool
            | BytecodeType::UInt8
            | BytecodeType::Char
            | BytecodeType::Int32
            | BytecodeType::Int64
            | BytecodeType::Float32
            | BytecodeType::Float64 => {
                InstanceSize::PrimitiveArray(ty_from_bty(element_ty).size(vm))
            }

            BytecodeType::TypeParam(_) => {
                unreachable!()
            }
        }
    } else {
        InstanceSize::Str
    };

    let mut specializations = vm.class_specializations.write();

    if let Some(&id) = specializations.get(&(cls.id(), type_params.clone())) {
        return id;
    }

    let class_instance_id = create_class_instance_with_vtable(
        vm,
        ShapeKind::Class(cls.id(), ty_array_from_bty(type_params)),
        size,
        Vec::new(),
        0,
    );

    let old = specializations.insert((cls.id(), type_params.clone()), class_instance_id);
    assert!(old.is_none());

    class_instance_id
}

pub fn specialize_lambda(
    vm: &VM,
    fct_id: FctDefinitionId,
    type_params: SourceTypeArray,
) -> ClassInstanceId {
    // Lambda object only has context field at the moment.
    let size = InstanceSize::Fixed(Header::size() + mem::ptr_width());
    let fields = vec![FieldInstance {
        offset: Header::size(),
        ty: SourceType::Ptr,
    }];

    create_class_instance_with_vtable(
        vm,
        ShapeKind::Lambda(fct_id, type_params.clone()),
        size,
        fields,
        1,
    )
}

pub fn specialize_trait_object(
    vm: &VM,
    trait_id: TraitDefinitionId,
    trait_type_params: &SourceTypeArray,
    object_type: SourceType,
) -> ClassInstanceId {
    let trait_ = vm.traits[trait_id].read();

    let combined_type_params = trait_type_params.connect_single(object_type.clone());

    if let Some(&id) = vm
        .trait_vtables
        .read()
        .get(&(trait_id, combined_type_params.clone()))
    {
        return id;
    }

    create_specialized_class_for_trait_object(vm, &*trait_, combined_type_params, object_type)
}

fn create_specialized_class_for_trait_object(
    vm: &VM,
    trait_: &TraitDefinition,
    combined_type_params: SourceTypeArray,
    object_type: SourceType,
) -> ClassInstanceId {
    let mut csize;
    let mut fields;
    let mut ref_fields;

    fields = Vec::with_capacity(1);
    ref_fields = Vec::new();
    csize = Header::size();

    debug_assert!(object_type.is_concrete_type());

    let field_size = object_type.size(vm);
    let field_align = object_type.align(vm);

    let offset = mem::align_i32(csize, field_align);
    fields.push(FieldInstance {
        offset,
        ty: object_type.clone(),
    });
    add_ref_fields(vm, &mut ref_fields, offset, object_type.clone());
    csize = offset + field_size;
    csize = mem::align_i32(csize, mem::ptr_width());
    let size = InstanceSize::Fixed(csize);

    let mut vtables = vm.trait_vtables.write();

    if let Some(&id) = vtables.get(&(trait_.id(), combined_type_params.clone())) {
        return id;
    }

    let class_instance_id = create_class_instance_with_vtable(
        vm,
        ShapeKind::TraitObject {
            object_ty: object_type.clone(),
            trait_id: trait_.id(),
            combined_type_params: combined_type_params.clone(),
        },
        size,
        fields,
        trait_.methods.len(),
    );

    let old = vtables.insert((trait_.id(), combined_type_params), class_instance_id);
    assert!(old.is_none());

    class_instance_id
}

pub fn replace_type_param(
    vm: &VM,
    ty: SourceType,
    type_params: &SourceTypeArray,
    self_ty: Option<SourceType>,
) -> SourceType {
    match ty {
        SourceType::TypeParam(tpid) => type_params[tpid.to_usize()].clone(),

        SourceType::Class(cls_id, params) => {
            let params = SourceTypeArray::with(
                params
                    .iter()
                    .map(|p| replace_type_param(vm, p, type_params, self_ty.clone()))
                    .collect::<Vec<_>>(),
            );

            SourceType::Class(cls_id, params)
        }

        SourceType::Trait(trait_id, old_type_params) => {
            let new_type_params = SourceTypeArray::with(
                old_type_params
                    .iter()
                    .map(|p| replace_type_param(vm, p, type_params, self_ty.clone()))
                    .collect::<Vec<_>>(),
            );

            SourceType::Trait(trait_id, new_type_params)
        }

        SourceType::Struct(struct_id, old_type_params) => {
            let new_type_params = SourceTypeArray::with(
                old_type_params
                    .iter()
                    .map(|p| replace_type_param(vm, p, type_params, self_ty.clone()))
                    .collect::<Vec<_>>(),
            );

            SourceType::Struct(struct_id, new_type_params)
        }

        SourceType::Enum(enum_id, old_type_params) => {
            let new_type_params = SourceTypeArray::with(
                old_type_params
                    .iter()
                    .map(|p| replace_type_param(vm, p, type_params, self_ty.clone()))
                    .collect::<Vec<_>>(),
            );

            SourceType::Enum(enum_id, new_type_params)
        }

        SourceType::This => self_ty.expect("no type for Self given"),

        SourceType::Lambda(params, return_type) => {
            let new_params = SourceTypeArray::with(
                params
                    .iter()
                    .map(|p| replace_type_param(vm, p, type_params, self_ty.clone()))
                    .collect::<Vec<_>>(),
            );

            let return_type = replace_type_param(
                vm,
                return_type.as_ref().clone(),
                type_params,
                self_ty.clone(),
            );

            SourceType::Lambda(new_params, Box::new(return_type))
        }

        SourceType::Tuple(subtypes) => {
            let new_subtypes = subtypes
                .iter()
                .map(|t| replace_type_param(vm, t.clone(), type_params, self_ty.clone()))
                .collect::<Vec<_>>();

            SourceType::Tuple(SourceTypeArray::with(new_subtypes))
        }

        SourceType::Unit
        | SourceType::UInt8
        | SourceType::Bool
        | SourceType::Char
        | SourceType::Int32
        | SourceType::Int64
        | SourceType::Float32
        | SourceType::Float64
        | SourceType::Error
        | SourceType::Ptr => ty,

        SourceType::Any => {
            panic!("unexpected type = {:?}", ty);
            // unreachable!()
        }
    }
}

pub fn specialize_bty_array(
    types: &BytecodeTypeArray,
    type_params: &BytecodeTypeArray,
) -> BytecodeTypeArray {
    let types = types
        .iter()
        .map(|p| specialize_bty(p, type_params))
        .collect();
    BytecodeTypeArray::new(types)
}

pub fn specialize_bty(ty: BytecodeType, type_params: &BytecodeTypeArray) -> BytecodeType {
    match ty {
        BytecodeType::TypeParam(tpid) => type_params[tpid as usize].clone(),

        BytecodeType::Class(cls_id, params) => {
            let params = specialize_bty_array(&params, type_params);
            BytecodeType::Class(cls_id, params)
        }

        BytecodeType::Trait(trait_id, params) => {
            let params = specialize_bty_array(&params, type_params);
            BytecodeType::Trait(trait_id, params)
        }

        BytecodeType::Struct(struct_id, params) => {
            let params = specialize_bty_array(&params, type_params);
            BytecodeType::Struct(struct_id, params)
        }

        BytecodeType::Enum(enum_id, params) => {
            let params = specialize_bty_array(&params, type_params);
            BytecodeType::Enum(enum_id, params)
        }

        BytecodeType::Lambda(params, return_type) => {
            let params = specialize_bty_array(&params, type_params);
            let return_type = specialize_bty(return_type.as_ref().clone(), type_params);
            BytecodeType::Lambda(params, Box::new(return_type))
        }

        BytecodeType::Tuple(subtypes) => {
            let subtypes = specialize_bty_array(&subtypes, type_params);
            BytecodeType::Tuple(subtypes)
        }

        BytecodeType::Unit
        | BytecodeType::UInt8
        | BytecodeType::Bool
        | BytecodeType::Char
        | BytecodeType::Int32
        | BytecodeType::Int64
        | BytecodeType::Float32
        | BytecodeType::Float64
        | BytecodeType::Ptr => ty,
    }
}
