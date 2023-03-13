use parking_lot::RwLock;
use std::cmp::max;

use crate::cannon::codegen::{align, size};
use crate::mem;
use crate::object::Header;
use crate::size::InstanceSize;
use crate::vm::{
    create_class_instance_with_vtable, get_concrete_tuple_bty, ClassInstanceId, EnumInstance,
    EnumInstanceId, EnumLayout, FieldInstance, ShapeKind, StructInstance, StructInstanceField,
    StructInstanceId, VM,
};
use dora_frontend::bytecode::{
    BytecodeType, BytecodeTypeArray, ClassData, ClassId, EnumData, EnumId, FunctionId, StructData,
    StructId, TraitData, TraitId,
};

pub fn create_struct_instance(
    vm: &VM,
    struct_id: StructId,
    type_params: BytecodeTypeArray,
) -> StructInstanceId {
    let struct_ = &vm.program.structs[struct_id.0 as usize];
    create_specialized_struct(vm, struct_id, struct_, type_params)
}

fn create_specialized_struct(
    vm: &VM,
    struct_id: StructId,
    struct_: &StructData,
    type_params: BytecodeTypeArray,
) -> StructInstanceId {
    let mut struct_size = 0;
    let mut struct_align = 0;
    let mut fields = Vec::with_capacity(struct_.fields.len());
    let mut ref_fields = Vec::new();

    for f in &struct_.fields {
        let ty = specialize_bty(f.ty.clone(), &type_params);
        debug_assert!(ty.is_concrete_type());

        let field_size = size(vm, ty.clone());
        let field_align = align(vm, ty.clone());

        let offset = mem::align_i32(struct_size, field_align);
        fields.push(StructInstanceField {
            offset,
            ty: ty.clone(),
        });

        struct_size = offset + field_size;
        struct_align = max(struct_align, field_align);

        add_ref_fields(vm, &mut ref_fields, offset, ty);
    }

    struct_size = mem::align_i32(struct_size, struct_align);

    let mut specializations = vm.struct_specializations.write();

    if let Some(&id) = specializations.get(&(struct_id, type_params.clone())) {
        return id;
    }

    let id = vm.struct_instances.push(StructInstance {
        size: struct_size,
        align: struct_align,
        fields,
        ref_fields,
    });

    let old = specializations.insert((struct_id, type_params.clone()), id);
    assert!(old.is_none());

    id
}

pub fn create_enum_instance(
    vm: &VM,
    enum_id: EnumId,
    type_params: BytecodeTypeArray,
) -> EnumInstanceId {
    let enum_ = &vm.program.enums[enum_id.0 as usize];
    specialize_enum(vm, enum_id, enum_, type_params)
}

fn specialize_enum(
    vm: &VM,
    enum_id: EnumId,
    enum_: &EnumData,
    type_params: BytecodeTypeArray,
) -> EnumInstanceId {
    if let Some(&id) = vm
        .enum_specializations
        .read()
        .get(&(enum_id, type_params.clone()))
    {
        return id;
    }

    create_specialized_enum(vm, enum_id, enum_, type_params)
}

fn create_specialized_enum(
    vm: &VM,
    enum_id: EnumId,
    enum_: &EnumData,
    type_params: BytecodeTypeArray,
) -> EnumInstanceId {
    let layout = if enum_is_simple_integer(enum_) {
        EnumLayout::Int
    } else if enum_is_ptr(vm, enum_, &type_params) {
        EnumLayout::Ptr
    } else {
        EnumLayout::Tagged
    };

    let mut specializations = vm.enum_specializations.write();

    if let Some(&id) = specializations.get(&(enum_id, type_params.clone())) {
        return id;
    }

    let variants = if let EnumLayout::Tagged = layout {
        vec![None; enum_.variants.len()]
    } else {
        Vec::new()
    };

    let id = vm.enum_instances.push(EnumInstance {
        enum_id,
        type_params: type_params.clone(),
        layout,
        variants: RwLock::new(variants),
    });

    let old = specializations.insert((enum_id, type_params.clone()), id);
    assert!(old.is_none());

    id
}

fn enum_is_simple_integer(enum_: &EnumData) -> bool {
    for variant in &enum_.variants {
        if !variant.arguments.is_empty() {
            return false;
        }
    }

    true
}

fn enum_is_ptr(_vm: &VM, enum_: &EnumData, type_params: &BytecodeTypeArray) -> bool {
    if enum_.variants.len() != 2 {
        return false;
    }

    let variant1 = enum_.variants.first().unwrap();
    let variant2 = enum_.variants.last().unwrap();

    let (none_variant, some_variant) = if variant1.arguments.is_empty() {
        (variant1, variant2)
    } else {
        (variant2, variant1)
    };

    none_variant.arguments.len() == 0
        && some_variant.arguments.len() == 1
        && specialize_bty(some_variant.arguments.first().unwrap().clone(), type_params)
            .is_reference_type()
}

pub fn ensure_class_instance_for_enum_variant(
    vm: &VM,
    edef: &EnumInstance,
    enum_: &EnumData,
    variant_idx: u32,
) -> ClassInstanceId {
    let mut variants = edef.variants.write();
    let variant = variants[variant_idx as usize];

    if let Some(cls_def_id) = variant {
        return cls_def_id;
    }

    let enum_variant = &enum_.variants[variant_idx as usize];
    let mut csize = Header::size() + 4;
    let mut fields = vec![FieldInstance {
        offset: Header::size(),
        ty: BytecodeType::Int32,
    }];
    let mut ref_fields = Vec::new();

    for ty in &enum_variant.arguments {
        let ty = specialize_bty(ty.clone(), &edef.type_params);
        assert!(ty.is_concrete_type());

        let field_size = size(vm, ty.clone());
        let field_align = align(vm, ty.clone());

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

    variants[variant_idx as usize] = Some(class_instance_id);

    class_instance_id
}

pub fn add_ref_fields(vm: &VM, ref_fields: &mut Vec<i32>, offset: i32, ty: BytecodeType) {
    assert!(ty.is_concrete_type());

    match ty {
        BytecodeType::Tuple(..) => {
            let tuple = get_concrete_tuple_bty(vm, &ty);

            for &ref_offset in tuple.references() {
                ref_fields.push(offset + ref_offset);
            }
        }

        BytecodeType::Enum(enum_id, type_params) => {
            let edef_id = create_enum_instance(vm, enum_id, type_params);
            let edef = vm.enum_instances.idx(edef_id);

            match edef.layout {
                EnumLayout::Int => {}
                EnumLayout::Ptr | EnumLayout::Tagged => {
                    ref_fields.push(offset);
                }
            }
        }

        BytecodeType::Struct(struct_id, type_params) => {
            let sdef_id = create_struct_instance(vm, struct_id, type_params);
            let sdef = vm.struct_instances.idx(sdef_id);

            for &ref_offset in &sdef.ref_fields {
                ref_fields.push(offset + ref_offset);
            }
        }

        BytecodeType::Bool
        | BytecodeType::Char
        | BytecodeType::UInt8
        | BytecodeType::Int32
        | BytecodeType::Int64
        | BytecodeType::Float32
        | BytecodeType::Float64
        | BytecodeType::Unit => {}

        BytecodeType::TypeParam(..) => unreachable!(),

        BytecodeType::Ptr
        | BytecodeType::Class(..)
        | BytecodeType::Lambda(..)
        | BytecodeType::Trait(..) => {
            ref_fields.push(offset);
        }
    }
}

pub fn create_class_instance(
    vm: &VM,
    cls_id: ClassId,
    type_params: &BytecodeTypeArray,
) -> ClassInstanceId {
    let cls = &vm.program.classes[cls_id.0 as usize];
    specialize_class(vm, cls_id, cls, type_params)
}

fn specialize_class(
    vm: &VM,
    cls_id: ClassId,
    cls: &ClassData,
    type_params: &BytecodeTypeArray,
) -> ClassInstanceId {
    if let Some(&id) = vm
        .class_specializations
        .read()
        .get(&(cls_id, type_params.clone()))
    {
        return id;
    }

    create_specialized_class(vm, cls_id, cls, type_params)
}

fn create_specialized_class(
    vm: &VM,
    cls_id: ClassId,
    cls: &ClassData,
    type_params: &BytecodeTypeArray,
) -> ClassInstanceId {
    debug_assert!(type_params.iter().all(|ty| ty.is_concrete_type()));

    if cls.layout.is_array() || cls.layout.is_string() {
        create_specialized_class_array(vm, cls_id, cls, type_params)
    } else {
        assert!(cls.layout.is_regular());
        create_specialized_class_regular(vm, cls_id, cls, type_params)
    }
}

fn create_specialized_class_regular(
    vm: &VM,
    cls_id: ClassId,
    cls: &ClassData,
    type_params: &BytecodeTypeArray,
) -> ClassInstanceId {
    let mut csize = Header::size();
    let mut fields = Vec::new();
    let mut ref_fields = Vec::new();

    for f in &cls.fields {
        let ty = specialize_bty(f.ty.clone(), &type_params);
        debug_assert!(ty.is_concrete_type());

        let field_size = size(vm, ty.clone());
        let field_align = align(vm, ty.clone());

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

    if let Some(&id) = specializations.get(&(cls_id, type_params.clone())) {
        return id;
    }

    let class_instance_id = create_class_instance_with_vtable(
        vm,
        ShapeKind::Class(cls_id, type_params.clone()),
        size,
        fields,
        0,
    );

    let old = specializations.insert((cls_id, type_params.clone()), class_instance_id);
    assert!(old.is_none());

    class_instance_id
}

fn create_specialized_class_array(
    vm: &VM,
    cls_id: ClassId,
    cls: &ClassData,
    type_params: &BytecodeTypeArray,
) -> ClassInstanceId {
    assert!(cls.layout.is_array() || cls.layout.is_string());

    assert!(cls.fields.is_empty());

    let size = if cls.layout.is_array() {
        let element_ty = type_params[0].clone();

        match element_ty {
            BytecodeType::Unit => InstanceSize::UnitArray,
            BytecodeType::Ptr
            | BytecodeType::Class(_, _)
            | BytecodeType::Trait(_, _)
            | BytecodeType::Lambda(_, _) => InstanceSize::ObjArray,

            BytecodeType::Tuple(_) => {
                let tuple = get_concrete_tuple_bty(vm, &element_ty);
                InstanceSize::StructArray(tuple.size())
            }

            BytecodeType::Struct(struct_id, type_params) => {
                let sdef_id = create_struct_instance(vm, struct_id, type_params);
                let sdef = vm.struct_instances.idx(sdef_id);

                InstanceSize::StructArray(sdef.size)
            }

            BytecodeType::Enum(enum_id, type_params) => {
                let edef_id = create_enum_instance(vm, enum_id, type_params);
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
            | BytecodeType::Float64 => InstanceSize::PrimitiveArray(size(vm, element_ty)),

            BytecodeType::TypeParam(_) => {
                unreachable!()
            }
        }
    } else {
        assert!(cls.layout.is_string());
        InstanceSize::Str
    };

    let mut specializations = vm.class_specializations.write();

    if let Some(&id) = specializations.get(&(cls_id, type_params.clone())) {
        return id;
    }

    let class_instance_id = create_class_instance_with_vtable(
        vm,
        ShapeKind::Class(cls_id, type_params.clone()),
        size,
        Vec::new(),
        0,
    );

    let old = specializations.insert((cls_id, type_params.clone()), class_instance_id);
    assert!(old.is_none());

    class_instance_id
}

pub fn ensure_class_instance_for_lambda(
    vm: &VM,
    fct_id: FunctionId,
    type_params: BytecodeTypeArray,
) -> ClassInstanceId {
    // Lambda object only has context field at the moment.
    let size = InstanceSize::Fixed(Header::size() + mem::ptr_width());
    let fields = vec![FieldInstance {
        offset: Header::size(),
        ty: BytecodeType::Ptr,
    }];

    create_class_instance_with_vtable(vm, ShapeKind::Lambda(fct_id, type_params), size, fields, 1)
}

pub fn ensure_class_instance_for_trait_object(
    vm: &VM,
    trait_id: TraitId,
    trait_type_params: &BytecodeTypeArray,
    object_type: BytecodeType,
) -> ClassInstanceId {
    let trait_ = &vm.program.traits[trait_id.0 as usize];

    let combined_type_params = trait_type_params.append(object_type.clone());

    if let Some(&id) = vm
        .trait_vtables
        .read()
        .get(&(trait_id, combined_type_params.clone()))
    {
        return id;
    }

    create_specialized_class_for_trait_object(
        vm,
        trait_id,
        trait_,
        combined_type_params,
        object_type,
    )
}

fn create_specialized_class_for_trait_object(
    vm: &VM,
    trait_id: TraitId,
    trait_: &TraitData,
    combined_type_params: BytecodeTypeArray,
    object_type: BytecodeType,
) -> ClassInstanceId {
    let mut csize;
    let mut fields;
    let mut ref_fields;

    fields = Vec::with_capacity(1);
    ref_fields = Vec::new();
    csize = Header::size();

    debug_assert!(object_type.is_concrete_type());

    let field_size = size(vm, object_type.clone());
    let field_align = align(vm, object_type.clone());

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

    if let Some(&id) = vtables.get(&(trait_id, combined_type_params.clone())) {
        return id;
    }

    let class_instance_id = create_class_instance_with_vtable(
        vm,
        ShapeKind::TraitObject {
            object_ty: object_type,
            trait_id,
            combined_type_params: combined_type_params.clone(),
        },
        size,
        fields,
        trait_.methods.len(),
    );

    let old = vtables.insert((trait_id, combined_type_params), class_instance_id);
    assert!(old.is_none());

    class_instance_id
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
