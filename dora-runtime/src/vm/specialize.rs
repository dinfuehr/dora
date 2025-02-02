use parking_lot::RwLock;
use std::cmp::max;

use crate::cannon::codegen::{align, size};
use crate::mem;
use crate::mirror::Header;
use crate::size::InstanceSize;
use crate::vm::{
    create_shape, find_impl, get_concrete_tuple_bty, BytecodeTypeExt, EnumInstance, EnumInstanceId,
    EnumLayout, FieldInstance, ShapeKind, StructInstance, StructInstanceField, StructInstanceId,
    VM,
};
use crate::Shape;
use dora_bytecode::{
    BytecodeType, BytecodeTypeArray, ClassData, ClassId, EnumData, EnumId, FunctionId, Program,
    StructData, StructId, TraitId, TypeParamData,
};

pub fn create_struct_instance(
    vm: &VM,
    struct_id: StructId,
    type_params: BytecodeTypeArray,
) -> StructInstanceId {
    let struct_ = vm.struct_(struct_id);
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
    let enum_ = &vm.enum_(enum_id);
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

pub fn ensure_shape_for_enum_variant(
    vm: &VM,
    edef: &EnumInstance,
    enum_: &EnumData,
    variant_idx: u32,
) -> *const Shape {
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

    let shape = create_shape(
        vm,
        ShapeKind::Enum(edef.enum_id, edef.type_params.clone()),
        InstanceSize::Fixed(instance_size),
        fields,
        0,
    );

    variants[variant_idx as usize] = Some(shape);

    shape
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

        BytecodeType::TypeAlias(..)
        | BytecodeType::Assoc(..)
        | BytecodeType::GenericAssoc { .. }
        | BytecodeType::TypeParam(..)
        | BytecodeType::This => {
            unreachable!()
        }

        BytecodeType::Ptr
        | BytecodeType::Class(..)
        | BytecodeType::Lambda(..)
        | BytecodeType::TraitObject(..) => {
            ref_fields.push(offset);
        }
    }
}

pub fn create_shape_for_class(
    vm: &VM,
    cls_id: ClassId,
    type_params: &BytecodeTypeArray,
) -> *const Shape {
    let cls = vm.class(cls_id);

    if let Some(&shape) = vm.class_shapes.read().get(&(cls_id, type_params.clone())) {
        return shape;
    }

    debug_assert!(type_params.iter().all(|ty| ty.is_concrete_type()));

    if vm.known.array_class_id() == cls_id || vm.known.string_class_id() == cls_id {
        create_shape_for_array_class(vm, cls_id, cls, type_params)
    } else {
        create_shape_for_regular_class(vm, cls_id, cls, type_params)
    }
}

fn create_shape_for_regular_class(
    vm: &VM,
    cls_id: ClassId,
    cls: &ClassData,
    type_params: &BytecodeTypeArray,
) -> *const Shape {
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

    let mut specializations = vm.class_shapes.write();

    if let Some(&shape) = specializations.get(&(cls_id, type_params.clone())) {
        return shape;
    }

    let shape = create_shape(
        vm,
        ShapeKind::Class(cls_id, type_params.clone()),
        size,
        fields,
        0,
    );

    let old = specializations.insert((cls_id, type_params.clone()), shape);
    assert!(old.is_none());

    shape
}

fn create_shape_for_array_class(
    vm: &VM,
    cls_id: ClassId,
    cls: &ClassData,
    type_params: &BytecodeTypeArray,
) -> *const Shape {
    assert!(cls.fields.is_empty());

    let size = if vm.known.array_class_id() == cls_id {
        assert_eq!(type_params.len(), 1);
        let element_ty = type_params[0].clone();

        match element_ty {
            BytecodeType::Unit => InstanceSize::UnitArray,
            BytecodeType::Ptr
            | BytecodeType::Class(..)
            | BytecodeType::TraitObject(..)
            | BytecodeType::Lambda(..) => InstanceSize::ObjArray,

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

            BytecodeType::TypeAlias(..)
            | BytecodeType::Assoc(..)
            | BytecodeType::GenericAssoc { .. }
            | BytecodeType::TypeParam(_)
            | BytecodeType::This => {
                unreachable!()
            }
        }
    } else {
        assert!(type_params.is_empty());
        InstanceSize::Str
    };

    let mut specializations = vm.class_shapes.write();

    if let Some(&shape) = specializations.get(&(cls_id, type_params.clone())) {
        return shape;
    }

    let shape = create_shape(
        vm,
        ShapeKind::Class(cls_id, type_params.clone()),
        size,
        Vec::new(),
        0,
    );

    let old = specializations.insert((cls_id, type_params.clone()), shape);
    assert!(old.is_none());

    shape
}

pub fn ensure_shape_for_lambda(
    vm: &VM,
    fct_id: FunctionId,
    type_params: BytecodeTypeArray,
) -> *const Shape {
    let key = (fct_id, type_params.clone());

    let mut lambda_shapes = vm.lambda_shapes.write();

    if let Some(&shape) = lambda_shapes.get(&key) {
        return shape;
    }

    // Lambda object only has context field at the moment.
    let size = InstanceSize::Fixed(Header::size() + mem::ptr_width());
    let fields = vec![FieldInstance {
        offset: Header::size(),
        ty: BytecodeType::Ptr,
    }];

    let shape = create_shape(vm, ShapeKind::Lambda(fct_id, type_params), size, fields, 1);

    lambda_shapes.insert(key, shape);
    shape
}

pub fn compute_vtable_index(vm: &VM, trait_id: TraitId, trait_fct_id: FunctionId) -> u32 {
    let trait_ = vm.trait_(trait_id);
    let vtable_index = trait_
        .methods
        .iter()
        .position(|m| *m == trait_fct_id)
        .expect("missing trait function");
    vtable_index.try_into().expect("overflow")
}

pub fn ensure_shape_for_trait_object(
    vm: &VM,
    trait_ty: BytecodeType,
    actual_object_ty: BytecodeType,
) -> *const Shape {
    if let Some(&shape) = vm
        .trait_shapes
        .read()
        .get(&(trait_ty.clone(), actual_object_ty.clone()))
    {
        return shape;
    }

    create_shape_for_trait_object(vm, trait_ty, actual_object_ty)
}

fn create_shape_for_trait_object(
    vm: &VM,
    trait_ty: BytecodeType,
    actual_object_ty: BytecodeType,
) -> *const Shape {
    let trait_id = trait_ty.trait_id().expect("trait expected");
    let trait_ = vm.trait_(trait_id);

    let mut csize;
    let mut fields;
    let mut ref_fields;

    fields = Vec::with_capacity(1);
    ref_fields = Vec::new();
    csize = Header::size();

    debug_assert!(actual_object_ty.is_concrete_type());

    let field_size = size(vm, actual_object_ty.clone());
    let field_align = align(vm, actual_object_ty.clone());

    let offset = mem::align_i32(csize, field_align);
    fields.push(FieldInstance {
        offset,
        ty: actual_object_ty.clone(),
    });
    add_ref_fields(vm, &mut ref_fields, offset, actual_object_ty.clone());
    csize = offset + field_size;
    csize = mem::align_i32(csize, mem::ptr_width());
    let size = InstanceSize::Fixed(csize);

    let mut shapes = vm.trait_shapes.write();

    if let Some(&shape) = shapes.get(&(trait_ty.clone(), actual_object_ty.clone())) {
        return shape;
    }

    let shape = create_shape(
        vm,
        ShapeKind::TraitObject {
            trait_ty: trait_ty.clone(),
            actual_object_ty: actual_object_ty.clone(),
        },
        size,
        fields,
        trait_.methods.len(),
    );

    let old = shapes.insert((trait_ty, actual_object_ty), shape);
    assert!(old.is_none());

    shape
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

        BytecodeType::TraitObject(trait_id, params, assoc_types) => {
            let params = specialize_bty_array(&params, type_params);
            let assoc_types = specialize_bty_array(&assoc_types, type_params);
            BytecodeType::TraitObject(trait_id, params, assoc_types)
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

        BytecodeType::TypeAlias(..)
        | BytecodeType::Assoc(..)
        | BytecodeType::GenericAssoc { .. }
        | BytecodeType::This => {
            unreachable!()
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

pub fn specialize_ty_array(
    vm: &VM,
    self_ty: Option<&BytecodeType>,
    types: &BytecodeTypeArray,
    type_params: &BytecodeTypeArray,
) -> BytecodeTypeArray {
    let types = types
        .iter()
        .map(|p| specialize_ty(vm, self_ty, p, type_params))
        .collect();
    BytecodeTypeArray::new(types)
}

pub fn specialize_ty(
    vm: &VM,
    self_ty: Option<&BytecodeType>,
    ty: BytecodeType,
    type_params: &BytecodeTypeArray,
) -> BytecodeType {
    match ty {
        BytecodeType::TypeParam(tpid) => type_params[tpid as usize].clone(),

        BytecodeType::Class(cls_id, params) => {
            let params = specialize_ty_array(vm, self_ty, &params, type_params);
            BytecodeType::Class(cls_id, params)
        }

        BytecodeType::TraitObject(trait_id, params, assoc_types) => {
            let params = specialize_ty_array(vm, self_ty, &params, type_params);
            let assoc_types = specialize_ty_array(vm, self_ty, &assoc_types, type_params);
            BytecodeType::TraitObject(trait_id, params, assoc_types)
        }

        BytecodeType::Struct(struct_id, params) => {
            let params = specialize_ty_array(vm, self_ty, &params, type_params);
            BytecodeType::Struct(struct_id, params)
        }

        BytecodeType::Enum(enum_id, params) => {
            let params = specialize_ty_array(vm, self_ty, &params, type_params);
            BytecodeType::Enum(enum_id, params)
        }

        BytecodeType::Lambda(params, return_type) => {
            let params = specialize_ty_array(vm, self_ty, &params, type_params);
            let return_type = specialize_ty(vm, self_ty, return_type.as_ref().clone(), type_params);
            BytecodeType::Lambda(params, Box::new(return_type))
        }

        BytecodeType::Tuple(subtypes) => {
            let subtypes = specialize_ty_array(vm, self_ty, &subtypes, type_params);
            BytecodeType::Tuple(subtypes)
        }

        BytecodeType::GenericAssoc {
            type_param_id,
            trait_ty,
            assoc_id,
        } => {
            let type_param_ty = type_params[type_param_id as usize].clone();
            assert!(type_param_ty.is_concrete_type());

            let type_param_data = TypeParamData {
                names: Vec::new(),
                bounds: Vec::new(),
            };

            let (impl_id, bindings) =
                find_impl(vm, type_param_ty, &type_param_data, trait_ty.clone())
                    .expect("no impl found for generic trait method call");

            let impl_ = vm.impl_(impl_id);

            let impl_alias_id = impl_
                .trait_alias_map
                .iter()
                .filter(|(trait_alias_id, _)| *trait_alias_id == assoc_id)
                .map(|(_, impl_alias_id)| impl_alias_id)
                .next()
                .cloned()
                .expect("missing");

            let impl_alias = vm.alias(impl_alias_id);
            let impl_alias_ty = impl_alias.ty.as_ref().expect("value expected").clone();

            specialize_ty(vm, self_ty, impl_alias_ty, &bindings)
        }

        BytecodeType::TypeAlias(..) | BytecodeType::Assoc(..) | BytecodeType::This => {
            unreachable!()
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

pub fn specialize_bty_for_trait_object(
    program: &Program,
    ty: BytecodeType,
    trait_id: TraitId,
    type_params: &BytecodeTypeArray,
    assoc_types: &BytecodeTypeArray,
) -> BytecodeType {
    match ty {
        BytecodeType::TypeParam(tpid) => type_params[tpid as usize].clone(),

        BytecodeType::Class(cls_id, params) => {
            let params = specialize_bty_for_trait_object_array(
                program,
                &params,
                trait_id,
                type_params,
                assoc_types,
            );
            BytecodeType::Class(cls_id, params)
        }

        BytecodeType::TraitObject(trait_id, trait_params, trait_assoc_types) => {
            let trait_params = specialize_bty_for_trait_object_array(
                program,
                &trait_params,
                trait_id,
                type_params,
                assoc_types,
            );
            let trait_assoc_types = specialize_bty_for_trait_object_array(
                program,
                &trait_assoc_types,
                trait_id,
                type_params,
                assoc_types,
            );
            BytecodeType::TraitObject(trait_id, trait_params, trait_assoc_types)
        }

        BytecodeType::Struct(struct_id, params) => {
            let params = specialize_bty_for_trait_object_array(
                program,
                &params,
                trait_id,
                type_params,
                assoc_types,
            );
            BytecodeType::Struct(struct_id, params)
        }

        BytecodeType::Enum(enum_id, params) => {
            let params = specialize_bty_for_trait_object_array(
                program,
                &params,
                trait_id,
                type_params,
                assoc_types,
            );
            BytecodeType::Enum(enum_id, params)
        }

        BytecodeType::Lambda(params, return_type) => {
            let params = specialize_bty_for_trait_object_array(
                program,
                &params,
                trait_id,
                type_params,
                assoc_types,
            );
            let return_type = specialize_bty_for_trait_object(
                program,
                *return_type,
                trait_id,
                type_params,
                assoc_types,
            );
            BytecodeType::Lambda(params, Box::new(return_type))
        }

        BytecodeType::Tuple(subtypes) => {
            let subtypes = specialize_bty_for_trait_object_array(
                program,
                &subtypes,
                trait_id,
                type_params,
                assoc_types,
            );
            BytecodeType::Tuple(subtypes)
        }

        BytecodeType::Assoc(alias_id, alias_type_params) => {
            let alias = program.alias(alias_id);
            assert!(alias_type_params.is_empty());
            assoc_types[alias.idx_in_trait()].clone()
        }

        BytecodeType::GenericAssoc { .. } => unreachable!(),

        BytecodeType::TypeAlias(..) | BytecodeType::This => {
            unreachable!()
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

pub fn specialize_bty_for_trait_object_array(
    program: &Program,
    types: &BytecodeTypeArray,
    trait_id: TraitId,
    type_params: &BytecodeTypeArray,
    assoc_types: &BytecodeTypeArray,
) -> BytecodeTypeArray {
    let types = types
        .iter()
        .map(|p| specialize_bty_for_trait_object(program, p, trait_id, type_params, assoc_types))
        .collect();
    BytecodeTypeArray::new(types)
}
