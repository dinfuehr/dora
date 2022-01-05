use parking_lot::RwLock;
use std::cmp::max;
use std::sync::Arc;

use crate::mem;
use crate::semck::specialize::specialize_type;
use crate::ty::{SourceType, SourceTypeArray};
use crate::vm::{
    EnumDefinition, EnumDefinitionId, EnumInstance, EnumInstanceId, EnumLayout, StructDefinition,
    StructDefinitionId, StructInstance, StructInstanceField, StructInstanceId, VM,
};

pub fn specialize_struct_id_params(
    vm: &VM,
    struct_id: StructDefinitionId,
    type_params: SourceTypeArray,
) -> StructInstanceId {
    let struc = vm.structs.idx(struct_id);
    let struc = struc.read();
    specialize_struct(vm, &*struc, type_params)
}

pub fn specialize_struct(
    vm: &VM,
    struc: &StructDefinition,
    type_params: SourceTypeArray,
) -> StructInstanceId {
    if let Some(&id) = struc.specializations.read().get(&type_params) {
        return id;
    }

    create_specialized_struct(vm, struc, type_params)
}

fn create_specialized_struct(
    vm: &VM,
    xstruct: &StructDefinition,
    type_params: SourceTypeArray,
) -> StructInstanceId {
    assert!(xstruct.primitive_ty.is_none());

    let mut size = 0;
    let mut align = 0;
    let mut fields = Vec::with_capacity(xstruct.fields.len());
    let mut ref_fields = Vec::new();

    for f in &xstruct.fields {
        let ty = specialize_type(vm, f.ty.clone(), &type_params);
        debug_assert!(!ty.contains_type_param(vm));

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

    let mut struct_defs = vm.struct_defs.lock();
    let id: StructInstanceId = struct_defs.len().into();

    let mut specializations = xstruct.specializations.write();

    if let Some(&id) = specializations.get(&type_params) {
        return id;
    }

    let old = specializations.insert(type_params.clone(), id);
    assert!(old.is_none());

    struct_defs.push(Arc::new(StructInstance {
        size,
        align,
        fields,
        ref_fields,
    }));

    id
}

pub fn specialize_enum_id_params(
    vm: &VM,
    enum_id: EnumDefinitionId,
    type_params: SourceTypeArray,
) -> EnumInstanceId {
    let xenum = &vm.enums[enum_id];
    let xenum = xenum.read();
    specialize_enum(vm, &*xenum, type_params)
}

pub fn specialize_enum(
    vm: &VM,
    xenum: &EnumDefinition,
    type_params: SourceTypeArray,
) -> EnumInstanceId {
    if let Some(&id) = xenum.specializations.read().get(&type_params) {
        return id;
    }

    create_specialized_enum(vm, xenum, type_params)
}

fn create_specialized_enum(
    vm: &VM,
    xenum: &EnumDefinition,
    type_params: SourceTypeArray,
) -> EnumInstanceId {
    let layout = if enum_is_simple_integer(xenum) {
        EnumLayout::Int
    } else if enum_is_ptr(vm, xenum, &type_params) {
        EnumLayout::Ptr
    } else {
        EnumLayout::Tagged
    };

    let mut enum_defs = vm.enum_defs.lock();
    let id: EnumInstanceId = enum_defs.len().into();

    let mut specializations = xenum.specializations.write();

    if let Some(&id) = specializations.get(&type_params) {
        return id;
    }

    let old = specializations.insert(type_params.clone(), id);
    assert!(old.is_none());

    let variants = if let EnumLayout::Tagged = layout {
        vec![None; xenum.variants.len()]
    } else {
        Vec::new()
    };

    let enum_def = Arc::new(EnumInstance {
        id,
        enum_id: xenum.id,
        type_params: type_params.clone(),
        layout,
        variants: RwLock::new(variants),
    });

    enum_defs.push(enum_def);

    id
}

fn enum_is_simple_integer(xenum: &EnumDefinition) -> bool {
    for variant in &xenum.variants {
        if !variant.types.is_empty() {
            return false;
        }
    }

    true
}

fn enum_is_ptr(vm: &VM, xenum: &EnumDefinition, type_params: &SourceTypeArray) -> bool {
    if xenum.variants.len() != 2 {
        return false;
    }

    let variant1 = xenum.variants.first().unwrap();
    let variant2 = xenum.variants.last().unwrap();

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

pub fn add_ref_fields(vm: &VM, ref_fields: &mut Vec<i32>, offset: i32, ty: SourceType) {
    if let Some(tuple_id) = ty.tuple_id() {
        let tuples = vm.tuples.lock();
        let tuple = tuples.get_tuple(tuple_id);

        for &ref_offset in tuple.references() {
            ref_fields.push(offset + ref_offset);
        }
    } else if let SourceType::Enum(enum_id, type_params_id) = ty.clone() {
        let type_params = vm.source_type_arrays.lock().get(type_params_id);
        let edef_id = specialize_enum_id_params(vm, enum_id, type_params);
        let edef = vm.enum_defs.idx(edef_id);

        match edef.layout {
            EnumLayout::Int => {}
            EnumLayout::Ptr | EnumLayout::Tagged => {
                ref_fields.push(offset);
            }
        }
    } else if let SourceType::Struct(struct_id, type_params_id) = ty.clone() {
        let type_params = vm.source_type_arrays.lock().get(type_params_id);
        let sdef_id = specialize_struct_id_params(vm, struct_id, type_params);
        let sdef = vm.struct_defs.idx(sdef_id);

        for &ref_offset in &sdef.ref_fields {
            ref_fields.push(offset + ref_offset);
        }
    } else if ty.reference_type() {
        ref_fields.push(offset);
    }
}
