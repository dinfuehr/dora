use parking_lot::RwLock;
use std::cmp::max;
use std::ptr;
use std::sync::Arc;

use crate::language::ty::{SourceType, SourceTypeArray, SourceTypeArrayId};
use crate::mem;
use crate::object::Header;
use crate::size::InstanceSize;
use crate::vm::{
    ensure_tuple, ClassDefinition, ClassDefinitionId, ClassInstance, ClassInstanceId,
    EnumDefinition, EnumDefinitionId, EnumInstance, EnumInstanceId, EnumLayout, FieldDef,
    StructDefinition, StructDefinitionId, StructInstance, StructInstanceField, StructInstanceId,
    TraitDefinition, TraitDefinitionId, TupleId, VM,
};
use crate::vtable::{VTableBox, DISPLAY_SIZE};

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

pub fn specialize_enum_class(
    vm: &VM,
    edef: &EnumInstance,
    xenum: &EnumDefinition,
    variant_id: usize,
) -> ClassInstanceId {
    let mut variants = edef.variants.write();
    let variant = variants[variant_id];

    if let Some(cls_def_id) = variant {
        return cls_def_id;
    }

    let enum_variant = &xenum.variants[variant_id];
    let mut csize = Header::size() + 4;
    let mut fields = vec![FieldDef {
        offset: Header::size(),
        ty: SourceType::Int32,
    }];
    let mut ref_fields = Vec::new();

    for ty in &enum_variant.types {
        let ty = replace_type_param(vm, ty.clone(), &edef.type_params, None);
        assert!(ty.is_concrete_type(vm));

        if ty.is_unit() {
            continue;
        }

        let field_size = ty.size(vm);
        let field_align = ty.align(vm);

        let offset = mem::align_i32(csize, field_align);
        fields.push(FieldDef {
            offset,
            ty: ty.clone(),
        });

        csize = offset + field_size;

        add_ref_fields(vm, &mut ref_fields, offset, ty);
    }

    let instance_size = mem::align_i32(csize, mem::ptr_width());

    let mut class_defs = vm.class_defs.lock();
    let id: ClassInstanceId = class_defs.len().into();

    variants[variant_id] = Some(id);

    let class_def = Arc::new(ClassInstance {
        id,
        cls_id: None,
        trait_object: None,
        type_params: SourceTypeArray::empty(),
        parent_id: None,
        size: InstanceSize::Fixed(instance_size),
        fields,
        ref_fields,
        vtable: RwLock::new(None),
    });

    class_defs.push(class_def.clone());

    let clsptr = &*class_def as *const ClassInstance as *mut ClassInstance;
    let vtable = VTableBox::new(clsptr, instance_size as usize, 0, &[]);
    *class_def.vtable.write() = Some(vtable);

    id
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

pub fn specialize_class_id(vm: &VM, cls_id: ClassDefinitionId) -> ClassInstanceId {
    let cls = vm.classes.idx(cls_id);
    let cls = cls.read();
    specialize_class(vm, &*cls, &SourceTypeArray::empty())
}

pub fn specialize_class_id_params(
    vm: &VM,
    cls_id: ClassDefinitionId,
    type_params: &SourceTypeArray,
) -> ClassInstanceId {
    let cls = vm.classes.idx(cls_id);
    let cls = cls.read();
    specialize_class(vm, &*cls, &type_params)
}

pub fn specialize_class_ty(vm: &VM, ty: SourceType) -> ClassInstanceId {
    match ty {
        SourceType::Class(cls_id, list_id) => {
            let params = vm.source_type_arrays.lock().get(list_id);
            specialize_class_id_params(vm, cls_id, &params)
        }

        _ => unreachable!(),
    }
}

pub fn specialize_class(
    vm: &VM,
    cls: &ClassDefinition,
    type_params: &SourceTypeArray,
) -> ClassInstanceId {
    if let Some(&id) = cls.specializations.read().get(&type_params) {
        return id;
    }

    create_specialized_class(vm, cls, type_params)
}

fn create_specialized_class(
    vm: &VM,
    cls: &ClassDefinition,
    type_params: &SourceTypeArray,
) -> ClassInstanceId {
    debug_assert!(type_params.iter().all(|ty| ty.is_concrete_type(vm)));

    if cls.is_array || cls.is_str {
        create_specialized_class_array(vm, cls, type_params)
    } else {
        create_specialized_class_regular(vm, cls, type_params)
    }
}

fn create_specialized_class_regular(
    vm: &VM,
    cls: &ClassDefinition,
    type_params: &SourceTypeArray,
) -> ClassInstanceId {
    let mut csize;
    let mut fields;
    let mut ref_fields;
    let parent_id;

    if let Some(parent_class) = cls.parent_class.clone() {
        let parent_class = specialize_type(vm, parent_class, type_params);
        let id = specialize_class_ty(vm, parent_class);
        let cls_def = vm.class_defs.idx(id);

        fields = Vec::new();
        ref_fields = cls_def.ref_fields.clone();
        csize = match cls_def.size {
            InstanceSize::Fixed(size) => size,
            _ => unreachable!(),
        };
        parent_id = Some(id);
    } else {
        fields = Vec::with_capacity(cls.fields.len());
        ref_fields = Vec::new();
        csize = Header::size();
        parent_id = None;
    };

    for f in &cls.fields {
        let ty = specialize_type(vm, f.ty.clone(), &type_params);
        debug_assert!(!ty.contains_type_param(vm));

        let field_size = ty.size(vm);
        let field_align = ty.align(vm);

        let offset = mem::align_i32(csize, field_align);
        fields.push(FieldDef {
            offset,
            ty: ty.clone(),
        });

        csize = offset + field_size;

        add_ref_fields(vm, &mut ref_fields, offset, ty);
    }

    let size = InstanceSize::Fixed(mem::align_i32(csize, mem::ptr_width()));

    let stub = vm.compile_stub().to_usize();
    let vtable_entries = vec![stub; cls.virtual_fcts.len()];

    let (instance_size, element_size) = match size {
        InstanceSize::Fixed(instance_size) => (instance_size as usize, 0),
        _ => unreachable!(),
    };

    let mut vtable = VTableBox::new(
        std::ptr::null(),
        instance_size,
        element_size,
        &vtable_entries,
    );
    ensure_display(vm, &mut vtable, parent_id);

    let mut class_defs = vm.class_defs.lock();
    let id: ClassInstanceId = class_defs.len().into();

    let mut specializations = cls.specializations.write();

    if let Some(&id) = specializations.get(type_params) {
        return id;
    }

    let old = specializations.insert(type_params.clone(), id);
    assert!(old.is_none());

    let class_def = Arc::new(ClassInstance {
        id,
        cls_id: Some(cls.id),
        trait_object: None,
        type_params: type_params.clone(),
        parent_id,
        size,
        fields,
        ref_fields,
        vtable: RwLock::new(None),
    });

    vtable.initialize_class_def(Arc::as_ptr(&class_def));
    *class_def.vtable.write() = Some(vtable);

    class_defs.push(class_def.clone());

    id
}

fn create_specialized_class_array(
    vm: &VM,
    cls: &ClassDefinition,
    type_params: &SourceTypeArray,
) -> ClassInstanceId {
    let parent_class = cls
        .parent_class
        .clone()
        .expect("Array & String should have super class");
    let parent_cls_def_id = specialize_class_ty(vm, parent_class);

    let fields = Vec::new();
    let mut ref_fields = Vec::new();

    assert!(cls.fields.is_empty());
    assert!(cls.is_array || cls.is_str);

    let size = if cls.is_array {
        let element_ty = type_params[0].clone();

        match element_ty {
            SourceType::Unit => InstanceSize::UnitArray,
            SourceType::Ptr | SourceType::Class(_, _) | SourceType::Trait(_, _) => {
                InstanceSize::ObjArray
            }
            SourceType::Tuple(tuple_id) => {
                let tuples = vm.tuples.lock();
                let tuple = tuples.get_tuple(tuple_id);

                if tuple.contains_references() {
                    for &offset in tuple.references() {
                        ref_fields.push(offset);
                    }

                    InstanceSize::TupleArray(tuple.size())
                } else {
                    InstanceSize::PrimitiveArray(tuple.size())
                }
            }

            SourceType::Struct(struct_id, type_params_id) => {
                let type_params = vm.source_type_arrays.lock().get(type_params_id);
                let sdef_id = specialize_struct_id_params(vm, struct_id, type_params);
                let sdef = vm.struct_defs.idx(sdef_id);

                if sdef.contains_references() {
                    for &offset in &sdef.ref_fields {
                        ref_fields.push(offset);
                    }

                    InstanceSize::StructArray(sdef.size)
                } else {
                    InstanceSize::PrimitiveArray(sdef.size)
                }
            }

            SourceType::Enum(enum_id, type_params_id) => {
                let type_params = vm.source_type_arrays.lock().get(type_params_id);
                let edef_id = specialize_enum_id_params(vm, enum_id, type_params);
                let edef = vm.enum_defs.idx(edef_id);

                match edef.layout {
                    EnumLayout::Int => InstanceSize::PrimitiveArray(4),
                    EnumLayout::Ptr | EnumLayout::Tagged => InstanceSize::ObjArray,
                }
            }

            _ => InstanceSize::PrimitiveArray(element_ty.size(vm)),
        }
    } else {
        InstanceSize::Str
    };

    let stub = vm.compile_stub().to_usize();
    let vtable_entries = vec![stub; cls.virtual_fcts.len()];

    let (instance_size, element_size) = match size {
        InstanceSize::Fixed(_) => unreachable!(),
        InstanceSize::PrimitiveArray(element_size) => (0, element_size as usize),
        InstanceSize::ObjArray => (0, mem::ptr_width_usize()),
        InstanceSize::UnitArray => (Header::size() as usize + mem::ptr_width_usize(), 0),
        InstanceSize::FreeArray => (0, mem::ptr_width_usize()),
        InstanceSize::Str => (0, 1),
        InstanceSize::TupleArray(element_size) => (0, element_size as usize),
        InstanceSize::StructArray(element_size) => (0, element_size as usize),
    };

    let mut vtable = VTableBox::new(
        std::ptr::null(),
        instance_size,
        element_size,
        &vtable_entries,
    );
    ensure_display(vm, &mut vtable, Some(parent_cls_def_id));

    let mut class_defs = vm.class_defs.lock();
    let id: ClassInstanceId = class_defs.len().into();

    let mut specializations = cls.specializations.write();

    if let Some(&id) = specializations.get(type_params) {
        return id;
    }

    let old = specializations.insert(type_params.clone(), id);
    assert!(old.is_none());

    let class_def = Arc::new(ClassInstance {
        id,
        cls_id: Some(cls.id),
        trait_object: None,
        type_params: type_params.clone(),
        parent_id: Some(parent_cls_def_id),
        size,
        fields,
        ref_fields,
        vtable: RwLock::new(None),
    });

    vtable.initialize_class_def(Arc::as_ptr(&class_def));
    *class_def.vtable.write() = Some(vtable);

    class_defs.push(class_def.clone());

    id
}

pub fn ensure_display(
    vm: &VM,
    vtable: &mut VTableBox,
    parent_id: Option<ClassInstanceId>,
) -> usize {
    // if subtype_display[0] is set, vtable was already initialized
    assert!(vtable.subtype_display[0].is_null());

    if let Some(parent_id) = parent_id {
        let parent = vm.class_defs.idx(parent_id);

        let parent_vtable = parent.vtable.read();
        let parent_vtable = parent_vtable.as_ref().unwrap();
        assert!(!parent_vtable.subtype_display[0].is_null());

        let depth = 1 + parent_vtable.subtype_depth;

        let depth_fixed;

        if depth >= DISPLAY_SIZE {
            depth_fixed = DISPLAY_SIZE;

            vtable.allocate_overflow(depth as usize - DISPLAY_SIZE + 1);

            unsafe {
                if depth > DISPLAY_SIZE {
                    ptr::copy_nonoverlapping(
                        parent_vtable.subtype_overflow,
                        vtable.subtype_overflow as *mut _,
                        depth as usize - DISPLAY_SIZE,
                    );
                }

                let ptr = vtable
                    .subtype_overflow
                    .offset(depth as isize - DISPLAY_SIZE as isize)
                    as *mut _;

                *ptr = &**vtable as *const _;
            }
        } else {
            depth_fixed = depth;

            vtable.subtype_display[depth] = &**vtable as *const _;
        }

        vtable.subtype_depth = depth;
        vtable.subtype_display[0..depth_fixed]
            .clone_from_slice(&parent_vtable.subtype_display[0..depth_fixed]);

        depth
    } else {
        vtable.subtype_depth = 0;
        vtable.subtype_display[0] = &**vtable as *const _;

        0
    }
}

pub fn specialize_trait_object(
    vm: &VM,
    trait_id: TraitDefinitionId,
    trait_type_params: &SourceTypeArray,
    object_type: SourceType,
) -> ClassInstanceId {
    let xtrait = vm.traits[trait_id].read();

    let combined_type_params = trait_type_params.connect_single(object_type.clone());
    let combined_type_params_id = vm.source_type_arrays.lock().insert(combined_type_params);

    if let Some(&id) = xtrait.vtables.read().get(&combined_type_params_id) {
        return id;
    }

    create_specialized_class_for_trait_object(vm, &*xtrait, combined_type_params_id, object_type)
}

fn create_specialized_class_for_trait_object(
    vm: &VM,
    xtrait: &TraitDefinition,
    combined_type_params_id: SourceTypeArrayId,
    object_type: SourceType,
) -> ClassInstanceId {
    let mut csize;
    let mut fields;
    let mut ref_fields;
    let parent_id;

    fields = Vec::with_capacity(1);
    ref_fields = Vec::new();
    csize = Header::size();
    parent_id = None;

    debug_assert!(!object_type.contains_type_param(vm));

    let field_size = object_type.size(vm);
    let field_align = object_type.align(vm);

    let offset = mem::align_i32(csize, field_align);
    fields.push(FieldDef {
        offset,
        ty: object_type.clone(),
    });
    add_ref_fields(vm, &mut ref_fields, offset, object_type.clone());
    csize = offset + field_size;
    csize = mem::align_i32(csize, mem::ptr_width());
    let size = InstanceSize::Fixed(csize);

    let stub = vm.compile_stub().to_usize();
    let vtable_entries = vec![stub; xtrait.methods.len()];

    let mut vtable = VTableBox::new(std::ptr::null(), csize as usize, 0, &vtable_entries);
    ensure_display(vm, &mut vtable, parent_id);

    let mut class_defs = vm.class_defs.lock();
    let id: ClassInstanceId = class_defs.len().into();

    let mut vtables = xtrait.vtables.write();

    if let Some(&id) = vtables.get(&combined_type_params_id) {
        return id;
    }

    let old = vtables.insert(combined_type_params_id, id);
    assert!(old.is_none());

    let class_def = Arc::new(ClassInstance {
        id,
        cls_id: None,
        trait_object: Some(object_type),
        type_params: SourceTypeArray::empty(),
        parent_id: None,
        size,
        fields,
        ref_fields,
        vtable: RwLock::new(None),
    });

    vtable.initialize_class_def(Arc::as_ptr(&class_def));
    *class_def.vtable.write() = Some(vtable);

    class_defs.push(class_def.clone());

    id
}

pub fn specialize_tuple(vm: &VM, tuple_id: TupleId, type_params: &SourceTypeArray) -> TupleId {
    let subtypes = {
        let tuples = vm.tuples.lock();
        let tuple = tuples.get_tuple(tuple_id);

        if tuple.is_concrete_type() {
            return tuple_id;
        }

        tuple.args()
    };

    let new_subtypes = subtypes
        .iter()
        .map(|t| specialize_type(vm, t.clone(), type_params))
        .collect::<Vec<_>>();

    ensure_tuple(vm, new_subtypes)
}

pub fn replace_type_param(
    vm: &VM,
    ty: SourceType,
    type_params: &SourceTypeArray,
    self_ty: Option<SourceType>,
) -> SourceType {
    match ty {
        SourceType::TypeParam(tpid) => type_params[tpid.to_usize()].clone(),

        SourceType::Class(cls_id, list_id) => {
            let params = vm.source_type_arrays.lock().get(list_id);

            let params = SourceTypeArray::with(
                params
                    .iter()
                    .map(|p| replace_type_param(vm, p, type_params, self_ty.clone()))
                    .collect::<Vec<_>>(),
            );

            let list_id = vm.source_type_arrays.lock().insert(params);
            SourceType::Class(cls_id, list_id)
        }

        SourceType::Trait(trait_id, list_id) => {
            let old_type_params = vm.source_type_arrays.lock().get(list_id);

            let new_type_params = SourceTypeArray::with(
                old_type_params
                    .iter()
                    .map(|p| replace_type_param(vm, p, type_params, self_ty.clone()))
                    .collect::<Vec<_>>(),
            );

            let new_type_params_id = vm.source_type_arrays.lock().insert(new_type_params);
            SourceType::Trait(trait_id, new_type_params_id)
        }

        SourceType::Struct(struct_id, list_id) => {
            let old_type_params = vm.source_type_arrays.lock().get(list_id);

            let new_type_params = SourceTypeArray::with(
                old_type_params
                    .iter()
                    .map(|p| replace_type_param(vm, p, type_params, self_ty.clone()))
                    .collect::<Vec<_>>(),
            );

            let new_type_params_id = vm.source_type_arrays.lock().insert(new_type_params);
            SourceType::Struct(struct_id, new_type_params_id)
        }

        SourceType::Enum(enum_id, list_id) => {
            let old_type_params = vm.source_type_arrays.lock().get(list_id);

            let new_type_params = SourceTypeArray::with(
                old_type_params
                    .iter()
                    .map(|p| replace_type_param(vm, p, type_params, self_ty.clone()))
                    .collect::<Vec<_>>(),
            );

            let new_type_params_id = vm.source_type_arrays.lock().insert(new_type_params);
            SourceType::Enum(enum_id, new_type_params_id)
        }

        SourceType::This => self_ty.expect("no type for Self given"),

        SourceType::Lambda(_) => unimplemented!(),

        SourceType::Tuple(tuple_id) => {
            let subtypes = {
                let tuples = vm.tuples.lock();
                let tuple = tuples.get_tuple(tuple_id);

                if tuple.is_concrete_type() {
                    return ty;
                }

                tuple.args()
            };

            let new_subtypes = subtypes
                .iter()
                .map(|t| replace_type_param(vm, t.clone(), type_params, self_ty.clone()))
                .collect::<Vec<_>>();

            let tuple_id = ensure_tuple(vm, new_subtypes);
            SourceType::Tuple(tuple_id)
        }

        SourceType::Unit
        | SourceType::UInt8
        | SourceType::Bool
        | SourceType::Char
        | SourceType::Int32
        | SourceType::Int64
        | SourceType::Float32
        | SourceType::Float64
        | SourceType::Module(_)
        | SourceType::Error => ty,

        SourceType::Any | SourceType::Ptr => {
            panic!("unexpected type = {:?}", ty);
            // unreachable!()
        }
    }
}
