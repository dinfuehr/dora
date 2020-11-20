use parking_lot::RwLock;
use std::cmp::max;
use std::ptr;
use std::sync::Arc;

use crate::mem;
use crate::object::Header;
use crate::size::InstanceSize;
use crate::ty::{SourceType, SourceTypeArray};
use crate::vm::{
    ensure_tuple, Class, ClassDef, ClassDefId, ClassId, EnumData, EnumDef, EnumDefId, EnumId,
    EnumLayout, FieldDef, StructData, StructDef, StructDefId, StructFieldDef, StructId, TupleId,
    VM,
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

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum SpecializeFor {
    Fct,
    Class,
}

pub fn specialize_struct_id(vm: &VM, struct_id: StructId) -> StructDefId {
    let struc = vm.structs.idx(struct_id);
    let struc = struc.read();
    specialize_struct(vm, &*struc, SourceTypeArray::empty())
}

pub fn specialize_struct_id_params(
    vm: &VM,
    struct_id: StructId,
    type_params: SourceTypeArray,
) -> StructDefId {
    let struc = vm.structs.idx(struct_id);
    let struc = struc.read();
    specialize_struct(vm, &*struc, type_params)
}

pub fn specialize_struct(vm: &VM, struc: &StructData, type_params: SourceTypeArray) -> StructDefId {
    if let Some(&id) = struc.specializations.read().get(&type_params) {
        return id;
    }

    create_specialized_struct(vm, struc, type_params)
}

fn create_specialized_struct(
    vm: &VM,
    struc: &StructData,
    type_params: SourceTypeArray,
) -> StructDefId {
    let mut size = 0;
    let mut align = 0;
    let mut fields = Vec::with_capacity(struc.fields.len());
    let mut ref_fields = Vec::new();

    for f in &struc.fields {
        let ty = specialize_type(vm, f.ty.clone(), &type_params);
        debug_assert!(!ty.contains_type_param(vm));

        let field_size = ty.size(vm);
        let field_align = ty.align(vm);

        let offset = mem::align_i32(size, field_align);
        fields.push(StructFieldDef {
            offset,
            ty: ty.clone(),
        });

        size = offset + field_size;
        align = max(align, field_align);

        if ty.reference_type() {
            ref_fields.push(offset);
        }
    }

    size = mem::align_i32(size, align);

    let mut struct_defs = vm.struct_defs.lock();
    let id: StructDefId = struct_defs.len().into();

    let mut specializations = struc.specializations.write();

    if let Some(&id) = specializations.get(&type_params) {
        return id;
    }

    let old = specializations.insert(type_params.clone(), id);
    assert!(old.is_none());

    struct_defs.push(Arc::new(StructDef {
        size,
        align,
        fields,
        ref_fields,
    }));

    id
}

pub fn specialize_enum_id_params(
    vm: &VM,
    enum_id: EnumId,
    type_params: SourceTypeArray,
) -> EnumDefId {
    let xenum = &vm.enums[enum_id];
    let xenum = xenum.read();
    specialize_enum(vm, &*xenum, type_params)
}

pub fn specialize_enum(vm: &VM, xenum: &EnumData, type_params: SourceTypeArray) -> EnumDefId {
    if let Some(&id) = xenum.specializations.read().get(&type_params) {
        return id;
    }

    create_specialized_enum(vm, xenum, type_params)
}

fn create_specialized_enum(vm: &VM, xenum: &EnumData, type_params: SourceTypeArray) -> EnumDefId {
    let layout = if enum_is_simple_integer(xenum) {
        EnumLayout::Int
    } else if enum_is_ptr(vm, xenum, &type_params) {
        EnumLayout::Ptr
    } else {
        EnumLayout::Tagged
    };

    let mut enum_defs = vm.enum_defs.lock();
    let id: EnumDefId = enum_defs.len().into();

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

    let enum_def = Arc::new(EnumDef {
        id,
        enum_id: xenum.id,
        type_params: type_params.clone(),
        layout,
        variants: RwLock::new(variants),
    });

    enum_defs.push(enum_def);

    id
}

fn enum_is_simple_integer(xenum: &EnumData) -> bool {
    for variant in &xenum.variants {
        if !variant.types.is_empty() {
            return false;
        }
    }

    true
}

fn enum_is_ptr(vm: &VM, xenum: &EnumData, type_params: &SourceTypeArray) -> bool {
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

pub fn specialize_class_id(vm: &VM, cls_id: ClassId) -> ClassDefId {
    let cls = vm.classes.idx(cls_id);
    let cls = cls.read();
    specialize_class(vm, &*cls, &SourceTypeArray::empty())
}

pub fn specialize_class_id_params(
    vm: &VM,
    cls_id: ClassId,
    type_params: &SourceTypeArray,
) -> ClassDefId {
    let cls = vm.classes.idx(cls_id);
    let cls = cls.read();
    specialize_class(vm, &*cls, &type_params)
}

pub fn specialize_class_ty(vm: &VM, ty: SourceType) -> ClassDefId {
    match ty {
        SourceType::Class(cls_id, list_id) => {
            let params = vm.lists.lock().get(list_id);
            specialize_class_id_params(vm, cls_id, &params)
        }

        _ => unreachable!(),
    }
}

pub fn specialize_class(vm: &VM, cls: &Class, type_params: &SourceTypeArray) -> ClassDefId {
    if let Some(&id) = cls.specializations.read().get(&type_params) {
        return id;
    }

    create_specialized_class(vm, cls, type_params)
}

fn create_specialized_class(vm: &VM, cls: &Class, type_params: &SourceTypeArray) -> ClassDefId {
    debug_assert!(type_params.iter().all(|ty| ty.is_concrete_type(vm)));

    if cls.is_array || cls.is_str {
        create_specialized_class_array(vm, cls, type_params)
    } else {
        create_specialized_class_regular(vm, cls, type_params)
    }
}

fn create_specialized_class_regular(
    vm: &VM,
    cls: &Class,
    type_params: &SourceTypeArray,
) -> ClassDefId {
    if let Some(parent_class) = cls.parent_class.clone() {
        let parent_class = specialize_type(vm, parent_class, type_params);
        specialize_class_ty(vm, parent_class);
    }

    for f in &cls.fields {
        let ty = specialize_type(vm, f.ty.clone(), &type_params);
        debug_assert!(!ty.contains_type_param(vm));

        if let Some(tuple_id) = ty.tuple_id() {
            let tuples = vm.tuples.lock();
            tuples.get_tuple(tuple_id);
        } else if let SourceType::Enum(enum_id, type_params_id) = ty.clone() {
            let type_params = vm.lists.lock().get(type_params_id);
            specialize_enum_id_params(vm, enum_id, type_params);
        }
    }

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

        if let Some(tuple_id) = ty.tuple_id() {
            let tuples = vm.tuples.lock();
            let tuple = tuples.get_tuple(tuple_id);

            for &ref_offset in tuple.references() {
                ref_fields.push(offset + ref_offset);
            }
        } else if let SourceType::Enum(enum_id, type_params_id) = ty.clone() {
            let type_params = vm.lists.lock().get(type_params_id);
            let edef_id = specialize_enum_id_params(vm, enum_id, type_params);
            let edef = vm.enum_defs.idx(edef_id);

            match edef.layout {
                EnumLayout::Int => {}
                EnumLayout::Ptr | EnumLayout::Tagged => {
                    ref_fields.push(offset);
                }
            }
        } else if ty.reference_type() {
            ref_fields.push(offset);
        }
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
    let id: ClassDefId = class_defs.len().into();

    let mut specializations = cls.specializations.write();

    if let Some(&id) = specializations.get(type_params) {
        return id;
    }

    let old = specializations.insert(type_params.clone(), id);
    assert!(old.is_none());

    let class_def = Arc::new(ClassDef {
        id,
        cls_id: Some(cls.id),
        type_params: type_params.clone(),
        parent_id,
        size,
        fields,
        ref_fields,
        vtable: RwLock::new(None),
    });

    vtable.initialize_classptr(Arc::as_ptr(&class_def));
    *class_def.vtable.write() = Some(vtable);

    class_defs.push(class_def.clone());

    id
}

fn create_specialized_class_array(
    vm: &VM,
    cls: &Class,
    type_params: &SourceTypeArray,
) -> ClassDefId {
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
            SourceType::Ptr | SourceType::Class(_, _) | SourceType::TraitObject(_) => {
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
                let type_params = vm.lists.lock().get(type_params_id);
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
                let type_params = vm.lists.lock().get(type_params_id);
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
    let id: ClassDefId = class_defs.len().into();

    let mut specializations = cls.specializations.write();

    if let Some(&id) = specializations.get(type_params) {
        return id;
    }

    let old = specializations.insert(type_params.clone(), id);
    assert!(old.is_none());

    let class_def = Arc::new(ClassDef {
        id,
        cls_id: Some(cls.id),
        type_params: type_params.clone(),
        parent_id: Some(parent_cls_def_id),
        size,
        fields,
        ref_fields,
        vtable: RwLock::new(None),
    });

    vtable.initialize_classptr(Arc::as_ptr(&class_def));
    *class_def.vtable.write() = Some(vtable);

    class_defs.push(class_def.clone());

    id
}

fn ensure_display(vm: &VM, vtable: &mut VTableBox, parent_id: Option<ClassDefId>) -> usize {
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
            let params = vm.lists.lock().get(list_id);

            let params = SourceTypeArray::with(
                params
                    .iter()
                    .map(|p| replace_type_param(vm, p, type_params, self_ty.clone()))
                    .collect::<Vec<_>>(),
            );

            let list_id = vm.lists.lock().insert(params);
            SourceType::Class(cls_id, list_id)
        }

        SourceType::Enum(enum_id, list_id) => {
            let old_type_params = vm.lists.lock().get(list_id);

            let new_type_params = SourceTypeArray::with(
                old_type_params
                    .iter()
                    .map(|p| replace_type_param(vm, p, type_params, self_ty.clone()))
                    .collect::<Vec<_>>(),
            );

            let new_type_params_id = vm.lists.lock().insert(new_type_params);
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

        _ => ty,
    }
}
