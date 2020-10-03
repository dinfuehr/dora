use parking_lot::{Mutex, RwLock};
use std::cmp::max;
use std::ptr;
use std::sync::Arc;

use crate::mem;
use crate::object::Header;
use crate::size::InstanceSize;
use crate::ty::{BuiltinType, TypeList};
use crate::vm::{
    ensure_tuple, Class, ClassDef, ClassDefId, ClassId, EnumData, EnumDef, EnumDefId, EnumId,
    EnumLayout, FieldDef, StructData, StructDef, StructDefId, StructFieldDef, StructId, TupleId,
    VM,
};
use crate::vtable::{VTableBox, DISPLAY_SIZE};

pub fn specialize_type(vm: &VM, ty: BuiltinType, type_params: &TypeList) -> BuiltinType {
    replace_type_param(vm, ty, type_params, None)
}

pub fn specialize_type_list(vm: &VM, list: &TypeList, type_params: &TypeList) -> TypeList {
    let types = list.types();

    if types.is_empty() {
        return TypeList::empty();
    }

    let mut specialized_types = Vec::with_capacity(types.len());

    for &ty in types {
        let ty = replace_type_param(vm, ty, type_params, None);
        specialized_types.push(ty);
    }

    TypeList::with(specialized_types)
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum SpecializeFor {
    Fct,
    Class,
}

pub fn specialize_struct_id(vm: &VM, struct_id: StructId) -> StructDefId {
    let struc = vm.structs.idx(struct_id);
    let struc = struc.lock();
    specialize_struct(vm, &*struc, TypeList::empty())
}

pub fn specialize_struct_id_params(
    vm: &VM,
    struct_id: StructId,
    type_params: TypeList,
) -> StructDefId {
    let struc = vm.structs.idx(struct_id);
    let struc = struc.lock();
    specialize_struct(vm, &*struc, type_params)
}

pub fn specialize_struct(vm: &VM, struc: &StructData, type_params: TypeList) -> StructDefId {
    if let Some(&id) = struc.specializations.read().get(&type_params) {
        return id;
    }

    create_specialized_struct(vm, struc, type_params)
}

fn create_specialized_struct(vm: &VM, struc: &StructData, type_params: TypeList) -> StructDefId {
    let id = {
        let mut struct_defs = vm.struct_defs.lock();
        let id: StructDefId = struct_defs.len().into();

        let old = struc
            .specializations
            .write()
            .insert(type_params.clone(), id);
        assert!(old.is_none());

        struct_defs.push(Arc::new(Mutex::new(StructDef {
            size: 0,
            align: 0,
            fields: Vec::new(),
            ref_fields: Vec::new(),
        })));

        id
    };

    let mut size = 0;
    let mut align = 0;
    let mut fields = Vec::with_capacity(struc.fields.len());
    let mut ref_fields = Vec::new();

    for f in &struc.fields {
        let ty = specialize_type(vm, f.ty, &type_params);
        debug_assert!(!ty.contains_type_param(vm));

        let field_size = ty.size(vm);
        let field_align = ty.align(vm);

        let offset = mem::align_i32(size, field_align);
        fields.push(StructFieldDef { offset, ty });

        size = offset + field_size;
        align = max(align, field_align);

        if ty.reference_type() {
            ref_fields.push(offset);
        }
    }

    let struct_def = vm.struct_defs.idx(id);
    let mut struct_def = struct_def.lock();
    struct_def.size = size;
    struct_def.align = align;
    struct_def.fields = fields;
    struct_def.ref_fields = ref_fields;

    id
}

pub fn specialize_enum_id_params(vm: &VM, enum_id: EnumId, type_params: TypeList) -> EnumDefId {
    let xenum = &vm.enums[enum_id];
    let xenum = xenum.read();
    specialize_enum(vm, &*xenum, type_params)
}

pub fn specialize_enum(vm: &VM, xenum: &EnumData, type_params: TypeList) -> EnumDefId {
    if let Some(&id) = xenum.specializations.read().get(&type_params) {
        return id;
    }

    create_specialized_enum(vm, xenum, type_params)
}

fn create_specialized_enum(vm: &VM, xenum: &EnumData, type_params: TypeList) -> EnumDefId {
    let id = {
        let mut enum_defs = vm.enum_defs.lock();
        let id: EnumDefId = enum_defs.len().into();

        let old = xenum
            .specializations
            .write()
            .insert(type_params.clone(), id);
        assert!(old.is_none());

        enum_defs.push(Arc::new(RwLock::new(EnumDef {
            id,
            enum_id: xenum.id,
            type_params: type_params.clone(),
            layout: EnumLayout::Int,
            variants: Vec::new(),
        })));

        id
    };

    let layout = if enum_is_simple_integer(xenum) {
        EnumLayout::Int
    } else if enum_is_ptr(vm, xenum, &type_params) {
        EnumLayout::Ptr
    } else {
        EnumLayout::Tagged
    };

    let enum_def = vm.enum_defs.idx(id);
    let mut enum_def = enum_def.write();
    enum_def.layout = layout.clone();
    if let EnumLayout::Tagged = layout {
        enum_def.variants = vec![None; xenum.variants.len()];
    }

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

fn enum_is_ptr(vm: &VM, xenum: &EnumData, type_params: &TypeList) -> bool {
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
        && specialize_type(vm, *some_variant.types.first().unwrap(), type_params).reference_type()
}

pub fn specialize_class_id(vm: &VM, cls_id: ClassId) -> ClassDefId {
    let cls = vm.classes.idx(cls_id);
    let cls = cls.read();
    specialize_class(vm, &*cls, &TypeList::empty())
}

pub fn specialize_class_id_params(vm: &VM, cls_id: ClassId, type_params: &TypeList) -> ClassDefId {
    let cls = vm.classes.idx(cls_id);
    let cls = cls.read();
    specialize_class(vm, &*cls, &type_params)
}

pub fn specialize_class_ty(vm: &VM, ty: BuiltinType) -> ClassDefId {
    match ty {
        BuiltinType::Class(cls_id, list_id) => {
            let params = vm.lists.lock().get(list_id);
            specialize_class_id_params(vm, cls_id, &params)
        }

        _ => unreachable!(),
    }
}

pub fn specialize_class(vm: &VM, cls: &Class, type_params: &TypeList) -> ClassDefId {
    if let Some(&id) = cls.specializations.read().get(&type_params) {
        return id;
    }

    create_specialized_class(vm, cls, type_params)
}

fn create_specialized_class(vm: &VM, cls: &Class, type_params: &TypeList) -> ClassDefId {
    debug_assert!(type_params.iter().all(|ty| ty.is_concrete_type(vm)));

    let id = {
        let mut class_defs = vm.class_defs.lock();
        let id: ClassDefId = class_defs.len().into();

        let old = cls.specializations.write().insert(type_params.clone(), id);
        assert!(old.is_none());

        class_defs.push(Arc::new(RwLock::new(ClassDef {
            id,
            cls_id: Some(cls.id),
            type_params: type_params.clone(),
            parent_id: None,
            size: InstanceSize::Fixed(0),
            fields: Vec::new(),
            ref_fields: Vec::new(),
            vtable: None,
        })));

        id
    };

    let mut fields;
    let mut ref_fields;
    let size;
    let parent_id;

    if cls.is_array || cls.is_str {
        fields = Vec::new();
        ref_fields = Vec::new();

        size = if cls.is_array {
            let element_ty = type_params[0];

            if element_ty.is_unit() {
                InstanceSize::UnitArray
            } else if element_ty.reference_type() {
                InstanceSize::ObjArray
            } else if let Some(tuple_id) = element_ty.tuple_id() {
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
            } else {
                InstanceSize::PrimitiveArray(element_ty.size(vm))
            }
        } else {
            InstanceSize::Str
        };

        let parent_class = cls
            .parent_class
            .expect("Array & String should have super class");
        let id = specialize_class_ty(vm, parent_class);
        parent_id = Some(id);
    } else {
        let mut csize;

        if let Some(parent_class) = cls.parent_class {
            let parent_class = specialize_type(vm, parent_class, type_params);
            let id = specialize_class_ty(vm, parent_class);
            let cls_def = vm.class_defs.idx(id);
            let cls_def = cls_def.read();

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
            let ty = specialize_type(vm, f.ty, &type_params);
            debug_assert!(!ty.contains_type_param(vm));

            let field_size = ty.size(vm);
            let field_align = ty.align(vm);

            let offset = mem::align_i32(csize, field_align);
            fields.push(FieldDef { offset, ty });

            csize = offset + field_size;

            if let Some(tuple_id) = ty.tuple_id() {
                let tuples = vm.tuples.lock();
                let tuple = tuples.get_tuple(tuple_id);

                for &ref_offset in tuple.references() {
                    ref_fields.push(offset + ref_offset);
                }
            } else if ty.reference_type() {
                ref_fields.push(offset);
            }
        }

        size = InstanceSize::Fixed(mem::align_i32(csize, mem::ptr_width()));
    }

    let stub = vm.compile_stub().to_usize();
    let vtable_entries = vec![stub; cls.virtual_fcts.len()];

    let cls_def = vm.class_defs.idx(id);
    let mut cls_def = cls_def.write();
    cls_def.size = size;
    cls_def.fields = fields;
    cls_def.ref_fields = ref_fields;
    cls_def.parent_id = parent_id;

    let (instance_size, element_size) = match size {
        InstanceSize::Fixed(instance_size) => (instance_size as usize, 0),
        InstanceSize::PrimitiveArray(element_size) => (0, element_size as usize),
        InstanceSize::ObjArray => (0, mem::ptr_width_usize()),
        InstanceSize::UnitArray => (Header::size() as usize + mem::ptr_width_usize(), 0),
        InstanceSize::FreeArray => (0, mem::ptr_width_usize()),
        InstanceSize::Str => (0, 1),
        InstanceSize::TupleArray(element_size) => (0, element_size as usize),
    };

    let clsptr = (&*cls_def) as *const ClassDef as *mut ClassDef;
    let vtable = VTableBox::new(clsptr, instance_size, element_size, &vtable_entries);
    cls_def.vtable = Some(vtable);

    ensure_display(vm, &mut cls_def);

    id
}

fn ensure_display<'ast>(vm: &VM<'ast>, cls_def: &mut ClassDef) -> usize {
    let vtable = cls_def.vtable.as_mut().unwrap();

    // if subtype_display[0] is set, vtable was already initialized
    if !vtable.subtype_display[0].is_null() {
        return vtable.subtype_depth as usize;
    }

    if let Some(parent_id) = cls_def.parent_id {
        let parent = vm.class_defs.idx(parent_id);
        let mut parent = parent.write();
        let depth = 1 + ensure_display(vm, &mut *parent);

        let parent_vtable = parent.vtable.as_ref().unwrap();
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

pub fn specialize_tuple(vm: &VM, tuple_id: TupleId, type_params: &TypeList) -> TupleId {
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
        .map(|&t| specialize_type(vm, t, type_params))
        .collect::<Vec<_>>();

    ensure_tuple(vm, new_subtypes)
}

pub fn replace_type_param(
    vm: &VM,
    ty: BuiltinType,
    type_params: &TypeList,
    self_ty: Option<BuiltinType>,
) -> BuiltinType {
    match ty {
        BuiltinType::TypeParam(tpid) => type_params[tpid.to_usize()],

        BuiltinType::Class(cls_id, list_id) => {
            let params = vm.lists.lock().get(list_id);

            let params = TypeList::with(
                params
                    .iter()
                    .map(|p| replace_type_param(vm, p, type_params, self_ty))
                    .collect::<Vec<_>>(),
            );

            let list_id = vm.lists.lock().insert(params);
            BuiltinType::Class(cls_id, list_id)
        }

        BuiltinType::This => self_ty.expect("no type for Self given"),

        BuiltinType::Lambda(_) => unimplemented!(),

        BuiltinType::Tuple(tuple_id) => {
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
                .map(|&t| replace_type_param(vm, t, type_params, self_ty))
                .collect::<Vec<_>>();

            let tuple_id = ensure_tuple(vm, new_subtypes);
            BuiltinType::Tuple(tuple_id)
        }

        _ => ty,
    }
}
