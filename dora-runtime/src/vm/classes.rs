use crate::size::InstanceSize;
use crate::vm::add_ref_fields;
use crate::{Shape, ShapeVisitor, VM};
use dora_bytecode::{BytecodeType, BytecodeTypeArray, ClassId, EnumId, FunctionId};

#[derive(Clone, Debug)]
pub enum ShapeKind {
    Array(ClassId, BytecodeTypeArray),
    Class(ClassId, BytecodeTypeArray),
    Lambda(FunctionId, BytecodeTypeArray),
    TraitObject {
        trait_ty: BytecodeType,
        actual_object_ty: BytecodeType,
    },
    Enum(EnumId, BytecodeTypeArray),
    Builtin,
}

#[derive(Debug, Clone)]
pub struct FieldInstance {
    pub offset: i32,
    pub ty: BytecodeType,
}

pub fn create_shape(
    vm: &VM,
    kind: ShapeKind,
    size: InstanceSize,
    fields: Vec<FieldInstance>,
    vtable_entries: usize,
) -> *const Shape {
    let ref_fields = build_ref_fields(vm, &kind, size, &fields);

    let size = match size {
        InstanceSize::StructArray(element_size) if ref_fields.is_empty() => {
            InstanceSize::PrimitiveArray(element_size)
        }
        _ => size,
    };

    let instance_size = size.instance_size().unwrap_or(0) as usize;
    let element_size = size.element_size().unwrap_or(-1) as usize;

    let visitor = match size {
        InstanceSize::PrimitiveArray(_) => ShapeVisitor::None,
        InstanceSize::ObjArray => ShapeVisitor::PointerArray,
        InstanceSize::Str => ShapeVisitor::None,
        InstanceSize::Fixed(..) => ShapeVisitor::Regular,
        InstanceSize::FillerWord | InstanceSize::FillerArray | InstanceSize::FreeSpace => {
            ShapeVisitor::None
        }
        InstanceSize::StructArray(_) => ShapeVisitor::RecordArray,
        InstanceSize::UnitArray => ShapeVisitor::None,
        InstanceSize::CodeObject => ShapeVisitor::Invalid,
    };

    let vtable_mtdptrs = if vtable_entries > 0 {
        let compilation_stub = vm.native_methods.lazy_compilation_stub().to_usize();
        vec![compilation_stub; vtable_entries]
    } else {
        Vec::new()
    };

    let shape = Shape::new(
        vm,
        kind,
        visitor,
        ref_fields,
        fields,
        instance_size,
        element_size,
        &vtable_mtdptrs,
    );

    shape
}

fn build_ref_fields(
    vm: &VM,
    kind: &ShapeKind,
    size: InstanceSize,
    fields: &[FieldInstance],
) -> Vec<i32> {
    match &kind {
        ShapeKind::Class(cls_id, type_params) => {
            if vm.known.array_class_id() == *cls_id {
                if size == InstanceSize::ObjArray {
                    Vec::new()
                } else {
                    create_array_ref_fields(vm, type_params[0].clone())
                }
            } else if vm.known.string_class_id() == *cls_id {
                Vec::new()
            } else {
                let ref_fields = Vec::new();
                create_ref_fields(vm, &fields, ref_fields)
            }
        }

        _ => create_ref_fields(vm, &fields, Vec::new()),
    }
}

fn create_ref_fields(vm: &VM, fields: &[FieldInstance], mut ref_fields: Vec<i32>) -> Vec<i32> {
    for field in fields {
        add_ref_fields(vm, &mut ref_fields, field.offset, field.ty.clone());
    }

    ref_fields
}

fn create_array_ref_fields(vm: &VM, ty: BytecodeType) -> Vec<i32> {
    let mut ref_fields = Vec::new();
    add_ref_fields(vm, &mut ref_fields, 0, ty);
    ref_fields
}
