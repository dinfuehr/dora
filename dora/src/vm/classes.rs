use parking_lot::RwLock;

use crate::size::InstanceSize;
use crate::vm::{add_ref_fields, module_path_with_name, VM};
use crate::vtable::VTableBox;
use dora_frontend::bytecode::{BytecodeType, BytecodeTypeArray};
use dora_frontend::language::sem_analysis::{
    ClassDefinition, ClassDefinitionId, EnumDefinitionId, FctDefinitionId, TraitDefinitionId,
};
use dora_frontend::Id;

pub fn class_definition_name(cls: &ClassDefinition, vm: &VM) -> String {
    module_path_with_name(vm, cls.module_id, cls.name)
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct ClassInstanceId(usize);

impl ClassInstanceId {
    pub fn to_usize(self) -> usize {
        self.0
    }
}

impl Id for ClassInstance {
    type IdType = ClassInstanceId;

    fn id_to_usize(id: ClassInstanceId) -> usize {
        id.0 as usize
    }

    fn usize_to_id(value: usize) -> ClassInstanceId {
        ClassInstanceId(value.try_into().unwrap())
    }

    fn store_id(value: &mut ClassInstance, id: ClassInstanceId) {
        value.id = Some(id);
    }
}

#[derive(Debug)]
pub enum ShapeKind {
    Class(ClassDefinitionId, BytecodeTypeArray),
    Lambda(FctDefinitionId, BytecodeTypeArray),
    TraitObject {
        object_ty: BytecodeType,
        trait_id: TraitDefinitionId,
        combined_type_params: BytecodeTypeArray,
    },
    Enum(EnumDefinitionId, BytecodeTypeArray),
    Builtin,
}

#[derive(Debug)]
pub struct ClassInstance {
    pub id: Option<ClassInstanceId>,
    pub kind: ShapeKind,
    pub fields: Vec<FieldInstance>,
    pub size: InstanceSize,
    pub ref_fields: Vec<i32>,
    pub vtable: RwLock<Option<VTableBox>>,
}

impl ClassInstance {
    pub fn id(&self) -> ClassInstanceId {
        self.id.expect("missing id")
    }

    pub fn trait_object(&self) -> Option<BytecodeType> {
        match &self.kind {
            ShapeKind::TraitObject { object_ty, .. } => Some(object_ty.clone()),
            _ => None,
        }
    }

    pub fn cls_id(&self) -> Option<ClassDefinitionId> {
        match &self.kind {
            ShapeKind::Class(cls_id, _) => Some(*cls_id),
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct FieldInstance {
    pub offset: i32,
    pub ty: BytecodeType,
}

pub fn create_class_instance_with_vtable(
    vm: &VM,
    kind: ShapeKind,
    size: InstanceSize,
    fields: Vec<FieldInstance>,
    vtable_entries: usize,
) -> ClassInstanceId {
    let ref_fields = build_ref_fields(vm, &kind, size, &fields);

    let size = match size {
        InstanceSize::StructArray(element_size) if ref_fields.is_empty() => {
            InstanceSize::PrimitiveArray(element_size)
        }
        _ => size,
    };

    let class_instance_id = vm.class_instances.push(ClassInstance {
        id: None,
        kind,
        fields,
        size,
        ref_fields,
        vtable: RwLock::new(None),
    });
    let class_instance = vm.class_instances.idx(class_instance_id);
    let class_instance_ptr = &*class_instance as *const ClassInstance as *mut ClassInstance;

    let instance_size = size.instance_size().unwrap_or(0) as usize;
    let element_size = size.element_size().unwrap_or(-1) as usize;

    let vtable_mtdptrs = if vtable_entries > 0 {
        let compilation_stub = vm.stubs.lazy_compilation().to_usize();
        vec![compilation_stub; vtable_entries]
    } else {
        Vec::new()
    };

    let vtable = VTableBox::new(
        class_instance_ptr,
        instance_size,
        element_size,
        &vtable_mtdptrs,
    );

    *class_instance.vtable.write() = Some(vtable);

    class_instance_id
}

fn build_ref_fields(
    vm: &VM,
    kind: &ShapeKind,
    size: InstanceSize,
    fields: &[FieldInstance],
) -> Vec<i32> {
    match &kind {
        ShapeKind::Class(cls_id, type_params) => {
            let cls = &vm.program.classes[cls_id.to_usize()];

            if cls.layout.is_array() {
                if size == InstanceSize::ObjArray {
                    Vec::new()
                } else {
                    create_array_ref_fields(vm, type_params[0].clone())
                }
            } else if cls.layout.is_string() {
                Vec::new()
            } else {
                assert!(cls.layout.is_regular());
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
