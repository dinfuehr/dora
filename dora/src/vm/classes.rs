use parking_lot::RwLock;

use crate::language::sem_analysis::{
    ClassDefinitionId, EnumDefinitionId, FctDefinitionId, TraitDefinitionId,
};
use crate::language::ty::{SourceType, SourceTypeArray};
use crate::size::InstanceSize;
use crate::utils::Id;
use crate::vm::VM;
use crate::vtable::{ensure_display, VTableBox};

pub static DISPLAY_SIZE: usize = 6;

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
    Class(ClassDefinitionId, SourceTypeArray),
    Lambda(FctDefinitionId, SourceTypeArray),
    TraitObject(SourceType, TraitDefinitionId, SourceTypeArray),
    Enum(EnumDefinitionId, SourceTypeArray),
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

    pub fn trait_object(&self) -> Option<SourceType> {
        match &self.kind {
            ShapeKind::TraitObject(object, _, _) => Some(object.clone()),
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
    pub ty: SourceType,
}

pub fn create_class_instance_with_vtable(
    vm: &VM,
    kind: ShapeKind,
    size: InstanceSize,
    fields: Vec<FieldInstance>,
    ref_fields: Vec<i32>,
    parent_id: Option<ClassInstanceId>,
    vtable_entries: usize,
) -> ClassInstanceId {
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

    let mut vtable = VTableBox::new(
        class_instance_ptr,
        instance_size,
        element_size,
        &vtable_mtdptrs,
    );
    ensure_display(vm, &mut vtable, parent_id);

    *class_instance.vtable.write() = Some(vtable);

    class_instance_id
}
