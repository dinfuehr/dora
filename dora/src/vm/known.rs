use parking_lot::Mutex;

use crate::semck::specialize::{specialize_class_id, specialize_class_id_params};
use crate::ty::{SourceType, SourceTypeArray};
use crate::vm::{AnnotationId, ClassDefId, ClassId, EnumId, FctId, StructId, TraitId, VM};

#[derive(Debug)]
pub struct KnownElements {
    pub classes: KnownClasses,
    pub traits: KnownTraits,
    pub annotations: KnownAnnotations,
    pub functions: KnownFunctions,
    pub enums: KnownEnums,
    pub structs: KnownStructs,

    pub byte_array_def: Mutex<Option<ClassDefId>>,
    pub int_array_def: Mutex<Option<ClassDefId>>,
    pub str_class_def: Mutex<Option<ClassDefId>>,
    pub obj_class_def: Mutex<Option<ClassDefId>>,
    pub ste_class_def: Mutex<Option<ClassDefId>>,
    pub ex_class_def: Mutex<Option<ClassDefId>>,

    pub free_object_class_def: ClassDefId,
    pub free_array_class_def: ClassDefId,
}

#[derive(Debug)]
pub struct KnownEnums {
    pub option: EnumId,
}

#[derive(Debug)]
pub struct KnownClasses {
    pub atomic_int32: Option<ClassId>,
    pub atomic_int64: Option<ClassId>,
    pub object: Option<ClassId>,
    pub array: Option<ClassId>,
    pub string: Option<ClassId>,
    pub string_buffer: Option<ClassId>,
    pub testing: Option<ClassId>,
    pub stacktrace: Option<ClassId>,
    pub stacktrace_element: Option<ClassId>,
}

impl KnownClasses {
    pub fn new() -> KnownClasses {
        KnownClasses {
            atomic_int32: None,
            atomic_int64: None,
            object: None,
            array: None,
            string: None,
            string_buffer: None,
            testing: None,
            stacktrace: None,
            stacktrace_element: None,
        }
    }

    pub fn atomic_int32(&self) -> ClassId {
        self.object.expect("uninitialized")
    }

    pub fn object(&self) -> ClassId {
        self.object.expect("uninitialized")
    }

    pub fn array(&self) -> ClassId {
        self.array.expect("uninitialized")
    }

    pub fn string(&self) -> ClassId {
        self.string.expect("uninitialized")
    }

    pub fn string_buffer(&self) -> ClassId {
        self.string_buffer.expect("uninitialized")
    }

    pub fn testing(&self) -> ClassId {
        self.testing.expect("uninitialized")
    }

    pub fn stacktrace(&self) -> ClassId {
        self.stacktrace.expect("uninitialized")
    }

    pub fn stacktrace_element(&self) -> ClassId {
        self.stacktrace_element.expect("uninitialized")
    }
}

#[derive(Debug)]
pub struct KnownStructs {
    pub bool: StructId,
    pub uint8: StructId,
    pub char: StructId,
    pub int32: StructId,
    pub int64: StructId,
    pub float32: StructId,
    pub float64: StructId,
}

#[derive(Debug)]
pub struct KnownTraits {
    pub equals: TraitId,
    pub comparable: TraitId,
    pub stringable: TraitId,
    pub iterator: TraitId,
    pub zero: TraitId,
}

#[derive(Debug)]
pub struct KnownAnnotations {
    pub abstract_: AnnotationId,
    pub final_: AnnotationId,
    pub internal: AnnotationId,
    pub override_: AnnotationId,
    pub open: AnnotationId,
    pub pub_: AnnotationId,
    pub static_: AnnotationId,

    pub test: AnnotationId,

    pub cannon: AnnotationId,
    pub optimize_immediately: AnnotationId,
}

#[derive(Debug)]
pub struct KnownFunctions {
    pub string_buffer_empty: FctId,
    pub string_buffer_append: FctId,
    pub string_buffer_to_string: FctId,
}

impl KnownElements {
    pub fn array_ty(&self, vm: &VM, element: SourceType) -> SourceType {
        let list = SourceTypeArray::single(element);
        let list_id = vm.source_type_arrays.lock().insert(list);
        SourceType::Class(self.classes.array(), list_id)
    }

    pub fn byte_array(&self, vm: &VM) -> ClassDefId {
        let mut byte_array_def = self.byte_array_def.lock();

        if let Some(cls_id) = *byte_array_def {
            cls_id
        } else {
            let type_args = SourceTypeArray::single(SourceType::UInt8);
            let cls_id = specialize_class_id_params(vm, self.classes.array(), &type_args);
            *byte_array_def = Some(cls_id);
            cls_id
        }
    }

    pub fn int_array(&self, vm: &VM) -> ClassDefId {
        let mut int_array_def = self.int_array_def.lock();

        if let Some(cls_id) = *int_array_def {
            cls_id
        } else {
            let type_args = SourceTypeArray::single(SourceType::Int32);
            let cls_id = specialize_class_id_params(vm, self.classes.array(), &type_args);
            *int_array_def = Some(cls_id);
            cls_id
        }
    }

    pub fn str(&self, vm: &VM) -> ClassDefId {
        let mut str_class_def = self.str_class_def.lock();

        if let Some(cls_id) = *str_class_def {
            cls_id
        } else {
            let cls_id = specialize_class_id(vm, self.classes.string());
            *str_class_def = Some(cls_id);
            cls_id
        }
    }

    pub fn obj(&self, vm: &VM) -> ClassDefId {
        let mut obj_class_def = self.obj_class_def.lock();

        if let Some(cls_id) = *obj_class_def {
            cls_id
        } else {
            let cls_id = specialize_class_id(vm, self.classes.object());
            *obj_class_def = Some(cls_id);
            cls_id
        }
    }

    pub fn stack_trace_element(&self, vm: &VM) -> ClassDefId {
        let mut ste_class_def = self.ste_class_def.lock();

        if let Some(cls_id) = *ste_class_def {
            cls_id
        } else {
            let cls_id = specialize_class_id(vm, self.classes.stacktrace_element());
            *ste_class_def = Some(cls_id);
            cls_id
        }
    }
}
