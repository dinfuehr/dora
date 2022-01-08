use parking_lot::Mutex;

use crate::language::ty::{SourceType, SourceTypeArray};
use crate::vm::{
    specialize_class_id, specialize_class_id_params, AnnotationDefinitionId, ClassDefinitionId,
    ClassInstanceId, EnumDefinitionId, FctDefinitionId, StructDefinitionId, TraitDefinitionId, VM,
};

#[derive(Debug)]
pub struct KnownElements {
    pub classes: KnownClasses,
    pub traits: KnownTraits,
    pub annotations: KnownAnnotations,
    pub functions: KnownFunctions,
    pub enums: KnownEnums,
    pub structs: KnownStructs,

    pub byte_array_def: Mutex<Option<ClassInstanceId>>,
    pub int_array_def: Mutex<Option<ClassInstanceId>>,
    pub str_class_def: Mutex<Option<ClassInstanceId>>,
    pub obj_class_def: Mutex<Option<ClassInstanceId>>,
    pub ste_class_def: Mutex<Option<ClassInstanceId>>,
    pub ex_class_def: Mutex<Option<ClassInstanceId>>,

    pub free_object_class_def: ClassInstanceId,
    pub free_array_class_def: ClassInstanceId,
}

#[derive(Debug)]
pub struct KnownEnums {
    pub option: EnumDefinitionId,
}

#[derive(Debug)]
pub struct KnownClasses {
    pub atomic_int32: Option<ClassDefinitionId>,
    pub atomic_int64: Option<ClassDefinitionId>,
    pub object: Option<ClassDefinitionId>,
    pub array: Option<ClassDefinitionId>,
    pub string: Option<ClassDefinitionId>,
    pub string_buffer: Option<ClassDefinitionId>,
    pub testing: Option<ClassDefinitionId>,
    pub stacktrace: Option<ClassDefinitionId>,
    pub stacktrace_element: Option<ClassDefinitionId>,
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

    pub fn atomic_int32(&self) -> ClassDefinitionId {
        self.object.expect("uninitialized")
    }

    pub fn object(&self) -> ClassDefinitionId {
        self.object.expect("uninitialized")
    }

    pub fn array(&self) -> ClassDefinitionId {
        self.array.expect("uninitialized")
    }

    pub fn string(&self) -> ClassDefinitionId {
        self.string.expect("uninitialized")
    }

    pub fn string_buffer(&self) -> ClassDefinitionId {
        self.string_buffer.expect("uninitialized")
    }

    pub fn testing(&self) -> ClassDefinitionId {
        self.testing.expect("uninitialized")
    }

    pub fn stacktrace(&self) -> ClassDefinitionId {
        self.stacktrace.expect("uninitialized")
    }

    pub fn stacktrace_element(&self) -> ClassDefinitionId {
        self.stacktrace_element.expect("uninitialized")
    }
}

#[derive(Debug)]
pub struct KnownStructs {
    pub bool: StructDefinitionId,
    pub uint8: StructDefinitionId,
    pub char: StructDefinitionId,
    pub int32: StructDefinitionId,
    pub int64: StructDefinitionId,
    pub float32: StructDefinitionId,
    pub float64: StructDefinitionId,
}

#[derive(Debug)]
pub struct KnownTraits {
    pub equals: TraitDefinitionId,
    pub comparable: TraitDefinitionId,
    pub stringable: TraitDefinitionId,
    pub iterator: TraitDefinitionId,
    pub zero: TraitDefinitionId,
}

#[derive(Debug)]
pub struct KnownAnnotations {
    pub abstract_: AnnotationDefinitionId,
    pub final_: AnnotationDefinitionId,
    pub internal: AnnotationDefinitionId,
    pub override_: AnnotationDefinitionId,
    pub open: AnnotationDefinitionId,
    pub pub_: AnnotationDefinitionId,
    pub static_: AnnotationDefinitionId,

    pub test: AnnotationDefinitionId,

    pub cannon: AnnotationDefinitionId,
    pub optimize_immediately: AnnotationDefinitionId,
}

#[derive(Debug)]
pub struct KnownFunctions {
    pub string_buffer_empty: FctDefinitionId,
    pub string_buffer_append: FctDefinitionId,
    pub string_buffer_to_string: FctDefinitionId,
}

impl KnownElements {
    pub fn array_ty(&self, element: SourceType) -> SourceType {
        SourceType::Class(self.classes.array(), SourceTypeArray::single(element))
    }

    pub fn byte_array(&self, vm: &VM) -> ClassInstanceId {
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

    pub fn int_array(&self, vm: &VM) -> ClassInstanceId {
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

    pub fn str(&self, vm: &VM) -> ClassInstanceId {
        let mut str_class_def = self.str_class_def.lock();

        if let Some(cls_id) = *str_class_def {
            cls_id
        } else {
            let cls_id = specialize_class_id(vm, self.classes.string());
            *str_class_def = Some(cls_id);
            cls_id
        }
    }

    pub fn obj(&self, vm: &VM) -> ClassInstanceId {
        let mut obj_class_def = self.obj_class_def.lock();

        if let Some(cls_id) = *obj_class_def {
            cls_id
        } else {
            let cls_id = specialize_class_id(vm, self.classes.object());
            *obj_class_def = Some(cls_id);
            cls_id
        }
    }

    pub fn stack_trace_element(&self, vm: &VM) -> ClassInstanceId {
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
