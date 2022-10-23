use parking_lot::Mutex;

use crate::language::sem_analysis::{ClassDefinitionId, FctDefinitionId, TraitDefinitionId};
use crate::language::ty::{SourceType, SourceTypeArray};
use crate::vm::{
    specialize_class_id, specialize_class_id_params, AnnotationDefinitionId, ClassInstanceId,
    EnumDefinitionId, StructDefinitionId, VM,
};

#[derive(Debug)]
pub struct KnownElements {
    pub classes: KnownClasses,
    pub traits: KnownTraits,
    pub annotations: KnownAnnotations,
    pub functions: KnownFunctions,
    pub enums: KnownEnums,
    pub structs: KnownStructs,

    pub byte_array_class_instance: Mutex<Option<ClassInstanceId>>,
    pub int_array_class_instance: Mutex<Option<ClassInstanceId>>,
    pub str_class_instance: Mutex<Option<ClassInstanceId>>,
    pub obj_class_instance: Mutex<Option<ClassInstanceId>>,
    pub ste_class_instance: Mutex<Option<ClassInstanceId>>,
    pub ex_class_instance: Mutex<Option<ClassInstanceId>>,

    pub free_object_class_instance: Option<ClassInstanceId>,
    pub free_array_class_instance: Option<ClassInstanceId>,
    pub code_class_instance: Option<ClassInstanceId>,
}

impl KnownElements {
    pub fn new() -> KnownElements {
        KnownElements {
            classes: KnownClasses::new(),
            functions: KnownFunctions::new(),
            traits: KnownTraits::new(),
            enums: KnownEnums::new(),
            annotations: KnownAnnotations::new(),
            structs: KnownStructs::new(),

            byte_array_class_instance: Mutex::new(None),
            int_array_class_instance: Mutex::new(None),
            str_class_instance: Mutex::new(None),
            obj_class_instance: Mutex::new(None),
            ste_class_instance: Mutex::new(None),
            ex_class_instance: Mutex::new(None),

            free_object_class_instance: None,
            free_array_class_instance: None,
            code_class_instance: None,
        }
    }

    pub fn free_object_class_instance(&self) -> ClassInstanceId {
        self.free_object_class_instance.expect("uninitialized")
    }

    pub fn free_array_class_instance(&self) -> ClassInstanceId {
        self.free_array_class_instance.expect("uninitialized")
    }

    pub fn code_class_instance(&self) -> ClassInstanceId {
        self.code_class_instance.expect("uninitialized")
    }
}

#[derive(Debug)]
pub struct KnownEnums {
    pub option: Option<EnumDefinitionId>,
}

impl KnownEnums {
    pub fn new() -> KnownEnums {
        KnownEnums { option: None }
    }

    pub fn option(&self) -> EnumDefinitionId {
        self.option.expect("uninitialized")
    }
}

#[derive(Debug)]
pub struct KnownClasses {
    pub atomic_int32: Option<ClassDefinitionId>,
    pub atomic_int64: Option<ClassDefinitionId>,
    pub array: Option<ClassDefinitionId>,
    pub string: Option<ClassDefinitionId>,
    pub string_buffer: Option<ClassDefinitionId>,
    pub stacktrace: Option<ClassDefinitionId>,
    pub stacktrace_element: Option<ClassDefinitionId>,
    pub thread: Option<ClassDefinitionId>,
    pub lambda: Option<ClassDefinitionId>,
}

impl KnownClasses {
    pub fn new() -> KnownClasses {
        KnownClasses {
            atomic_int32: None,
            atomic_int64: None,
            array: None,
            string: None,
            string_buffer: None,
            stacktrace: None,
            stacktrace_element: None,
            thread: None,
            lambda: None,
        }
    }

    pub fn atomic_int32(&self) -> ClassDefinitionId {
        self.atomic_int32.expect("uninitialized")
    }

    pub fn atomic_int64(&self) -> ClassDefinitionId {
        self.atomic_int64.expect("uninitialized")
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

    pub fn stacktrace(&self) -> ClassDefinitionId {
        self.stacktrace.expect("uninitialized")
    }

    pub fn stacktrace_element(&self) -> ClassDefinitionId {
        self.stacktrace_element.expect("uninitialized")
    }

    pub fn thread(&self) -> ClassDefinitionId {
        self.thread.expect("uninitialized")
    }

    pub fn thread_class_instance(&self, vm: &VM) -> ClassInstanceId {
        specialize_class_id(vm, self.thread())
    }

    pub fn lambda(&self) -> ClassDefinitionId {
        self.lambda.expect("uninitialized")
    }
}

#[derive(Debug)]
pub struct KnownStructs {
    pub unit: Option<StructDefinitionId>,
    pub bool: Option<StructDefinitionId>,
    pub uint8: Option<StructDefinitionId>,
    pub char: Option<StructDefinitionId>,
    pub int32: Option<StructDefinitionId>,
    pub int64: Option<StructDefinitionId>,
    pub float32: Option<StructDefinitionId>,
    pub float64: Option<StructDefinitionId>,
}

impl KnownStructs {
    pub fn new() -> KnownStructs {
        KnownStructs {
            unit: None,
            bool: None,
            uint8: None,
            char: None,
            int32: None,
            int64: None,
            float32: None,
            float64: None,
        }
    }

    pub fn unit(&self) -> StructDefinitionId {
        self.unit.expect("uninitialized")
    }

    pub fn bool(&self) -> StructDefinitionId {
        self.bool.expect("uninitialized")
    }

    pub fn uint8(&self) -> StructDefinitionId {
        self.uint8.expect("uninitialized")
    }

    pub fn char(&self) -> StructDefinitionId {
        self.char.expect("uninitialized")
    }

    pub fn int32(&self) -> StructDefinitionId {
        self.int32.expect("uninitialized")
    }

    pub fn int64(&self) -> StructDefinitionId {
        self.int64.expect("uninitialized")
    }

    pub fn float32(&self) -> StructDefinitionId {
        self.float32.expect("uninitialized")
    }

    pub fn float64(&self) -> StructDefinitionId {
        self.float64.expect("uninitialized")
    }
}

#[derive(Debug)]
pub struct KnownTraits {
    pub equals: Option<TraitDefinitionId>,
    pub comparable: Option<TraitDefinitionId>,
    pub stringable: Option<TraitDefinitionId>,
    pub iterator: Option<TraitDefinitionId>,
    pub zero: Option<TraitDefinitionId>,
}

impl KnownTraits {
    pub fn new() -> KnownTraits {
        KnownTraits {
            equals: None,
            comparable: None,
            stringable: None,
            iterator: None,
            zero: None,
        }
    }

    pub fn equals(&self) -> TraitDefinitionId {
        self.equals.expect("uninitialized")
    }

    pub fn comparable(&self) -> TraitDefinitionId {
        self.comparable.expect("uninitialized")
    }

    pub fn stringable(&self) -> TraitDefinitionId {
        self.stringable.expect("uninitialized")
    }

    pub fn iterator(&self) -> TraitDefinitionId {
        self.iterator.expect("uninitialized")
    }

    pub fn zero(&self) -> TraitDefinitionId {
        self.zero.expect("uninitialized")
    }
}

#[derive(Debug)]
pub struct KnownAnnotations {
    pub internal: Option<AnnotationDefinitionId>,
    pub pub_: Option<AnnotationDefinitionId>,
    pub static_: Option<AnnotationDefinitionId>,
    pub test: Option<AnnotationDefinitionId>,
    pub cannon: Option<AnnotationDefinitionId>,
    pub optimize_immediately: Option<AnnotationDefinitionId>,
}

impl KnownAnnotations {
    pub fn new() -> KnownAnnotations {
        KnownAnnotations {
            internal: None,
            pub_: None,
            static_: None,
            test: None,
            cannon: None,
            optimize_immediately: None,
        }
    }

    pub fn internal(&self) -> AnnotationDefinitionId {
        self.internal.expect("uninitialized")
    }

    pub fn pub_(&self) -> AnnotationDefinitionId {
        self.pub_.expect("uninitialized")
    }

    pub fn static_(&self) -> AnnotationDefinitionId {
        self.static_.expect("uninitialized")
    }

    pub fn test(&self) -> AnnotationDefinitionId {
        self.test.expect("uninitialized")
    }

    pub fn cannon(&self) -> AnnotationDefinitionId {
        self.cannon.expect("uninitialized")
    }

    pub fn optimize_immediately(&self) -> AnnotationDefinitionId {
        self.optimize_immediately.expect("uninitialized")
    }
}

#[derive(Debug)]
pub struct KnownFunctions {
    pub string_buffer_empty: Option<FctDefinitionId>,
    pub string_buffer_append: Option<FctDefinitionId>,
    pub string_buffer_to_string: Option<FctDefinitionId>,
    pub assert: Option<FctDefinitionId>,
    pub option_is_some: Option<FctDefinitionId>,
    pub option_is_none: Option<FctDefinitionId>,
    pub option_unwrap: Option<FctDefinitionId>,
    pub stacktrace_retrieve: Option<FctDefinitionId>,
    pub compile: Option<FctDefinitionId>,
}

impl KnownFunctions {
    pub fn new() -> KnownFunctions {
        KnownFunctions {
            string_buffer_empty: None,
            string_buffer_append: None,
            string_buffer_to_string: None,
            assert: None,
            option_is_none: None,
            option_is_some: None,
            option_unwrap: None,
            stacktrace_retrieve: None,
            compile: None,
        }
    }

    pub fn string_buffer_empty(&self) -> FctDefinitionId {
        self.string_buffer_empty.expect("uninitialized")
    }

    pub fn string_buffer_append(&self) -> FctDefinitionId {
        self.string_buffer_append.expect("uninitialized")
    }

    pub fn string_buffer_to_string(&self) -> FctDefinitionId {
        self.string_buffer_to_string.expect("uninitialized")
    }

    pub fn assert(&self) -> FctDefinitionId {
        self.assert.expect("uninitialized")
    }

    pub fn option_is_some(&self) -> FctDefinitionId {
        self.option_is_some.expect("uninitialized")
    }

    pub fn option_is_none(&self) -> FctDefinitionId {
        self.option_is_none.expect("uninitialized")
    }

    pub fn option_unwrap(&self) -> FctDefinitionId {
        self.option_unwrap.expect("uninitialized")
    }

    pub fn stacktrace_retrieve(&self) -> FctDefinitionId {
        self.stacktrace_retrieve.expect("uninitialized")
    }

    pub fn compile(&self) -> FctDefinitionId {
        self.compile.expect("uninitialized")
    }
}

impl KnownElements {
    pub fn array_ty(&self, element: SourceType) -> SourceType {
        SourceType::Class(self.classes.array(), SourceTypeArray::single(element))
    }

    pub fn byte_array(&self, vm: &VM) -> ClassInstanceId {
        let mut byte_array_def = self.byte_array_class_instance.lock();

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
        let mut int_array_def = self.int_array_class_instance.lock();

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
        let mut str_class_def = self.str_class_instance.lock();

        if let Some(cls_id) = *str_class_def {
            cls_id
        } else {
            let cls_id = specialize_class_id(vm, self.classes.string());
            *str_class_def = Some(cls_id);
            cls_id
        }
    }

    pub fn stack_trace_element(&self, vm: &VM) -> ClassInstanceId {
        let mut ste_class_def = self.ste_class_instance.lock();

        if let Some(cls_id) = *ste_class_def {
            cls_id
        } else {
            let cls_id = specialize_class_id(vm, self.classes.stacktrace_element());
            *ste_class_def = Some(cls_id);
            cls_id
        }
    }
}
