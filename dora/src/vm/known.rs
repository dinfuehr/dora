use parking_lot::Mutex;

use crate::vm::ClassInstanceId;
use dora_bytecode::{ClassId, FunctionId, TraitId};

#[derive(Debug)]
pub struct KnownInstances {
    pub byte_array_class_instance: Mutex<Option<ClassInstanceId>>,
    pub int_array_class_instance: Mutex<Option<ClassInstanceId>>,
    pub str_class_instance: Mutex<Option<ClassInstanceId>>,
    pub obj_class_instance: Mutex<Option<ClassInstanceId>>,
    pub ste_class_instance: Mutex<Option<ClassInstanceId>>,
    pub ex_class_instance: Mutex<Option<ClassInstanceId>>,

    pub free_object_class_instance: Option<ClassInstanceId>,
    pub free_array_class_instance: Option<ClassInstanceId>,
    pub code_class_instance: Option<ClassInstanceId>,

    pub zero_trait_id: Option<TraitId>,
    pub array_class_id: Option<ClassId>,
    pub string_class_id: Option<ClassId>,
    pub thread_class_id: Option<ClassId>,
    pub stacktrace_element_class_id: Option<ClassId>,
    pub stacktrace_retrieve_fct_id: Option<FunctionId>,
    pub boots_compile_fct_id: Option<FunctionId>,
}

impl KnownInstances {
    pub fn new() -> KnownInstances {
        KnownInstances {
            byte_array_class_instance: Mutex::new(None),
            int_array_class_instance: Mutex::new(None),
            str_class_instance: Mutex::new(None),
            obj_class_instance: Mutex::new(None),
            ste_class_instance: Mutex::new(None),
            ex_class_instance: Mutex::new(None),

            free_object_class_instance: None,
            free_array_class_instance: None,
            code_class_instance: None,

            zero_trait_id: None,
            array_class_id: None,
            string_class_id: None,
            thread_class_id: None,
            stacktrace_element_class_id: None,
            stacktrace_retrieve_fct_id: None,
            boots_compile_fct_id: None,
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

    pub fn zero_trait_id(&self) -> TraitId {
        self.zero_trait_id.expect("uninitialized")
    }

    pub fn array_class_id(&self) -> ClassId {
        self.array_class_id.expect("uninitialized")
    }

    pub fn string_class_id(&self) -> ClassId {
        self.string_class_id.expect("uninitialized")
    }

    pub fn thread_class_id(&self) -> ClassId {
        self.thread_class_id.expect("uninitialized")
    }

    pub fn stacktrace_element_class_id(&self) -> ClassId {
        self.stacktrace_element_class_id.expect("uninitialized")
    }

    pub fn stacktrace_retrieve_fct_id(&self) -> FunctionId {
        self.stacktrace_retrieve_fct_id.expect("uninitialized")
    }

    pub fn boots_compile_fct_id(&self) -> FunctionId {
        self.boots_compile_fct_id.expect("uninitialized")
    }
}
