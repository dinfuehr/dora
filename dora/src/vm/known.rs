use parking_lot::Mutex;

use crate::vm::ClassInstanceId;

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
