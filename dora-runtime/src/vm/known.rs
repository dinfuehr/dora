use parking_lot::Mutex;

use std::cell::OnceCell;

use crate::gc::Address;
use crate::vm::ClassInstanceId;
use dora_bytecode::{ClassId, FunctionId, TraitId};

#[derive(Debug)]
pub struct KnownElements {
    pub byte_array_class_instance_id: Mutex<Option<ClassInstanceId>>,
    pub int_array_class_instance_id: Mutex<Option<ClassInstanceId>>,
    pub string_class_instance_id: Mutex<Option<ClassInstanceId>>,
    pub ste_class_instance_id: Mutex<Option<ClassInstanceId>>,

    pub filler_word_class_instance_id: Option<ClassInstanceId>,
    pub filler_array_class_instance_id: Option<ClassInstanceId>,
    pub free_space_class_instance_id: Option<ClassInstanceId>,
    pub code_class_instance_id: Option<ClassInstanceId>,

    pub filler_word_class_address: Address,
    pub filler_array_class_address: Address,
    pub free_space_class_address: Address,

    pub zero_trait_id: Option<TraitId>,
    pub array_class_id: Option<ClassId>,
    pub string_class_id: Option<ClassId>,
    pub thread_class_id: Option<ClassId>,
    pub boots_compile_fct_id: Option<FunctionId>,
    pub boots_compile_fct_address: OnceCell<Address>,
}

impl KnownElements {
    pub fn new() -> KnownElements {
        KnownElements {
            byte_array_class_instance_id: Mutex::new(None),
            int_array_class_instance_id: Mutex::new(None),
            string_class_instance_id: Mutex::new(None),
            ste_class_instance_id: Mutex::new(None),

            filler_word_class_instance_id: None,
            filler_array_class_instance_id: None,
            free_space_class_instance_id: None,
            code_class_instance_id: None,

            free_space_class_address: Address::null(),
            filler_word_class_address: Address::null(),
            filler_array_class_address: Address::null(),

            zero_trait_id: None,
            array_class_id: None,
            string_class_id: None,
            thread_class_id: None,
            boots_compile_fct_id: None,
            boots_compile_fct_address: OnceCell::new(),
        }
    }

    pub fn filler_word_class_instance(&self) -> ClassInstanceId {
        self.filler_word_class_instance_id.expect("uninitialized")
    }

    pub fn filler_word_class_address(&self) -> Address {
        self.filler_word_class_address
    }

    pub fn filler_array_class_instance(&self) -> ClassInstanceId {
        self.filler_array_class_instance_id.expect("uninitialized")
    }

    pub fn filler_array_class_address(&self) -> Address {
        self.filler_array_class_address
    }

    pub fn free_space_class_instance(&self) -> ClassInstanceId {
        self.free_space_class_instance_id.expect("uninitialized")
    }

    pub fn free_space_class_address(&self) -> Address {
        self.free_space_class_address
    }

    pub fn code_class_instance(&self) -> ClassInstanceId {
        self.code_class_instance_id.expect("uninitialized")
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

    pub fn boots_compile_fct_id(&self) -> FunctionId {
        self.boots_compile_fct_id.expect("uninitialized")
    }

    pub fn boots_compile_fct_address(&self) -> Address {
        self.boots_compile_fct_address
            .get()
            .cloned()
            .expect("uninitialized")
    }
}
