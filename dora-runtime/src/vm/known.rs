use std::{cell::OnceCell, ptr};

use crate::{Address, Shape};
use dora_bytecode::{ClassId, FunctionId, TraitId};

#[derive(Debug)]
pub struct KnownElements {
    pub byte_array_shape: *const Shape,
    pub int32_array_shape: *const Shape,
    pub string_shape: *const Shape,
    pub thread_shape: *const Shape,

    pub filler_word_shape: *const Shape,
    pub filler_array_shape: *const Shape,
    pub free_space_shape: *const Shape,
    pub code_shape: *const Shape,

    pub zero_trait_id: Option<TraitId>,
    pub array_class_id: Option<ClassId>,
    pub string_class_id: Option<ClassId>,
    pub thread_class_id: Option<ClassId>,
    pub boots_compile_fct_id: Option<FunctionId>,
    pub boots_compile_fct_address: OnceCell<Address>,
    pub unreachable_fct_id: Option<FunctionId>,
    pub fatal_error_fct_id: Option<FunctionId>,
}

impl KnownElements {
    pub fn new() -> KnownElements {
        KnownElements {
            byte_array_shape: ptr::null(),
            int32_array_shape: ptr::null(),
            string_shape: ptr::null(),
            thread_shape: ptr::null(),

            free_space_shape: ptr::null(),
            filler_word_shape: ptr::null(),
            filler_array_shape: ptr::null(),
            code_shape: ptr::null(),

            zero_trait_id: None,
            array_class_id: None,
            string_class_id: None,
            thread_class_id: None,
            boots_compile_fct_id: None,
            boots_compile_fct_address: OnceCell::new(),
            unreachable_fct_id: None,
            fatal_error_fct_id: None,
        }
    }

    pub fn byte_array_shape(&self) -> &Shape {
        unsafe { &*self.byte_array_shape }
    }

    pub fn int32_array_shape(&self) -> &Shape {
        unsafe { &*self.int32_array_shape }
    }

    pub fn string_shape(&self) -> &Shape {
        unsafe { &*self.string_shape }
    }

    pub fn thread_shape(&self) -> &Shape {
        unsafe { &*self.thread_shape }
    }

    pub fn filler_word_shape(&self) -> &Shape {
        unsafe { &*self.filler_word_shape }
    }

    pub fn filler_array_shape(&self) -> &Shape {
        unsafe { &*self.filler_array_shape }
    }

    pub fn free_space_shape(&self) -> &Shape {
        unsafe { &*self.free_space_shape }
    }

    pub fn code_shape(&self) -> &Shape {
        unsafe { &*self.code_shape }
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

pub use dora_compiler::Intrinsic;
