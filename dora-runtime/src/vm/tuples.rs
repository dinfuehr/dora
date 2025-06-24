use std::cmp::max;

use crate::cannon::codegen::{align, size};
use crate::mem;
use crate::vm::{VM, add_ref_fields};
use dora_bytecode::{BytecodeType, BytecodeTypeArray};

#[derive(Clone)]
pub struct ConcreteTuple {
    offsets: Vec<i32>,
    references: Vec<i32>,
    size: i32,
    align: i32,
}

impl ConcreteTuple {
    pub fn offsets(&self) -> &[i32] {
        &self.offsets
    }

    pub fn contains_references(&self) -> bool {
        !self.references.is_empty()
    }

    pub fn references(&self) -> &[i32] {
        &self.references
    }

    pub fn size(&self) -> i32 {
        self.size
    }

    pub fn align(&self) -> i32 {
        self.align
    }
}

fn determine_tuple_size(vm: &VM, subtypes: BytecodeTypeArray) -> ConcreteTuple {
    let mut total_size = 0;
    let mut offsets = Vec::new();
    let mut references = Vec::new();
    let mut total_align = 0;

    for ty in subtypes.iter() {
        assert!(ty.is_concrete_type());

        let element_size = size(vm, ty.clone());
        let element_align = align(vm, ty.clone());

        let element_offset = mem::align_i32(total_size, element_align);
        offsets.push(element_offset);

        add_ref_fields(vm, &mut references, element_offset, ty);

        total_size = element_offset + element_size;
        total_align = max(total_align, element_align);
    }

    total_size = mem::align_i32(total_size, total_align);
    ConcreteTuple {
        offsets,
        references,
        size: total_size,
        align: total_align,
    }
}

pub fn get_concrete_tuple_bty(vm: &VM, ty: &BytecodeType) -> ConcreteTuple {
    let subtypes = ty.tuple_subtypes();
    determine_tuple_size(vm, subtypes)
}

pub fn get_concrete_tuple_bty_array(vm: &VM, subtypes: BytecodeTypeArray) -> ConcreteTuple {
    determine_tuple_size(vm, subtypes)
}
