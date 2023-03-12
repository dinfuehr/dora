use crate::cannon::codegen::{align, size};
use crate::gc::{Address, Region};
use crate::mem;
use crate::os;
use crate::vm::{add_ref_fields, VM};
use dora_frontend::bytecode::GlobalId;

pub fn init_global_addresses(vm: &mut VM) {
    let number_globals = vm.program.globals.len();
    let mut backing_memory_size = 0;
    let mut offsets = Vec::with_capacity(number_globals);
    let mut references = Vec::new();

    let initialized_field_size = 1;

    for global_var in &vm.program.globals {
        let initialized_offset = backing_memory_size;
        backing_memory_size += initialized_field_size;

        let ty = global_var.ty.clone();
        assert!(ty.is_concrete_type());

        let ty_size = size(vm, ty.clone()) as usize;
        let ty_align = align(vm, ty.clone()) as usize;

        let value_offset = mem::align_usize(backing_memory_size, ty_align);
        add_ref_fields(vm, &mut references, value_offset as i32, ty);
        offsets.push((initialized_offset, value_offset));
        backing_memory_size = value_offset + ty_size as usize;
    }

    let size = mem::page_align(backing_memory_size);
    let start = if backing_memory_size > 0 {
        os::commit(size, false)
    } else {
        Address::null()
    };
    let mut variables = Vec::with_capacity(number_globals);

    for global in offsets {
        let (initialized_offset, value_offset) = global;

        variables.push(GlobalVariableLocation {
            address_init: start.offset(initialized_offset),
            address_value: start.offset(value_offset),
        });
    }

    vm.global_variable_memory = Some(GlobalVariableMemory {
        region: start.region_start(size),
        variables,
        references,
    });
}

pub struct GlobalVariableMemory {
    region: Region,
    variables: Vec<GlobalVariableLocation>,
    references: Vec<i32>,
}

impl GlobalVariableMemory {
    pub fn address_value(&self, idx: GlobalId) -> Address {
        self.variables[idx.0 as usize].address_value
    }

    pub fn address_init(&self, idx: GlobalId) -> Address {
        self.variables[idx.0 as usize].address_init
    }

    pub fn is_initialized(&self, idx: GlobalId) -> bool {
        unsafe { *self.address_init(idx).to_ptr::<bool>() }
    }

    pub fn start(&self) -> Address {
        self.region.start()
    }

    pub fn references(&self) -> &[i32] {
        &self.references
    }
}

impl Drop for GlobalVariableMemory {
    fn drop(&mut self) {
        if self.region.start().is_non_null() {
            os::free(self.region.start(), self.region.size());
        }
    }
}

pub struct GlobalVariableLocation {
    address_init: Address,
    address_value: Address,
}
