use crate::cannon::codegen::{align, size};
use crate::gc::{Address, Region};
use crate::language::generator::bty_from_ty;
use crate::language::sem_analysis::GlobalDefinitionId;
use crate::mem;
use crate::os;
use crate::vm::VM;

pub fn init_global_addresses(vm: &mut VM) {
    let mut backing_memory_size = 0;
    let mut offsets = Vec::with_capacity(vm.globals.len());

    let initialized_field_size = 1;

    for global_var in vm.globals.iter() {
        let global_var = global_var.read();

        let initialized_offset = backing_memory_size;
        backing_memory_size += initialized_field_size;

        let ty = bty_from_ty(global_var.ty.clone());

        let ty_size = size(vm, ty.clone()) as usize;
        let ty_align = align(vm, ty) as usize;

        let value_offset = mem::align_usize(backing_memory_size, ty_align);
        offsets.push((initialized_offset, value_offset));
        backing_memory_size = value_offset + ty_size as usize;
    }

    if backing_memory_size == 0 {
        return;
    }

    let size = mem::page_align(backing_memory_size);
    let start = os::commit(size, false);
    let mut variables = Vec::with_capacity(vm.globals.len());

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
    });
}

pub struct GlobalVariableMemory {
    region: Region,
    variables: Vec<GlobalVariableLocation>,
}

impl GlobalVariableMemory {
    pub fn address_value(&self, idx: GlobalDefinitionId) -> Address {
        self.variables[idx.to_usize()].address_value
    }

    pub fn address_init(&self, idx: GlobalDefinitionId) -> Address {
        self.variables[idx.to_usize()].address_init
    }

    pub fn is_initialized(&self, idx: GlobalDefinitionId) -> bool {
        unsafe { *self.address_init(idx).to_ptr::<bool>() }
    }
}

impl Drop for GlobalVariableMemory {
    fn drop(&mut self) {
        os::free(self.region.start(), self.region.size());
    }
}

pub struct GlobalVariableLocation {
    address_init: Address,
    address_value: Address,
}
