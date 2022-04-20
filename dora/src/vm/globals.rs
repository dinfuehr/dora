use crate::gc::Region;
use crate::language::ty::SourceType;
use crate::mem;
use crate::os;
use crate::vm::VM;

pub fn init_global_addresses(vm: &mut VM) {
    let mut size = 0;
    let mut offsets = Vec::with_capacity(vm.globals.len());

    for global_var in vm.globals.iter() {
        let global_var = global_var.read();

        let initialized_offset = size;
        size += SourceType::Bool.size(vm) as usize;

        let ty_size = global_var.ty.size(vm) as usize;
        let ty_align = global_var.ty.align(vm) as usize;

        let value_offset = mem::align_usize(size, ty_align);
        offsets.push((initialized_offset, value_offset));
        size = value_offset + ty_size as usize;
    }

    if size == 0 {
        return;
    }

    let size = mem::page_align(size);
    let start = os::commit(size, false);

    for (ind, global_var) in vm.globals.iter().enumerate() {
        let mut global_var = global_var.write();
        let (initialized_offset, value_offset) = offsets[ind];

        global_var.address_init = start.offset(initialized_offset);
        global_var.address_value = start.offset(value_offset);
    }

    vm.global_variable_memory = Some(GlobalVariableMemory {
        region: start.region_start(size),
    });
}

pub struct GlobalVariableMemory {
    region: Region,
}

impl Drop for GlobalVariableMemory {
    fn drop(&mut self) {
        os::free(self.region.start(), self.region.size());
    }
}
