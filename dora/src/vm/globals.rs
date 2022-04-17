use crate::gc::Region;
use crate::language::ty::SourceType;
use crate::mem;
use crate::os;
use crate::vm::VM;

pub fn init_global_addresses(vm: &VM) {
    let mut size = 0;
    let mut offsets = Vec::with_capacity(vm.globals.len());

    for glob in vm.globals.iter() {
        let glob = glob.read();

        let initialized_offset = size;
        size += SourceType::Bool.size(vm) as usize;

        let ty_size = glob.ty.size(vm) as usize;
        let ty_align = glob.ty.align(vm) as usize;

        let value_offset = mem::align_usize(size, ty_align);
        offsets.push((initialized_offset, value_offset));
        size = value_offset + ty_size as usize;
    }

    if size == 0 {
        return;
    }

    let size = mem::page_align(size);
    let start = os::commit(size, false);

    for (ind, glob) in vm.globals.iter().enumerate() {
        let mut glob = glob.write();
        let (initialized_offset, value_offset) = offsets[ind];

        glob.address_init = start.offset(initialized_offset);
        glob.address_value = start.offset(value_offset);
    }

    let mut global_variable_memory = vm.global_variable_memory.lock();
    *global_variable_memory = Some(GlobalVariableMemory {
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
