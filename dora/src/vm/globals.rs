use crate::language::ty::SourceType;
use crate::mem;
use crate::vm::VM;

pub fn init_global_addresses(vm: &VM) {
    let globals = vm.globals.lock();
    let mut size = 0;
    let mut offsets = Vec::with_capacity(globals.len());

    for glob in globals.iter() {
        let glob = glob.read();

        let initialized = size;
        size += SourceType::Bool.size(vm);

        let ty_size = glob.ty.size(vm);
        let ty_align = glob.ty.align(vm);

        let value = mem::align_i32(size, ty_align);
        offsets.push((initialized, value));
        size = value + ty_size;
    }

    let ptr = vm.gc.alloc_perm(size as usize);

    for (ind, glob) in globals.iter().enumerate() {
        let mut glob = glob.write();
        let (initialized, value) = offsets[ind];

        glob.address_init = ptr.offset(initialized as usize);
        glob.address_value = ptr.offset(value as usize);
    }
}
