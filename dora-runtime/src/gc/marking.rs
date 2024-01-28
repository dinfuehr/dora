use crate::gc::root::Slot;
use crate::gc::{Address, Region};
use crate::vm::VM;

pub fn start(vm: &VM, rootset: &[Slot], heap: Region, perm: Region) {
    let mut marking_stack: Vec<Address> = Vec::new();

    for root in rootset {
        let object = root.get();

        if heap.contains(object) {
            let root_obj = object.to_obj();

            if root_obj.header().try_mark() {
                marking_stack.push(object);
            }
        } else {
            debug_assert!(object.is_null() || perm.contains(object));
        }
    }

    while marking_stack.len() > 0 {
        let object_addr = marking_stack.pop().expect("stack already empty");
        let object = object_addr.to_obj();

        object.visit_reference_fields(vm.meta_space_start(), |field| {
            let field_addr = field.get();

            if heap.contains(field_addr) {
                let field_obj = field_addr.to_obj();

                if field_obj.header().try_mark() {
                    marking_stack.push(field_addr);
                }
            } else {
                debug_assert!(field_addr.is_null() || perm.contains(field_addr));
            }
        });
    }
}
