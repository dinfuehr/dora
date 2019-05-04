use gc::root::Slot;
use gc::{Address, Region};

pub fn start(rootset: &[Slot], heap: Region, perm: Region) {
    let mut marking_stack: Vec<Address> = Vec::new();

    for root in rootset {
        let root_ptr = root.get();

        if heap.contains(root_ptr) {
            let root_obj = root_ptr.to_mut_obj();

            if !root_obj.header().is_marked_non_atomic() {
                marking_stack.push(root_ptr);
                root_obj.header_mut().mark_non_atomic();
            }
        } else {
            debug_assert!(root_ptr.is_null() || perm.contains(root_ptr));
        }
    }

    while marking_stack.len() > 0 {
        let object_addr = marking_stack.pop().expect("stack already empty");
        let object = object_addr.to_mut_obj();

        object.visit_reference_fields(|field| {
            let field_addr = field.get();

            if heap.contains(field_addr) {
                let field_obj = field_addr.to_mut_obj();

                if !field_obj.header().is_marked_non_atomic() {
                    marking_stack.push(field_addr);
                    field_obj.header_mut().mark_non_atomic();
                }
            } else {
                debug_assert!(field_addr.is_null() || perm.contains(field_addr));
            }
        });
    }
}
