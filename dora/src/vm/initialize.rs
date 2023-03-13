use dora_frontend::bytecode::FunctionId;

use crate::object::Header;
use crate::size::InstanceSize;
use crate::vm::functions::display_fct;
use crate::vm::{create_class_instance_with_vtable, setup_stubs, stdlib, ShapeKind, VM};

pub(super) fn setup(vm: &mut VM) {
    stdlib::resolve_internal_functions(vm);
    check_unresolved_functions(vm);
    create_special_classes(vm);
    setup_stubs(vm);
}

fn check_unresolved_functions(vm: &VM) {
    for fct in vm.fcts.iter() {
        let fct = fct.read();

        if fct.internal && !fct.internal_resolved && !fct.has_body() {
            let fct_id = FunctionId(fct.id().0 as u32);
            eprintln!("function {}", display_fct(vm, fct_id));
            panic!("found function without implementation");
        }
    }
}

fn create_special_classes(vm: &mut VM) {
    let free_object_class_id = create_class_instance_with_vtable(
        vm,
        ShapeKind::Builtin,
        InstanceSize::Fixed(Header::size()),
        Vec::new(),
        0,
    );
    vm.known_instances.free_object_class_instance = Some(free_object_class_id);

    let free_array_class_id = create_class_instance_with_vtable(
        vm,
        ShapeKind::Builtin,
        InstanceSize::FreeArray,
        Vec::new(),
        0,
    );
    vm.known_instances.free_array_class_instance = Some(free_array_class_id);

    let code_class_id = create_class_instance_with_vtable(
        vm,
        ShapeKind::Builtin,
        InstanceSize::CodeObject,
        Vec::new(),
        0,
    );
    vm.known_instances.code_class_instance = Some(code_class_id);
}
