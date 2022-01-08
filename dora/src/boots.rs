use std::mem;
use std::ptr;

use crate::boots::serializer::{
    allocate_encoded_bytecode_function, allocate_encoded_compilation_info,
};
use crate::bytecode::{self, InstructionSet};
use crate::compiler::codegen::should_emit_bytecode;
use crate::gc::Address;
use crate::handle::handle;
use crate::language::sym::NestedSymTable;
use crate::object::{Obj, Ref, UInt8Array};
use crate::threads::current_thread;
use crate::ty::SourceTypeArray;
use crate::vm::{Code, CodeDescriptor, FctDefinition, VM};

mod serializer;

pub fn compile(vm: &VM, fct: &FctDefinition, type_params: &SourceTypeArray) -> Code {
    let bytecode_fct = fct.bytecode.as_ref().expect("bytecode missing");

    if should_emit_bytecode(vm, fct) {
        bytecode::dump(vm, Some(fct), &bytecode_fct);
    }

    let compile_name = vm.interner.intern("compile");
    let compile_fct_id = NestedSymTable::new(vm, vm.boots_namespace_id)
        .get_fct(compile_name)
        .expect("compile()-method missing");
    let compile_address = vm.ensure_compiled(compile_fct_id);

    let encoded_compilation_info = handle(allocate_encoded_compilation_info(
        vm,
        bytecode_fct,
        type_params,
        get_architecture(),
    ));

    let tld_address = current_thread().tld_address();

    let dora_stub_address = vm.dora_stub();
    let compile_fct_ptr: extern "C" fn(Address, Address, Address) -> Ref<UInt8Array> =
        unsafe { mem::transmute(dora_stub_address) };

    let machine_code = handle(compile_fct_ptr(
        tld_address,
        compile_address,
        encoded_compilation_info.direct_ptr(),
    ));
    let mut machine_code_array = vec![0; machine_code.len()];

    unsafe {
        ptr::copy_nonoverlapping(
            machine_code.data() as *mut u8,
            machine_code_array.as_mut_ptr(),
            machine_code.len(),
        );
    }

    Code::from_optimized_buffer(vm, &machine_code_array, CodeDescriptor::DoraFct(fct.id))
}

pub fn encode_test(vm: &VM, fct: &FctDefinition, type_params: &SourceTypeArray) {
    let bytecode_fct = fct.bytecode.as_ref().expect("bytecode missing");
    let _encoded_compilation_info = handle(allocate_encoded_compilation_info(
        vm,
        bytecode_fct,
        type_params,
        get_architecture(),
    ));
}

fn get_architecture() -> InstructionSet {
    if cfg!(target_arch = "x86_64") {
        InstructionSet::X64
    } else if cfg!(target_arch = "aarch64") {
        InstructionSet::Arm64
    } else {
        panic!("unsupported architecture")
    }
}

pub fn get_encoded_bytecode_function_by_name(vm: &VM, name: &str) -> Ref<Obj> {
    let fct_name = vm.interner.intern(name);
    let bc_fct_id = NestedSymTable::new(vm, vm.boots_namespace_id)
        .get_fct(fct_name)
        .expect("method not found");

    let fct = vm.fcts.idx(bc_fct_id);
    let fct = fct.read();

    let bytecode_fct = fct.bytecode.as_ref().expect("bytecode missing");

    if should_emit_bytecode(vm, &*fct) {
        bytecode::dump(vm, Some(&*fct), bytecode_fct);
    }

    allocate_encoded_bytecode_function(vm, bytecode_fct)
}
