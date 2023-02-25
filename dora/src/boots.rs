use std::mem;
use std::ptr;

use crate::boots::serializer::allocate_encoded_compilation_info;
use crate::bytecode::{BytecodeTypeArray, InstructionSet};
use crate::gc::Address;
use crate::handle::handle;
use crate::language::sem_analysis::FctDefinition;
use crate::masm::CodeDescriptor;
use crate::object::{Ref, UInt8Array};
use crate::threads::current_thread;
use crate::vm::VM;

mod serializer;

pub fn compile(vm: &VM, fct: &FctDefinition, type_params: &BytecodeTypeArray) -> CodeDescriptor {
    let bytecode_fct = fct.bytecode.as_ref().expect("bytecode missing");

    let compile_fct_id = vm.known.functions.compile();
    let compile_address = vm.ensure_compiled(compile_fct_id);

    let encoded_compilation_info = handle(allocate_encoded_compilation_info(
        vm,
        bytecode_fct,
        type_params,
        get_architecture(),
    ));

    let tld_address = current_thread().tld_address();

    let dora_stub_address = vm.stubs.dora_entry();
    let compile_fct_ptr: extern "C" fn(Address, Address, Address) -> Ref<UInt8Array> =
        unsafe { mem::transmute(dora_stub_address) };

    let machine_code = handle(compile_fct_ptr(
        tld_address,
        compile_address,
        encoded_compilation_info.direct_ptr(),
    ));
    let mut code = vec![0; machine_code.len()];

    unsafe {
        ptr::copy_nonoverlapping(
            machine_code.data() as *mut u8,
            code.as_mut_ptr(),
            machine_code.len(),
        );
    }

    CodeDescriptor::from_buffer(code)
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
