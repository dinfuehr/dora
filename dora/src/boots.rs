use std::mem;
use std::ptr;

use crate::boots::serializer::allocate_compilation_info;
use crate::bytecode;
use crate::compiler::codegen::should_emit_bytecode;
use crate::compiler::fct::{Code, JitDescriptor};
use crate::gc::Address;
use crate::handle::handle;
use crate::object::{Obj, Ref, UInt8Array};
use crate::sym::NestedSymTable;
use crate::threads::current_thread;
use crate::ty::SourceTypeArray;
use crate::vm::{Fct, VM};

mod serializer;

pub fn compile(vm: &VM, fct: &Fct, type_params: &SourceTypeArray) -> Code {
    let bytecode_fct = fct.bytecode.as_ref().expect("bytecode missing");

    if should_emit_bytecode(vm, fct) {
        bytecode::dump(vm, Some(fct), &bytecode_fct);
    }

    let compile_name = vm.interner.intern("compile");
    let compile_fct_id = NestedSymTable::new(vm, vm.boots_namespace_id)
        .get_fct(compile_name)
        .expect("compile()-method missing");
    let compile_address = vm.ensure_compiled(compile_fct_id);

    let encoded_compilation_info = handle(allocate_compilation_info(vm, bytecode_fct, type_params));

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

    Code::from_optimized_buffer(vm, &machine_code_array, JitDescriptor::DoraFct(fct.id))
}

pub fn encode_test(vm: &VM, fct: &Fct, type_params: &SourceTypeArray) {
    let bytecode_fct = fct.bytecode.as_ref().expect("bytecode missing");
    let _encoded_compilation_info =
        handle(allocate_compilation_info(vm, bytecode_fct, type_params));

    // println!("before getting decode in rust");

    // let decode_name = vm.interner.intern("decode");
    // let decode_fct_id = NestedSymTable::new(vm, vm.boots_namespace_id)
    //     .get_fct(decode_name)
    //     .expect("decode()-method missing");
    // let decode_address = vm.ensure_compiled(decode_fct_id);

    // let tld_address = current_thread().tld_address();
    // let dora_stub_address = vm.dora_stub();

    // let decode_fct_ptr: extern "C" fn(Address, Address, Address) =
    //     unsafe { mem::transmute(dora_stub_address) };

    // vm.code_map.lock().dump(vm);

    // decode_fct_ptr(
    //     tld_address,
    //     decode_address,
    //     encoded_compilation_info.direct_ptr(),
    // );

    // println!("after decode in rust");
}

pub fn bytecode(vm: &VM, name: &str) -> Ref<Obj> {
    let fct_name = vm.interner.intern(name);
    let bc_fct_id = NestedSymTable::new(vm, vm.boots_namespace_id)
        .get_fct(fct_name)
        .expect("compile()-method missing");

    let fct = vm.fcts.idx(bc_fct_id);
    let fct = fct.read();

    let bytecode_fct = fct.bytecode.as_ref().expect("bytecode missing");

    if should_emit_bytecode(vm, &*fct) {
        bytecode::dump(vm, Some(&*fct), bytecode_fct);
    }

    allocate_compilation_info(vm, bytecode_fct, &SourceTypeArray::empty())
}
