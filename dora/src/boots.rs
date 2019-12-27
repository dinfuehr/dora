use std::mem;

use crate::bytecode;
use crate::compiler::codegen::should_emit_bytecode;
use crate::compiler::fct::JitFct;
use crate::gc::Address;
use crate::object::{byte_array_from_buffer, ByteArray, Ref};
use crate::threads::THREAD;
use crate::ty::TypeList;
use crate::vm::{Fct, FctSrc, VM};

pub fn compile<'a, 'ast: 'a>(
    vm: &'a VM<'ast>,
    fct: &Fct<'ast>,
    src: &'a FctSrc,
    cls_type_params: &TypeList,
    fct_type_params: &TypeList,
) -> JitFct {
    let bytecode_fct = bytecode::generate(vm, fct, src, cls_type_params, fct_type_params);

    if should_emit_bytecode(vm, fct) {
        bytecode_fct.dump();
    }

    let compile_fct = vm.fct_by_name("compile").expect("compile()-method missing");

    let bytecode_array = byte_array_from_buffer(vm, bytecode_fct.data());

    let tld = THREAD.with(|thread| {
        let thread = thread.borrow();
        let ptr = &thread.tld;

        Address::from_ptr(ptr as *const _)
    });
    let ptr = vm.ensure_compiled(compile_fct);
    let dora_stub_address = vm.dora_stub();
    let fct: extern "C" fn(Address, Address, Ref<ByteArray>) =
        unsafe { mem::transmute(dora_stub_address) };
    fct(tld, ptr, bytecode_array);

    unimplemented!()
}
