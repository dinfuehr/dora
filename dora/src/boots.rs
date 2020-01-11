use std::mem;
use std::ptr;

use crate::bytecode;
use crate::compiler::codegen::should_emit_bytecode;
use crate::compiler::fct::{JitBaselineFct, JitDescriptor, JitFct};
use crate::gc::Address;
use crate::handle::root;
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
        bytecode::dump(&bytecode_fct);
    }

    let compile_fct = vm.fct_by_name("compile").expect("compile()-method missing");

    let bytecode_array = root(byte_array_from_buffer(vm, bytecode_fct.code()));

    let tld = THREAD.with(|thread| {
        let thread = thread.borrow();
        let ptr = &thread.tld;

        Address::from_ptr(ptr as *const _)
    });
    let ptr = vm.ensure_compiled(compile_fct);
    let dora_stub_address = vm.dora_stub();
    let fctptr: extern "C" fn(Address, Address, Ref<ByteArray>, i32) -> Ref<ByteArray> =
        unsafe { mem::transmute(dora_stub_address) };
    let machine_code = root(fctptr(
        tld,
        ptr,
        bytecode_array.direct(),
        bytecode_fct.arguments() as i32,
    ));
    let mut machine_code_array = vec![0; machine_code.len()];

    unsafe {
        ptr::copy_nonoverlapping(
            machine_code.data() as *mut u8,
            machine_code_array.as_mut_ptr(),
            machine_code.len(),
        );
    }

    let jit_fct = JitBaselineFct::from_optimized_buffer(
        vm,
        &machine_code_array,
        JitDescriptor::DoraFct(fct.id),
        fct.throws,
    );

    JitFct::Base(jit_fct)
}
