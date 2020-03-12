use std::mem;
use std::ptr;

use crate::bytecode::{self, BytecodeFunction};
use crate::compiler::codegen::should_emit_bytecode;
use crate::compiler::fct::{Code, JitDescriptor};
use crate::gc::Address;
use crate::handle::root;
use crate::object::{byte_array_from_buffer, int_array_alloc_heap, ByteArray, IntArray, Ref};
use crate::threads::THREAD;
use crate::ty::TypeList;
use crate::vm::{Fct, FctSrc, VM};

pub fn compile<'a, 'ast: 'a>(
    vm: &'a VM<'ast>,
    fct: &Fct<'ast>,
    src: &'a FctSrc,
    cls_type_params: &TypeList,
    fct_type_params: &TypeList,
) -> Code {
    let bytecode_fct = bytecode::generate(vm, fct, src, cls_type_params, fct_type_params);

    if should_emit_bytecode(vm, fct) {
        bytecode::dump(&bytecode_fct);
    }

    let compile_fct_id = vm.fct_by_name("compile").expect("compile()-method missing");
    let compile_fct = vm.ensure_compiled(compile_fct_id);

    let bytecode_array = root(byte_array_from_buffer(vm, bytecode_fct.code()));
    let _register_array = root(allocate_register_array(vm, &bytecode_fct));

    let tld_address = THREAD.with(|thread| {
        let thread = thread.borrow();
        let ptr = &thread.tld;

        Address::from_ptr(ptr as *const _)
    });

    let dora_stub_address = vm.dora_stub();
    let compile_fct_ptr: extern "C" fn(Address, Address, Ref<ByteArray>, i32) -> Ref<ByteArray> =
        unsafe { mem::transmute(dora_stub_address) };

    let machine_code = root(compile_fct_ptr(
        tld_address,
        compile_fct,
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

    Code::from_optimized_buffer(vm, &machine_code_array, JitDescriptor::DoraFct(fct.id))
}

fn allocate_register_array(vm: &VM, fct: &BytecodeFunction) -> Ref<IntArray> {
    let mut array = int_array_alloc_heap(vm, fct.registers().len());

    for (idx, &ty) in fct.registers().iter().enumerate() {
        array.set_at(idx, ty as u32 as i32);
    }

    array
}
