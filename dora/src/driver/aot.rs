use crate::gc::Address;
use crate::threads::{
    current_thread, deinit_current_thread, init_current_thread, DoraThread, ThreadState, STACK_SIZE,
};
use crate::vm::{set_vm, stack_pointer, VM};

pub fn start() -> i32 {
    let vm = VM::new(Default::default());
    set_vm(&vm);

    execute_main(&vm);

    vm.threads.join_all();

    0
}

extern "C" {
    fn dora_entry_stub(tld: Address, dora_fct: Address);
    fn dora_uf_main();
}

fn execute_main(vm: &VM) {
    let thread = DoraThread::new(vm, ThreadState::Running);
    init_current_thread(thread.clone());
    vm.threads.attach_thread(thread);

    let stack_top = stack_pointer();
    let stack_limit = stack_top.sub(STACK_SIZE);

    let thread = current_thread();
    thread.tld.set_stack_limit(stack_limit);

    let tld = thread.tld_address();
    unsafe {
        let main_ptr = Address::from_ptr(dora_uf_main as *const ());
        dora_entry_stub(tld, main_ptr);
    }

    vm.threads.detach_current_thread();
    deinit_current_thread();
}
