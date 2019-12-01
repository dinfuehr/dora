use crate::gc::Address;
use crate::threads::THREAD;

pub fn exception_get_and_clear() -> Address {
    THREAD.with(|thread| {
        let thread = thread.borrow();
        let tld = &thread.tld;
        let object = tld.exception_object();
        tld.set_exception_object(Address::null());
        object
    })
}

pub fn exception_set(val: Address) {
    THREAD.with(|thread| {
        thread.borrow().tld.set_exception_object(val);
    });
}
