use execstate::ExecState;

use os::signal::Trap;

pub fn read(es: &ExecState) -> Option<Trap> {
    let v1;
    let v2;

    unsafe {
        let mut ptr: *const u32 = es.pc as *const u32;

        v1 = *ptr;
        ptr = ptr.offset(1);
        v2 = *ptr;
    }

    if v1 == 0x25148b4c {
        Trap::from(v2)
    } else {
        None
    }
}
