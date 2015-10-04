use std::process::exit;

pub extern "C" fn assert(val: bool) {
    if !val {
        exit(1);
    }
}
