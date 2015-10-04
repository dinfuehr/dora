use std::process::exit;

extern "C" {
    fn assert(val: bool) {
        if !val {
            exit(1);
        }
    }
}
