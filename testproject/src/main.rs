extern crate dora;

use dora::ctxt::*;
use dora::native_fct;
use dora::start;
use std::path::Path;
use std::process::exit;
fn main() {
    exit(start(
        Path::new("src/main.dora"),
        Some(reg_fct as *const u8),
    ));
}

pub fn reg_fct(ctxt: &mut SemContext) -> i32 {
    native_fct(ctxt, "myfunc", my_funcs::myfunc as *const u8);
    return 1;
}

pub mod my_funcs {
    pub extern "C" fn myfunc() {
        println!("Called rust function!");
    }
}
