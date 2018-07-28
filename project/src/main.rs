#![feature(type_ascription)]
#[macro_use]extern crate dora;

use dora::start;

use std::process::exit;

use std::path::Path;
use dora::ctxt::SemContext;
use dora::native_fct;
use dora::types::int;
use dora::object::{Handle,Str};

fn myfuns(ctxt: &mut SemContext) -> i32 {
    
    dora_ffi! (
        ctxt;

        fun square(x: int) -> int {
            return 2 * x;
        }

        fun triple(x: int) -> int {
            return 3 * x;
        }
    );
    return 1;
}

fn main() {

exit(start(Path::new("src/main.dora"),Some(myfuns as *const u8)));

}
