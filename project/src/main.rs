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
    
    dora_ffi! (ctxt;
        fun test() -> int {
            println!("Hello world!");
            12 * 2
        }

        fun read_line_test() -> Handle<Str> {
            use std::io::stdin;
            let mut buffer = String::new();
            stdin().read_line(&mut buffer).unwrap();
            
            return Str::from_buffer2(buffer.as_bytes());
            
        }
    );
    return 1;
}

fn main() {

exit(start(Path::new("src/main.dora"),Some(myfuns as *const u8)));

}
