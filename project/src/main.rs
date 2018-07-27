#[macro_use]extern crate dora;

use dora::start;

use std::process::exit;

use std::path::Path;
use dora::ctxt::SemContext;
use dora::native_fct;


fn myfuns(ctxt: &mut SemContext) -> i32 {
    dora_fn!{ctxt; fn test() -> i32 {
        println!("hello world!");
        return 12 + 2;
    }};
    return 1;
}

fn main() {

exit(start(Path::new("src/main.dora"),Some(myfuns as *const u8)));

}
