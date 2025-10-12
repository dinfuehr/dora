#[macro_use]
extern crate afl;

use dora_frontend::sema::{Sema, SemaCreationParams};

fn main() {
    fuzz!(|data: &[u8]| {
        if let Ok(s) = std::str::from_utf8(data) {
            check_program(s.into());
        }
    });
}

fn check_program(program: String) {
    let params = SemaCreationParams::new().set_program_content(program);
    let mut sa = Sema::new(params);
    dora_frontend::check_program(&mut sa);
}
