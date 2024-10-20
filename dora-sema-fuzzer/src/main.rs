#[macro_use]
extern crate afl;

use dora_frontend::sema::{Sema, SemaArgs};

fn main() {
    fuzz!(|data: &[u8]| {
        if let Ok(s) = std::str::from_utf8(data) {
            check_program(s.into());
        }
    });
}

fn check_program(program: String) {
    let sem_args = SemaArgs {
        arg_file: None,
        packages: Vec::new(),
        test_file_as_string: Some(program),
        include_boots: false,
    };

    let mut sa = Sema::new(sem_args);
    dora_frontend::check_program(&mut sa);
}
