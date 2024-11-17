#[macro_use]
extern crate afl;

use dora_frontend::sema::{FileContent, Sema, SemaFlags};

fn main() {
    fuzz!(|data: &[u8]| {
        if let Ok(s) = std::str::from_utf8(data) {
            check_program(s.into());
        }
    });
}

fn check_program(program: String) {
    let sem_args = SemaFlags {
        program_file: Some(FileContent::Content(program)),
        packages: Vec::new(),
        boots: false,
    };

    let mut sa = Sema::new(sem_args);
    dora_frontend::check_program(&mut sa);
}
