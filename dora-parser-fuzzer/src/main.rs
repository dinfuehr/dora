#[macro_use]
extern crate afl;
extern crate dora_parser;

use std::sync::Arc;

use dora_parser::interner::Interner;
use dora_parser::Parser;

fn main() {
    fuzz!(|data: &[u8]| {
        if let Ok(code) = std::str::from_utf8(data) {
            let mut interner = Interner::new();
            let code = Arc::new(code.to_string());
            let parser = Parser::from_shared_string(code, &mut interner);
            let _ = parser.parse();
        }
    });
}
