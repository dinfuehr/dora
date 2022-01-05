use std::sync::Arc;

use dora_parser::lexer::reader::Reader;
use dora_parser::parser::Parser;

use crate::driver::cmd::Args;
use crate::semck;
use crate::vm::SemAnalysis;

pub fn parse<F, T>(code: &'static str, f: F) -> T
where
    F: FnOnce(&SemAnalysis) -> T,
{
    parse_with_errors(code, |sa| {
        if sa.diag.lock().has_errors() {
            sa.diag.lock().dump(sa);
            println!("{}", code);
            panic!("unexpected error in test::parse()");
        }

        f(sa)
    })
}

pub fn parse_with_errors<F, T>(code: &'static str, f: F) -> T
where
    F: FnOnce(&SemAnalysis) -> T,
{
    let args: Args = Default::default();
    let mut sa = SemAnalysis::new(args);
    sa.parse_arg_file = false;

    {
        let filename = "<<code>>";
        let reader = Reader::from_string(filename, code);
        let parser = Parser::new(reader, &sa.id_generator, &mut sa.interner);
        match parser.parse() {
            Ok(ast) => {
                sa.add_file(None, sa.global_namespace_id, Arc::new(ast));
            }

            Err(error) => {
                println!(
                    "error in {} at {}: {}",
                    filename,
                    error.pos,
                    error.error.message()
                );
                panic!("error during parsing.");
            }
        }
    }

    assert!(semck::check(&mut sa));

    f(&sa)
}
