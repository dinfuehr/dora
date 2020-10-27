use std::sync::Arc;

use dora_parser::lexer::reader::Reader;
use dora_parser::parser::Parser;

use crate::driver::cmd::Args;
use crate::driver::start::parse_bundled_stdlib;
use crate::semck;
use crate::vm::VM;

pub fn parse<F, T>(code: &'static str, f: F) -> T
where
    F: FnOnce(&VM) -> T,
{
    parse_with_errors(code, |vm| {
        if vm.diag.lock().has_errors() {
            vm.diag.lock().dump(vm);
            println!("{}", code);
            panic!("unexpected error in test::parse()");
        }

        f(vm)
    })
}

pub fn parse_with_errors<F, T>(code: &'static str, f: F) -> T
where
    F: FnOnce(&VM) -> T,
{
    let args: Args = Default::default();
    let mut vm = VM::new(args);

    parse_bundled_stdlib(&mut vm).expect("failed parsing stdlib");

    {
        let filename = "<<code>>";
        let reader = Reader::from_string(filename, code);
        let parser = Parser::new(reader, &vm.id_generator, &mut vm.interner);
        match parser.parse() {
            Ok(file) => {
                vm.files.write().push(Arc::new(file));
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

    semck::check(&mut vm);

    f(&vm)
}
