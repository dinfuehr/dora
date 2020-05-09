use dora_parser::ast::Ast;
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
    let mut ast = Ast::new();
    let args: Args = Default::default();
    let empty = Ast::new();
    let mut vm = VM::new(args, &empty);

    parse_bundled_stdlib(&mut vm, &mut ast).expect("failed parsing stdlib");

    {
        let filename = "<<code>>";
        let reader = Reader::from_string(filename, code);
        let parser = Parser::new(reader, &vm.id_generator, &mut ast, &mut vm.interner);
        match parser.parse() {
            Ok(file) => {
                vm.files.push(file);
                assert_eq!(ast.files.len(), vm.files.len());
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

    vm.ast = &ast;

    semck::check(&mut vm);

    f(&vm)
}
