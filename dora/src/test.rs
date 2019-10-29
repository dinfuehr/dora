use dora_parser::ast::Ast;
use dora_parser::lexer::reader::Reader;
use dora_parser::parser::Parser;

use crate::driver::cmd::Args;
use crate::os;
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
    os::mem::init_page_size();

    let mut ast = Ast::new();
    let args: Args = Default::default();
    let empty = Ast::new();
    let mut vm = VM::new(args, &empty);

    for file in &[
        "stdlib/Identity.dora",
        "stdlib/Equals.dora",
        "stdlib/Bool.dora",
        "stdlib/Byte.dora",
        "stdlib/Char.dora",
        "stdlib/Int.dora",
        "stdlib/Long.dora",
        "stdlib/Float.dora",
        "stdlib/Double.dora",
        "stdlib/Array.dora",
        "stdlib/String.dora",
        "stdlib/Stringable.dora",
        "stdlib/StringBuffer.dora",
        "stdlib/Throwable.dora",
        "stdlib/Error.dora",
        "stdlib/Exception.dora",
        "stdlib/Thread.dora",
        "stdlib/Comparable.dora",
        "stdlib/Sortable.dora",
        "stdlib/Hash.dora",
        "stdlib/Default.dora",
        "stdlib/prelude.dora",
        "stdlib/Testing.dora",
    ] {
        let reader = Reader::from_file(file).unwrap();
        let parser = Parser::new(reader, &vm.id_generator, &mut ast, &mut vm.interner);
        parser.parse().unwrap();
    }

    {
        let reader = Reader::from_string(code);
        let parser = Parser::new(reader, &vm.id_generator, &mut ast, &mut vm.interner);
        parser.parse().unwrap();
    }

    vm.ast = &ast;

    semck::check(&mut vm);

    f(&vm)
}
