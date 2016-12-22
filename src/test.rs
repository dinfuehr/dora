use ctxt::Context;
use driver::cmd::Args;
use ast::{self, Ast};
use interner::Interner;
use parser::Parser;
use semck;

pub fn parse<F, T>(code: &'static str, f: F) -> T
    where F: FnOnce(&Context) -> T
{
    parse_with_errors(code, |ctxt| {
        if ctxt.diag.borrow().has_errors() {
            ctxt.diag.borrow().dump();
            println!("{}", code);
            panic!("unexpected error in test::parse()");
        }

        f(ctxt)
    })
}

pub fn parse_with_errors<F, T>(code: &'static str, f: F) -> T
    where F: FnOnce(&Context) -> T
{
    let mut interner = Interner::new();
    let mut ast = Ast::new();
    let args: Args = Default::default();

    {
        let mut parser = Parser::from_file("stdlib/prelude.dora", &mut ast, &mut interner).unwrap();
        parser.parse().unwrap()
    }

    {
        let mut parser = Parser::from_str(code, &mut ast, &mut interner);
        parser.parse().unwrap()
    }

    ast::dump::dump(&ast, &interner);

    let mut ctxt = Context::new(args, &ast, interner);

    semck::check(&mut ctxt);

    f(&ctxt)
}
