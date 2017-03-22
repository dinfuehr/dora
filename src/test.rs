use ctxt::Context;
use driver::cmd::Args;
use ast::{self, Ast};
use interner::Interner;
use lexer::reader::Reader;
use os;
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
    os::mem::init_page_size();

    let mut interner = Interner::new();
    let mut ast = Ast::new();
    let args: Args = Default::default();

    {
        let reader = Reader::from_file("stdlib/prelude.dora").unwrap();
        let mut parser = Parser::new(reader, &mut ast, &mut interner);
        parser.parse().unwrap()
    }

    {
        let reader = Reader::from_string(code);
        let mut parser = Parser::new(reader, &mut ast, &mut interner);
        parser.parse().unwrap()
    }

    ast::dump::dump(&ast, &interner);

    let mut ctxt = Context::new(args, &ast, interner);

    semck::check(&mut ctxt);

    f(&ctxt)
}
