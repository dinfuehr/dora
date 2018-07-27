use ctxt::SemContext;
use driver::cmd::Args;
use dora_parser::ast::Ast;
use dora_parser::interner::Interner;
use dora_parser::lexer::reader::Reader;
use os;
use dora_parser::parser::{NodeIdGenerator, Parser};
use semck;

pub fn parse<F, T>(code: &'static str, f: F) -> T
where
    F: FnOnce(&SemContext) -> T,
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
where
    F: FnOnce(&SemContext) -> T,
{
    os::mem::init_page_size();

    let id_generator = NodeIdGenerator::new();
    let mut interner = Interner::new();
    let mut ast = Ast::new();
    let args: Args = Default::default();

    for file in &["stdlib/prelude.dora", "stdlib/str.dora", "stdlib/test.dora"] {
        let reader = Reader::from_file(file).unwrap();
        let mut parser = Parser::new(reader, &id_generator, &mut ast, &mut interner);
        parser.parse().unwrap()
    }

    {
        let reader = Reader::from_string(code);
        let mut parser = Parser::new(reader, &id_generator, &mut ast, &mut interner);
        parser.parse().unwrap()
    }

    let mut ctxt = SemContext::new(args, &ast, interner);

    semck::check(&mut ctxt,None);

    f(&ctxt)
}
