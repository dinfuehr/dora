use ctxt::Context;
use driver::cmd::Args;
use ast;
use parser::Parser;
use semck;

pub fn parse<F, T>(code: &'static str, f: F) -> T where F: FnOnce(&Context) -> T {
    parse_with_errors(code, |ctxt| {
        assert!(!ctxt.diag.borrow().has_errors());

        f(ctxt)
    })
}

pub fn parse_with_errors<F, T>(code: &'static str, f: F) -> T where F: FnOnce(&Context) -> T {
    let mut parser = Parser::from_str(code);
    let (ast, interner) = parser.parse().unwrap();
    let map = ast::map::build(&ast, &interner);
    let args : Args = Default::default();

    ast::dump::dump(&ast, &interner);

    let mut ctxt = Context::new(args, &interner, &map, &ast);

    semck::check(&mut ctxt);

    f(&ctxt)
}
