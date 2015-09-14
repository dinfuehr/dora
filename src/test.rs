use parser::ast::ctxt::Context;
use driver::cmd::Args;
use parser::ast;
use parser::Parser;
use semck;

pub fn parse<F>(code: &'static str, f: F) where F: FnOnce(&Context) -> () {
    parse_with_errors(code, |ctxt| {
        assert!(!ctxt.diag.borrow().has_errors());
        f(ctxt);
    });
}

pub fn parse_with_errors<F>(code: &'static str, f: F) where F: FnOnce(&Context) -> () {
    let mut parser = Parser::from_str(code);
    let (ast, interner) = parser.parse().unwrap();
    let map = ast::map::build(&ast, &interner);
    let args : Args = Default::default();

    ast::dump::dump(&ast, &interner);

    let ctxt = Context::new(&args, &interner, &map, &ast);

    semck::check(&ctxt);

    f(&ctxt);
}
