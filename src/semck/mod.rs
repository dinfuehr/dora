use driver::ctxt::Context;

mod prelude;
mod typeck;
mod nameck;

pub fn check(ctxt: &Context) {
    prelude::init(ctxt);

    nameck::check(ctxt, ctxt.ast);
    typeck::check(ctxt, ctxt.ast);
}

#[cfg(test)]
mod tests {
    use driver::ctxt::Context;
    use driver::cmd::Args;
    use error::msg::Msg;
    use parser::ast;
    use parser::lexer::position::Position;
    use parser::Parser;
    use semck;

    pub fn check<F>(code: &'static str, f: F) where F: FnOnce(&Context) -> () {
        let mut parser = Parser::from_str(code);
        let (ast, interner) = parser.parse().unwrap();
        let map = ast::map::build(&ast, &interner);
        let args : Args = Default::default();

        ast::dump::dump(&ast, &interner);

        let ctxt = Context::new(&args, &interner, &map, &ast);

        semck::check(&ctxt);

        f(&ctxt);
    }

    pub fn ok(code: &'static str) {
        check(code, |ctxt| {
            let diag = ctxt.diag.borrow();
            let errors = diag.errors();

            println!("errors = {:?}", errors);
            assert!(!ctxt.diag.borrow().has_errors());
        });
    }

    pub fn err(code: &'static str, pos: Position, msg: Msg) {
        check(code, |ctxt| {
            let diag = ctxt.diag.borrow();
            let errors = diag.errors();

            println!("errors = {:?}", errors);

            assert_eq!(1, errors.len());
            assert_eq!(pos, errors[0].pos);
            assert_eq!(msg, errors[0].msg);
        });
    }

    pub fn pos(line: u32, col: u32) -> Position {
        Position::new(line, col)
    }
}
