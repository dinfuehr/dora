use ctxt::Context;

mod flowck;
mod globaldef;
mod nameck;
mod prelude;
mod typeck;
mod returnck;

macro_rules! return_on_error {
    ($ctxt: ident) => {{
        if $ctxt.diag.borrow().has_errors() { return; }
    }};
}

pub fn check<'a>(ctxt: &Context<'a, 'a>) {
    prelude::init(ctxt);

    globaldef::check(ctxt, ctxt.ast);
    nameck::check(ctxt, ctxt.ast);
    return_on_error!(ctxt);

    typeck::check(ctxt, ctxt.ast);
    return_on_error!(ctxt);

    flowck::check(ctxt, ctxt.ast);
    returnck::check(ctxt, ctxt.ast);
}

#[cfg(test)]
mod tests {
    use error::msg::Msg;
    use lexer::position::Position;
    use test;

    pub fn ok(code: &'static str) {
        test::parse_with_errors(code, |ctxt| {
            let diag = ctxt.diag.borrow();
            let errors = diag.errors();

            println!("errors = {:?}", errors);
            assert!(!ctxt.diag.borrow().has_errors());
        });
    }

    pub fn err(code: &'static str, pos: Position, msg: Msg) {
        test::parse_with_errors(code, |ctxt| {
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
