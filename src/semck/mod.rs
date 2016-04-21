use ast::Type;
use ast::Type::TypeBasic;
use ctxt::Context;
use error::msg::Msg;
use ty::BuiltinType;

mod clsdefck;
mod fctdefck;
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

pub fn check<'ast>(ctxt: &mut Context<'ast>) {
    // add builtin fcts and types to ctxt
    prelude::init(ctxt);

    // add user defined fcts and classes to ctxt
    // this check does not look into fct or class bodies
    globaldef::check(ctxt);
    return_on_error!(ctxt);

    // checks class definitions/bodies
    clsdefck::check(ctxt);
    return_on_error!(ctxt);

    // check names/identifiers of local variables
    // and their usage (variable def/use, function calls) in function bodies
    nameck::check(ctxt);
    return_on_error!(ctxt);

    // check type definitions of params,
    // return types and local variables in functions
    fctdefck::check(ctxt);
    return_on_error!(ctxt);

    // check types of expressions in functions
    typeck::check(ctxt);
    return_on_error!(ctxt);

    // are break and continue used in the right places?
    flowck::check(ctxt);

    // checks if function has a return value
    returnck::check(ctxt);
}

pub fn read_type<'ast>(ctxt: &Context<'ast>, t: &'ast Type) -> BuiltinType {
    match *t {
        TypeBasic(ref basic) => {
            if let Some(cls_id) = ctxt.sym.borrow().get_class(basic.name) {
                let cls = ctxt.cls_by_id(cls_id);

                return cls.ty;

            } else {
                let msg = Msg::UnknownType(basic.name);
                ctxt.diag.borrow_mut().report(basic.pos, msg);
            }
        }

        _ => ctxt.diag.borrow_mut().report_unimplemented(t.pos())
    }

    BuiltinType::Unit
}

#[cfg(test)]
mod tests {
    use ctxt::Context;
    use error::msg::Msg;
    use lexer::position::Position;
    use test;

    pub fn ok(code: &'static str) {
        test::parse_with_errors(code, |ctxt| {
            let diag = ctxt.diag.borrow();
            let errors = diag.errors();

            println!("errors = {:?}", errors);

            for e in errors {
                println!("{}", e.message(ctxt));
            }

            assert!(!ctxt.diag.borrow().has_errors());
        });
    }

    pub fn ok_with_test<F, R>(code: &'static str, f: F) -> R where F: FnOnce(&Context) -> R {
        test::parse_with_errors(code, |ctxt| {
            let diag = ctxt.diag.borrow();
            let errors = diag.errors();

            println!("errors = {:?}", errors);

            for e in errors {
                println!("{}", e.message(ctxt));
            }

            assert!(!ctxt.diag.borrow().has_errors());

            f(ctxt)
        })
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
