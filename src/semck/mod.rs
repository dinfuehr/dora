use ast::{Stmt, Type};
use ast::Type::{TypeBasic, TypeSelf};
use ctxt::{Context, NodeMap};
use error::msg::Msg;
use sym::Sym::{SymClass, SymStruct};
use ty::BuiltinType;

mod clsdefck;
mod fctdefck;
mod flowck;
mod globaldef;
mod implck;
mod impldefck;
mod nameck;
mod prelude;
mod typeck;
mod returnck;
mod structdefck;
mod superck;
mod traitdefck;

macro_rules! return_on_error {
    ($ctxt: ident) => {{
        if $ctxt.diag.borrow().has_errors() { return; }
    }};
}

pub fn check<'ast>(ctxt: &mut Context<'ast>) {
    let mut map_cls_defs = NodeMap::new(); // get ClassId from ast node
    let mut map_struct_defs = NodeMap::new(); // get StructId from ast node
    let mut map_trait_defs = NodeMap::new(); // get TraitId from ast node
    let mut map_impl_defs = NodeMap::new(); // get ImplId from ast node
    let mut map_global_defs = NodeMap::new(); // get GlobalId from ast node

    // add user defined fcts and classes to ctxt
    // this check does not look into fct or class bodies
    globaldef::check(ctxt,
                     &mut map_cls_defs,
                     &mut map_struct_defs,
                     &mut map_trait_defs,
                     &mut map_impl_defs,
                     &mut map_global_defs);
    return_on_error!(ctxt);

    // define internal classes
    prelude::internal_classes(ctxt);

    // checks class/struct/trait definitions/bodies
    clsdefck::check(ctxt, &map_cls_defs);
    structdefck::check(ctxt, &map_struct_defs);
    traitdefck::check(ctxt, &map_trait_defs);
    impldefck::check(ctxt, &map_impl_defs);
    return_on_error!(ctxt);

    // check names/identifiers of local variables
    // and their usage (variable def/use, function calls) in function bodies
    nameck::check(ctxt);
    return_on_error!(ctxt);

    // check type definitions of params,
    // return types and local variables in functions
    fctdefck::check(ctxt, &map_global_defs);
    return_on_error!(ctxt);

    // add size of super classes to field offsets
    superck::check(ctxt);

    // check impl methods against trait definition
    implck::check(ctxt);
    return_on_error!(ctxt);

    // check types of expressions in functions
    typeck::check(ctxt);
    return_on_error!(ctxt);

    // are break and continue used in the right places?
    flowck::check(ctxt);

    // checks if function has a return value
    returnck::check(ctxt);

    // define internal functions
    prelude::internal_functions(ctxt);

    // check for internal functions or classes
    internalck(ctxt);
    return_on_error!(ctxt);
}

fn internalck<'ast>(ctxt: &Context<'ast>) {
    for fct in &ctxt.fcts {
        let fct = fct.borrow();

        if fct.internal && !fct.internal_resolved {
            ctxt.diag.borrow_mut().report(fct.pos, Msg::UnresolvedInternal);
        }

        if fct.kind.is_definition() && !fct.in_trait() {
            ctxt.diag.borrow_mut().report(fct.pos, Msg::MissingFctBody);
        }
    }

    for cls in &ctxt.classes {
        let cls = cls.borrow();

        if cls.internal && !cls.internal_resolved {
            ctxt.diag.borrow_mut().report(cls.pos, Msg::UnresolvedInternal);
        }

        for method in &cls.methods {
            let method = ctxt.fcts[*method].borrow();

            if method.internal && !method.internal_resolved {
                ctxt.diag.borrow_mut().report(method.pos, Msg::UnresolvedInternal);
            }

            if method.kind.is_definition() {
                ctxt.diag.borrow_mut().report(method.pos, Msg::MissingFctBody);
            }
        }
    }
}

pub fn read_type<'ast>(ctxt: &Context<'ast>, t: &'ast Type) -> Option<BuiltinType> {
    match *t {
        TypeSelf(_) => {
            return Some(BuiltinType::This);
        }

        TypeBasic(ref basic) => {
            if let Some(sym) = ctxt.sym.borrow().get(basic.name) {
                match sym {
                    SymClass(cls_id) => {
                        let cls = ctxt.classes[cls_id].borrow();
                        return Some(cls.ty);
                    }

                    SymStruct(struct_id) => {
                        return Some(BuiltinType::Struct(struct_id));
                    }

                    _ => {
                        let name = ctxt.interner.str(basic.name).to_string();
                        let msg = Msg::ExpectedType(name);
                        ctxt.diag.borrow_mut().report(basic.pos, msg);
                    }
                }

            } else {
                let name = ctxt.interner.str(basic.name).to_string();
                let msg = Msg::UnknownType(name);
                ctxt.diag.borrow_mut().report(basic.pos, msg);
            }
        }

        _ => ctxt.diag.borrow_mut().report_unimplemented(t.pos()),
    }

    None
}

pub fn always_returns(s: &Stmt) -> bool {
    match returnck::returns_value(s) {
        Ok(_) => true,
        Err(_) => false,
    }
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
                println!("{}", e.message());
            }

            assert!(!ctxt.diag.borrow().has_errors());
        });
    }

    pub fn ok_with_test<F, R>(code: &'static str, f: F) -> R
        where F: FnOnce(&Context) -> R
    {
        test::parse_with_errors(code, |ctxt| {
            let diag = ctxt.diag.borrow();
            let errors = diag.errors();

            println!("errors = {:?}", errors);

            for e in errors {
                println!("{}", e.message());
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

    pub fn errors(code: &'static str, vec: &[(Position, Msg)]) {
        test::parse_with_errors(code, |ctxt| {
            let diag = ctxt.diag.borrow();
            let errors = diag.errors();

            println!("errors = {:?}", errors);
            assert_eq!(vec.len(), errors.len());

            for (ind, error) in errors.iter().enumerate() {
                assert_eq!(vec[ind].0, error.pos);
                assert_eq!(vec[ind].1, error.msg);
            }
        });
    }

    pub fn pos(line: u32, col: u32) -> Position {
        Position::new(line, col)
    }
}
