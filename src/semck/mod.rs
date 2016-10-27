use libc;

use ast::{Stmt, Type};
use ast::Type::TypeBasic;
use class::ClassId;
use ctxt::{Context, FctKind};
use error::msg::Msg;
use mem::ptr::Ptr;
use stdlib;
use ty::BuiltinType;

mod clsdefck;
mod fctdefck;
mod flowck;
mod globaldef;
mod nameck;
mod prelude;
mod typeck;
mod returnck;
mod superck;

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

    internal_classes(ctxt);

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

    // add size of super classes to field offsets
    superck::check(ctxt);
    return_on_error!(ctxt);

    // check types of expressions in functions
    typeck::check(ctxt);
    return_on_error!(ctxt);

    // are break and continue used in the right places?
    flowck::check(ctxt);

    // checks if function has a return value
    returnck::check(ctxt);

    prelude_internal(ctxt);

    // check for internal functions or classes
    internalck(ctxt);
    return_on_error!(ctxt);
}

fn prelude_internal<'ast>(ctxt: &mut Context<'ast>) {
    native_fct(ctxt, "print", stdlib::print as *const u8);
    native_fct(ctxt, "println", stdlib::println as *const u8);
    native_fct(ctxt, "assert", stdlib::assert as *const u8);
    native_fct(ctxt, "argc", stdlib::argc as *const u8);
    native_fct(ctxt, "argv", stdlib::argv as *const u8);
    native_fct(ctxt, "forceCollect", stdlib::gc_collect as *const u8);
    native_fct(ctxt, "intArrayWith", stdlib::ctor_int_array_elem as *const u8);
    native_fct(ctxt, "emptyIntArray", stdlib::ctor_int_array_empty as *const u8);

    let clsid = ctxt.primitive_classes.int_class;
    native_method(ctxt, clsid, "toString", stdlib::int_to_string as *const u8);

    let clsid = ctxt.primitive_classes.bool_class;
    native_method(ctxt, clsid, "toInt", stdlib::bool_to_int as *const u8);
    native_method(ctxt, clsid, "toString", stdlib::bool_to_string as *const u8);

    let clsid = ctxt.primitive_classes.str_class;
    native_method(ctxt, clsid, "len", stdlib::str_len as *const u8);
    native_method(ctxt, clsid, "parseInt", stdlib::str_parse_int as *const u8);

    let clsid = ctxt.primitive_classes.int_array;
    intrinsic_method(ctxt, clsid, "len");
    intrinsic_method(ctxt, clsid, "get");
    intrinsic_method(ctxt, clsid, "set");

    intrinsic_fct(ctxt, "shl");
}

fn native_method<'ast>(ctxt: &mut Context<'ast>, clsid: ClassId, name: &str, fctptr: *const u8) {
    internal_method(ctxt, clsid, name, FctKind::Builtin(Ptr::new(fctptr as *mut libc::c_void)));
}

fn intrinsic_method<'ast>(ctxt: &mut Context<'ast>, clsid: ClassId, name: &str) {
    internal_method(ctxt, clsid, name, FctKind::Intrinsic);
}

fn internal_method<'ast>(ctxt: &mut Context<'ast>, clsid: ClassId, name: &str,
                         kind: FctKind<'ast>) {
    let methods = ctxt.cls_by_id(clsid).methods.clone();
    let name = ctxt.interner.intern(name);

    for mid in &methods {
      let mtd = ctxt.fct_by_id_mut(*mid);

      if mtd.name == name && mtd.internal {
        mtd.kind = kind;
        mtd.internal = false;
        break;
      }
    }
}

fn internal_classes<'ast>(ctxt: &mut Context<'ast>) {
    ctxt.primitive_classes.int_class =
        internal_class(ctxt, "int", BuiltinType::Int, BuiltinType::Int.size());
    ctxt.primitive_classes.bool_class =
        internal_class(ctxt, "bool", BuiltinType::Bool, BuiltinType::Bool.size());
    ctxt.primitive_classes.str_class =
        internal_class(ctxt, "Str", BuiltinType::Str, 0);
    ctxt.primitive_classes.int_array =
        internal_class(ctxt, "IntArray", BuiltinType::IntArray, 0);
}

fn internal_class<'ast>(ctxt: &mut Context<'ast>, name: &str,
                        ty: BuiltinType, size: i32) -> ClassId {
    let name = ctxt.interner.intern(name);
    let clsid = ctxt.sym.borrow().get_class(name);

    if let Some(clsid) = clsid {
        {
            let cls = ctxt.cls_by_id_mut(clsid);

            if cls.internal {
                cls.ty = ty;
                cls.size = size;
                cls.internal = false;
            }
        }

        clsid
    } else {
        panic!("class int not found!");
    }
}

fn native_fct<'ast>(ctxt: &mut Context<'ast>, name: &str, fctptr: *const u8) {
    internal_fct(ctxt, name, FctKind::Builtin(Ptr::new(fctptr as *mut libc::c_void)));
}

fn intrinsic_fct<'ast>(ctxt: &mut Context<'ast>, name: &str) {
    internal_fct(ctxt, name, FctKind::Intrinsic);
}

fn internal_fct<'ast>(ctxt: &mut Context<'ast>, name: &str, kind: FctKind<'ast>) {
    let name = ctxt.interner.intern(name);
    let fctid = ctxt.sym.borrow().get_fct(name);

    if let Some(fctid) = fctid {
        let fct = ctxt.fct_by_id_mut(fctid);

        if fct.internal {
            fct.kind = kind;
            fct.internal = false;
        }
    }
}

fn internalck<'ast>(ctxt: &Context<'ast>) {
    for fct in &ctxt.fcts {
        if fct.internal {
            ctxt.diag.borrow_mut().report(fct.pos, Msg::UnresolvedInternal);
        }

        if fct.kind.is_definition() {
            ctxt.diag.borrow_mut().report(fct.pos, Msg::MissingFctBody);
        }
    }

    for cls in &ctxt.classes {
        if cls.internal {
            ctxt.diag.borrow_mut().report(cls.ast.unwrap().pos, Msg::UnresolvedInternal);
        }

        for method in &cls.methods {
            let method = ctxt.fct_by_id(*method);

            if method.internal {
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
        TypeBasic(ref basic) => {
            if let Some(cls_id) = ctxt.sym.borrow().get_class(basic.name) {
                let cls = ctxt.cls_by_id(cls_id);

                return Some(cls.ty);

            } else {
                let name = ctxt.interner.str(basic.name).to_string();
                let msg = Msg::UnknownType(name);
                ctxt.diag.borrow_mut().report(basic.pos, msg);
            }
        }

        _ => ctxt.diag.borrow_mut().report_unimplemented(t.pos())
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

    pub fn ok_with_test<F, R>(code: &'static str, f: F) -> R where F: FnOnce(&Context) -> R {
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
