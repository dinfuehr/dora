use libc::c_void;
use std::ptr;

use stdlib;

use ast::ctxt::*;

use sym::Sym::*;
use sym::BuiltinType;

pub fn init(ctxt: &Context) {
    add_builtin_types(ctxt);
    add_builtin_functions(ctxt);
}

fn add_builtin_types(ctxt: &Context) {
    builtin_type("int", BuiltinType::Int, ctxt);
    builtin_type("bool", BuiltinType::Bool, ctxt);
    builtin_type("str", BuiltinType::Str, ctxt);
}

fn builtin_type(name: &str, ty: BuiltinType, ctxt: &Context) {
    let name = ctxt.interner.intern(name.into());
    assert!(ctxt.sym.borrow_mut().insert(name, SymType(ty)).is_none());
}

fn add_builtin_functions(ctxt: &Context) {
    builtin_function("assert", vec![BuiltinType::Bool], BuiltinType::Unit, ctxt, stdlib::assert as *const c_void);
    builtin_function("print", vec![BuiltinType::Str], BuiltinType::Unit, ctxt, ptr::null());
    builtin_function("println", vec![BuiltinType::Str], BuiltinType::Unit, ctxt, ptr::null());
}

fn builtin_function(name: &str, args: Vec<BuiltinType>, ret: BuiltinType, ctxt: &Context,
        fct: *const c_void) {
    let name = ctxt.interner.intern(name);

    let fct_info = FctInfo {
        name: name,
        params_types: args,
        return_type: ret,
        ast: None,
        vars: Vec::new(),
        stacksize: 0,
        always_returns: false,
        contains_fct_invocation: false,
        compiled_fct: fct,
    };

    assert!(ctxt.add_function(fct_info).is_ok());
}

#[cfg(test)]
mod tests {
    use semck::tests::*;

    #[test]
    fn builtin_functions() {
        ok("fn f() { assert(true); }");
        ok("fn f() { print(\"test\"); }");
        ok("fn f() { println(\"test\"); }");
    }
}
