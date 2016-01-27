use std::collections::HashMap;
use libc::c_void;

use stdlib;

use ctxt::*;
use mem::Ptr;
use sym::Sym::*;
use ty::BuiltinType;

pub fn init<'a, 'ast: 'a>(ctxt: &mut Context<'a, 'ast>) {
    add_builtin_types(ctxt);
    add_builtin_functions(ctxt);
}

fn add_builtin_types<'a, 'ast: 'a>(ctxt: &mut Context<'a, 'ast>) {
    builtin_type("int", BuiltinType::Int, ctxt);
    builtin_type("bool", BuiltinType::Bool, ctxt);
    builtin_type("str", BuiltinType::Str, ctxt);
}

fn builtin_type<'a, 'ast: 'a>(name: &str, ty: BuiltinType, ctxt: &mut Context<'a, 'ast>) {
    let name = ctxt.interner.intern(name.into());
    assert!(ctxt.sym.borrow_mut().insert(name, SymType(ty)).is_none());
}

fn add_builtin_functions<'a, 'ast: 'a>(ctxt: &mut Context<'a, 'ast>) {
    builtin_function("assert", vec![BuiltinType::Bool], BuiltinType::Unit,
        ctxt, FctCode::Builtin(Ptr::new(stdlib::assert as *mut c_void)));

    builtin_function("print", vec![BuiltinType::Str],
        BuiltinType::Unit, ctxt, FctCode::Uncompiled);

    builtin_function("println", vec![BuiltinType::Str],
        BuiltinType::Unit, ctxt, FctCode::Uncompiled);
}

fn builtin_function<'a, 'ast: 'a>(name: &str, args: Vec<BuiltinType>, ret: BuiltinType,
                    ctxt: &mut Context<'a, 'ast>, fct: FctCode) {
    let name = ctxt.interner.intern(name);

    let fct = FctContext {
        id: FctContextId(0),
        name: name,
        params_types: args,
        return_type: ret,
        ast: None,
        calls: HashMap::new(),
        defs: HashMap::new(),
        ir: None,
        tempsize: 0,
        localsize: 0,
        leaf: false,
        vars: Vec::new(),
        always_returns: false,
        code: fct,
        stub: None,
    };

    assert!(ctxt.add_function(fct).is_ok());
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
