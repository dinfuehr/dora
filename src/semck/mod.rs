use driver::ctxt::Context;
use sym::BuiltinType;
use sym::Sym::SymType;

mod typeck;
mod nameck;

pub fn check(ctxt: &Context) {
    add_builtin_types(ctxt);

    typeck::check(ctxt, ctxt.ast);
    nameck::check(ctxt, ctxt.ast);
}

fn add_builtin_types(ctxt: &Context) {
    let mut sym = ctxt.sym.borrow_mut();

    let name = ctxt.interner.intern("int".into());
    assert!(sym.insert(name, SymType(BuiltinType::Int)).is_none());

    let name = ctxt.interner.intern("bool".into());
    assert!(sym.insert(name, SymType(BuiltinType::Bool)).is_none());

    let name = ctxt.interner.intern("str".into());
    assert!(sym.insert(name, SymType(BuiltinType::Str)).is_none());
}
