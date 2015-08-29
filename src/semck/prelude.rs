use driver::ctxt::Context;

use sym::Sym::*;
use sym::BuiltinType;

pub fn init(ctxt: &Context) {
    let mut sym = ctxt.sym.borrow_mut();

    let name = ctxt.interner.intern("int".into());
    assert!(sym.insert(name, SymType(BuiltinType::Int)).is_none());

    let name = ctxt.interner.intern("bool".into());
    assert!(sym.insert(name, SymType(BuiltinType::Bool)).is_none());

    let name = ctxt.interner.intern("str".into());
    assert!(sym.insert(name, SymType(BuiltinType::Str)).is_none());
}
