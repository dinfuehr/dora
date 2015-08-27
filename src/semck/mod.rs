use driver::ctxt::Context;

use sym::Sym::*;
use sym::BuiltinType;

pub fn check<'a>(ctxt: &'a mut Context) {
    add_builtin_types(ctxt);
}

fn add_builtin_types(ctxt: &mut Context) {
    let name = ctxt.interner.intern("int".into());
    ctxt.sym.insert(name, SymType(BuiltinType::Int)).unwrap();

    let name = ctxt.interner.intern("bool".into());
    ctxt.sym.insert(name, SymType(BuiltinType::Bool)).unwrap();

    let name = ctxt.interner.intern("str".into());
    ctxt.sym.insert(name, SymType(BuiltinType::Str)).unwrap();
}
