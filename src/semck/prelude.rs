use libc::c_void;

use stdlib;

use ctxt::*;
use class::*;
use interner::Name;
use object::IntArray;
use mem::{self, Ptr};
use sym::Sym::*;
use ty::BuiltinType;

pub fn init<'ast>(ctxt: &mut Context<'ast>) {
    add_builtin_types(ctxt);
    add_builtin_functions(ctxt);
    add_builtin_classes(ctxt);
}

fn add_builtin_classes<'ast>(ctxt: &mut Context<'ast>) {
    let cls_id = ClassId(ctxt.classes.len());
    let cls_name = ctxt.interner.intern("IntArray");

    let ctor_empty = add_ctor(ctxt, cls_id, cls_name,
        Vec::new(), Ptr::new(stdlib::int_array_empty as *mut c_void));
    let ctor_elem = add_ctor(ctxt, cls_id, cls_name,
        vec![BuiltinType::Int, BuiltinType::Int],
        Ptr::new(stdlib::int_array_elem as *mut c_void));

    let mtd_len = add_method(ctxt, cls_id, "len", Vec::new(), BuiltinType::Int,
        FctKind::Builtin(Ptr::new(stdlib::int_array_len as *mut c_void)));

    let mtd_get = add_method(ctxt, cls_id, "get", vec![BuiltinType::Int], BuiltinType::Int,
        FctKind::Intrinsic);

    let mtd_set = add_method(ctxt, cls_id, "set", vec![BuiltinType::Int, BuiltinType::Int],
        BuiltinType::Unit, FctKind::Intrinsic);

    let cls = Box::new(Class {
        id: cls_id,
        name: cls_name,
        ctors: vec![ctor_empty, ctor_elem],
        props: Vec::new(),
        methods: vec![mtd_len, mtd_get, mtd_set],
        size: IntArray::size() as i32,
        ast: None
    });

    ctxt.classes.push(cls);

    let sym = SymType(BuiltinType::Class(cls_id));
    assert!(ctxt.sym.borrow_mut().insert(cls_name, sym).is_none());
}

fn add_ctor<'ast>(ctxt: &mut Context<'ast>, cls_id: ClassId, name: Name,
                  args: Vec<BuiltinType>, fct: Ptr) -> FctId {
    let fct = Fct {
        id: FctId(0),
        name: name,
        params_types: args,
        return_type: BuiltinType::Class(cls_id),
        owner_class: Some(cls_id),
        ctor: true,
        initialized: true,
        kind: FctKind::Builtin(fct),
    };

    ctxt.add_fct(fct)
}

fn add_method<'ast>(ctxt: &mut Context<'ast>, cls_id: ClassId, name: &'static str,
                    mut args: Vec<BuiltinType>, return_type: BuiltinType,
                    kind: FctKind<'ast>) -> FctId {
    let name = ctxt.interner.intern(name);
    args.insert(0, BuiltinType::Class(cls_id));

    let fct = Fct {
        id: FctId(0),
        name: name,
        params_types: args,
        return_type: return_type,
        owner_class: Some(cls_id),
        ctor: false,
        initialized: true,
        kind: kind,
    };

    ctxt.add_fct(fct)
}

fn add_builtin_types<'ast>(ctxt: &mut Context<'ast>) {
    builtin_type("int", BuiltinType::Int, ctxt);
    builtin_type("bool", BuiltinType::Bool, ctxt);
    builtin_type("Str", BuiltinType::Str, ctxt);
}

fn builtin_type<'a, 'ast: 'a>(name: &str, ty: BuiltinType, ctxt: &mut Context<'ast>) {
    let name = ctxt.interner.intern(name.into());
    assert!(ctxt.sym.borrow_mut().insert(name, SymType(ty)).is_none());
}

fn add_builtin_functions<'ast>(ctxt: &mut Context<'ast>) {
    builtin_function("assert", vec![BuiltinType::Bool], BuiltinType::Unit,
        ctxt, Ptr::new(stdlib::assert as *mut c_void));

    builtin_function("print", vec![BuiltinType::Str], BuiltinType::Unit, ctxt,
        Ptr::new(stdlib::print as *mut c_void));

    builtin_function("println", vec![BuiltinType::Str], BuiltinType::Unit, ctxt,
        Ptr::new(stdlib::println as *mut c_void));

    builtin_function("to_string", vec![BuiltinType::Int], BuiltinType::Str, ctxt,
        Ptr::new(stdlib::to_string as *mut c_void));

    builtin_function("argc", vec![], BuiltinType::Int, ctxt,
        Ptr::new(stdlib::argc as *mut c_void));

    builtin_function("argv", vec![BuiltinType::Int], BuiltinType::Str, ctxt,
        Ptr::new(stdlib::argv as *mut c_void));

    builtin_function("parse", vec![BuiltinType::Str], BuiltinType::Int, ctxt,
        Ptr::new(stdlib::parse as *mut c_void));
}

fn builtin_function<'ast>(name: &str, args: Vec<BuiltinType>, ret: BuiltinType,
                    ctxt: &mut Context<'ast>, fct: Ptr) {
    let name = ctxt.interner.intern(name);

    let fct = Fct {
        id: FctId(0),
        name: name,
        params_types: args,
        return_type: ret,
        owner_class: None,
        ctor: false,
        initialized: true,
        kind: FctKind::Builtin(fct),
    };

    assert!(ctxt.add_fct_to_sym(fct).is_ok());
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
