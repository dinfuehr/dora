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
    add_builtin_classes(ctxt);
    add_builtin_functions(ctxt);
}

fn add_builtin_classes<'ast>(ctxt: &mut Context<'ast>) {
    add_class_bool(ctxt);
    add_class_int(ctxt);
    add_class_str(ctxt);
    add_class_int_array(ctxt);
}

fn add_class_bool<'ast>(ctxt: &mut Context<'ast>) {
    let cls_id = ClassId(ctxt.classes.len());
    let cls_name = ctxt.interner.intern("bool");

    let cls = Box::new(Class {
        id: cls_id,
        name: cls_name,
        ty: BuiltinType::Bool,
        ctors: Vec::new(),
        props: Vec::new(),
        methods: Vec::new(),
        size: BuiltinType::Bool.size(),
        ast: None,
    });

    ctxt.classes.push(cls);
    ctxt.primitive_classes.bool_class = cls_id;

    let sym = SymClass(cls_id);
    assert!(ctxt.sym.borrow_mut().insert(cls_name, sym).is_none());
}

fn add_class_int<'ast>(ctxt: &mut Context<'ast>) {
    let cls_id = ClassId(ctxt.classes.len());
    let cls_name = ctxt.interner.intern("int");

    let mtd_tos = add_method(ctxt, cls_id, BuiltinType::Int, "toString", Vec::new(),
        BuiltinType::Str,
        FctKind::Builtin(Ptr::new(stdlib::to_string as *mut c_void)));

    let cls = Box::new(Class {
        id: cls_id,
        name: cls_name,
        ty: BuiltinType::Int,
        ctors: Vec::new(),
        props: Vec::new(),
        methods: vec![mtd_tos],
        size: BuiltinType::Int.size(),
        ast: None,
    });

    ctxt.classes.push(cls);
    ctxt.primitive_classes.int_class = cls_id;

    let sym = SymClass(cls_id);
    assert!(ctxt.sym.borrow_mut().insert(cls_name, sym).is_none());
}

fn add_class_str<'ast>(ctxt: &mut Context<'ast>) {
    let cls_id = ClassId(ctxt.classes.len());
    let cls_name = ctxt.interner.intern("Str");

    let mtd_len = add_method(ctxt, cls_id, BuiltinType::Str, "len", Vec::new(),
        BuiltinType::Int,
        FctKind::Builtin(Ptr::new(stdlib::str_array_len as *mut c_void)));

    let mtd_parse = add_method(ctxt, cls_id, BuiltinType::Str, "parseInt", Vec::new(),
        BuiltinType::Int,
        FctKind::Builtin(Ptr::new(stdlib::parse as *mut c_void)));

    let cls = Box::new(Class {
        id: cls_id,
        name: cls_name,
        ty: BuiltinType::Str,
        ctors: Vec::new(),
        props: Vec::new(),
        methods: vec![mtd_len, mtd_parse],
        size: BuiltinType::Str.size(),
        ast: None,
    });

    ctxt.classes.push(cls);
    ctxt.primitive_classes.str_class = cls_id;

    let sym = SymClass(cls_id);
    assert!(ctxt.sym.borrow_mut().insert(cls_name, sym).is_none());
}

fn add_class_int_array<'ast>(ctxt: &mut Context<'ast>) {
    let cls_id = ClassId(ctxt.classes.len());
    let cls_name = ctxt.interner.intern("IntArray");
    let cls_type = BuiltinType::Class(cls_id);

    let ctor_empty = add_ctor(ctxt, cls_id, cls_name,
        Vec::new(), Ptr::new(stdlib::ctor_int_array_empty as *mut c_void));
    let ctor_elem = add_ctor(ctxt, cls_id, cls_name,
        vec![BuiltinType::Int, BuiltinType::Int],
        Ptr::new(stdlib::ctor_int_array_elem as *mut c_void));

    let mtd_len = add_method(ctxt, cls_id, cls_type, "len", Vec::new(), BuiltinType::Int,
        FctKind::Builtin(Ptr::new(stdlib::int_array_len as *mut c_void)));

    let mtd_get = add_method(ctxt, cls_id, cls_type, "get", vec![BuiltinType::Int], BuiltinType::Int,
        FctKind::Intrinsic);

    let mtd_set = add_method(ctxt, cls_id, cls_type, "set", vec![BuiltinType::Int, BuiltinType::Int],
        BuiltinType::Unit, FctKind::Intrinsic);

    let cls = Box::new(Class {
        id: cls_id,
        name: cls_name,
        ty: BuiltinType::Class(cls_id),
        ctors: vec![ctor_empty, ctor_elem],
        props: Vec::new(),
        methods: vec![mtd_len, mtd_get, mtd_set],
        size: IntArray::size() as i32,
        ast: None
    });

    ctxt.classes.push(cls);

    let sym = SymClass(cls_id);
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

fn add_method<'ast>(ctxt: &mut Context<'ast>, cls_id: ClassId, cls_type: BuiltinType,
                    name: &'static str, mut args: Vec<BuiltinType>, return_type: BuiltinType,
                    kind: FctKind<'ast>) -> FctId {
    let name = ctxt.interner.intern(name);
    args.insert(0, cls_type);

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

fn add_builtin_functions<'ast>(ctxt: &mut Context<'ast>) {
    builtin_function("assert", vec![BuiltinType::Bool], BuiltinType::Unit,
        ctxt, Ptr::new(stdlib::assert as *mut c_void));

    builtin_function("print", vec![BuiltinType::Str], BuiltinType::Unit, ctxt,
        Ptr::new(stdlib::print as *mut c_void));

    builtin_function("println", vec![BuiltinType::Str], BuiltinType::Unit, ctxt,
        Ptr::new(stdlib::println as *mut c_void));

    builtin_function("argc", vec![], BuiltinType::Int, ctxt,
        Ptr::new(stdlib::argc as *mut c_void));

    builtin_function("argv", vec![BuiltinType::Int], BuiltinType::Str, ctxt,
        Ptr::new(stdlib::argv as *mut c_void));
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
