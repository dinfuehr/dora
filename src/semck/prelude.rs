use libc::c_void;

use stdlib;

use ast;
use ctxt::*;
use class::*;
use interner::Name;
use lexer::position::Position;
use mem::Ptr;
use sym::Sym::*;
use ty::BuiltinType;

pub fn init<'ast>(ctxt: &mut Context<'ast>) {
    let fct = {
        let mut fct = None;

        for file in &ctxt.ast.files {
            for elem in &file.elements {
                match *elem {
                    ast::Elem::ElemFunction(ref f) => {
                        fct = Some(f);
                        break;
                    }

                    ast::Elem::ElemClass(ref cls) => {
                        for m in &cls.methods {
                            fct = Some(m);
                            break;
                        }

                        if fct.is_some() { break; }

                        for ctor in &cls.ctors {
                            fct = Some(ctor);
                            break;
                        }

                        if fct.is_some() { break; }
                    }
                }
            }

            if fct.is_some() {
                break;
            }
        }

        fct.unwrap()
    };

    add_builtin_classes(ctxt, fct);
    add_builtin_functions(ctxt, fct);
}

fn add_builtin_classes<'ast>(ctxt: &mut Context<'ast>, definition: &'ast ast::Function) {
    add_class_bool(ctxt, definition);
    add_class_int(ctxt, definition);
    add_class_str(ctxt, definition);
    add_class_int_array(ctxt, definition);
}

fn add_class_bool<'ast>(ctxt: &mut Context<'ast>, definition: &'ast ast::Function) {
    let cls_id: ClassId = ctxt.classes.len().into();
    let cls_name = ctxt.interner.intern("bool");

    let mtd_tos = add_method(ctxt, cls_id, "toString", Vec::new(),
        BuiltinType::Str,
        FctKind::Builtin(Ptr::new(stdlib::bool_to_string as *mut c_void)),
        definition);

    let mtd_toi = add_method(ctxt, cls_id, "toInt", Vec::new(),
        BuiltinType::Int,
        FctKind::Builtin(Ptr::new(stdlib::bool_to_int as *mut c_void)),
        definition);

    let cls = Box::new(Class {
        id: cls_id,
        name: cls_name,
        ty: BuiltinType::Bool,
        parent_class: None,
        has_open: false,
        internal: false,
        ctors: Vec::new(),
        fields: Vec::new(),
        methods: vec![mtd_tos, mtd_toi],
        size: BuiltinType::Bool.size(),
        ast: None,
        vtable: None,
    });

    ctxt.classes.push(cls);
    ctxt.primitive_classes.bool_class = cls_id;

    let sym = SymClass(cls_id);
    assert!(ctxt.sym.borrow_mut().insert(cls_name, sym).is_none());
}

fn add_class_int<'ast>(ctxt: &mut Context<'ast>, definition: &'ast ast::Function) {
    let cls_id: ClassId = ctxt.classes.len().into();
    let cls_name = ctxt.interner.intern("int");

    let mtd_tos = add_method(ctxt, cls_id, "toString", Vec::new(),
        BuiltinType::Str,
        FctKind::Builtin(Ptr::new(stdlib::int_to_string as *mut c_void)),
        definition);

    let cls = Box::new(Class {
        id: cls_id,
        name: cls_name,
        ty: BuiltinType::Int,
        parent_class: None,
        has_open: false,
        internal: false,
        ctors: Vec::new(),
        fields: Vec::new(),
        methods: vec![mtd_tos],
        size: BuiltinType::Int.size(),
        ast: None,
        vtable: None,
    });

    ctxt.classes.push(cls);
    ctxt.primitive_classes.int_class = cls_id;

    let sym = SymClass(cls_id);
    assert!(ctxt.sym.borrow_mut().insert(cls_name, sym).is_none());
}

fn add_class_str<'ast>(ctxt: &mut Context<'ast>, definition: &'ast ast::Function) {
    let cls_id: ClassId = ctxt.classes.len().into();
    let cls_name = ctxt.interner.intern("Str");

    let mtd_len = add_method(ctxt, cls_id, "len", Vec::new(),
        BuiltinType::Int,
        FctKind::Builtin(Ptr::new(stdlib::str_array_len as *mut c_void)),
        definition);

    let mtd_parse = add_method(ctxt, cls_id, "parseInt", Vec::new(),
        BuiltinType::Int,
        FctKind::Builtin(Ptr::new(stdlib::parse as *mut c_void)),
        definition);

    let cls = Box::new(Class {
        id: cls_id,
        name: cls_name,
        ty: BuiltinType::Str,
        parent_class: None,
        has_open: false,
        internal: false,
        ctors: Vec::new(),
        fields: Vec::new(),
        methods: vec![mtd_len, mtd_parse],
        size: 0,
        ast: None,
        vtable: None,
    });

    ctxt.primitive_classes.str_class = cls_id;
    ctxt.classes.push(cls);

    let sym = SymClass(cls_id);
    assert!(ctxt.sym.borrow_mut().insert(cls_name, sym).is_none());
}

fn add_class_int_array<'ast>(ctxt: &mut Context<'ast>, definition: &'ast ast::Function) {
    let cls_id: ClassId = ctxt.classes.len().into();
    let cls_name = ctxt.interner.intern("IntArray");

    let mtd_len = add_method(ctxt, cls_id, "len", Vec::new(), BuiltinType::Int,
        FctKind::Intrinsic, definition);

    let mtd_get = add_method(ctxt, cls_id, "get", vec![BuiltinType::Int], BuiltinType::Int,
        FctKind::Intrinsic, definition);

    let mtd_set = add_method(ctxt, cls_id, "set", vec![BuiltinType::Int, BuiltinType::Int],
        BuiltinType::Unit, FctKind::Intrinsic, definition);

    let cls = Box::new(Class {
        id: cls_id,
        name: cls_name,
        ty: BuiltinType::IntArray,
        parent_class: None,
        has_open: false,
        internal: false,
        ctors: Vec::new(),
        fields: Vec::new(),
        methods: vec![mtd_len, mtd_get, mtd_set],
        size: 0,
        ast: None,
        vtable: None,
    });

    ctxt.primitive_classes.int_array = cls_id;
    ctxt.classes.push(cls);

    let sym = SymClass(cls_id);
    assert!(ctxt.sym.borrow_mut().insert(cls_name, sym).is_none());
    }

fn add_ctor<'ast>(ctxt: &mut Context<'ast>, cls_id: ClassId, name: Name,
                  args: Vec<BuiltinType>, fct: Ptr, definition: &'ast ast::Function) -> FctId {
    let fct = Fct {
        id: FctId(0),
        pos: Position::new(1, 1),
        ast: definition,
        name: name,
        params_types: args,
        return_type: BuiltinType::Class(cls_id),
        owner_class: Some(cls_id),
        has_override: false,
        has_open: false,
        has_final: false,
        internal: false,
        overrides: None,
        throws: false,
        ctor: Some(CtorType::Primary),
        vtable_index: None,
        initialized: true,
        kind: FctKind::Builtin(fct),
    };

    ctxt.add_fct(fct)
}

fn add_method<'ast>(ctxt: &mut Context<'ast>, cls_id: ClassId,
                    name: &'static str, args: Vec<BuiltinType>, return_type: BuiltinType,
                    kind: FctKind<'ast>, definition: &'ast ast::Function) -> FctId {
    let name = ctxt.interner.intern(name);

    let fct = Fct {
        id: FctId(0),
        pos: Position::new(1, 1),
        ast: definition,
        name: name,
        params_types: args,
        return_type: return_type,
        owner_class: Some(cls_id),
        has_override: false,
        has_open: false,
        has_final: false,
        internal: false,
        overrides: None,
        throws: false,
        ctor: None,
        vtable_index: None,
        initialized: true,
        kind: kind,
    };

    ctxt.add_fct(fct)
}

fn add_builtin_functions<'ast>(ctxt: &mut Context<'ast>, definition: &'ast ast::Function) {
    builtin_function("shl", vec![BuiltinType::Int, BuiltinType::Int], BuiltinType::Int,
        ctxt, FctKind::Intrinsic, definition);

    builtin_function("assert", vec![BuiltinType::Bool], BuiltinType::Unit,
        ctxt, FctKind::Builtin(Ptr::new(stdlib::assert as *mut c_void)), definition);

    builtin_function("print", vec![BuiltinType::Str], BuiltinType::Unit, ctxt,
        FctKind::Builtin(Ptr::new(stdlib::print as *mut c_void)), definition);

    builtin_function("println", vec![BuiltinType::Str], BuiltinType::Unit, ctxt,
        FctKind::Builtin(Ptr::new(stdlib::println as *mut c_void)), definition);

    builtin_function("argc", vec![], BuiltinType::Int, ctxt,
        FctKind::Builtin(Ptr::new(stdlib::argc as *mut c_void)), definition);

    builtin_function("argv", vec![BuiltinType::Int], BuiltinType::Str, ctxt,
        FctKind::Builtin(Ptr::new(stdlib::argv as *mut c_void)), definition);

    builtin_function("forceCollect", vec![], BuiltinType::Unit, ctxt,
        FctKind::Builtin(Ptr::new(stdlib::gc_collect as *mut c_void)), definition);

    builtin_function("intArrayWith", vec![BuiltinType::Int, BuiltinType::Int],
        BuiltinType::IntArray, ctxt,
        FctKind::Builtin(Ptr::new(stdlib::ctor_int_array_elem as *mut c_void)),
        definition);

    builtin_function("emptyIntArray", vec![], BuiltinType::IntArray, ctxt,
        FctKind::Builtin(Ptr::new(stdlib::ctor_int_array_empty as *mut c_void)),
        definition);
}

fn builtin_function<'ast>(name: &str, args: Vec<BuiltinType>, ret: BuiltinType,
                    ctxt: &mut Context<'ast>, kind: FctKind<'ast>,
                    definition: &'ast ast::Function) {
    let name = ctxt.interner.intern(name);

    let fct = Fct {
        id: FctId(0),
        pos: Position::new(1, 1),
        ast: definition,
        name: name,
        params_types: args,
        return_type: ret,
        owner_class: None,
        has_override: false,
        has_open: false,
        has_final: false,
        internal: false,
        overrides: None,
        throws: false,
        ctor: None,
        vtable_index: None,
        initialized: true,
        kind: kind,
    };

    assert!(ctxt.add_fct_to_sym(fct).is_ok());
}

#[cfg(test)]
mod tests {
    use semck::tests::*;

    #[test]
    fn builtin_functions() {
        ok("fun f() { assert(true); }");
        ok("fun f() { print(\"test\"); }");
        ok("fun f() { println(\"test\"); }");
    }
}
