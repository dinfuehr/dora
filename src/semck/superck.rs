use std::cmp::max;
use std::collections::{HashMap, HashSet};
use std::ptr;

use baseline::stub::Stub;
use class::{Class, ClassId};
use ctxt::{Context, Fct, FctId};
use error::msg::Msg;
use lexer::position::Position;
use object::Header;
use vtable::{DISPLAY_SIZE, VTable, VTableBox};

pub fn check<'ast>(ctxt: &mut Context<'ast>) {
    cycle_detection(ctxt);
    if ctxt.diag.borrow().has_errors() {
        return;
    }

    check_override(ctxt);
    if ctxt.diag.borrow().has_errors() {
        return;
    }

    determine_sizes(ctxt);
    create_vtables(ctxt);

    create_displays(ctxt);
}

fn cycle_detection<'ast>(ctxt: &mut Context<'ast>) {
    for cls in &ctxt.classes {
        let mut map: HashSet<ClassId> = HashSet::new();
        map.insert(cls.id);

        let mut cur: &Class<'ast> = cls;

        loop {
            if let Some(parent) = cur.parent_class {
                if !map.insert(parent) {
                    let pos = cls.ast.map(|ast| ast.pos).unwrap_or(Position::new(1, 1));
                    ctxt.diag.borrow_mut().report(pos, Msg::CycleInHierarchy);

                    break;
                }

                cur = ctxt.cls_by_id(parent);

            } else {
                break;
            }
        }

    }
}

fn determine_sizes<'ast>(ctxt: &mut Context<'ast>) {
    let mut super_sizes: HashMap<ClassId, i32> = HashMap::new();

    for cls in &ctxt.classes {
        determine_recursive_size(ctxt, &mut super_sizes, cls.id);
    }

    for cls in &mut ctxt.classes {
        let super_size = *super_sizes.get(&cls.id).unwrap();
        cls.size += super_size;

        for field in &mut cls.fields {
            field.offset += super_size;
        }
    }
}

fn determine_recursive_size<'ast>(ctxt: &Context<'ast>,
                                  super_sizes: &mut HashMap<ClassId, i32>,
                                  id: ClassId)
                                  -> i32 {
    if let Some(&val) = super_sizes.get(&id) {
        return val;
    }

    let cls = ctxt.cls_by_id(id);

    let super_size = if let Some(parent_class) = cls.parent_class {
        let super_class = ctxt.cls_by_id(parent_class);
        super_class.size + determine_recursive_size(ctxt, super_sizes, parent_class)

    } else {
        Header::size()
    };

    super_sizes.insert(id, super_size);

    super_size
}

fn check_override<'ast>(ctxt: &mut Context<'ast>) {
    let mut updates: Vec<(FctId, FctId)> = Vec::new();

    for class in &ctxt.classes {
        for &fct_id in &class.methods {
            let fct = ctxt.fct_by_id(fct_id);
            check_fct_modifier(ctxt, class, fct, &mut updates);
        }
    }

    for update in updates {
        let fct = ctxt.fct_by_id_mut(update.0);
        fct.overrides = Some(update.1);
    }
}

fn check_fct_modifier<'ast>(ctxt: &Context<'ast>,
                            cls: &Class,
                            fct: &Fct<'ast>,
                            updates: &mut Vec<(FctId, FctId)>) {
    // catch: class A { open fun f() } (A is not derivable)
    // catch: open final fun f()
    if fct.has_open && (!cls.has_open || fct.has_final) {
        let name = ctxt.interner.str(fct.name).to_string();
        ctxt.diag.borrow_mut().report(fct.pos(), Msg::SuperfluousOpen(name));
        return;
    }

    if cls.parent_class.is_none() {
        if fct.has_override {
            let name = ctxt.interner.str(fct.name).to_string();
            ctxt.diag.borrow_mut().report(fct.pos(), Msg::SuperfluousOverride(name));
            return;
        }

        return;
    }

    let parent = cls.parent_class.unwrap();
    let parent = ctxt.cls_by_id(parent);

    let super_method = parent.find_method(ctxt, fct.name, &fct.params_types);

    if let Some(super_method) = super_method {
        let super_method = ctxt.fct_by_id(super_method);

        if !fct.has_override {
            let name = ctxt.interner.str(fct.name).to_string();
            ctxt.diag.borrow_mut().report(fct.pos(), Msg::MissingOverride(name));
        }

        if !(super_method.has_open || super_method.has_override) || super_method.has_final {
            let name = ctxt.interner.str(fct.name).to_string();
            ctxt.diag.borrow_mut().report(fct.pos(), Msg::MethodNotOverridable(name));
        }

        if super_method.throws != fct.throws {
            let name = ctxt.interner.str(fct.name).to_string();
            ctxt.diag.borrow_mut().report(fct.pos(), Msg::ThrowsDifference(name));
        }

        if super_method.return_type != fct.return_type {
            let pos = fct.pos();
            let fct = fct.return_type.name(ctxt);
            let sup = super_method.return_type.name(ctxt);
            ctxt.diag.borrow_mut().report(pos, Msg::ReturnTypeMismatch(fct, sup));
        }

        updates.push((fct.id, super_method.id));
    } else {
        if fct.has_override {
            let name = ctxt.interner.str(fct.name).to_string();
            ctxt.diag.borrow_mut().report(fct.pos(), Msg::SuperfluousOverride(name));
        }
    }
}

fn create_vtables<'ast>(ctxt: &mut Context<'ast>) {
    for clsid in 0..ctxt.classes.len() {
        let clsid: ClassId = clsid.into();

        ensure_super_vtables(ctxt, clsid);
    }
}

fn ensure_super_vtables<'ast>(ctxt: &mut Context<'ast>, clsid: ClassId) {
    if ctxt.cls_by_id(clsid).vtable.is_some() {
        return;
    }

    let mut vtable_entries: Vec<usize> = Vec::new();

    if let Some(superid) = ctxt.cls_by_id(clsid).parent_class {
        ensure_super_vtables(ctxt, superid);

        let cls = ctxt.cls_by_id(superid);
        let vtable = cls.vtable.as_ref().unwrap();

        vtable_entries.extend_from_slice(vtable.table());
    }

    for ind in 0..ctxt.cls_by_id_mut(clsid).methods.len() {
        let fctid = ctxt.cls_by_id_mut(clsid).methods[ind];
        let is_virtual = {
            let fct = ctxt.fct_by_id(fctid);

            if fct.vtable_index.is_some() {
                continue;
            }

            fct.is_virtual()
        };

        if is_virtual {
            let overrides = ctxt.fct_by_id(fctid).overrides;
            let vtable_index = if let Some(overrides) = overrides {
                ctxt.fct_by_id(overrides).vtable_index.unwrap()

            } else {
                let vtable_index = vtable_entries.len();
                vtable_entries.push(0);

                vtable_index as u32
            };

            let is_src = {
                let fct = ctxt.fct_by_id_mut(fctid);
                fct.vtable_index = Some(vtable_index);

                fct.is_src()
            };

            if is_src {
                vtable_entries[vtable_index as usize] = ensure_stub(ctxt, fctid) as usize;
            }
        }
    }

    let cls = ctxt.cls_by_id_mut(clsid);
    let classptr: *mut Class<'ast> = &mut *cls;
    cls.vtable = Some(VTableBox::new(classptr, &vtable_entries));
}

fn ensure_stub<'ast>(ctxt: &mut Context<'ast>, fid: FctId) -> *const u8 {
    let stub = Stub::new(fid);

    {
        let mut code_map = ctxt.code_map.lock().unwrap();
        code_map.insert(stub.ptr_start(), stub.ptr_end(), fid);
    }

    if ctxt.args.flag_emit_stubs {
        println!("create stub at {:x}", stub.ptr_start() as usize);
    }

    let ptr = stub.ptr_start();

    let src = ctxt.fct_by_id_mut(fid).src();
    let mut src = src.lock().unwrap();
    assert!(src.stub.is_none());
    src.stub = Some(stub);

    ptr
}

fn create_displays<'ast>(ctxt: &mut Context<'ast>) {
    for clsid in 0..ctxt.classes.len() {
        let clsid: ClassId = clsid.into();

        ensure_display(ctxt, clsid);
    }
}

fn ensure_display<'ast>(ctxt: &mut Context<'ast>, clsid: ClassId) -> usize {
    let parent_id = {
        let cls = ctxt.cls_by_id(clsid);
        let vtable = cls.vtable.as_ref().unwrap();

        // if subtype_display[0] is set, vtable was already initialized
        if !vtable.subtype_display[0].is_null() {
            return vtable.subtype_depth as usize;
        }

        cls.parent_class
    };

    if let Some(parent_id) = parent_id {
        let depth = 1 + ensure_display(ctxt, parent_id);

        let (cls, parent) = index_twice(&mut ctxt.classes, clsid.into(), parent_id.into());

        let vtable: &mut VTable<'ast> = cls.vtable.as_mut().unwrap();
        let parent_vtable: &mut VTable<'ast> = parent.vtable.as_mut().unwrap();
        let depth_fixed;

        if depth >= DISPLAY_SIZE {
            depth_fixed = DISPLAY_SIZE;

            vtable.allocate_overflow(depth as usize - DISPLAY_SIZE + 1);

            unsafe {
                if depth > DISPLAY_SIZE {
                    ptr::copy_nonoverlapping(parent_vtable.subtype_overflow,
                                             vtable.subtype_overflow as *mut _,
                                             depth as usize - DISPLAY_SIZE);
                }

                let ptr = vtable.subtype_overflow
                    .offset(depth as isize -
                            DISPLAY_SIZE as isize) as *mut _;

                *ptr = vtable as *const _;
            }

        } else {
            depth_fixed = depth;

            vtable.subtype_display[depth] = vtable as *const _;
        }

        vtable.subtype_depth = depth as i32;
        vtable.subtype_display[0..depth_fixed]
            .clone_from_slice(&parent_vtable.subtype_display[0..depth_fixed]);

        depth

    } else {
        let cls = ctxt.cls_by_id_mut(clsid);
        let vtable: &mut VTable<'ast> = cls.vtable.as_mut().unwrap();

        vtable.subtype_depth = 0;
        vtable.subtype_display[0] = vtable as *const _;

        0
    }
}

fn index_twice<T>(array: &mut [T], a: usize, b: usize) -> (&mut T, &mut T) {
    assert!(a != b);
    assert!(max(a, b) < array.len());

    unsafe {
        let ao = &mut *(array.get_unchecked_mut(a) as *mut _);
        let bo = &mut *(array.get_unchecked_mut(b) as *mut _);

        (ao, bo)
    }
}

#[cfg(test)]
mod tests {
    use class::Class;
    use ctxt::Context;
    use error::msg::Msg;
    use interner::Name;
    use object::Header;
    use semck::tests::{err, errors, ok, ok_with_test, pos};
    use vtable::VTable;

    #[test]
    fn test_super_size() {
        ok_with_test("open class A { var a: int; }
            open class B: A { var b1: int; var b2: int; }
            class C: B { var c: Str; }",
                     |ctxt| {
            check_class(ctxt, "A", 4, None);
            check_field(ctxt, "A", "a", Header::size());
            check_class(ctxt, "B", 4 * 3, Some("A"));
            check_field(ctxt, "B", "b1", Header::size() + 4);
            check_class(ctxt, "C", 4 * 3 + 8, Some("B"));
            check_field(ctxt, "C", "c", Header::size() + 4 * 3);
        });
    }

    #[test]
    fn test_cycle() {
        errors("open class A: B open class B: A",
               &[(pos(1, 6), Msg::CycleInHierarchy), (pos(1, 22), Msg::CycleInHierarchy)]);
    }

    #[test]
    fn test_superfluous_override() {
        err("class A { override fun f() {} }",
            pos(1, 20),
            Msg::SuperfluousOverride("f".into()));
        err("open class B { } class A: B { override fun f() {} }",
            pos(1, 40),
            Msg::SuperfluousOverride("f".into()));
        err("open class B { fun g() {} } class A: B { override fun f() {} }",
            pos(1, 51),
            Msg::SuperfluousOverride("f".into()));
        err("open class B { fun f(a: int) {} } class A: B { override fun f() {} }",
            pos(1, 57),
            Msg::SuperfluousOverride("f".into()));
    }

    #[test]
    fn test_override() {
        err("open class A { fun f() {} } class B: A { override fun f() {} }",
            pos(1, 51),
            Msg::MethodNotOverridable("f".into()));
        ok("open class A { open fun f() {} } class B: A { override fun f() {} }");
        ok("open class A { open fun f() {} }
            open class B: A { override fun f() {} }
            open class C: B { override fun f() {} }");
        err("open class A { open fun f() {} } class B: A { fun f() {} }",
            pos(1, 47),
            Msg::MissingOverride("f".into()));
        err("open class A { open fun f() {} }
             open class B: A { final override fun f() {} }
             class C: B { override fun f() {} }",
            pos(3, 36),
            Msg::MethodNotOverridable("f".into()));
    }

    #[test]
    fn test_override_with_wrong_return_type() {
        err("open class A { open fun f() {} }
             class B: A { override fun f() -> int { return 1; } }",
            pos(2, 36),
            Msg::ReturnTypeMismatch("int".into(), "()".into()));
    }

    #[test]
    fn test_override_with_missing_throws() {
        err("open class A { open fun f() throws {} }
             class B: A { override fun f() {} }",
            pos(2, 36),
            Msg::ThrowsDifference("f".into()));
    }

    #[test]
    fn test_open() {
        ok("open class A { open fun f() {} }");
    }

    #[test]
    fn test_superfluous_open() {
        err("class A { open fun f() {} }",
            pos(1, 16),
            Msg::SuperfluousOpen("f".into()));
    }

    #[test]
    fn test_final() {
        ok("open class A { final fun f() {} }");
    }

    #[test]
    fn test_depth() {
        ok_with_test("class A { } class B { }", |ctxt| {
            assert_eq!(vtable_by_name(ctxt, "A").subtype_depth, 0);
            assert_eq!(vtable_by_name(ctxt, "B").subtype_depth, 0);
        });
    }

    #[test]
    fn test_depth_with_multiple_levels() {
        ok_with_test("open class A { } open class B: A { }
                      class C: B { }",
                     |ctxt| {
            assert_eq!(vtable_by_name(ctxt, "A").subtype_depth, 0);
            assert_eq!(vtable_by_name(ctxt, "B").subtype_depth, 1);
            assert_eq!(vtable_by_name(ctxt, "C").subtype_depth, 2);

            {
                let vtable = vtable_by_name(ctxt, "C");

                assert_name(ctxt, vtable_name(vtable), "C");
                assert_name(ctxt, vtable_display_name(vtable, 0), "A");
                assert_name(ctxt, vtable_display_name(vtable, 1), "B");
                assert_name(ctxt, vtable_display_name(vtable, 2), "C");
                assert!(vtable.subtype_display[3].is_null());
            }

            {
                let vtable = vtable_by_name(ctxt, "B");

                assert_name(ctxt, vtable_name(vtable), "B");
                assert_name(ctxt, vtable_display_name(vtable, 0), "A");
                assert_name(ctxt, vtable_display_name(vtable, 1), "B");
                assert!(vtable.subtype_display[2].is_null());
            }

            {
                let vtable = vtable_by_name(ctxt, "A");

                assert_name(ctxt, vtable_name(vtable), "A");
                assert_name(ctxt, vtable_display_name(vtable, 0), "A");
                assert!(vtable.subtype_display[1].is_null());
            }
        });
    }

    #[test]
    fn test_depth_greater_display_size() {
        ok_with_test("  open class L1 { }
                        open class L2: L1 { }
                        open class L3: L2 { }
                        open class L4: L3 { }
                        open class L5: L4 { }
                        open class L6: L5 { }
                        open class L7: L6 { }
                        open class L8: L7 { }
                        open class L9: L8 { }
                        class L10: L9 { }",
                     |ctxt| {
            assert_eq!(vtable_by_name(ctxt, "L1").subtype_depth, 0);
            assert_eq!(vtable_by_name(ctxt, "L2").subtype_depth, 1);
            assert_eq!(vtable_by_name(ctxt, "L3").subtype_depth, 2);
            assert_eq!(vtable_by_name(ctxt, "L4").subtype_depth, 3);
            assert_eq!(vtable_by_name(ctxt, "L5").subtype_depth, 4);
            assert_eq!(vtable_by_name(ctxt, "L6").subtype_depth, 5);
            assert_eq!(vtable_by_name(ctxt, "L7").subtype_depth, 6);

            let vtable = vtable_by_name(ctxt, "L7");
            assert_name(ctxt, vtable_display_name(vtable, 0), "L1");
            assert_name(ctxt, vtable_display_name(vtable, 1), "L2");
            assert_name(ctxt, vtable_display_name(vtable, 2), "L3");
            assert_name(ctxt, vtable_display_name(vtable, 3), "L4");
            assert_name(ctxt, vtable_display_name(vtable, 4), "L5");
            assert_name(ctxt, vtable_display_name(vtable, 5), "L6");

            assert!(!vtable.subtype_overflow.is_null());
            assert_eq!(vtable_by_name(ctxt, "L7") as *const _,
                       vtable.get_subtype_overflow(0));

            let vtable = vtable_by_name(ctxt, "L10");
            assert_name(ctxt, vtable_display_name(vtable, 0), "L1");
            assert_name(ctxt, vtable_display_name(vtable, 1), "L2");
            assert_name(ctxt, vtable_display_name(vtable, 2), "L3");
            assert_name(ctxt, vtable_display_name(vtable, 3), "L4");
            assert_name(ctxt, vtable_display_name(vtable, 4), "L5");
            assert_name(ctxt, vtable_display_name(vtable, 5), "L6");

            assert!(!vtable.subtype_overflow.is_null());
            assert_eq!(vtable_by_name(ctxt, "L7") as *const _,
                       vtable.get_subtype_overflow(0));
            assert_eq!(vtable_by_name(ctxt, "L8") as *const _,
                       vtable.get_subtype_overflow(1));
            assert_eq!(vtable_by_name(ctxt, "L9") as *const _,
                       vtable.get_subtype_overflow(2));
            assert_eq!(vtable_by_name(ctxt, "L10") as *const _,
                       vtable.get_subtype_overflow(3));
        });
    }

    fn assert_name<'a, 'ast>(ctxt: &'a Context<'ast>, a: Name, b: &'static str) {
        let bname = ctxt.interner.intern(b);

        println!("{} {}", ctxt.interner.str(a), b);

        assert_eq!(a, bname);
    }

    fn vtable_name<'ast>(vtable: *const VTable<'ast>) -> Name {
        let cls = unsafe { &*(*vtable).classptr };

        cls.name
    }

    fn vtable_display_name<'ast>(vtable: *const VTable<'ast>, ind: usize) -> Name {
        unsafe {
            let vtable = (*vtable).subtype_display[ind] as *const VTable<'ast>;
            let cls = &*(*vtable).classptr;

            cls.name
        }
    }

    fn vtable_by_name<'a, 'ast>(ctxt: &'a Context<'ast>, name: &'static str) -> &'a VTable<'ast> {
        cls_by_name(ctxt, name).vtable.as_ref().unwrap()
    }

    fn cls_by_name<'a, 'ast>(ctxt: &'a Context<'ast>, name: &'static str) -> &'a Class<'ast> {
        let name = ctxt.interner.intern(name);
        let cls_id = ctxt.sym.borrow().get_class(name).unwrap();

        ctxt.cls_by_id(cls_id)
    }

    fn check_class<'ast>(ctxt: &Context<'ast>,
                         name: &'static str,
                         size: i32,
                         parent: Option<&'static str>) {
        let name = ctxt.interner.intern(name);
        let cls_id = ctxt.sym.borrow().get_class(name).unwrap();

        let parent_id = parent.map(|name| ctxt.interner.intern(name))
            .map(|name| ctxt.sym.borrow().get_class(name).unwrap());

        let cls = ctxt.cls_by_id(cls_id);
        assert_eq!(parent_id, cls.parent_class);
        assert_eq!(Header::size() + size, cls.size);
    }

    fn check_field<'ast>(ctxt: &Context<'ast>,
                         cls_name: &'static str,
                         field_name: &'static str,
                         offset: i32) {
        let cls_name = ctxt.interner.intern(cls_name);
        let field_name = ctxt.interner.intern(field_name);
        let cls_id = ctxt.sym.borrow().get_class(cls_name).unwrap();
        let cls = ctxt.cls_by_id(cls_id);

        for field in &cls.fields {
            if field_name == field.name {
                assert_eq!(offset, field.offset);
                return;
            }
        }

        unreachable!();
    }
}
