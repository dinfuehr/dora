use std::cmp::max;
use std::collections::{HashMap, HashSet};
use std::ptr;

use baseline::stub::ensure_stub;
use class::{Class, ClassId};
use ctxt::{SemContext, Fct, StructId, StructData};
use dora_parser::error::msg::Msg;
use mem;
use object::Header;
use ty::BuiltinType;
use vtable::{DISPLAY_SIZE, VTableBox};

pub fn check<'ast>(ctxt: &mut SemContext<'ast>) {
    cycle_detection(ctxt);

    if ctxt.diag.borrow().has_errors() {
        return;
    }

    determine_struct_sizes(ctxt);
    determine_class_sizes(ctxt);

    create_vtables(ctxt);
    create_displays(ctxt);
}

fn cycle_detection<'ast>(ctxt: &mut SemContext<'ast>) {
    for cls in ctxt.classes.iter() {
        let cls = cls.borrow();

        let mut map: HashSet<ClassId> = HashSet::new();
        map.insert(cls.id);

        let mut parent = cls.parent_class;

        while parent.is_some() {
            let p = parent.unwrap();

            if !map.insert(p) {
                ctxt.diag
                    .borrow_mut()
                    .report(cls.pos, Msg::CycleInHierarchy);
                break;
            }

            parent = ctxt.classes[p].borrow().parent_class;
        }

    }
}

fn determine_struct_sizes<'ast>(ctxt: &SemContext<'ast>) {
    let mut path = Vec::new();
    let mut sizes = HashMap::new();

    for struc in ctxt.structs.iter() {
        let mut struc = struc.borrow_mut();
        determine_struct_size(ctxt, &mut path, &mut sizes, &mut *struc);
    }
}

fn determine_struct_size<'ast>(ctxt: &SemContext<'ast>,
                               path: &mut Vec<StructId>,
                               sizes: &mut HashMap<StructId, (i32, i32)>,
                               struc: &mut StructData)
                               -> (i32, i32) {
    let mut size = 0;
    let mut align = 0;

    path.push(struc.id);

    for field in &mut struc.fields {
        let (field_size, field_align) = if let BuiltinType::Struct(id) = field.ty {
            if let Some(&(size, align)) = sizes.get(&id) {
                (size, align)

            } else {
                if path.iter().find(|&&x| x == id).is_some() {
                    ctxt.diag
                        .borrow_mut()
                        .report(field.pos, Msg::RecursiveStructure);
                    return (0, 0);
                }

                let mut struc = ctxt.structs[id].borrow_mut();
                determine_struct_size(ctxt, path, sizes, &mut *struc)
            }
        } else {
            let ty = field.ty;

            (ty.size(ctxt), ty.align(ctxt))
        };

        field.offset = mem::align_i32(size, field_align);

        size = field.offset + field_size;
        align = max(align, field_align);
    }

    size = mem::align_i32(size, align);

    struc.size = size;
    struc.align = align;

    sizes.insert(struc.id, (size, align));
    path.pop();

    (size, align)
}

fn determine_class_sizes<'ast>(ctxt: &SemContext<'ast>) {
    let mut sizes = HashMap::new();

    for cls in ctxt.classes.iter() {
        let mut cls = cls.borrow_mut();

        // internal classes like Array should have size 0, since
        // their "real" size is dynamic and not static
        if !cls.internal && !cls.is_generic() {
            determine_class_size(ctxt, &mut *cls, &mut sizes);
        }
    }
}

fn determine_class_size<'ast>(ctxt: &SemContext<'ast>,
                              cls: &mut Class,
                              sizes: &mut HashMap<ClassId, (i32, i32)>)
                              -> (i32, i32) {
    if let Some(&(size, size_without_padding)) = sizes.get(&cls.id) {
        return (size, size_without_padding);
    }

    let mut size = if let Some(parent) = cls.parent_class {
        let mut parent_cls = ctxt.classes[parent].borrow_mut();
        let (_, size_without_padding) = determine_class_size(ctxt, &mut *parent_cls, sizes);

        cls.ref_fields = parent_cls.ref_fields.to_vec();

        size_without_padding

    } else {
        Header::size()
    };

    let mut align = mem::ptr_width();

    for f in &mut cls.fields {
        let field_size = f.ty.size(ctxt);
        let field_align = f.ty.align(ctxt);

        let offset = mem::align_i32(size, field_align);

        f.offset = offset;

        size = offset + field_size;
        align = max(align, field_align);

        if f.ty.reference_type() {
            cls.ref_fields.push(f.offset);
        }
    }

    let size_without_padding = size;
    cls.size = mem::align_i32(size, align);
    sizes.insert(cls.id, (cls.size, size_without_padding));

    (cls.size, size_without_padding)
}

pub fn check_override<'ast>(ctxt: &SemContext<'ast>) {
    for cls in ctxt.classes.iter() {
        let cls = cls.borrow();

        for &fct_id in &cls.methods {
            let mut fct = ctxt.fcts[fct_id].borrow_mut();
            check_fct_modifier(ctxt, &*cls, &mut *fct);
        }
    }
}

fn check_fct_modifier<'ast>(ctxt: &SemContext<'ast>, cls: &Class, fct: &mut Fct<'ast>) {
    // catch: class A { open fun f() } (A is not derivable)
    // catch: open final fun f()
    if fct.has_open && (!cls.has_open || fct.has_final) {
        let name = ctxt.interner.str(fct.name).to_string();
        ctxt.diag
            .borrow_mut()
            .report(fct.pos(), Msg::SuperfluousOpen(name));
        return;
    }

    if cls.parent_class.is_none() {
        if fct.has_override {
            let name = ctxt.interner.str(fct.name).to_string();
            ctxt.diag
                .borrow_mut()
                .report(fct.pos(), Msg::SuperfluousOverride(name));
            return;
        }

        return;
    }

    let parent = cls.parent_class.unwrap();
    let parent = ctxt.classes[parent].borrow();

    let super_method = parent.find_method(ctxt, fct.name, false);

    if let Some(super_method) = super_method {
        let super_method = ctxt.fcts[super_method].borrow();

        if !fct.has_override {
            let name = ctxt.interner.str(fct.name).to_string();
            ctxt.diag
                .borrow_mut()
                .report(fct.pos(), Msg::MissingOverride(name));
        }

        if !(super_method.has_open || super_method.has_override) || super_method.has_final {
            let name = ctxt.interner.str(fct.name).to_string();
            ctxt.diag
                .borrow_mut()
                .report(fct.pos(), Msg::MethodNotOverridable(name));
        }

        if super_method.throws != fct.throws {
            let name = ctxt.interner.str(fct.name).to_string();
            ctxt.diag
                .borrow_mut()
                .report(fct.pos(), Msg::ThrowsDifference(name));
        }

        if super_method.return_type != fct.return_type {
            let pos = fct.pos();
            let fct = fct.return_type.name(ctxt);
            let sup = super_method.return_type.name(ctxt);
            ctxt.diag
                .borrow_mut()
                .report(pos, Msg::ReturnTypeMismatch(fct, sup));
        }

        fct.overrides = Some(super_method.id);
    } else {
        if fct.has_override {
            let name = ctxt.interner.str(fct.name).to_string();
            ctxt.diag
                .borrow_mut()
                .report(fct.pos(), Msg::SuperfluousOverride(name));
        }
    }
}

fn create_vtables<'ast>(ctxt: &SemContext<'ast>) {
    for cls in ctxt.classes.iter() {
        let mut cls = cls.borrow_mut();
        ensure_super_vtables(ctxt, &mut *cls);
    }
}

fn ensure_super_vtables<'ast>(ctxt: &SemContext<'ast>, cls: &mut Class) {
    if cls.vtable.is_some() {
        return;
    }

    let mut vtable_entries: Vec<usize> = Vec::new();

    if let Some(superid) = cls.parent_class {
        let mut sup = ctxt.classes[superid].borrow_mut();
        ensure_super_vtables(ctxt, &mut sup);

        let vtable = sup.vtable.as_ref().unwrap();

        vtable_entries.extend_from_slice(vtable.table());
    }

    for &mid in &cls.methods {
        let mut fct = ctxt.fcts[mid].borrow_mut();

        if fct.vtable_index.is_some() {
            continue;
        }

        if fct.is_virtual() {
            let vtable_index = if let Some(overrides) = fct.overrides {
                ctxt.fcts[overrides].borrow().vtable_index.unwrap()

            } else {
                let vtable_index = vtable_entries.len();
                vtable_entries.push(0);

                vtable_index as u32
            };

            fct.vtable_index = Some(vtable_index);

            if fct.is_src() {
                vtable_entries[vtable_index as usize] = ensure_stub(ctxt) as usize;
            }
        }
    }

    let classptr: *mut Class = &mut *cls;
    cls.vtable = Some(VTableBox::new(classptr, &vtable_entries));
}

fn create_displays<'ast>(ctxt: &SemContext<'ast>) {
    for cls in ctxt.classes.iter() {
        let mut cls = cls.borrow_mut();

        ensure_display(ctxt, &mut *cls);
    }
}

fn ensure_display<'ast>(ctxt: &SemContext<'ast>, cls: &mut Class) -> usize {
    let vtable = cls.vtable.as_mut().unwrap();

    // if subtype_display[0] is set, vtable was already initialized
    if !vtable.subtype_display[0].is_null() {
        return vtable.subtype_depth as usize;
    }

    if let Some(parent_id) = cls.parent_class {
        let mut parent = ctxt.classes[parent_id].borrow_mut();
        let depth = 1 + ensure_display(ctxt, &mut *parent);

        let parent_vtable = parent.vtable.as_ref().unwrap();
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

                let ptr = vtable
                    .subtype_overflow
                    .offset(depth as isize - DISPLAY_SIZE as isize) as
                          *mut _;

                *ptr = &**vtable as *const _;
            }

        } else {
            depth_fixed = depth;

            vtable.subtype_display[depth] = &**vtable as *const _;
        }

        vtable.subtype_depth = depth as i32;
        vtable.subtype_display[0..depth_fixed].clone_from_slice(&parent_vtable.subtype_display
                                                                     [0..depth_fixed]);

        depth

    } else {
        vtable.subtype_depth = 0;
        vtable.subtype_display[0] = &**vtable as *const _;

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
    use class::ClassId;
    use ctxt::SemContext;
    use dora_parser::error::msg::Msg;
    use dora_parser::interner::Name;
    use object::Header;
    use mem;
    use semck::tests::{err, errors, ok, ok_with_test, pos};
    use vtable::VTable;

    #[test]
    fn test_class_size() {
        assert_eq!(Header::size(), class_size("class Foo"));
        assert_eq!(Header::size() + mem::ptr_width(), class_size("class Foo(let a: int)"));
        assert_eq!(Header::size() + 8, class_size("class Foo(let a: long)"));
        assert_eq!(Header::size() + mem::ptr_width(), class_size("class Foo(let a: bool)"));
        assert_eq!(Header::size() + mem::ptr_width(),
                   class_size("class Foo(let a: Str)"));
    }

    fn class_size(code: &'static str) -> i32 {
        ok_with_test(code, |ctxt| {
            let name = ctxt.interner.intern("Foo");
            let cid = ctxt.sym.borrow().get_class(name).unwrap();
            let cls = ctxt.classes[cid].borrow();

            cls.size
        })
    }

    #[test]
    fn test_internal_class_size() {
        ok_with_test("", |ctxt| {
            assert_eq!(0, class_size_name(ctxt, "Array"));
            assert_eq!(0, class_size_name(ctxt, "Str"));
            assert_eq!(1, class_size_name(ctxt, "bool"));
            assert_eq!(4, class_size_name(ctxt, "int"));
            assert_eq!(1, class_size_name(ctxt, "byte"));
            assert_eq!(8, class_size_name(ctxt, "long"));
        });
    }

    fn class_size_name(ctxt: &SemContext, name: &'static str) -> i32 {
        let name = ctxt.interner.intern(name);
        let cid = ctxt.sym.borrow().get_class(name).unwrap();
        let cls = ctxt.classes[cid].borrow();

        cls.size
    }

    #[test]
    fn test_super_size() {
        ok_with_test("open class A { var a: int; }
            open class B: A { var b1: int; var b2: int; }
            class C: B { var c: Str; }",
                     |ctxt| {
            check_class(ctxt, "A", mem::ptr_width(), Some("Object"));
            check_field(ctxt, "A", "a", Header::size());
            check_class(ctxt, "B", 2 * mem::ptr_width(), Some("A"));
            check_field(ctxt, "B", "b1", Header::size() + 4);
            check_field(ctxt, "B", "b2", Header::size() + 2 * 4);

            // if pointer size is 32-bit, we need 4 words, on
            // 64-bit systems we need 3 words
            let words = if mem::ptr_width() == 4 { 4 } else { 3 };
            check_class(ctxt, "C", words * mem::ptr_width(), Some("B"));

            // if pointer size is 32-bit, we do not need padding
            let offset = if mem::ptr_width() == 4 { 3 * 4 } else { 4 * 4 };
            check_field(ctxt, "C", "c", Header::size() + offset);
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
            Msg::MethodNotOverridable("f".into()));
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
    fn test_overload_method_in_super_class() {
        errors("open class A { fun f() {} }
            class B: A { fun f(a: int) {} }",
               &[(pos(2, 26), Msg::MissingOverride("f".into())),
                 (pos(2, 26), Msg::MethodNotOverridable("f".into()))]);

        ok("open class A { static fun f() {} }
            class B: A { static fun f(a: int) {} }");
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
            assert_eq!(vtable_by_name(ctxt, "A", |f| f.subtype_depth), 1);
            assert_eq!(vtable_by_name(ctxt, "B", |f| f.subtype_depth), 1);
        });
    }

    #[test]
    fn test_depth_with_multiple_levels() {
        ok_with_test("open class A { } open class B: A { }
                      class C: B { }",
                     |ctxt| {
            assert_eq!(vtable_by_name(ctxt, "A", |f| f.subtype_depth), 1);
            assert_eq!(vtable_by_name(ctxt, "B", |f| f.subtype_depth), 2);
            assert_eq!(vtable_by_name(ctxt, "C", |f| f.subtype_depth), 3);

            {
                let vtable = vtable_by_name(ctxt, "C", |vtable| {
                    assert!(vtable.subtype_display[4].is_null());
                    vtable as *const _
                });

                assert_name(ctxt, vtable_name(vtable), "C");
                assert_name(ctxt, vtable_display_name(vtable, 0), "Object");
                assert_name(ctxt, vtable_display_name(vtable, 1), "A");
                assert_name(ctxt, vtable_display_name(vtable, 2), "B");
                assert_name(ctxt, vtable_display_name(vtable, 3), "C");
            }

            {
                let vtable = vtable_by_name(ctxt, "B", |vtable| {
                    assert!(vtable.subtype_display[3].is_null());
                    vtable as *const _
                });

                assert_name(ctxt, vtable_name(vtable), "B");
                assert_name(ctxt, vtable_display_name(vtable, 0), "Object");
                assert_name(ctxt, vtable_display_name(vtable, 1), "A");
                assert_name(ctxt, vtable_display_name(vtable, 2), "B");
            }

            {
                let vtable = vtable_by_name(ctxt, "A", |vtable| {
                    assert!(vtable.subtype_display[2].is_null());
                    vtable as *const _
                });

                assert_name(ctxt, vtable_name(vtable), "A");
                assert_name(ctxt, vtable_display_name(vtable, 0), "Object");
                assert_name(ctxt, vtable_display_name(vtable, 1), "A");
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
            assert_eq!(vtable_by_name(ctxt, "Object", |f| f.subtype_depth), 0);
            assert_eq!(vtable_by_name(ctxt, "L1", |f| f.subtype_depth), 1);
            assert_eq!(vtable_by_name(ctxt, "L2", |f| f.subtype_depth), 2);
            assert_eq!(vtable_by_name(ctxt, "L3", |f| f.subtype_depth), 3);
            assert_eq!(vtable_by_name(ctxt, "L4", |f| f.subtype_depth), 4);
            assert_eq!(vtable_by_name(ctxt, "L5", |f| f.subtype_depth), 5);
            assert_eq!(vtable_by_name(ctxt, "L6", |f| f.subtype_depth), 6);
            assert_eq!(vtable_by_name(ctxt, "L7", |f| f.subtype_depth), 7);

            let vtable = vtable_by_name(ctxt, "L7", |vtable| {
                assert!(!vtable.subtype_overflow.is_null());
                assert_eq!(vtable_by_name(ctxt, "L6", |v| v as *const _),
                           vtable.get_subtype_overflow(0));
                assert_eq!(vtable_by_name(ctxt, "L7", |v| v as *const _),
                           vtable.get_subtype_overflow(1));

                vtable as *const _
            });

            assert_name(ctxt, vtable_display_name(vtable, 1), "L1");
            assert_name(ctxt, vtable_display_name(vtable, 2), "L2");
            assert_name(ctxt, vtable_display_name(vtable, 3), "L3");
            assert_name(ctxt, vtable_display_name(vtable, 4), "L4");
            assert_name(ctxt, vtable_display_name(vtable, 5), "L5");

            let vtable = vtable_by_name(ctxt, "L10", |vtable| {
                assert!(!vtable.subtype_overflow.is_null());
                assert_eq!(vtable_by_name(ctxt, "L6", |v| v as *const _),
                           vtable.get_subtype_overflow(0));
                assert_eq!(vtable_by_name(ctxt, "L7", |v| v as *const _),
                           vtable.get_subtype_overflow(1));
                assert_eq!(vtable_by_name(ctxt, "L8", |v| v as *const _),
                           vtable.get_subtype_overflow(2));
                assert_eq!(vtable_by_name(ctxt, "L9", |v| v as *const _),
                           vtable.get_subtype_overflow(3));
                assert_eq!(vtable_by_name(ctxt, "L10", |v| v as *const _),
                           vtable.get_subtype_overflow(4));

                vtable as *const _
            });

            assert_name(ctxt, vtable_display_name(vtable, 0), "Object");
            assert_name(ctxt, vtable_display_name(vtable, 1), "L1");
            assert_name(ctxt, vtable_display_name(vtable, 2), "L2");
            assert_name(ctxt, vtable_display_name(vtable, 3), "L3");
            assert_name(ctxt, vtable_display_name(vtable, 4), "L4");
            assert_name(ctxt, vtable_display_name(vtable, 5), "L5");
        });
    }

    #[test]
    fn test_ref_fields() {
        let header = Header::size();
        let pw = mem::ptr_width();

        ok_with_test("open class A(let a: A) class B(a: A, let b: B) : A(a)",
                     |ctxt| {
            let cls = cls_by_name(ctxt, "A");
            let cls = ctxt.classes[cls].borrow();
            assert_eq!(vec![header], cls.ref_fields);

            let cls = cls_by_name(ctxt, "B");
            let cls = ctxt.classes[cls].borrow();
            assert_eq!(vec![header, header + pw], cls.ref_fields);
        });

        ok_with_test("class A(let x: Data, d: Data): B(d)
                      open class B(let y: Data)
                      class Data(let data: int)",
                     |ctxt| {
            let cls = cls_by_name(ctxt, "A");
            let cls = ctxt.classes[cls].borrow();
            assert_eq!(vec![header, header + pw], cls.ref_fields);

            let cls = cls_by_name(ctxt, "B");
            let cls = ctxt.classes[cls].borrow();
            assert_eq!(vec![header], cls.ref_fields);
        });
    }

    #[test]
    fn test_struct_size() {
        ok_with_test("struct Foo { a: int, b: int }
                      struct Foo1 { a: bool, b: int, c: bool }
                      struct Bar { }",
                     |ctxt| {
                         assert_eq!(8, ctxt.structs[0].borrow().size);
                         assert_eq!(12, ctxt.structs[1].borrow().size);
                         assert_eq!(0, ctxt.structs[2].borrow().size);
                     });
    }

    #[test]
    fn test_struct_in_struct() {
        ok_with_test("struct Foo { a: bool, bar: Bar }
                      struct Bar { a: int }",
                     |ctxt| {
                         assert_eq!(8, ctxt.structs[0].borrow().size);
                     });

        ok_with_test("struct Bar { a: int }
                      struct Foo { a: bool, bar: Bar }",
                     |ctxt| {
                         assert_eq!(8, ctxt.structs[1].borrow().size);
                     });

        err("struct Foo { a: int, bar: Bar }
             struct Bar { b: int, foo: Foo }",
            pos(2, 35),
            Msg::RecursiveStructure);
    }

    #[test]
    fn test_class_in_struct() {
        ok_with_test("class Foo(a: bool, b: int)
                      struct Bar { a: int, foo: Foo }",
                     |ctxt| {
                         assert_eq!(2 * mem::ptr_width(), ctxt.structs[0].borrow().size);
                     });
    }

    #[test]
    fn test_struct_in_class() {
        ok_with_test("class Foo { var bar: Bar; }
                      struct Bar { a: int, foo: Foo }",
                     |ctxt| {
                         let cls = cls_by_name(ctxt, "Foo");
                         let cls = ctxt.classes[cls].borrow();
                         assert_eq!(Header::size() + 2 * mem::ptr_width(), cls.size);
                         assert_eq!(2 * mem::ptr_width(), ctxt.structs[0].borrow().size);
                     });
    }

    fn assert_name<'a, 'ast>(ctxt: &'a SemContext<'ast>, a: Name, b: &'static str) {
        let bname = ctxt.interner.intern(b);

        println!("{} {}", ctxt.interner.str(a), b);

        assert_eq!(a, bname);
    }

    fn vtable_name(vtable: *const VTable) -> Name {
        let cls = unsafe { &*(*vtable).classptr };

        cls.name
    }

    fn vtable_display_name(vtable: *const VTable, ind: usize) -> Name {
        unsafe {
            let vtable = (*vtable).subtype_display[ind] as *const VTable;
            let cls = &*(*vtable).classptr;

            cls.name
        }
    }

    fn vtable_by_name<'a, 'ast: 'a, F, R>(ctxt: &'a SemContext<'ast>,
                                          name: &'static str,
                                          fct: F)
                                          -> R
        where F: FnOnce(&VTable) -> R
    {
        let cid = cls_by_name(ctxt, name);
        let cls = ctxt.classes[cid].borrow();
        let vtable = cls.vtable.as_ref().unwrap();

        fct(vtable)
    }

    fn cls_by_name<'a, 'ast>(ctxt: &'a SemContext<'ast>, name: &'static str) -> ClassId {
        let name = ctxt.interner.intern(name);
        ctxt.sym
            .borrow()
            .get_class(name)
            .expect("class not found")
    }

    fn check_class<'ast>(ctxt: &SemContext<'ast>,
                         name: &'static str,
                         size: i32,
                         parent: Option<&'static str>) {
        let name = ctxt.interner.intern(name);
        let cls_id = ctxt.sym.borrow().get_class(name).unwrap();

        let parent_id = parent
            .map(|name| ctxt.interner.intern(name))
            .map(|name| ctxt.sym.borrow().get_class(name).unwrap());

        let cls = ctxt.classes[cls_id].borrow();
        assert_eq!(parent_id, cls.parent_class);
        assert_eq!(Header::size() + size, cls.size);
    }

    fn check_field<'ast>(ctxt: &SemContext<'ast>,
                         cls_name: &'static str,
                         field_name: &'static str,
                         offset: i32) {
        let cls_name = ctxt.interner.intern(cls_name);
        let field_name = ctxt.interner.intern(field_name);
        let cls_id = ctxt.sym.borrow().get_class(cls_name).unwrap();
        let cls = ctxt.classes[cls_id].borrow();

        for field in &cls.fields {
            if field_name == field.name {
                assert_eq!(offset, field.offset);
                return;
            }
        }

        unreachable!();
    }
}
