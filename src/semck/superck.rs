use std::collections::{HashMap, HashSet};

use class::{Class, ClassId};
use ctxt::{Fct, SemContext};
use dora_parser::error::msg::Msg;

pub fn check<'ast>(ctxt: &mut SemContext<'ast>) {
    cycle_detection(ctxt);

    if ctxt.diag.lock().has_errors() {
        return;
    }

    // determine_struct_sizes(ctxt);
    determine_vtables(ctxt);
}

fn cycle_detection<'ast>(ctxt: &mut SemContext<'ast>) {
    for cls in ctxt.classes.iter() {
        let cls = cls.read();

        let mut map: HashSet<ClassId> = HashSet::new();
        map.insert(cls.id);

        let mut parent = cls.parent_class;

        while parent.is_some() {
            let p = parent.unwrap();

            if !map.insert(p) {
                ctxt.diag.lock().report_without_path(cls.pos, Msg::CycleInHierarchy);
                break;
            }

            let cls = ctxt.classes.idx(p);
            let cls = cls.read();
            parent = cls.parent_class;
        }
    }
}

fn determine_vtables<'ast>(ctxt: &SemContext<'ast>) {
    let mut lens = HashMap::new();

    for cls in ctxt.classes.iter() {
        let mut cls = cls.write();
        determine_vtable(ctxt, &mut lens, &mut *cls);
    }
}

fn determine_vtable<'ast>(
    ctxt: &SemContext<'ast>,
    lens: &mut HashMap<ClassId, u32>,
    cls: &mut Class,
) -> u32 {
    let mut vtable_len = if let Some(parent_cls_id) = cls.parent_class {
        let parent_vtable_len = lens.get(&parent_cls_id).map(|&len| len);

        if let Some(parent_vtable_len) = parent_vtable_len {
            parent_vtable_len
        } else {
            let parent = ctxt.classes.idx(parent_cls_id);
            let mut parent = parent.write();
            determine_vtable(ctxt, lens, &mut *parent)
        }
    } else {
        0
    };

    for &mid in &cls.methods {
        let fct = ctxt.fcts.idx(mid);
        let mut fct = fct.write();

        assert!(fct.vtable_index.is_none());

        if fct.is_virtual() {
            let vtable_index = if let Some(overrides) = fct.overrides {
                let overrides = ctxt.fcts.idx(overrides);
                let overrides = overrides.read();
                overrides.vtable_index.unwrap()
            } else {
                let vtable_index = vtable_len;
                vtable_len += 1;

                vtable_index
            };

            fct.vtable_index = Some(vtable_index);
        }
    }

    cls.vtable_len = vtable_len;
    lens.insert(cls.id, vtable_len);

    vtable_len
}

// fn determine_struct_sizes<'ast>(ctxt: &SemContext<'ast>) {
//     let mut path = Vec::new();
//     let mut sizes = HashMap::new();

//     for struc in ctxt.structs.iter() {
//         let mut struc = struc.borrow_mut();
//         determine_struct_size(ctxt, &mut path, &mut sizes, &mut *struc);
//     }
// }

// fn determine_struct_size<'ast>(
//     ctxt: &SemContext<'ast>,
//     path: &mut Vec<StructId>,
//     sizes: &mut HashMap<StructId, (i32, i32)>,
//     struc: &mut StructData,
// ) -> (i32, i32) {
//     let mut size = 0;
//     let mut align = 0;

//     path.push(struc.id);

//     for field in &mut struc.fields {
//         let (field_size, field_align) = if let BuiltinType::Struct(id, _) = field.ty {
//             if let Some(&(size, align)) = sizes.get(&id) {
//                 (size, align)
//             } else {
//                 if path.iter().find(|&&x| x == id).is_some() {
//                     ctxt.diag
//                         .borrow_mut()
//                         .report(field.pos, Msg::RecursiveStructure);
//                     return (0, 0);
//                 }

//                 let mut struc = ctxt.structs[id].borrow_mut();
//                 determine_struct_size(ctxt, path, sizes, &mut *struc)
//             }
//         } else {
//             let ty = field.ty;

//             (ty.size(ctxt), ty.align(ctxt))
//         };

//         field.offset = mem::align_i32(size, field_align);

//         size = field.offset + field_size;
//         align = max(align, field_align);
//     }

//     size = mem::align_i32(size, align);

//     // struc.size = size;
//     // struc.align = align;

//     sizes.insert(struc.id, (size, align));
//     path.pop();

//     (size, align)
// }

pub fn check_override<'ast>(ctxt: &SemContext<'ast>) {
    for cls in ctxt.classes.iter() {
        let cls = cls.read();

        for &fct_id in &cls.methods {
            let fct = ctxt.fcts.idx(fct_id);
            let mut fct = fct.write();
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
            .lock()
            .report_without_path(fct.pos(), Msg::SuperfluousOpen(name));
        return;
    }

    if cls.parent_class.is_none() {
        if fct.has_override {
            let name = ctxt.interner.str(fct.name).to_string();
            ctxt.diag
                .lock()
                .report_without_path(fct.pos(), Msg::SuperfluousOverride(name));
            return;
        }

        return;
    }

    let parent = cls.parent_class.unwrap();
    let parent = ctxt.classes.idx(parent);
    let parent = parent.read();

    let super_method = parent.find_method(ctxt, fct.name, false);

    if let Some(super_method) = super_method {
        let super_method = ctxt.fcts.idx(super_method);
        let super_method = super_method.read();

        if !fct.has_override {
            let name = ctxt.interner.str(fct.name).to_string();
            ctxt.diag
                .lock()
                .report_without_path(fct.pos(), Msg::MissingOverride(name));
        }

        if !(super_method.has_open || super_method.has_override) || super_method.has_final {
            let name = ctxt.interner.str(fct.name).to_string();
            ctxt.diag
                .lock()
                .report_without_path(fct.pos(), Msg::MethodNotOverridable(name));
        }

        if super_method.throws != fct.throws {
            let name = ctxt.interner.str(fct.name).to_string();
            ctxt.diag
                .lock()
                .report_without_path(fct.pos(), Msg::ThrowsDifference(name));
        }

        if super_method.return_type != fct.return_type {
            let pos = fct.pos();
            let fct = fct.return_type.name(ctxt);
            let sup = super_method.return_type.name(ctxt);
            ctxt.diag
                .lock()
                .report_without_path(pos, Msg::ReturnTypeMismatch(fct, sup));
        }

        fct.overrides = Some(super_method.id);
    } else {
        if fct.has_override {
            let name = ctxt.interner.str(fct.name).to_string();
            ctxt.diag
                .lock()
                .report_without_path(fct.pos(), Msg::SuperfluousOverride(name));
        }
    }
}

#[cfg(test)]
mod tests {
    use ctxt::SemContext;
    use dora_parser::error::msg::Msg;
    use dora_parser::interner::Name;
    use semck::tests::{err, errors, ok, ok_with_test, pos};

    // #[test]
    // fn test_class_size() {
    //     assert_eq!(Header::size(), class_size("class Foo"));
    //     assert_eq!(Header::size() + mem::ptr_width(), class_size("class Foo(let a: int)"));
    //     assert_eq!(Header::size() + 8, class_size("class Foo(let a: long)"));
    //     assert_eq!(Header::size() + mem::ptr_width(), class_size("class Foo(let a: bool)"));
    //     assert_eq!(Header::size() + mem::ptr_width(),
    //                class_size("class Foo(let a: String)"));
    // }

    // fn class_size(code: &'static str) -> i32 {
    //     ok_with_test(code, |ctxt| {
    //         let name = ctxt.interner.intern("Foo");
    //         let cid = ctxt.sym.borrow().get_class(name).unwrap();
    //         let cls = ctxt.classes[cid].borrow();

    //         cls.size
    //     })
    // }

    // #[test]
    // fn test_intrinsic_class_size() {
    //     ok_with_test("", |ctxt| {
    //         assert_eq!(0, class_size_name(ctxt, "Array"));
    //         assert_eq!(0, class_size_name(ctxt, "String"));
    //         assert_eq!(1, class_size_name(ctxt, "bool"));
    //         assert_eq!(4, class_size_name(ctxt, "int"));
    //         assert_eq!(1, class_size_name(ctxt, "byte"));
    //         assert_eq!(8, class_size_name(ctxt, "long"));
    //     });
    // }

    // fn class_size_name(ctxt: &SemContext, name: &'static str) -> i32 {
    //     let name = ctxt.interner.intern(name);
    //     let cid = ctxt.sym.borrow().get_class(name).unwrap();
    //     let cls = ctxt.classes[cid].borrow();

    //     cls.size
    // }

    // #[test]
    // fn test_super_size() {
    //     ok_with_test("open class A { var a: int; }
    //         open class B: A { var b1: int; var b2: int; }
    //         class C: B { var c: String; }",
    //                  |ctxt| {
    //         check_class(ctxt, "A", mem::ptr_width(), Some("Object"));
    //         check_field(ctxt, "A", "a", Header::size());
    //         check_class(ctxt, "B", 2 * mem::ptr_width(), Some("A"));
    //         check_field(ctxt, "B", "b1", Header::size() + 4);
    //         check_field(ctxt, "B", "b2", Header::size() + 2 * 4);

    //         // if pointer size is 32-bit, we need 4 words, on
    //         // 64-bit systems we need 3 words
    //         let words = if mem::ptr_width() == 4 { 4 } else { 3 };
    //         check_class(ctxt, "C", words * mem::ptr_width(), Some("B"));

    //         // if pointer size is 32-bit, we do not need padding
    //         let offset = if mem::ptr_width() == 4 { 3 * 4 } else { 4 * 4 };
    //         check_field(ctxt, "C", "c", Header::size() + offset);
    //     });
    // }

    #[test]
    fn test_cycle() {
        errors(
            "open class A: B open class B: A",
            &[
                (pos(1, 6), Msg::CycleInHierarchy),
                (pos(1, 22), Msg::CycleInHierarchy),
            ],
        );
    }

    #[test]
    fn test_superfluous_override() {
        err(
            "class A { override fun f() {} }",
            pos(1, 20),
            Msg::SuperfluousOverride("f".into()),
        );
        err(
            "open class B { } class A: B { override fun f() {} }",
            pos(1, 40),
            Msg::SuperfluousOverride("f".into()),
        );
        err(
            "open class B { fun g() {} } class A: B { override fun f() {} }",
            pos(1, 51),
            Msg::SuperfluousOverride("f".into()),
        );
        err(
            "open class B { fun f(a: int) {} } class A: B { override fun f() {} }",
            pos(1, 57),
            Msg::MethodNotOverridable("f".into()),
        );
    }

    #[test]
    fn test_override() {
        err(
            "open class A { fun f() {} } class B: A { override fun f() {} }",
            pos(1, 51),
            Msg::MethodNotOverridable("f".into()),
        );
        ok("open class A { open fun f() {} } class B: A { override fun f() {} }");
        ok("open class A { open fun f() {} }
            open class B: A { override fun f() {} }
            open class C: B { override fun f() {} }");
        err(
            "open class A { open fun f() {} } class B: A { fun f() {} }",
            pos(1, 47),
            Msg::MissingOverride("f".into()),
        );
        err(
            "open class A { open fun f() {} }
             open class B: A { final override fun f() {} }
             class C: B { override fun f() {} }",
            pos(3, 36),
            Msg::MethodNotOverridable("f".into()),
        );
    }

    #[test]
    fn test_overload_method_in_super_class() {
        errors(
            "open class A { fun f() {} }
            class B: A { fun f(a: int) {} }",
            &[
                (pos(2, 26), Msg::MissingOverride("f".into())),
                (pos(2, 26), Msg::MethodNotOverridable("f".into())),
            ],
        );

        ok("open class A { static fun f() {} }
            class B: A { static fun f(a: int) {} }");
    }

    #[test]
    fn test_override_with_wrong_return_type() {
        err(
            "open class A { open fun f() {} }
             class B: A { override fun f() -> int { return 1; } }",
            pos(2, 36),
            Msg::ReturnTypeMismatch("int".into(), "()".into()),
        );
    }

    #[test]
    fn test_override_with_missing_throws() {
        err(
            "open class A { open fun f() throws {} }
             class B: A { override fun f() {} }",
            pos(2, 36),
            Msg::ThrowsDifference("f".into()),
        );
    }

    #[test]
    fn test_open() {
        ok("open class A { open fun f() {} }");
    }

    #[test]
    fn test_superfluous_open() {
        err(
            "class A { open fun f() {} }",
            pos(1, 16),
            Msg::SuperfluousOpen("f".into()),
        );
    }

    #[test]
    fn test_final() {
        ok("open class A { final fun f() {} }");
    }

    #[test]
    fn test_vtable_index_and_len() {
        ok_with_test("class A {}", |ctxt| {
            let cls_id = ctxt.cls_by_name("A");
            let cls = ctxt.classes.idx(cls_id);
            let cls = cls.read();
            assert_eq!(cls.vtable_len, 0);
        });

        ok_with_test("open class A { open fun f() {} }", |ctxt| {
            let cls_id = ctxt.cls_by_name("A");
            let cls = ctxt.classes.idx(cls_id);
            let cls = cls.read();
            assert_eq!(cls.vtable_len, 1);
        });

        ok_with_test(
            "open class A { open fun f() {} }
                      open class B: A { override fun f() {}
                                        open fun g() {} }",
            |ctxt| {
                let cls_id = ctxt.cls_by_name("A");
                let cls = ctxt.classes.idx(cls_id);
                let cls = cls.read();
                assert_eq!(cls.vtable_len, 1);

                let cls_id = ctxt.cls_by_name("B");
                let cls = ctxt.classes.idx(cls_id);
                let cls = cls.read();
                assert_eq!(cls.vtable_len, 2);
            },
        );
    }

    // #[test]
    // fn test_depth() {
    //     ok_with_test("class A { } class B { }", |ctxt| {
    //         assert_eq!(vtable_by_name(ctxt, "A", |f| f.subtype_depth), 1);
    //         assert_eq!(vtable_by_name(ctxt, "B", |f| f.subtype_depth), 1);
    //     });
    // }

    // #[test]
    // fn test_depth_with_multiple_levels() {
    //     ok_with_test("open class A { } open class B: A { }
    //                   class C: B { }",
    //                  |ctxt| {
    //         assert_eq!(vtable_by_name(ctxt, "A", |f| f.subtype_depth), 1);
    //         assert_eq!(vtable_by_name(ctxt, "B", |f| f.subtype_depth), 2);
    //         assert_eq!(vtable_by_name(ctxt, "C", |f| f.subtype_depth), 3);

    //         {
    //             let vtable = vtable_by_name(ctxt, "C", |vtable| {
    //                 assert!(vtable.subtype_display[4].is_null());
    //                 vtable as *const _
    //             });

    //             assert_name(ctxt, vtable_name(vtable), "C");
    //             assert_name(ctxt, vtable_display_name(vtable, 0), "Object");
    //             assert_name(ctxt, vtable_display_name(vtable, 1), "A");
    //             assert_name(ctxt, vtable_display_name(vtable, 2), "B");
    //             assert_name(ctxt, vtable_display_name(vtable, 3), "C");
    //         }

    //         {
    //             let vtable = vtable_by_name(ctxt, "B", |vtable| {
    //                 assert!(vtable.subtype_display[3].is_null());
    //                 vtable as *const _
    //             });

    //             assert_name(ctxt, vtable_name(vtable), "B");
    //             assert_name(ctxt, vtable_display_name(vtable, 0), "Object");
    //             assert_name(ctxt, vtable_display_name(vtable, 1), "A");
    //             assert_name(ctxt, vtable_display_name(vtable, 2), "B");
    //         }

    //         {
    //             let vtable = vtable_by_name(ctxt, "A", |vtable| {
    //                 assert!(vtable.subtype_display[2].is_null());
    //                 vtable as *const _
    //             });

    //             assert_name(ctxt, vtable_name(vtable), "A");
    //             assert_name(ctxt, vtable_display_name(vtable, 0), "Object");
    //             assert_name(ctxt, vtable_display_name(vtable, 1), "A");
    //         }
    //     });
    // }

    // #[test]
    // fn test_depth_greater_display_size() {
    //     ok_with_test("  open class L1 { }
    //                     open class L2: L1 { }
    //                     open class L3: L2 { }
    //                     open class L4: L3 { }
    //                     open class L5: L4 { }
    //                     open class L6: L5 { }
    //                     open class L7: L6 { }
    //                     open class L8: L7 { }
    //                     open class L9: L8 { }
    //                     class L10: L9 { }",
    //                  |ctxt| {
    //         assert_eq!(vtable_by_name(ctxt, "Object", |f| f.subtype_depth), 0);
    //         assert_eq!(vtable_by_name(ctxt, "L1", |f| f.subtype_depth), 1);
    //         assert_eq!(vtable_by_name(ctxt, "L2", |f| f.subtype_depth), 2);
    //         assert_eq!(vtable_by_name(ctxt, "L3", |f| f.subtype_depth), 3);
    //         assert_eq!(vtable_by_name(ctxt, "L4", |f| f.subtype_depth), 4);
    //         assert_eq!(vtable_by_name(ctxt, "L5", |f| f.subtype_depth), 5);
    //         assert_eq!(vtable_by_name(ctxt, "L6", |f| f.subtype_depth), 6);
    //         assert_eq!(vtable_by_name(ctxt, "L7", |f| f.subtype_depth), 7);

    //         let vtable = vtable_by_name(ctxt, "L7", |vtable| {
    //             assert!(!vtable.subtype_overflow.is_null());
    //             assert_eq!(vtable_by_name(ctxt, "L6", |v| v as *const _),
    //                        vtable.get_subtype_overflow(0));
    //             assert_eq!(vtable_by_name(ctxt, "L7", |v| v as *const _),
    //                        vtable.get_subtype_overflow(1));

    //             vtable as *const _
    //         });

    //         assert_name(ctxt, vtable_display_name(vtable, 1), "L1");
    //         assert_name(ctxt, vtable_display_name(vtable, 2), "L2");
    //         assert_name(ctxt, vtable_display_name(vtable, 3), "L3");
    //         assert_name(ctxt, vtable_display_name(vtable, 4), "L4");
    //         assert_name(ctxt, vtable_display_name(vtable, 5), "L5");

    //         let vtable = vtable_by_name(ctxt, "L10", |vtable| {
    //             assert!(!vtable.subtype_overflow.is_null());
    //             assert_eq!(vtable_by_name(ctxt, "L6", |v| v as *const _),
    //                        vtable.get_subtype_overflow(0));
    //             assert_eq!(vtable_by_name(ctxt, "L7", |v| v as *const _),
    //                        vtable.get_subtype_overflow(1));
    //             assert_eq!(vtable_by_name(ctxt, "L8", |v| v as *const _),
    //                        vtable.get_subtype_overflow(2));
    //             assert_eq!(vtable_by_name(ctxt, "L9", |v| v as *const _),
    //                        vtable.get_subtype_overflow(3));
    //             assert_eq!(vtable_by_name(ctxt, "L10", |v| v as *const _),
    //                        vtable.get_subtype_overflow(4));

    //             vtable as *const _
    //         });

    //         assert_name(ctxt, vtable_display_name(vtable, 0), "Object");
    //         assert_name(ctxt, vtable_display_name(vtable, 1), "L1");
    //         assert_name(ctxt, vtable_display_name(vtable, 2), "L2");
    //         assert_name(ctxt, vtable_display_name(vtable, 3), "L3");
    //         assert_name(ctxt, vtable_display_name(vtable, 4), "L4");
    //         assert_name(ctxt, vtable_display_name(vtable, 5), "L5");
    //     });
    // }

    // #[test]
    // fn test_ref_fields() {
    //     let header = Header::size();
    //     let pw = mem::ptr_width();

    //     ok_with_test("open class A(let a: A) class B(a: A, let b: B) : A(a)",
    //                  |ctxt| {
    //         let cls = cls_by_name(ctxt, "A");
    //         let cls = ctxt.classes[cls].borrow();
    //         assert_eq!(vec![header], cls.ref_fields);

    //         let cls = cls_by_name(ctxt, "B");
    //         let cls = ctxt.classes[cls].borrow();
    //         assert_eq!(vec![header, header + pw], cls.ref_fields);
    //     });

    //     ok_with_test("class A(let x: Data, d: Data): B(d)
    //                   open class B(let y: Data)
    //                   class Data(let data: int)",
    //                  |ctxt| {
    //         let cls = cls_by_name(ctxt, "A");
    //         let cls = ctxt.classes[cls].borrow();
    //         assert_eq!(vec![header, header + pw], cls.ref_fields);

    //         let cls = cls_by_name(ctxt, "B");
    //         let cls = ctxt.classes[cls].borrow();
    //         assert_eq!(vec![header], cls.ref_fields);
    //     });
    // }

    // #[test]
    // fn test_struct_size() {
    //     ok_with_test(
    //         "struct Foo { a: int, b: int }
    //                   struct Foo1 { a: bool, b: int, c: bool }
    //                   struct Bar { }",
    //         |ctxt| {
    //             assert_eq!(8, ctxt.structs[0].borrow().size);
    //             assert_eq!(12, ctxt.structs[1].borrow().size);
    //             assert_eq!(0, ctxt.structs[2].borrow().size);
    //         },
    //     );
    // }

    // #[test]
    // fn test_struct_in_struct() {
    //     ok_with_test(
    //         "struct Foo { a: bool, bar: Bar }
    //                   struct Bar { a: int }",
    //         |ctxt| {
    //             assert_eq!(8, ctxt.structs[0].borrow().size);
    //         },
    //     );

    //     ok_with_test(
    //         "struct Bar { a: int }
    //                   struct Foo { a: bool, bar: Bar }",
    //         |ctxt| {
    //             assert_eq!(8, ctxt.structs[1].borrow().size);
    //         },
    //     );

    //     err(
    //         "struct Foo { a: int, bar: Bar }
    //          struct Bar { b: int, foo: Foo }",
    //         pos(2, 35),
    //         Msg::RecursiveStructure,
    //     );
    // }

    // #[test]
    // fn test_class_in_struct() {
    //     ok_with_test("class Foo(a: bool, b: int)
    //                   struct Bar { a: int, foo: Foo }",
    //                  |ctxt| {
    //                      assert_eq!(2 * mem::ptr_width(), ctxt.structs[0].borrow().size);
    //                  });
    // }

    // #[test]
    // fn test_struct_in_class() {
    //     ok_with_test("class Foo { var bar: Bar; }
    //                   struct Bar { a: int, foo: Foo }",
    //                  |ctxt| {
    //                      let cls = cls_by_name(ctxt, "Foo");
    //                      let cls = ctxt.classes[cls].borrow();
    //                      assert_eq!(Header::size() + 2 * mem::ptr_width(), cls.size);
    //                      assert_eq!(2 * mem::ptr_width(), ctxt.structs[0].borrow().size);
    //                  });
    // }

    fn assert_name<'a, 'ast>(ctxt: &'a SemContext<'ast>, a: Name, b: &'static str) {
        let bname = ctxt.interner.intern(b);

        println!("{} {}", ctxt.interner.str(a), b);

        assert_eq!(a, bname);
    }

    // fn vtable_name(vtable: *const VTable) -> Name {
    //     let cls = unsafe { &*(*vtable).classptr };

    //     cls.name
    // }

    // fn vtable_display_name(vtable: *const VTable, ind: usize) -> Name {
    //     unsafe {
    //         let vtable = (*vtable).subtype_display[ind] as *const VTable;
    //         let cls = &*(*vtable).classptr;

    //         cls.name
    //     }
    // }

    // fn vtable_by_name<'a, 'ast: 'a, F, R>(ctxt: &'a SemContext<'ast>,
    //                                       name: &'static str,
    //                                       fct: F)
    //                                       -> R
    //     where F: FnOnce(&VTable) -> R
    // {
    //     let cid = cls_by_name(ctxt, name);
    //     let cls = ctxt.classes[cid].borrow();
    //     let vtable = cls.vtable.as_ref().unwrap();

    //     fct(vtable)
    // }

    // fn check_class<'ast>(ctxt: &SemContext<'ast>,
    //                      name: &'static str,
    //                      size: i32,
    //                      parent: Option<&'static str>) {
    //     let name = ctxt.interner.intern(name);
    //     let cls_id = ctxt.sym.borrow().get_class(name).unwrap();

    //     let parent_id = parent
    //         .map(|name| ctxt.interner.intern(name))
    //         .map(|name| ctxt.sym.borrow().get_class(name).unwrap());

    //     let cls = ctxt.classes[cls_id].borrow();
    //     assert_eq!(parent_id, cls.parent_class);
    //     assert_eq!(Header::size() + size, cls.size);
    // }

    // fn check_field<'ast>(ctxt: &SemContext<'ast>,
    //                      cls_name: &'static str,
    //                      field_name: &'static str,
    //                      offset: i32) {
    //     let cls_name = ctxt.interner.intern(cls_name);
    //     let field_name = ctxt.interner.intern(field_name);
    //     let cls_id = ctxt.sym.borrow().get_class(cls_name).unwrap();
    //     let cls = ctxt.classes[cls_id].borrow();

    //     for field in &cls.fields {
    //         if field_name == field.name {
    //             assert_eq!(offset, field.offset);
    //             return;
    //         }
    //     }

    //     unreachable!();
    // }
}
