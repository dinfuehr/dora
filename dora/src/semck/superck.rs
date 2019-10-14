use std::collections::HashSet;

use crate::class::{Class, ClassId};
use crate::error::msg::SemError;
use crate::vm::{Fct, VM};

pub fn check<'ast>(vm: &mut VM<'ast>) {
    cycle_detection(vm);

    if vm.diag.lock().has_errors() {
        return;
    }

    // determine_struct_sizes(vm);
    determine_vtables(vm);
}

fn cycle_detection<'ast>(vm: &mut VM<'ast>) {
    for cls in vm.classes.iter() {
        let cls = cls.read();

        let mut map: HashSet<ClassId> = HashSet::new();
        map.insert(cls.id);

        let mut parent = cls.parent_class;

        while parent.is_some() {
            let p = parent.unwrap();

            if !map.insert(p) {
                vm.diag
                    .lock()
                    .report(cls.file, cls.pos, SemError::CycleInHierarchy);
                break;
            }

            let cls = vm.classes.idx(p);
            let cls = cls.read();
            parent = cls.parent_class;
        }
    }
}

fn determine_vtables<'ast>(vm: &VM<'ast>) {
    let mut lens = HashSet::new();

    for cls in vm.classes.iter() {
        let mut cls = cls.write();
        if !lens.contains(&cls.id) {
            determine_vtable(vm, &mut lens, &mut *cls);
        }
    }
}

fn determine_vtable<'ast>(vm: &VM<'ast>, lens: &mut HashSet<ClassId>, cls: &mut Class) {
    if let Some(parent_cls_id) = cls.parent_class {
        let parent = vm.classes.idx(parent_cls_id);
        if !lens.contains(&parent_cls_id) {
            let mut parent = parent.write();
            determine_vtable(vm, lens, &mut *parent)
        }

        let parent = parent.read();
        cls.virtual_fcts
            .extend_from_slice(parent.virtual_fcts.as_slice());
    }

    for &mid in &cls.methods {
        let fct = vm.fcts.idx(mid);
        let mut fct = fct.write();

        assert!(fct.vtable_index.is_none());

        if fct.is_virtual() {
            let vtable_index = if let Some(overrides) = fct.overrides {
                let overrides = vm.fcts.idx(overrides);
                let overrides = overrides.read();
                let vtable_index = overrides.vtable_index.unwrap();
                cls.virtual_fcts[vtable_index as usize] = mid;

                vtable_index
            } else {
                let vtable_index = cls.virtual_fcts.len() as u32;
                cls.virtual_fcts.push(mid);

                vtable_index
            };

            fct.vtable_index = Some(vtable_index);
        }
    }

    lens.insert(cls.id);
}

// fn determine_struct_sizes<'ast>(vm: &SemContext<'ast>) {
//     let mut path = Vec::new();
//     let mut sizes = HashMap::new();

//     for struc in vm.structs.iter() {
//         let mut struc = struc.borrow_mut();
//         determine_struct_size(vm, &mut path, &mut sizes, &mut *struc);
//     }
// }

// fn determine_struct_size<'ast>(
//     vm: &SemContext<'ast>,
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
//                     vm.diag
//                         .borrow_mut()
//                         .report(field.pos, SemError::RecursiveStructure);
//                     return (0, 0);
//                 }

//                 let mut struc = vm.structs[id].borrow_mut();
//                 determine_struct_size(vm, path, sizes, &mut *struc)
//             }
//         } else {
//             let ty = field.ty;

//             (ty.size(vm), ty.align(vm))
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

pub fn check_override<'ast>(vm: &VM<'ast>) {
    for cls in vm.classes.iter() {
        let cls = cls.read();

        for &fct_id in &cls.methods {
            let fct = vm.fcts.idx(fct_id);
            let mut fct = fct.write();
            check_fct_modifier(vm, &*cls, &mut *fct);
        }
    }
}

fn check_fct_modifier<'ast>(vm: &VM<'ast>, cls: &Class, fct: &mut Fct<'ast>) {
    // catch: class A { @open fun f() } (A is not derivable)
    // catch: @open @final fun f()
    if fct.has_open && (!cls.has_open || fct.has_final) {
        let name = vm.interner.str(fct.name).to_string();
        vm.diag
            .lock()
            .report(fct.file, fct.pos(), SemError::SuperfluousOpen(name));
        return;
    }

    if cls.parent_class.is_none() {
        if fct.has_override {
            let name = vm.interner.str(fct.name).to_string();
            vm.diag
                .lock()
                .report(fct.file, fct.pos(), SemError::SuperfluousOverride(name));
            return;
        }

        return;
    }

    let parent = cls.parent_class.unwrap();
    let parent = vm.classes.idx(parent);
    let parent = parent.read();

    let super_method = parent.find_method(vm, fct.name, false);

    if let Some(super_method) = super_method {
        let super_method = vm.fcts.idx(super_method);
        let super_method = super_method.read();

        if !fct.has_override {
            let name = vm.interner.str(fct.name).to_string();
            vm.diag
                .lock()
                .report(fct.file, fct.pos(), SemError::MissingOverride(name));
        }

        if !(super_method.has_open || super_method.has_override) || super_method.has_final {
            let name = vm.interner.str(fct.name).to_string();
            vm.diag
                .lock()
                .report(fct.file, fct.pos(), SemError::MethodNotOverridable(name));
        }

        if super_method.throws != fct.throws {
            let name = vm.interner.str(fct.name).to_string();
            vm.diag
                .lock()
                .report(fct.file, fct.pos(), SemError::ThrowsDifference(name));
        }

        if super_method.return_type != fct.return_type {
            let pos = fct.pos();
            let fct_return = fct.return_type.name(vm);
            let sup = super_method.return_type.name(vm);
            vm.diag
                .lock()
                .report(fct.file, pos, SemError::ReturnTypeMismatch(fct_return, sup));
        }

        fct.overrides = Some(super_method.id);
    } else {
        if fct.has_override {
            let name = vm.interner.str(fct.name).to_string();
            vm.diag
                .lock()
                .report(fct.file, fct.pos(), SemError::SuperfluousOverride(name));
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::class::ClassSize;
    use crate::error::msg::SemError;
    use crate::mem;
    use crate::object::Header;
    use crate::semck::tests::{err, errors, ok, ok_with_test, pos};
    use crate::vm::VM;
    use dora_parser::interner::Name;

    #[test]
    fn test_class_size() {
        assert_eq!(
            ClassSize::Fixed(Header::size()),
            class_size("class Foo", "Foo")
        );
        assert_eq!(
            ClassSize::Fixed(Header::size() + mem::ptr_width()),
            class_size("class Foo(let a: Int)", "Foo")
        );
        assert_eq!(
            ClassSize::Fixed(Header::size() + 8),
            class_size("class Foo(let a: Long)", "Foo")
        );
        assert_eq!(
            ClassSize::Fixed(Header::size() + mem::ptr_width()),
            class_size("class Foo(let a: Bool)", "Foo")
        );
        assert_eq!(
            ClassSize::Fixed(Header::size() + mem::ptr_width()),
            class_size("class Foo(let a: String)", "Foo")
        );
    }

    fn class_size(code: &'static str, name: &'static str) -> ClassSize {
        ok_with_test(code, |vm| {
            let id = vm.cls_def_by_name(name);
            let cls = vm.class_defs.idx(id);
            let cls = cls.read();
            cls.size.clone()
        })
    }

    #[test]
    fn test_intrinsic_class_size() {
        ok_with_test("", |vm| {
            assert_eq!(ClassSize::Str, class_size_name(vm, "String"));
            assert_eq!(ClassSize::Fixed(16), class_size_name(vm, "Bool"));
            assert_eq!(ClassSize::Fixed(16), class_size_name(vm, "Int"));
            assert_eq!(ClassSize::Fixed(16), class_size_name(vm, "Byte"));
            assert_eq!(ClassSize::Fixed(16), class_size_name(vm, "Long"));
        });
    }

    fn class_size_name(vm: &VM, name: &'static str) -> ClassSize {
        let id = vm.cls_def_by_name(name);
        let cls = vm.class_defs.idx(id);
        let cls = cls.read();
        cls.size.clone()
    }

    // #[test]
    // fn test_super_size() {
    //     ok_with_test("@open class A { var a: int; }
    //         open class B: A { var b1: int; var b2: int; }
    //         class C: B { var c: String; }",
    //                  |vm| {
    //         check_class(vm, "A", mem::ptr_width(), Some("Object"));
    //         check_field(vm, "A", "a", Header::size());
    //         check_class(vm, "B", 2 * mem::ptr_width(), Some("A"));
    //         check_field(vm, "B", "b1", Header::size() + 4);
    //         check_field(vm, "B", "b2", Header::size() + 2 * 4);

    //         // if pointer size is 32-bit, we need 4 words, on
    //         // 64-bit systems we need 3 words
    //         let words = if mem::ptr_width() == 4 { 4 } else { 3 };
    //         check_class(vm, "C", words * mem::ptr_width(), Some("B"));

    //         // if pointer size is 32-bit, we do not need padding
    //         let offset = if mem::ptr_width() == 4 { 3 * 4 } else { 4 * 4 };
    //         check_field(vm, "C", "c", Header::size() + offset);
    //     });
    // }

    #[test]
    fn test_cycle() {
        errors(
            "@open class A: B @open class B: A",
            &[
                (pos(1, 7), SemError::CycleInHierarchy),
                (pos(1, 24), SemError::CycleInHierarchy),
            ],
        );
    }

    #[test]
    fn test_superfluous_override() {
        err(
            "class A { @override fun f() {} }",
            pos(1, 21),
            SemError::SuperfluousOverride("f".into()),
        );
        err(
            "@open class B { } class A: B { @override fun f() {} }",
            pos(1, 42),
            SemError::SuperfluousOverride("f".into()),
        );
        err(
            "@open class B { fun g() {} } class A: B { @override fun f() {} }",
            pos(1, 53),
            SemError::SuperfluousOverride("f".into()),
        );
        err(
            "@open class B { fun f(a: Int) {} } class A: B { @override fun f() {} }",
            pos(1, 59),
            SemError::MethodNotOverridable("f".into()),
        );
    }

    #[test]
    fn test_override() {
        err(
            "@open class A { fun f() {} } class B: A { @override fun f() {} }",
            pos(1, 53),
            SemError::MethodNotOverridable("f".into()),
        );
        ok("@open class A { @open fun f() {} } class B: A { @override fun f() {} }");
        ok("@open class A { @open fun f() {} }
            @open class B: A { @override fun f() {} }
            @open class C: B { @override fun f() {} }");
        err(
            "@open class A { @open fun f() {} } class B: A { fun f() {} }",
            pos(1, 49),
            SemError::MissingOverride("f".into()),
        );
        err(
            "@open class A { @open fun f() {} }
             @open class B: A { @final @override fun f() {} }
             class C: B { @override fun f() {} }",
            pos(3, 37),
            SemError::MethodNotOverridable("f".into()),
        );
    }

    #[test]
    fn test_overload_method_in_super_class() {
        errors(
            "@open class A { fun f() {} }
            class B: A { fun f(a: Int) {} }",
            &[
                (pos(2, 26), SemError::MissingOverride("f".into())),
                (pos(2, 26), SemError::MethodNotOverridable("f".into())),
            ],
        );

        ok("@open class A { @static fun f() {} }
            class B: A { @static fun f(a: Int) {} }");
    }

    #[test]
    fn test_override_with_wrong_return_type() {
        err(
            "@open class A { @open fun f() {} }
             class B: A { @override fun f() -> Int { return 1; } }",
            pos(2, 37),
            SemError::ReturnTypeMismatch("Int".into(), "()".into()),
        );
    }

    #[test]
    fn test_override_with_missing_throws() {
        err(
            "@open class A { @open fun f() throws {} }
             class B: A { @override fun f() {} }",
            pos(2, 37),
            SemError::ThrowsDifference("f".into()),
        );
    }

    #[test]
    fn test_open() {
        ok("@open class A { @open fun f() {} }");
    }

    #[test]
    fn test_superfluous_open() {
        err(
            "class A { @open fun f() {} }",
            pos(1, 17),
            SemError::SuperfluousOpen("f".into()),
        );
    }

    #[test]
    fn test_final() {
        ok("@open class A { @final fun f() {} }");
    }

    #[test]
    fn test_vtable_index_and_len() {
        ok_with_test("class A {}", |vm| {
            let cls_id = vm.cls_by_name("A");
            let cls = vm.classes.idx(cls_id);
            let cls = cls.read();
            assert_eq!(cls.virtual_fcts.len(), 0);
        });

        ok_with_test("@open class A { @open fun f() {} }", |vm| {
            let cls_id = vm.cls_by_name("A");
            let cls = vm.classes.idx(cls_id);
            let cls = cls.read();
            assert_eq!(cls.virtual_fcts.len(), 1);
        });

        ok_with_test(
            "@open class A { @open fun f() {} }
                      @open class B: A { @override fun f() {}
                                        @open fun g() {} }",
            |vm| {
                let cls_id = vm.cls_by_name("A");
                let cls = vm.classes.idx(cls_id);
                let cls = cls.read();
                assert_eq!(cls.virtual_fcts.len(), 1);

                let cls_id = vm.cls_by_name("B");
                let cls = vm.classes.idx(cls_id);
                let cls = cls.read();
                assert_eq!(cls.virtual_fcts.len(), 2);
            },
        );
    }

    // #[test]
    // fn test_depth() {
    //     ok_with_test("class A { } class B { }", |vm| {
    //         assert_eq!(vtable_by_name(vm, "A", |f| f.subtype_depth), 1);
    //         assert_eq!(vtable_by_name(vm, "B", |f| f.subtype_depth), 1);
    //     });
    // }

    // #[test]
    // fn test_depth_with_multiple_levels() {
    //     ok_with_test("@open class A { } open class B: A { }
    //                   class C: B { }",
    //                  |vm| {
    //         assert_eq!(vtable_by_name(vm, "A", |f| f.subtype_depth), 1);
    //         assert_eq!(vtable_by_name(vm, "B", |f| f.subtype_depth), 2);
    //         assert_eq!(vtable_by_name(vm, "C", |f| f.subtype_depth), 3);

    //         {
    //             let vtable = vtable_by_name(vm, "C", |vtable| {
    //                 assert!(vtable.subtype_display[4].is_null());
    //                 vtable as *const _
    //             });

    //             assert_name(vm, vtable_name(vtable), "C");
    //             assert_name(vm, vtable_display_name(vtable, 0), "Object");
    //             assert_name(vm, vtable_display_name(vtable, 1), "A");
    //             assert_name(vm, vtable_display_name(vtable, 2), "B");
    //             assert_name(vm, vtable_display_name(vtable, 3), "C");
    //         }

    //         {
    //             let vtable = vtable_by_name(vm, "B", |vtable| {
    //                 assert!(vtable.subtype_display[3].is_null());
    //                 vtable as *const _
    //             });

    //             assert_name(vm, vtable_name(vtable), "B");
    //             assert_name(vm, vtable_display_name(vtable, 0), "Object");
    //             assert_name(vm, vtable_display_name(vtable, 1), "A");
    //             assert_name(vm, vtable_display_name(vtable, 2), "B");
    //         }

    //         {
    //             let vtable = vtable_by_name(vm, "A", |vtable| {
    //                 assert!(vtable.subtype_display[2].is_null());
    //                 vtable as *const _
    //             });

    //             assert_name(vm, vtable_name(vtable), "A");
    //             assert_name(vm, vtable_display_name(vtable, 0), "Object");
    //             assert_name(vm, vtable_display_name(vtable, 1), "A");
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
    //                  |vm| {
    //         assert_eq!(vtable_by_name(vm, "Object", |f| f.subtype_depth), 0);
    //         assert_eq!(vtable_by_name(vm, "L1", |f| f.subtype_depth), 1);
    //         assert_eq!(vtable_by_name(vm, "L2", |f| f.subtype_depth), 2);
    //         assert_eq!(vtable_by_name(vm, "L3", |f| f.subtype_depth), 3);
    //         assert_eq!(vtable_by_name(vm, "L4", |f| f.subtype_depth), 4);
    //         assert_eq!(vtable_by_name(vm, "L5", |f| f.subtype_depth), 5);
    //         assert_eq!(vtable_by_name(vm, "L6", |f| f.subtype_depth), 6);
    //         assert_eq!(vtable_by_name(vm, "L7", |f| f.subtype_depth), 7);

    //         let vtable = vtable_by_name(vm, "L7", |vtable| {
    //             assert!(!vtable.subtype_overflow.is_null());
    //             assert_eq!(vtable_by_name(vm, "L6", |v| v as *const _),
    //                        vtable.get_subtype_overflow(0));
    //             assert_eq!(vtable_by_name(vm, "L7", |v| v as *const _),
    //                        vtable.get_subtype_overflow(1));

    //             vtable as *const _
    //         });

    //         assert_name(vm, vtable_display_name(vtable, 1), "L1");
    //         assert_name(vm, vtable_display_name(vtable, 2), "L2");
    //         assert_name(vm, vtable_display_name(vtable, 3), "L3");
    //         assert_name(vm, vtable_display_name(vtable, 4), "L4");
    //         assert_name(vm, vtable_display_name(vtable, 5), "L5");

    //         let vtable = vtable_by_name(vm, "L10", |vtable| {
    //             assert!(!vtable.subtype_overflow.is_null());
    //             assert_eq!(vtable_by_name(vm, "L6", |v| v as *const _),
    //                        vtable.get_subtype_overflow(0));
    //             assert_eq!(vtable_by_name(vm, "L7", |v| v as *const _),
    //                        vtable.get_subtype_overflow(1));
    //             assert_eq!(vtable_by_name(vm, "L8", |v| v as *const _),
    //                        vtable.get_subtype_overflow(2));
    //             assert_eq!(vtable_by_name(vm, "L9", |v| v as *const _),
    //                        vtable.get_subtype_overflow(3));
    //             assert_eq!(vtable_by_name(vm, "L10", |v| v as *const _),
    //                        vtable.get_subtype_overflow(4));

    //             vtable as *const _
    //         });

    //         assert_name(vm, vtable_display_name(vtable, 0), "Object");
    //         assert_name(vm, vtable_display_name(vtable, 1), "L1");
    //         assert_name(vm, vtable_display_name(vtable, 2), "L2");
    //         assert_name(vm, vtable_display_name(vtable, 3), "L3");
    //         assert_name(vm, vtable_display_name(vtable, 4), "L4");
    //         assert_name(vm, vtable_display_name(vtable, 5), "L5");
    //     });
    // }

    // #[test]
    // fn test_ref_fields() {
    //     let header = Header::size();
    //     let pw = mem::ptr_width();

    //     ok_with_test("@open class A(let a: A) class B(a: A, let b: B) : A(a)",
    //                  |vm| {
    //         let cls = cls_by_name(vm, "A");
    //         let cls = vm.classes[cls].borrow();
    //         assert_eq!(vec![header], cls.ref_fields);

    //         let cls = cls_by_name(vm, "B");
    //         let cls = vm.classes[cls].borrow();
    //         assert_eq!(vec![header, header + pw], cls.ref_fields);
    //     });

    //     ok_with_test("class A(let x: Data, d: Data): B(d)
    //                   @open class B(let y: Data)
    //                   class Data(let data: int)",
    //                  |vm| {
    //         let cls = cls_by_name(vm, "A");
    //         let cls = vm.classes[cls].borrow();
    //         assert_eq!(vec![header, header + pw], cls.ref_fields);

    //         let cls = cls_by_name(vm, "B");
    //         let cls = vm.classes[cls].borrow();
    //         assert_eq!(vec![header], cls.ref_fields);
    //     });
    // }

    // #[test]
    // fn test_struct_size() {
    //     ok_with_test(
    //         "struct Foo { a: int, b: int }
    //                   struct Foo1 { a: bool, b: int, c: bool }
    //                   struct Bar { }",
    //         |vm| {
    //             assert_eq!(8, vm.structs[0].borrow().size);
    //             assert_eq!(12, vm.structs[1].borrow().size);
    //             assert_eq!(0, vm.structs[2].borrow().size);
    //         },
    //     );
    // }

    // #[test]
    // fn test_struct_in_struct() {
    //     ok_with_test(
    //         "struct Foo { a: bool, bar: Bar }
    //                   struct Bar { a: int }",
    //         |vm| {
    //             assert_eq!(8, vm.structs[0].borrow().size);
    //         },
    //     );

    //     ok_with_test(
    //         "struct Bar { a: int }
    //                   struct Foo { a: bool, bar: Bar }",
    //         |vm| {
    //             assert_eq!(8, vm.structs[1].borrow().size);
    //         },
    //     );

    //     err(
    //         "struct Foo { a: int, bar: Bar }
    //          struct Bar { b: int, foo: Foo }",
    //         pos(2, 35),
    //         SemError::RecursiveStructure,
    //     );
    // }

    // #[test]
    // fn test_class_in_struct() {
    //     ok_with_test("class Foo(a: bool, b: int)
    //                   struct Bar { a: int, foo: Foo }",
    //                  |vm| {
    //                      assert_eq!(2 * mem::ptr_width(), vm.structs[0].borrow().size);
    //                  });
    // }

    // #[test]
    // fn test_struct_in_class() {
    //     ok_with_test("class Foo { var bar: Bar; }
    //                   struct Bar { a: int, foo: Foo }",
    //                  |vm| {
    //                      let cls = cls_by_name(vm, "Foo");
    //                      let cls = vm.classes[cls].borrow();
    //                      assert_eq!(Header::size() + 2 * mem::ptr_width(), cls.size);
    //                      assert_eq!(2 * mem::ptr_width(), vm.structs[0].borrow().size);
    //                  });
    // }

    fn assert_name<'a, 'ast>(vm: &'a VM<'ast>, a: Name, b: &'static str) {
        let bname = vm.interner.intern(b);

        println!("{} {}", vm.interner.str(a), b);

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

    // fn vtable_by_name<'a, 'ast: 'a, F, R>(vm: &'a SemContext<'ast>,
    //                                       name: &'static str,
    //                                       fct: F)
    //                                       -> R
    //     where F: FnOnce(&VTable) -> R
    // {
    //     let cid = cls_by_name(vm, name);
    //     let cls = vm.classes[cid].borrow();
    //     let vtable = cls.vtable.as_ref().unwrap();

    //     fct(vtable)
    // }

    // fn check_class<'ast>(vm: &SemContext<'ast>,
    //                      name: &'static str,
    //                      size: i32,
    //                      parent: Option<&'static str>) {
    //     let name = vm.interner.intern(name);
    //     let cls_id = vm.sym.borrow().get_class(name).unwrap();

    //     let parent_id = parent
    //         .map(|name| vm.interner.intern(name))
    //         .map(|name| vm.sym.borrow().get_class(name).unwrap());

    //     let cls = vm.classes[cls_id].borrow();
    //     assert_eq!(parent_id, cls.parent_class);
    //     assert_eq!(Header::size() + size, cls.size);
    // }

    // fn check_field<'ast>(vm: &SemContext<'ast>,
    //                      cls_name: &'static str,
    //                      field_name: &'static str,
    //                      offset: i32) {
    //     let cls_name = vm.interner.intern(cls_name);
    //     let field_name = vm.interner.intern(field_name);
    //     let cls_id = vm.sym.borrow().get_class(cls_name).unwrap();
    //     let cls = vm.classes[cls_id].borrow();

    //     for field in &cls.fields {
    //         if field_name == field.name {
    //             assert_eq!(offset, field.offset);
    //             return;
    //         }
    //     }

    //     unreachable!();
    // }
}
