use std::collections::HashSet;

use crate::semck::error::msg::SemError;
use crate::semck::specialize::replace_type_param;
use crate::vm::{
    find_method_in_class, ClassDefinition, ClassDefinitionId, FctDefinition, FctDefinitionId,
    SemAnalysis,
};

pub fn check(sa: &mut SemAnalysis) {
    cycle_detection(sa);

    if sa.diag.lock().has_errors() {
        return;
    }

    determine_vtables(sa);
}

fn cycle_detection(sa: &mut SemAnalysis) {
    for cls in sa.classes.iter() {
        let cls = cls.read();

        let mut map: HashSet<ClassDefinitionId> = HashSet::new();
        map.insert(cls.id);

        let mut parent_class = cls.parent_class.clone();

        while parent_class.is_some() {
            let parent_class_id = parent_class.unwrap().cls_id().expect("no class");

            if !map.insert(parent_class_id) {
                sa.diag
                    .lock()
                    .report(cls.file_id, cls.pos, SemError::CycleInHierarchy);
                break;
            }

            let cls = sa.classes.idx(parent_class_id);
            let cls = cls.read();
            parent_class = cls.parent_class.clone();
        }
    }
}

fn determine_vtables(sa: &SemAnalysis) {
    let mut lens = HashSet::new();

    for cls in sa.classes.iter() {
        let mut cls = cls.write();
        if !lens.contains(&cls.id) {
            determine_vtable(sa, &mut lens, &mut *cls);
        }
    }
}

fn determine_vtable(
    sa: &SemAnalysis,
    lens: &mut HashSet<ClassDefinitionId>,
    cls: &mut ClassDefinition,
) {
    if let Some(parent_class) = cls.parent_class.clone() {
        let parent_cls_id = parent_class.cls_id().expect("no class");
        let parent = sa.classes.idx(parent_cls_id);
        if !lens.contains(&parent_cls_id) {
            let mut parent = parent.write();
            determine_vtable(sa, lens, &mut *parent)
        }

        let parent = parent.read();
        cls.virtual_fcts
            .extend_from_slice(parent.virtual_fcts.as_slice());
    }

    for &mid in &cls.methods {
        let fct = sa.fcts.idx(mid);
        let mut fct = fct.write();

        assert!(fct.vtable_index.is_none());

        if fct.is_virtual() {
            let vtable_index = if let Some(overrides) = fct.overrides {
                let overrides = sa.fcts.idx(overrides);
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

pub fn check_override(sa: &SemAnalysis) {
    for cls in sa.classes.iter() {
        let cls = cls.read();

        for &fct_id in &cls.methods {
            let fct = sa.fcts.idx(fct_id);

            let overrides = {
                let fct = fct.read();
                check_fct_modifier(sa, &*cls, &*fct)
            };

            let mut fct = fct.write();
            fct.overrides = overrides;
        }
    }
}

fn check_fct_modifier(
    sa: &SemAnalysis,
    cls: &ClassDefinition,
    fct: &FctDefinition,
) -> Option<FctDefinitionId> {
    // catch: class A { @open fun f() } (A is not derivable)
    // catch: @open @final fun f()
    if fct.has_open && (!cls.has_open || fct.has_final) {
        let name = sa.interner.str(fct.name).to_string();
        sa.diag
            .lock()
            .report(fct.file_id, fct.pos(), SemError::SuperfluousOpen(name));
        return None;
    }

    if cls.parent_class.is_none() {
        if fct.has_override {
            let name = sa.interner.str(fct.name).to_string();
            sa.diag
                .lock()
                .report(fct.file_id, fct.pos(), SemError::SuperfluousOverride(name));
            return None;
        }

        return None;
    }

    let result = find_method_in_class(sa, cls.parent_class.clone().unwrap(), fct.name);

    if let Some((super_type, super_method)) = result {
        let super_method = sa.fcts.idx(super_method);
        let super_method = super_method.read();

        if !fct.has_override {
            let name = sa.interner.str(fct.name).to_string();
            sa.diag
                .lock()
                .report(fct.file_id, fct.pos(), SemError::MissingOverride(name));
        }

        if !(super_method.has_open || super_method.has_override) || super_method.has_final {
            let name = sa.interner.str(fct.name).to_string();
            sa.diag
                .lock()
                .report(fct.file_id, fct.pos(), SemError::MethodNotOverridable(name));
        }

        let type_params = super_type.type_params(sa);

        let super_method_params: Vec<_> = super_method
            .params_without_self()
            .iter()
            .map(|param_type| replace_type_param(sa, param_type.clone(), &type_params, None))
            .collect();

        let super_method_return_type =
            replace_type_param(sa, super_method.return_type.clone(), &type_params, None);

        if super_method_params != fct.params_without_self()
            || super_method_return_type != fct.return_type
        {
            sa.diag
                .lock()
                .report(fct.file_id, fct.pos(), SemError::OverrideMismatch);
        }

        Some(super_method.id)
    } else {
        if fct.has_override {
            let name = sa.interner.str(fct.name).to_string();
            sa.diag
                .lock()
                .report(fct.file_id, fct.pos(), SemError::SuperfluousOverride(name));
        }

        None
    }
}

#[cfg(test)]
mod tests {
    use crate::mem;
    use crate::object::Header;
    use crate::semck::error::msg::SemError;
    use crate::semck::tests::{err, errors, ok, ok_with_test, pos};
    use crate::size::InstanceSize;
    use crate::vm::SemAnalysis;
    use dora_parser::interner::Name;

    #[test]
    fn test_class_size() {
        assert_eq!(
            InstanceSize::Fixed(Header::size()),
            class_size("class Foo", "Foo")
        );
        assert_eq!(
            InstanceSize::Fixed(Header::size() + mem::ptr_width()),
            class_size("class Foo(let a: Int32)", "Foo")
        );
        assert_eq!(
            InstanceSize::Fixed(Header::size() + 8),
            class_size("class Foo(let a: Int64)", "Foo")
        );
        assert_eq!(
            InstanceSize::Fixed(Header::size() + mem::ptr_width()),
            class_size("class Foo(let a: Bool)", "Foo")
        );
        assert_eq!(
            InstanceSize::Fixed(Header::size() + mem::ptr_width()),
            class_size("class Foo(let a: String)", "Foo")
        );
    }

    fn class_size(code: &'static str, name: &'static str) -> InstanceSize {
        ok_with_test(code, |sa| {
            let id = sa.cls_def_by_name(sa.global_namespace_id, name);
            let cls = sa.class_defs.idx(id);
            cls.size.clone()
        })
    }

    #[test]
    fn test_cycle() {
        errors(
            "@open class A extends B @open class B extends A",
            &[
                (pos(1, 7), SemError::CycleInHierarchy),
                (pos(1, 31), SemError::CycleInHierarchy),
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
            "@open class B { } class A extends B { @override fun f() {} }",
            pos(1, 49),
            SemError::SuperfluousOverride("f".into()),
        );
        err(
            "@open class B { fun g() {} } class A extends B { @override fun f() {} }",
            pos(1, 60),
            SemError::SuperfluousOverride("f".into()),
        );
        err(
            "@open class B { @open fun f(a: Int32) {} } class A extends B { @override fun f() {} }",
            pos(1, 74),
            SemError::OverrideMismatch,
        );
    }

    #[test]
    fn test_override() {
        err(
            "@open class A { fun f() {} } class B extends A { @override fun f() {} }",
            pos(1, 60),
            SemError::MethodNotOverridable("f".into()),
        );
        ok("@open class A { @open fun f() {} } class B extends A { @override fun f() {} }");
        ok("@open class A { @open fun f() {} }
            @open class B extends A { @override fun f() {} }
            @open class C extends B { @override fun f() {} }");
        err(
            "@open class A { @open fun f() {} } class B extends A { fun f() {} }",
            pos(1, 56),
            SemError::MissingOverride("f".into()),
        );
        err(
            "@open class A { @open fun f() {} }
             @open class B extends A { @final @override fun f() {} }
             class C extends B { @override fun f() {} }",
            pos(3, 44),
            SemError::MethodNotOverridable("f".into()),
        );
    }

    #[test]
    fn test_overload_method_in_super_class() {
        errors(
            "@open class A { @open fun f() {} }
            class B extends A { fun f(a: Int32) {} }",
            &[
                (pos(2, 33), SemError::MissingOverride("f".into())),
                (pos(2, 33), SemError::OverrideMismatch),
            ],
        );

        ok("@open class A { @static fun f() {} }
            class B extends A { @static fun f(a: Int32) {} }");
    }

    #[test]
    fn test_override_with_wrong_return_type() {
        err(
            "@open class A { @open fun f() {} }
             class B extends A { @override fun f(): Int32 { return 1; } }",
            pos(2, 44),
            SemError::OverrideMismatch,
        );
    }

    #[test]
    fn test_wrong_parameters_in_override() {
        err(
            "
        @open @abstract class Foo {
            @open fun test(x: Int32): Int32 { x * 2 }
        }

        class Bar extends Foo {
            @override fun test(x: String): Int32 { 0 }
        }
    ",
            pos(7, 23),
            SemError::OverrideMismatch,
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
        ok_with_test("class A {}", |sa| {
            let cls_id = sa.cls_by_name("A");
            let cls = sa.classes.idx(cls_id);
            let cls = cls.read();
            assert_eq!(cls.virtual_fcts.len(), 0);
        });

        ok_with_test("@open class A { @open fun f() {} }", |sa| {
            let cls_id = sa.cls_by_name("A");
            let cls = sa.classes.idx(cls_id);
            let cls = cls.read();
            assert_eq!(cls.virtual_fcts.len(), 1);
        });

        ok_with_test(
            "@open class A { @open fun f() {} }
                      @open class B extends A { @override fun f() {}
                                        @open fun g() {} }",
            |sa| {
                let cls_id = sa.cls_by_name("A");
                let cls = sa.classes.idx(cls_id);
                let cls = cls.read();
                assert_eq!(cls.virtual_fcts.len(), 1);

                let cls_id = sa.cls_by_name("B");
                let cls = sa.classes.idx(cls_id);
                let cls = cls.read();
                assert_eq!(cls.virtual_fcts.len(), 2);
            },
        );
    }

    fn assert_name<'a>(sa: &'a SemAnalysis, a: Name, b: &'static str) {
        let bname = sa.interner.intern(b);

        println!("{} {}", sa.interner.str(a), b);

        assert_eq!(a, bname);
    }
}
