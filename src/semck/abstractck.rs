use std::collections::{HashMap, HashSet};
use std::rc::Rc;

use dora_parser::error::msg::Msg;

use crate::class::{Class, ClassId};
use crate::vm::{FctId, VM};

pub fn check<'ast>(vm: &mut VM<'ast>) {
    let mut abstract_methods: HashMap<ClassId, Rc<Vec<FctId>>> = HashMap::new();

    for cls in vm.classes.iter() {
        let cls = cls.read();

        // we are only interested in non-abstract classes
        // with abstract super classes

        if cls.is_abstract {
            continue;
        }

        if let Some(super_cls_id) = cls.parent_class {
            let super_cls = vm.classes.idx(super_cls_id);
            let super_cls = super_cls.read();

            if super_cls.is_abstract {
                check_abstract(vm, &*cls, &*super_cls, &mut abstract_methods);
            }
        }
    }
}

pub fn check_abstract<'ast>(
    vm: &VM<'ast>,
    cls: &Class,
    super_cls: &Class,
    abstract_methods: &mut HashMap<ClassId, Rc<Vec<FctId>>>,
) {
    assert!(!cls.is_abstract);
    assert!(super_cls.is_abstract);

    let mtds = find_abstract_methods(vm, super_cls, abstract_methods);
    let mut overrides = HashSet::new();

    for &mtd in &cls.methods {
        let mtd = vm.fcts.idx(mtd);
        let mtd = mtd.read();

        if let Some(overrides_mtd) = mtd.overrides {
            overrides.insert(overrides_mtd);
        }
    }

    for &mtd in mtds.iter() {
        if !overrides.contains(&mtd) {
            let mtd = vm.fcts.idx(mtd);
            let mtd = mtd.read();

            let mtd_cls = vm.classes.idx(mtd.parent.cls_id());
            let mtd_cls = mtd_cls.read();
            let cls_name = vm.interner.str(mtd_cls.name).to_string();
            let mtd_name = vm.interner.str(mtd.name).to_string();

            vm.diag
                .lock()
                .report_without_path(cls.pos, Msg::MissingAbstractOverride(cls_name, mtd_name));
        }
    }
}

fn find_abstract_methods<'ast>(
    vm: &VM<'ast>,
    cls: &Class,
    abstract_methods: &mut HashMap<ClassId, Rc<Vec<FctId>>>,
) -> Rc<Vec<FctId>> {
    assert!(cls.is_abstract);

    if let Some(mtds) = abstract_methods.get(&cls.id) {
        return mtds.clone();
    }

    let mut abstracts = Vec::new();
    let mut overrides = HashSet::new();

    for &mtd in &cls.methods {
        let mtd = vm.fcts.idx(mtd);
        let mtd = mtd.read();

        if mtd.is_abstract {
            abstracts.push(mtd.id);
        }

        if let Some(override_mtd) = mtd.overrides {
            overrides.insert(override_mtd);
        }
    }

    if let Some(super_cls_id) = cls.parent_class {
        let super_cls = vm.classes.idx(super_cls_id);
        let super_cls = super_cls.read();

        if super_cls.is_abstract {
            let super_abstracts = find_abstract_methods(vm, &*super_cls, abstract_methods);

            for &mtd in super_abstracts.iter() {
                if !overrides.contains(&mtd) {
                    abstracts.push(mtd);
                }
            }
        }
    }

    let ret = Rc::new(abstracts);
    abstract_methods.insert(cls.id, ret.clone());

    ret
}

#[cfg(test)]
mod tests {
    use crate::semck::tests::{err, ok, pos};
    use dora_parser::error::msg::Msg;

    #[test]
    fn test_abstract_class_without_abstract_methods() {
        ok("@open @abstract class A class B: A");
    }

    #[test]
    fn test_override_abstract_method() {
        ok("@open @abstract class A { @abstract fun foo(); }
            class B: A { @override fun foo() {} }");
    }

    #[test]
    fn test_override_abstract_method_in_super_class() {
        ok("@open @abstract class A { @abstract fun foo(); }
            @open @abstract class B: A { @override fun foo() {} }
            class C: B { }");
    }

    #[test]
    fn test_missing_abstract_override() {
        err(
            "@open @abstract class A { @abstract fun foo(); }
            class B: A { }",
            pos(2, 13),
            Msg::MissingAbstractOverride("A".into(), "foo".into()),
        );
    }

    #[test]
    fn test_missing_abstract_override_indirect() {
        err(
            "@open @abstract class A { @abstract fun foo(); }
            @open @abstract class B: A {}
            class C: B { }",
            pos(3, 13),
            Msg::MissingAbstractOverride("A".into(), "foo".into()),
        );
    }
}
