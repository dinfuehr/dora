use std::collections::{HashMap, HashSet};
use std::rc::Rc;

use dora_parser::error::msg::Msg;

use class::{Class, ClassId};
use ctxt::{SemContext, FctId};

pub fn check<'ast>(ctxt: &mut SemContext<'ast>) {
    let mut abstract_methods: HashMap<ClassId, Rc<Vec<FctId>>> = HashMap::new();

    for cls in ctxt.classes.iter() {
        let cls = cls.borrow();

        // we are only interested in non-abstract classes
        // with abstract super classes

        if cls.is_abstract {
            continue;
        }

        if let Some(super_cls_id) = cls.parent_class {
            let super_cls = ctxt.classes[super_cls_id].borrow();

            if super_cls.is_abstract {
                check_abstract(ctxt, &*cls, &*super_cls, &mut abstract_methods);
            }
        }
    }
}

pub fn check_abstract<'ast>(ctxt: &SemContext<'ast>,
                            cls: &Class,
                            super_cls: &Class,
                            abstract_methods: &mut HashMap<ClassId, Rc<Vec<FctId>>>) {
    assert!(!cls.is_abstract);
    assert!(super_cls.is_abstract);

    let mtds = find_abstract_methods(ctxt, super_cls, abstract_methods);
    let mut overrides = HashSet::new();

    for &mtd in &cls.methods {
        let mtd = ctxt.fcts[mtd].borrow();

        if let Some(overrides_mtd) = mtd.overrides {
            overrides.insert(overrides_mtd);
        }
    }

    for &mtd in mtds.iter() {
        if !overrides.contains(&mtd) {
            let mtd = ctxt.fcts[mtd].borrow();

            let mtd_cls = ctxt.classes[mtd.parent.cls_id()].borrow();
            let cls_name = ctxt.interner.str(mtd_cls.name).to_string();
            let mtd_name = ctxt.interner.str(mtd.name).to_string();

            ctxt.diag
                .borrow_mut()
                .report(cls.pos, Msg::MissingAbstractOverride(cls_name, mtd_name));
        }
    }
}

fn find_abstract_methods<'ast>(ctxt: &SemContext<'ast>,
                               cls: &Class,
                               abstract_methods: &mut HashMap<ClassId, Rc<Vec<FctId>>>)
                               -> Rc<Vec<FctId>> {
    assert!(cls.is_abstract);

    if let Some(mtds) = abstract_methods.get(&cls.id) {
        return mtds.clone();
    }

    let mut abstracts = Vec::new();
    let mut overrides = HashSet::new();

    for &mtd in &cls.methods {
        let mtd = ctxt.fcts[mtd].borrow();

        if mtd.is_abstract {
            abstracts.push(mtd.id);
        }

        if let Some(override_mtd) = mtd.overrides {
            overrides.insert(override_mtd);
        }
    }

    if let Some(super_cls_id) = cls.parent_class {
        let super_cls = ctxt.classes[super_cls_id].borrow();

        if super_cls.is_abstract {
            let super_abstracts = find_abstract_methods(ctxt, &*super_cls, abstract_methods);

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
    use semck::tests::{err, ok, pos};
    use dora_parser::error::msg::Msg;

    #[test]
    fn test_abstract_class_without_abstract_methods() {
        ok("open abstract class A class B: A");
    }

    #[test]
    fn test_override_abstract_method() {
        ok("open abstract class A { abstract fun foo(); }
            class B: A { override fun foo() {} }");
    }

    #[test]
    fn test_override_abstract_method_in_super_class() {
        ok("open abstract class A { abstract fun foo(); }
            open abstract class B: A { override fun foo() {} }
            class C: B { }");
    }

    #[test]
    fn test_missing_abstract_override() {
        err("open abstract class A { abstract fun foo(); }
            class B: A { }",
            pos(2, 13),
            Msg::MissingAbstractOverride("A".into(), "foo".into()));
    }

    #[test]
    fn test_missing_abstract_override_indirect() {
        err("open abstract class A { abstract fun foo(); }
            open abstract class B: A {}
            class C: B { }",
            pos(3, 13),
            Msg::MissingAbstractOverride("A".into(), "foo".into()));
    }
}
