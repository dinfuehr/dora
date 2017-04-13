use ctxt::Context;

pub fn check<'ast>(ctxt: &mut Context<'ast>) {
    let abstract_methods: HashSet<ClassId, Rc<Vec<FctId>>> = HashSet::new();

    for cls in &ctxt.classes {
        let cls = cls.borrow();

        // we are only interested in non-abstract classes
        // with abstract super classes

        if cls.is_abstract {
            continue;
        }

        if let Some(super_cls_id) = cls.owner_class {
            let super_cls = &ctxt.classes[super_cls_id];

            if super_cls.is_abstract {
                // check_abstract(ctxt, &*cls, &mut abstract_methods);
            }
        }
    }
}

// fn check_abstract<'ast>(ctxt: &Context<'ast>, cls: &Class<'ast>, abstract_methods: &mut HashSet<ClassId, Rc<Vec<FctId>>>) {
//     let overrides = HashSet::new();

//     for &mtd in cls.methods {
//         let mtd = ctxt.fcts[mtd].borrow();

//         if let Some(override) = mtd.overrides {
//             overrides.push(override);
//         }
//     }

//     if let Some(super_cls_id) = cls.owner_class {

//     }

//     let mtds = ...;

//     for &mtd in &mtds {
//         if !overrides.contains()
//     }
// }

// pub fn find_abstract_methods<'ast>(ctxt: &Context<'ast>, cls: &Class<'ast>) {

// }

#[cfg(test)]
mod tests {
    use semck::tests::{err, ok, pos};

    #[test]
    fn test_abstract_class_without_abstract_methods() {
        ok("open abstract class A class B: A");
    }

    #[test]
    fn test_override_abstract_method() {
        ok("open abstract class A { abstract fun foo(); }
            class B: A { override fun foo() {} }");
    }
}