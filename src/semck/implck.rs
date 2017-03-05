use std::collections::HashSet;

use ctxt::Context;
use error::msg::Msg;
use lexer::position::Position;

pub fn check<'ast>(ctxt: &mut Context<'ast>) {
    for ximpl in &ctxt.impls {
        let ximpl = ximpl.borrow();
        let xtrait = ctxt.traits[ximpl.trait_id()].borrow();

        let all: HashSet<_> = xtrait.methods.iter().cloned().collect();
        let mut defined = HashSet::new();

        for &method_id in &ximpl.methods {
            let method = ctxt.fcts[method_id].borrow();

            if let Some(fid) = xtrait.find_method(ctxt, method.name, method.params_without_self()) {
                defined.insert(fid);

            } else {
                let args = method.params_without_self().iter().map(|a| a.name(ctxt)).collect::<Vec<String>>();
                let mtd_name = ctxt.interner.str(method.name).to_string();
                let trait_name = ctxt.interner.str(xtrait.name).to_string();
                report(ctxt, method.pos, Msg::MethodNotInTrait(trait_name, mtd_name, args));
            }
        }

        for &method_id in all.difference(&defined) {
            let method = ctxt.fcts[method_id].borrow();

            let args = method.params_without_self().iter().map(|a| a.name(ctxt)).collect::<Vec<String>>();
            let mtd_name = ctxt.interner.str(method.name).to_string();
            let trait_name = ctxt.interner.str(xtrait.name).to_string();
            report(ctxt, ximpl.pos, Msg::MethodMissingFromTrait(trait_name, mtd_name, args));
        }
    }
}


fn report(ctxt: &Context, pos: Position, msg: Msg) {
    ctxt.diag.borrow_mut().report(pos, msg);
}

#[cfg(test)]
mod tests {
    use error::msg::Msg;
    use semck::tests::*;

    #[test]
    fn impl_method_not_in_trait() {
        err("
            trait Foo {}
            class A {}
            impl Foo for A {
                fun bar() {}
            }",
            pos(5, 17),
            Msg::MethodNotInTrait("Foo".into(), "bar".into(), vec![]));
    }

    #[test]
    fn impl_method_missing_from_trait() {
        err("
            trait Foo {
                fun bar();
            }
            class A {}
            impl Foo for A {}",
            pos(6, 13),
            Msg::MethodMissingFromTrait("Foo".into(), "bar".into(), vec![]));
    }
}
