use std::collections::HashSet;

use crate::error::msg::SemError;
use crate::ty::SourceType;
use crate::vm::{FileId, VM};

use dora_parser::lexer::position::Position;

pub fn check<'ast>(vm: &mut VM<'ast>) {
    for ximpl in &vm.impls {
        let ximpl = ximpl.read();
        let xtrait = vm.traits[ximpl.trait_id()].read();
        let cls = vm.classes.idx(ximpl.cls_id(vm));
        let cls = cls.read();
        let cls = cls.ty;

        let all: HashSet<_> = xtrait.methods.iter().cloned().collect();
        let mut defined = HashSet::new();

        for &method_id in &ximpl.methods {
            let method = vm.fcts.idx(method_id);
            let mut method = method.write();

            if let Some(fid) = xtrait.find_method_with_replace(
                vm,
                method.is_static,
                method.name,
                Some(cls),
                method.params_without_self(),
            ) {
                method.impl_for = Some(fid);
                defined.insert(fid);

                let trait_method = vm.fcts.idx(fid);
                let trait_method = trait_method.read();

                let return_type_valid = method.return_type
                    == if trait_method.return_type == SourceType::This {
                        cls
                    } else {
                        trait_method.return_type
                    };

                if !return_type_valid {
                    let impl_return_type = method.return_type.name_fct(vm, &*method);
                    let trait_return_type = trait_method.return_type.name_fct(vm, &*trait_method);

                    let msg = SemError::ReturnTypeMismatch(impl_return_type, trait_return_type);
                    vm.diag.lock().report(ximpl.file, method.pos, msg);
                }
            } else {
                let args = method
                    .params_without_self()
                    .iter()
                    .map(|a| a.name_fct(vm, &*method))
                    .collect::<Vec<String>>();
                let mtd_name = vm.interner.str(method.name).to_string();
                let trait_name = vm.interner.str(xtrait.name).to_string();

                let msg = if method.is_static {
                    SemError::StaticMethodNotInTrait(trait_name, mtd_name, args)
                } else {
                    SemError::MethodNotInTrait(trait_name, mtd_name, args)
                };

                report(vm, ximpl.file, method.pos, msg);
            }
        }

        for &method_id in all.difference(&defined) {
            let method = vm.fcts.idx(method_id);
            let method = method.read();

            let args = method
                .params_without_self()
                .iter()
                .map(|a| a.name_fct(vm, &*method))
                .collect::<Vec<String>>();
            let mtd_name = vm.interner.str(method.name).to_string();
            let trait_name = vm.interner.str(xtrait.name).to_string();

            let msg = if method.is_static {
                SemError::StaticMethodMissingFromTrait(trait_name, mtd_name, args)
            } else {
                SemError::MethodMissingFromTrait(trait_name, mtd_name, args)
            };

            report(vm, ximpl.file, ximpl.pos, msg);
        }
    }
}

fn report(vm: &VM, file: FileId, pos: Position, msg: SemError) {
    vm.diag.lock().report(file, pos, msg);
}

#[cfg(test)]
mod tests {
    use crate::error::msg::SemError;
    use crate::semck::tests::*;

    #[test]
    fn method_not_in_trait() {
        err(
            "
            trait Foo {}
            class A
            impl Foo for A {
                fun bar() {}
            }",
            pos(5, 17),
            SemError::MethodNotInTrait("Foo".into(), "bar".into(), vec![]),
        );
    }

    #[test]
    fn method_missing_in_impl() {
        err(
            "
            trait Foo {
                fun bar();
            }
            class A
            impl Foo for A {}",
            pos(6, 13),
            SemError::MethodMissingFromTrait("Foo".into(), "bar".into(), vec![]),
        );
    }

    #[test]
    fn method_returning_self() {
        ok("trait Foo {
                fun foo(): Self;
            }

            class A

            impl Foo for A {
                fun foo(): A { return A(); }
            }");
    }

    #[test]
    fn static_method_not_in_trait() {
        err(
            "
            trait Foo {}
            class A
            impl Foo for A {
                @static fun bar() {}
            }",
            pos(5, 25),
            SemError::StaticMethodNotInTrait("Foo".into(), "bar".into(), vec![]),
        );
    }

    #[test]
    fn static_method_missing_in_impl() {
        err(
            "
            trait Foo {
                @static fun bar();
            }
            class A
            impl Foo for A {}",
            pos(6, 13),
            SemError::StaticMethodMissingFromTrait("Foo".into(), "bar".into(), vec![]),
        );
    }

    #[test]
    fn method_return_type_check() {
        err(
            "trait X {
                fun m(): Bool;
                fun n(): Bool;
              }
              
              class CX
              
              impl X for CX {
                fun m(): Int32 = 0;
                fun n(): Bool = true;
              }",
            pos(9, 17),
            SemError::ReturnTypeMismatch("Int32".into(), "Bool".into()),
        );
    }
}
