use parking_lot::RwLock;

use crate::error::msg::SemError;
use crate::sym::Sym;
use crate::ty::BuiltinType;
use crate::vm::{Fct, FctId, FctKind, FctParent, FctSrc, FileId, ImplId, NodeMap, VM};

use dora_parser::ast::visit::{self, Visitor};
use dora_parser::ast::{self, Ast};
use dora_parser::lexer::position::Position;

pub fn check<'ast>(vm: &mut VM<'ast>, ast: &'ast Ast, map_impl_defs: &NodeMap<ImplId>) {
    let mut clsck = ImplCheck {
        vm,
        ast,
        impl_id: None,
        map_impl_defs,
        file_id: 0,
    };

    clsck.check();
}

struct ImplCheck<'x, 'ast: 'x> {
    vm: &'x mut VM<'ast>,
    ast: &'ast ast::Ast,
    map_impl_defs: &'x NodeMap<ImplId>,
    file_id: u32,

    impl_id: Option<ImplId>,
}

impl<'x, 'ast> ImplCheck<'x, 'ast> {
    fn check(&mut self) {
        self.visit_ast(self.ast);
    }
}

impl<'x, 'ast> Visitor<'ast> for ImplCheck<'x, 'ast> {
    fn visit_file(&mut self, f: &'ast ast::File) {
        visit::walk_file(self, f);
        self.file_id += 1;
    }

    fn visit_impl(&mut self, i: &'ast ast::Impl) {
        self.impl_id = Some(*self.map_impl_defs.get(i.id).unwrap());

        visit::walk_impl(self, i);

        let mut ximpl = self.vm.impls[self.impl_id.unwrap()].write();

        if i.type_params.is_some() {
            // We don't support type parameters for impl-blocks yet.
            report(self.vm, ximpl.file, i.pos, SemError::Unimplemented);
            self.impl_id = None;
            return;
        }

        if let Some(ref trait_type) = i.trait_type {
            if let Some(trait_name) = trait_type.to_basic_without_type_params() {
                if let Some(Sym::SymTrait(trait_id)) = self.vm.sym.lock().get(trait_name) {
                    ximpl.trait_id = Some(trait_id);
                } else {
                    let name = self.vm.interner.str(trait_name).to_string();
                    report(self.vm, ximpl.file, i.pos, SemError::ExpectedTrait(name));
                }
            } else {
                // We don't support type parameters for traits yet.
                report(self.vm, ximpl.file, i.pos, SemError::Unimplemented);
                self.impl_id = None;
                return;
            }
        } else {
            // We don't support extension blocks yet.
            report(self.vm, ximpl.file, i.pos, SemError::Unimplemented);
            self.impl_id = None;
            return;
        }

        if let Some(class_name) = i.class_type.to_basic_without_type_params() {
            if let Some(Sym::SymClass(class_id)) = self.vm.sym.lock().get(class_name) {
                ximpl.class_id = Some(class_id);
            } else {
                let name = self.vm.interner.str(class_name).to_string();
                report(self.vm, ximpl.file, i.pos, SemError::ExpectedClass(name));
            }
        } else {
            // We don't support type parameters for the class yet.
            report(self.vm, ximpl.file, i.pos, SemError::Unimplemented);
            self.impl_id = None;
            return;
        }

        if ximpl.trait_id.is_some() && ximpl.class_id.is_some() {
            let cls = self.vm.classes.idx(ximpl.cls_id());
            let mut cls = cls.write();
            cls.traits.push(ximpl.trait_id());
            cls.impls.push(ximpl.id);
        }

        self.impl_id = None;
    }

    fn visit_method(&mut self, f: &'ast ast::Function) {
        if self.impl_id.is_none() {
            return;
        }

        if f.block.is_none() && !f.internal {
            report(
                self.vm,
                self.file_id.into(),
                f.pos,
                SemError::MissingFctBody,
            );
        }

        let kind = if f.internal {
            FctKind::Definition
        } else {
            FctKind::Source(RwLock::new(FctSrc::new()))
        };

        let fct = Fct {
            id: FctId(0),
            ast: f,
            pos: f.pos,
            name: f.name,
            param_types: Vec::new(),
            return_type: BuiltinType::Unit,
            parent: FctParent::Impl(self.impl_id.unwrap()),
            has_override: f.has_override,
            has_open: f.has_open,
            has_final: f.has_final,
            is_pub: f.is_pub,
            is_static: f.is_static,
            is_abstract: false,
            is_test: f.is_test,
            internal: f.internal,
            internal_resolved: false,
            overrides: None,
            throws: f.throws,
            is_constructor: false,
            vtable_index: None,
            initialized: false,
            impl_for: None,
            file: self.file_id.into(),

            type_params: Vec::new(),
            kind,
        };

        let fctid = self.vm.add_fct(fct);

        let mut ximpl = self.vm.impls[self.impl_id.unwrap()].write();
        ximpl.methods.push(fctid);
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
    fn impl_method_without_body() {
        err(
            "
            trait Foo {
                fun foo() -> Int;
            }
            class Bar {}
            impl Foo for Bar { fun foo() -> Int;}",
            pos(6, 32),
            SemError::MissingFctBody,
        );
    }

    #[test]
    fn impl_method_defined_twice() {
        err(
            "
            trait Foo {
                fun foo() -> Int;
            }
            class Bar {}
            impl Foo for Bar {
                fun foo() -> Int { return 0; }
                fun foo() -> Int { return 1; }
            }",
            pos(8, 17),
            SemError::MethodExists("Foo".into(), "foo".into(), pos(7, 17)),
        );
    }

    #[test]
    fn impl_for_unknown_trait() {
        err(
            "class A {} impl Foo for A {}",
            pos(1, 12),
            SemError::ExpectedTrait("Foo".into()),
        );
    }

    #[test]
    fn impl_for_unknown_class() {
        err(
            "trait Foo {} impl Foo for A {}",
            pos(1, 14),
            SemError::ExpectedClass("A".into()),
        );
    }

    #[test]
    fn impl_definitions() {
        ok("trait Foo {} class A {} impl Foo for A {}");
        ok("trait Foo { fun toBool() -> Bool; }
            class A {}
            impl Foo for A { fun toBool() -> Bool { return false; } }");
    }
}
