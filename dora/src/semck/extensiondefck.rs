use parking_lot::RwLock;
use std::collections::HashMap;

use crate::error::msg::SemError;
use crate::semck;
use crate::ty::BuiltinType;
use crate::vm::{ExtensionId, Fct, FctId, FctKind, FctParent, FctSrc, FileId, NodeMap, VM};

use dora_parser::ast::visit::{self, Visitor};
use dora_parser::ast::{self, Ast};
use dora_parser::lexer::position::Position;

pub fn check<'ast>(vm: &mut VM<'ast>, ast: &'ast Ast, map_extension_defs: &NodeMap<ExtensionId>) {
    let mut clsck = ExtensionCheck {
        vm,
        ast,
        extension_id: None,
        map_extension_defs,
        file_id: 0,
    };

    clsck.check();
}

struct ExtensionCheck<'x, 'ast: 'x> {
    vm: &'x mut VM<'ast>,
    ast: &'ast ast::Ast,
    map_extension_defs: &'x NodeMap<ExtensionId>,
    file_id: u32,

    extension_id: Option<ExtensionId>,
}

impl<'x, 'ast> ExtensionCheck<'x, 'ast> {
    fn check(&mut self) {
        self.visit_ast(self.ast);
    }

    fn visit_extension(&mut self, i: &'ast ast::Impl) {
        assert!(i.trait_type.is_none());
        self.extension_id = Some(*self.map_extension_defs.get(i.id).unwrap());

        self.vm.sym.lock().push_level();

        if let Some(ref type_params) = i.type_params {
            self.check_type_params(self.extension_id.unwrap(), type_params);
        }

        visit::walk_impl(self, i);

        let mut extension = self.vm.extensions[self.extension_id.unwrap()].write();

        if let Some(class_ty) = semck::read_type(self.vm, self.file_id.into(), &i.class_type) {
            extension.class_ty = class_ty;
        } else {
            extension.class_ty = BuiltinType::Error;
        }

        self.extension_id = None;
        self.vm.sym.lock().pop_level();
    }

    fn check_type_params(
        &mut self,
        _extension_id: ExtensionId,
        _type_params: &'ast [ast::TypeParam],
    ) {
        unimplemented!();
    }
}

impl<'x, 'ast> Visitor<'ast> for ExtensionCheck<'x, 'ast> {
    fn visit_file(&mut self, f: &'ast ast::File) {
        visit::walk_file(self, f);
        self.file_id += 1;
    }

    fn visit_impl(&mut self, i: &'ast ast::Impl) {
        if i.trait_type.is_none() {
            self.visit_extension(i);
        }
    }

    fn visit_method(&mut self, f: &'ast ast::Function) {
        if self.extension_id.is_none() {
            return;
        }

        let extension_id = self.extension_id.unwrap();

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

        let parent = FctParent::Extension(extension_id);

        let fct = Fct {
            id: FctId(0),
            ast: f,
            pos: f.pos,
            name: f.name,
            param_types: Vec::new(),
            return_type: BuiltinType::Unit,
            parent: parent,
            has_override: f.has_override,
            has_open: f.has_open,
            has_final: f.has_final,
            has_optimize_immediately: f.has_optimize_immediately,
            is_pub: f.is_pub,
            is_static: f.is_static,
            is_abstract: false,
            is_test: f.is_test,
            use_cannon: f.use_cannon,
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

            specializations_fct_def: RwLock::new(HashMap::new()),
        };

        let fctid = self.vm.add_fct(fct);

        let mut extension = self.vm.extensions[extension_id].write();
        extension.methods.push(fctid);
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
    fn extension_empty() {
        ok("class A impl A {}");
        ok("class A impl A {} impl A {}");
        err(
            "class A impl A[String] {}",
            pos(1, 14),
            SemError::WrongNumberTypeParams(0, 1),
        );

        ok("class A[T] impl A[Int] {} impl A[String] {}");
        err(
            "class A[T: Zero] impl A[Int] {} impl A[String] {}",
            pos(1, 38),
            SemError::TraitBoundNotSatisfied("String".into(), "Zero".into()),
        );
    }
}
