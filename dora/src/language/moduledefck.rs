use std::sync::Arc;

use crate::language::error::msg::SemError;
use crate::language::sem_analysis::{FctDefinition, FctParent, NamespaceId};
use crate::language::sym::NestedSymTable;
use crate::language::ty::{SourceType, SourceTypeArray};
use crate::language::TypeParamContext;
use crate::language::{self, AllowSelf};
use crate::vm::{Field, FileId, ModuleId, SemAnalysis};

use dora_parser::ast;
use dora_parser::interner::Name;
use dora_parser::lexer::position::Position;

pub fn check(sa: &SemAnalysis) {
    for module in sa.modules.iter() {
        let (module_id, file_id, ast, namespace_id) = {
            let module = module.read();
            (
                module.id,
                module.file_id,
                module.ast.clone(),
                module.namespace_id,
            )
        };

        let mut module_check = ModuleCheck {
            sa,
            module_id,
            file_id,
            namespace_id,
            ast: &ast,
            sym: NestedSymTable::new(sa, namespace_id),
        };

        module_check.check();
    }
}

struct ModuleCheck<'x> {
    sa: &'x SemAnalysis,
    module_id: ModuleId,
    namespace_id: NamespaceId,
    file_id: FileId,
    ast: &'x ast::Module,
    sym: NestedSymTable<'x>,
}

impl<'x> ModuleCheck<'x> {
    fn check(&mut self) {
        self.sym.push_level();

        for field in &self.ast.fields {
            self.visit_field(field);
        }

        if let Some(ctor) = &self.ast.constructor {
            self.visit_ctor(ctor);
        }

        for method in &self.ast.methods {
            self.visit_method(method);
        }

        if let Some(ref parent_class) = self.ast.parent_class {
            self.check_parent_class(parent_class);
        } else {
            self.use_object_class_as_parent();
        }

        self.sym.pop_level();
    }

    fn visit_field(&mut self, f: &ast::Field) {
        let ty = language::read_type(
            self.sa,
            &self.sym,
            self.file_id.into(),
            &f.data_type,
            TypeParamContext::None,
            AllowSelf::No,
        )
        .unwrap_or(SourceType::Error);
        self.add_field(f.pos, f.name, ty, f.mutable);

        if !f.mutable && !f.primary_ctor && f.expr.is_none() {
            self.sa.diag.lock().report(
                self.file_id.into(),
                f.pos,
                SemError::LetMissingInitialization,
            );
        }
    }

    fn visit_ctor(&mut self, f: &Arc<ast::Function>) {
        let fct = FctDefinition::new(
            self.sa,
            self.file_id,
            self.namespace_id,
            f,
            FctParent::Module(self.module_id),
        );
        let fctid = self.sa.add_fct(fct);

        let module = self.sa.modules.idx(self.module_id);
        let mut module = module.write();
        module.constructor = Some(fctid);
    }

    fn visit_method(&mut self, f: &Arc<ast::Function>) {
        let fct = FctDefinition::new(
            self.sa,
            self.file_id,
            self.namespace_id,
            f,
            FctParent::Module(self.module_id),
        );

        let fctid = self.sa.add_fct(fct);

        let module = self.sa.modules.idx(self.module_id);
        let mut module = module.write();
        module.methods.push(fctid);
    }

    fn add_field(&mut self, pos: Position, name: Name, ty: SourceType, mutable: bool) {
        let module = self.sa.modules.idx(self.module_id);
        let mut module = module.write();

        for field in &module.fields {
            if field.name == name {
                let name = self.sa.interner.str(name).to_string();
                self.sa
                    .diag
                    .lock()
                    .report(module.file_id, pos, SemError::ShadowField(name));
            }
        }

        let field = Field {
            id: module.fields.len().into(),
            name,
            ty,
            offset: 0,
            mutable: mutable,
            is_pub: false,
        };

        module.fields.push(field);
    }

    fn check_parent_class(&mut self, parent_class: &ast::ParentClass) {
        let parent_ty = language::read_type(
            self.sa,
            &self.sym,
            self.file_id,
            &parent_class.parent_ty,
            TypeParamContext::None,
            AllowSelf::No,
        )
        .unwrap_or(SourceType::Error);

        match parent_ty.clone() {
            SourceType::Class(cls_id, _type_list_id) => {
                let super_cls = self.sa.classes.idx(cls_id);
                let super_cls = super_cls.read();

                if !super_cls.is_open {
                    let msg = SemError::UnderivableType(super_cls.name(self.sa));
                    self.sa
                        .diag
                        .lock()
                        .report(self.file_id.into(), parent_class.pos, msg);
                }

                let module = self.sa.modules.idx(self.module_id);
                let mut module = module.write();
                module.parent_class = Some(parent_ty);
            }

            SourceType::Error => {
                // error was already reported
            }

            _ => {
                let msg = SemError::ClassExpected;
                self.sa
                    .diag
                    .lock()
                    .report(self.file_id.into(), parent_class.pos, msg);
            }
        }
    }

    fn use_object_class_as_parent(&mut self) {
        let object_cls = self.sa.known.classes.object();

        let module = self.sa.modules.idx(self.module_id);
        let mut module = module.write();

        module.parent_class = Some(SourceType::Class(object_cls, SourceTypeArray::empty()));
    }
}

#[cfg(test)]
mod tests {
    use crate::language::error::msg::SemError;
    use crate::language::tests::*;

    #[test]
    fn test_multiple_definition() {
        err(
            "module Foo module Foo",
            pos(1, 12),
            SemError::ShadowModule("Foo".into()),
        );
    }

    #[test]
    fn test_module_and_function() {
        err(
            "fun Foo() {} module Foo",
            pos(1, 14),
            SemError::ShadowFunction("Foo".into()),
        );
        err(
            "module Foo fun Foo() {}",
            pos(1, 12),
            SemError::ShadowModule("Foo".into()),
        );
    }

    #[test]
    fn test_module_definition() {
        ok("module Foo");
    }

    #[test]
    fn module_with_unknown_super_class() {
        err(
            "module B : A {}",
            pos(1, 12),
            SemError::UnknownIdentifier("A".into()),
        );
        err("module B : Int32 {}", pos(1, 12), SemError::ClassExpected);
    }

    #[test]
    fn field_defined_twice() {
        err(
            "module Foo { var a: Int32; var a: Int32; }",
            pos(1, 28),
            SemError::ShadowField("a".into()),
        );
    }

    #[test]
    fn let_field_without_initialization() {
        err(
            "module Foo { let a: Int32; }",
            pos(1, 14),
            SemError::LetMissingInitialization,
        );
    }

    #[test]
    fn test_module_fun_call() {
        ok("module Foo { fun foo() {} } fun main() { Foo::foo(); }");
    }

    #[ignore]
    #[test] // should fail but doesn't
    fn field_self_assignment() {
        err(
            "module Foo { var b: Int32 = b; }",
            pos(1, 34),
            SemError::UnknownIdentifier("b".into()),
        );
    }

    #[ignore]
    #[test]
    fn test_generic_bound() {
        ok("module Foo class A[T: Foo]");
    }
}
