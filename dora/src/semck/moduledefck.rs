use parking_lot::RwLock;

use crate::error::msg::SemError;
use crate::semck;
use crate::sym::{SymTables, TypeSym};
use crate::ty::{SourceType, TypeList};

use crate::vm::module::ModuleId;
use crate::vm::{Fct, FctId, FctKind, FctParent, FctSrc, Field, NodeMap, VM};
use dora_parser::ast::visit::{self, Visitor};
use dora_parser::ast::{self, Ast};
use dora_parser::interner::Name;
use dora_parser::lexer::position::Position;

pub fn check<'ast>(vm: &VM<'ast>, ast: &'ast Ast, map_module_defs: &NodeMap<ModuleId>) {
    let global_namespace = vm.global_namespace.read();
    let mut module_check = ModuleCheck {
        vm,
        ast,
        sym: SymTables::new(&*global_namespace),
        module_id: None,
        map_module_defs,
        file_id: 0,
    };

    module_check.check();
}

struct ModuleCheck<'x, 'ast: 'x> {
    vm: &'x VM<'ast>,
    ast: &'ast ast::Ast,
    map_module_defs: &'x NodeMap<ModuleId>,
    file_id: u32,
    sym: SymTables<'x>,

    module_id: Option<ModuleId>,
}

impl<'x, 'ast> ModuleCheck<'x, 'ast> {
    fn check(&mut self) {
        self.visit_ast(self.ast);
    }

    fn add_field(&mut self, pos: Position, name: Name, ty: SourceType, reassignable: bool) {
        let module = self.vm.modules.idx(self.module_id.unwrap());
        let mut module = module.write();

        for field in &module.fields {
            if field.name == name {
                let name = self.vm.interner.str(name).to_string();
                self.vm
                    .diag
                    .lock()
                    .report(module.file, pos, SemError::ShadowField(name));
            }
        }

        let field = Field {
            id: module.fields.len().into(),
            name,
            ty,
            offset: 0,
            reassignable,
        };

        module.fields.push(field);
    }

    fn check_parent_class(&mut self, parent_class: &'ast ast::ParentClass) {
        let name = self.vm.interner.str(parent_class.name).to_string();
        let sym = self.sym.get_type(parent_class.name);

        match sym {
            Some(TypeSym::SymClass(cls_id)) => {
                let super_cls = self.vm.classes.idx(cls_id);
                let super_cls = super_cls.read();

                if !super_cls.has_open {
                    let msg = SemError::UnderivableType(name);
                    self.vm
                        .diag
                        .lock()
                        .report(self.file_id.into(), parent_class.pos, msg);
                }

                let number_type_params = parent_class.type_params.len();

                if number_type_params != super_cls.type_params.len() {
                    let msg = SemError::WrongNumberTypeParams(
                        super_cls.type_params.len(),
                        number_type_params,
                    );
                    self.vm
                        .diag
                        .lock()
                        .report(self.file_id.into(), parent_class.pos, msg);
                } else {
                    let mut types = Vec::new();

                    for tp in &parent_class.type_params {
                        let ty =
                            semck::read_type_table(self.vm, &self.sym, self.file_id.into(), tp)
                                .unwrap_or(SourceType::Error);
                        types.push(ty);
                    }

                    let list = TypeList::with(types);
                    let list_id = self.vm.lists.lock().insert(list);

                    let module = self.vm.modules.idx(self.module_id.unwrap());
                    let mut module = module.write();
                    module.parent_class = Some(SourceType::Class(cls_id, list_id));
                }
            }

            _ => {
                let msg = SemError::UnknownClass(name);
                self.vm
                    .diag
                    .lock()
                    .report(self.file_id.into(), parent_class.pos, msg);
            }
        }
    }

    fn use_object_class_as_parent(&mut self) {
        let object_cls = self.vm.known.classes.object;
        let module_id = self.module_id.unwrap();

        let module = self.vm.modules.idx(module_id);
        let mut module = module.write();

        let list = TypeList::empty();
        let list_id = self.vm.lists.lock().insert(list);
        module.parent_class = Some(SourceType::Class(object_cls, list_id));
    }
}

impl<'x, 'ast> Visitor<'ast> for ModuleCheck<'x, 'ast> {
    fn visit_file(&mut self, f: &'ast ast::File) {
        visit::walk_file(self, f);
        self.file_id += 1;
    }

    fn visit_class(&mut self, _: &'ast ast::Class) {}

    fn visit_module(&mut self, m: &'ast ast::Module) {
        self.module_id = Some(*self.map_module_defs.get(m.id).unwrap());

        self.sym.push_level();

        visit::walk_module(self, m);

        if let Some(ref parent_class) = m.parent_class {
            self.check_parent_class(parent_class);
        } else {
            self.use_object_class_as_parent();
        }

        self.module_id = None;
        self.sym.pop_level();
    }

    fn visit_field(&mut self, f: &'ast ast::Field) {
        let ty = semck::read_type_table(self.vm, &self.sym, self.file_id.into(), &f.data_type)
            .unwrap_or(SourceType::Unit);
        self.add_field(f.pos, f.name, ty, f.reassignable);

        if !f.reassignable && !f.primary_ctor && f.expr.is_none() {
            self.vm.diag.lock().report(
                self.file_id.into(),
                f.pos,
                SemError::LetMissingInitialization,
            );
        }
    }

    fn visit_ctor(&mut self, f: &'ast ast::Function) {
        let module_id = self.module_id.unwrap();

        let kind = if f.block.is_some() {
            FctKind::Source(RwLock::new(FctSrc::new()))
        } else {
            FctKind::Definition
        };

        let fct = Fct {
            id: FctId(0),
            pos: f.pos,
            ast: f,
            name: f.name,
            namespace_id: None,
            param_types: Vec::new(),
            return_type: SourceType::Unit,
            parent: FctParent::Module(module_id),
            has_override: f.has_override,
            has_open: f.has_open,
            has_final: f.has_final,
            has_optimize_immediately: f.has_optimize_immediately,
            is_pub: true,
            is_static: false,
            is_abstract: false,
            is_test: f.is_test,
            use_cannon: f.use_cannon,
            internal: f.internal,
            internal_resolved: false,
            overrides: None,
            is_constructor: f.is_constructor,
            vtable_index: None,
            initialized: false,
            impl_for: None,
            file: self.file_id.into(),
            variadic_arguments: false,

            type_params: Vec::new(),
            kind,
            bytecode: None,
            intrinsic: None,
        };

        let fctid = self.vm.add_fct(fct);

        let module = self.vm.modules.idx(self.module_id.unwrap());
        let mut module = module.write();
        module.constructor = Some(fctid);
    }

    fn visit_method(&mut self, f: &'ast ast::Function) {
        if self.module_id.is_none() {
            return;
        }

        let kind = if f.block.is_some() {
            FctKind::Source(RwLock::new(FctSrc::new()))
        } else {
            FctKind::Definition
        };

        let fct = Fct {
            id: FctId(0),
            ast: f,
            pos: f.pos,
            name: f.name,
            namespace_id: None,
            param_types: Vec::new(),
            return_type: SourceType::Unit,
            parent: FctParent::Module(self.module_id.unwrap()),
            has_override: f.has_override,
            has_optimize_immediately: f.has_optimize_immediately,
            variadic_arguments: false,

            // abstract for methods also means that method is open to
            // override
            has_open: f.has_open || f.is_abstract,
            has_final: f.has_final,
            is_pub: f.is_pub,
            is_static: f.is_static,
            is_abstract: f.is_abstract,
            is_test: f.is_test,
            use_cannon: f.use_cannon,
            internal: f.internal,
            internal_resolved: false,
            overrides: None,
            is_constructor: false,
            vtable_index: None,
            initialized: false,
            impl_for: None,
            file: self.file_id.into(),

            type_params: Vec::new(),
            kind,
            bytecode: None,
            intrinsic: None,
        };

        let fctid = self.vm.add_fct(fct);

        let module = self.vm.modules.idx(self.module_id.unwrap());
        let mut module = module.write();
        module.methods.push(fctid);
    }
}

#[cfg(test)]
mod tests {
    use crate::error::msg::SemError;
    use crate::semck::tests::*;

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
            SemError::UnknownClass("A".into()),
        );
        err(
            "module B : Int32 {}",
            pos(1, 12),
            SemError::UnderivableType("Int32".into()),
        );
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
