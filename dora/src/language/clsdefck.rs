use std::sync::Arc;

use crate::language::error::msg::SemError;
use crate::language::sym::{NestedSymTable, Sym, SymTable};
use crate::language::ty::{SourceType, SourceTypeArray};
use crate::language::typeparamck::{self, ErrorReporting};
use crate::language::{self, read_type, AllowSelf, TypeParamContext};
use crate::vm::{
    ClassDefinitionId, FctDefinition, FctParent, Field, FieldId, FileId, NamespaceId, SemAnalysis,
};

use dora_parser::ast;
use dora_parser::interner::Name;
use dora_parser::lexer::position::Position;

pub fn check(sa: &SemAnalysis) {
    for cls in sa.classes.iter() {
        let (cls_id, file_id, ast, namespace_id) = {
            let cls = cls.read();
            (cls.id, cls.file_id, cls.ast.clone(), cls.namespace_id)
        };

        let mut clsck = ClsDefCheck {
            sa,
            cls_id,
            file_id,
            ast: &ast,
            namespace_id,
            sym: NestedSymTable::new(sa, namespace_id),
        };

        clsck.check();
    }
}

struct ClsDefCheck<'x> {
    sa: &'x SemAnalysis,
    cls_id: ClassDefinitionId,
    file_id: FileId,
    ast: &'x ast::Class,
    namespace_id: NamespaceId,
    sym: NestedSymTable<'x>,
}

impl<'x> ClsDefCheck<'x> {
    fn check(&mut self) {
        self.sym.push_level();

        if let Some(ref type_params) = self.ast.type_params {
            self.check_type_params(type_params);
        } else {
            let cls = self.sa.classes.idx(self.cls_id);
            let mut cls = cls.write();
            cls.ty = Some(SourceType::Class(self.cls_id, SourceTypeArray::empty()));
        }

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
        let ty = read_type(
            self.sa,
            &self.sym,
            self.file_id.into(),
            &f.data_type,
            TypeParamContext::Class(self.cls_id),
            AllowSelf::No,
        )
        .unwrap_or(SourceType::Error);
        self.add_field(f.pos, f.name, ty, f.mutable, f.is_pub);

        if !f.primary_ctor && f.expr.is_none() {
            self.sa.diag.lock().report(
                self.file_id.into(),
                f.pos,
                SemError::LetMissingInitialization,
            );
        }
    }

    fn visit_ctor(&mut self, node: &Arc<ast::Function>) {
        let fct = FctDefinition::new(
            self.sa,
            self.file_id,
            self.namespace_id,
            node,
            FctParent::Class(self.cls_id),
        );

        let fctid = self.sa.add_fct(fct);

        let cls = self.sa.classes.idx(self.cls_id);
        let mut cls = cls.write();
        cls.constructor = Some(fctid);
    }

    fn visit_method(&mut self, f: &Arc<ast::Function>) {
        let fct = FctDefinition::new(
            self.sa,
            self.file_id,
            self.namespace_id,
            f,
            FctParent::Class(self.cls_id),
        );

        let fctid = self.sa.add_fct(fct);

        let cls = self.sa.classes.idx(self.cls_id);
        let mut cls = cls.write();

        self.check_if_symbol_exists(f.name, f.pos, &cls.table);
        cls.table.insert(f.name, Sym::Fct(fctid));

        cls.methods.push(fctid);
    }

    fn add_field(
        &mut self,
        pos: Position,
        name: Name,
        ty: SourceType,
        mutable: bool,
        is_pub: bool,
    ) {
        let cls = self.sa.classes.idx(self.cls_id);
        let mut cls = cls.write();

        let fid: FieldId = cls.fields.len().into();

        let field = Field {
            id: fid,
            name,
            ty,
            offset: 0,
            mutable: mutable,
            is_pub,
        };

        self.check_if_symbol_exists(name, pos, &cls.table);

        cls.fields.push(field);
        cls.table.insert(name, Sym::Field(fid));
    }

    fn check_type_params(&mut self, ast_type_params: &[ast::TypeParam]) {
        let cls = self.sa.classes.idx(self.cls_id);
        let mut cls = cls.write();

        let type_params = language::check_type_params(
            self.sa,
            ast_type_params,
            &mut cls.type_params,
            &mut self.sym,
            self.file_id,
            self.ast.pos,
        );

        let params = SourceTypeArray::with(type_params);
        cls.ty = Some(SourceType::Class(self.cls_id, params));
    }

    fn check_parent_class(&mut self, parent_class: &ast::ParentClass) {
        let parent_ty = read_type(
            self.sa,
            &self.sym,
            self.file_id,
            &parent_class.parent_ty,
            TypeParamContext::Class(self.cls_id),
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

                let cls = self.sa.classes.idx(self.cls_id);
                let mut cls = cls.write();
                cls.parent_class = Some(parent_ty);
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

        if self.cls_id != object_cls {
            let cls = self.sa.classes.idx(self.cls_id);
            let mut cls = cls.write();

            let type_params = SourceTypeArray::empty();
            cls.parent_class = Some(SourceType::Class(object_cls, type_params));
        }
    }

    fn check_if_symbol_exists(&mut self, name: Name, pos: Position, table: &SymTable) {
        if let Some(sym) = table.get(name) {
            let file: FileId = self.file_id.into();

            match sym {
                Sym::Fct(method) => {
                    let method = self.sa.fcts.idx(method);
                    let method = method.read();

                    let method_name = self.sa.interner.str(method.name).to_string();
                    let msg = SemError::MethodExists(method_name, method.pos);
                    self.sa.diag.lock().report(file, pos, msg);
                }

                Sym::Field(_) => {
                    let name = self.sa.interner.str(name).to_string();
                    self.sa
                        .diag
                        .lock()
                        .report(file, pos, SemError::ShadowField(name));
                }

                _ => unreachable!(),
            }
        }
    }
}

pub fn check_super_definition(sa: &SemAnalysis) {
    for cls in sa.classes.iter() {
        let cls = cls.read();

        if let Some(ref parent_class) = &cls.ast.parent_class {
            let error = ErrorReporting::Yes(cls.file_id, parent_class.pos);
            typeparamck::check_super(sa, &*cls, error);
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::language::error::msg::SemError;
    use crate::language::tests::*;

    #[test]
    fn test_class_definition() {
        ok("class Foo");
        ok("class Foo()");
        ok("class Foo(let a: Int32)");
        ok("class Foo(let a: Int32, let b:Int32)");
        ok("class Foo(let a: Foo)");
        ok("class Foo(let a: Bar) class Bar");
        err(
            "class Foo(let a: Unknown)",
            pos(1, 18),
            SemError::UnknownIdentifier("Unknown".into()),
        );
        err(
            "class Foo(let a: Int32, let a: Int32)",
            pos(1, 29),
            SemError::ShadowField("a".to_string()),
        );
    }

    #[test]
    fn class_with_unknown_super_class() {
        err(
            "class B extends A {}",
            pos(1, 17),
            SemError::UnknownIdentifier("A".into()),
        );
        err(
            "@open class B extends A {}",
            pos(1, 23),
            SemError::UnknownIdentifier("A".into()),
        );
        err(
            "class B extends Int32 {}",
            pos(1, 17),
            SemError::ClassExpected,
        );
    }

    #[test]
    fn class_with_open_modifier() {
        ok("@open class A {}");
        ok("@open class A {} class B extends A {}");
        err(
            "class A {} class B extends A {}",
            pos(1, 28),
            SemError::UnderivableType("A".into()),
        );
    }

    #[test]
    fn non_field_ctor_arguments() {
        ok("class Foo(a: Int32, b: Int32)");
        ok("class Foo(let a: Int32, b: Int32)");
        ok("class Foo(a: Int32, var b: Int32)");
        err(
            "class Foo(a: Int32, a: Int32)",
            pos(1, 21),
            SemError::ShadowParam("a".into()),
        );
        err(
            "class Foo(a: Int32, let a: Int32)",
            pos(1, 25),
            SemError::ShadowParam("a".into()),
        );
        err(
            "class Foo(let a: Int32, a: Int32)",
            pos(1, 25),
            SemError::ShadowParam("a".into()),
        );
        err(
            "class Foo(a: Int32) fun f(x: Foo) { x.a = 1I; }",
            pos(1, 38),
            SemError::UnknownField("a".into(), "Foo".into()),
        );

        ok("class Foo(a: Int32) fun foo(): Foo { return Foo(1I); } ");
    }

    #[test]
    fn field_defined_twice() {
        err(
            "class Foo { var a: Int32 = 0; var a: Int32 = 0; }",
            pos(1, 31),
            SemError::ShadowField("a".into()),
        );
    }

    #[test]
    fn field_defined_twice_via_constructor() {
        err(
            "class Foo(let a: Int32) { var a: Int32 = 0; }",
            pos(1, 27),
            SemError::ShadowField("a".into()),
        );
    }

    #[test]
    fn let_field_without_initialization() {
        err(
            "class Foo { let a: Int32; }",
            pos(1, 13),
            SemError::LetMissingInitialization,
        );
    }

    #[test]
    fn field_self_assignment() {
        err(
            "class Foo(a: Int32) { var b: Int32 = b; }",
            pos(1, 38),
            SemError::UnknownIdentifier("b".into()),
        );
    }

    #[test]
    fn test_generic_class() {
        ok("class A[T]");
        ok("class A[X, Y]");
        err(
            "class A[T, T]",
            pos(1, 12),
            SemError::TypeParamNameNotUnique("T".into()),
        );
        err("class A[]", pos(1, 1), SemError::TypeParamsExpected);
    }

    #[test]
    fn test_generic_argument() {
        ok("class A[T](val: T)");
        ok("class A[T](var val: T)");
        ok("class A[T](let val: T)");
    }

    #[test]
    fn test_generic_bound() {
        err(
            "class A[T: Foo]",
            pos(1, 12),
            SemError::UnknownIdentifier("Foo".into()),
        );
        err(
            "class Foo class A[T: Foo]",
            pos(1, 22),
            SemError::BoundExpected,
        );
        ok("trait Foo {} class A[T: Foo]");
    }

    #[test]
    fn test_duplicate_trait_bound() {
        err(
            "trait Foo {}
            class A[T: Foo + Foo]",
            pos(2, 21),
            SemError::DuplicateTraitBound,
        );
    }

    #[test]
    fn test_super_class_with_superfluous_type_params() {
        err(
            "
            @open class A
            class B extends A[Int32] {}",
            pos(3, 29),
            SemError::WrongNumberTypeParams(0, 1),
        );
    }

    #[test]
    fn test_defining_static_method_twice() {
        err(
            "class X { @static fun foo() {} @static fun foo(a: String) {} }",
            pos(1, 40),
            SemError::MethodExists("foo".into(), pos(1, 19)),
        );
    }
}
