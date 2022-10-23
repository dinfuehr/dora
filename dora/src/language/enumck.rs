use std::sync::Arc;

use parking_lot::RwLock;

use dora_parser::ast;

use crate::language::error::msg::ErrorMessage;
use crate::language::sem_analysis::{EnumDefinition, EnumVariant, SemAnalysis, SourceFileId};
use crate::language::sym::{ModuleSymTable, Sym};
use crate::language::ty::SourceType;
use crate::language::{read_type, AllowSelf, TypeParamContext};

pub fn check(sa: &SemAnalysis) {
    for enum_ in sa.enums.iter() {
        let ast = enum_.read().ast.clone();

        let mut enumck = EnumCheck {
            sa,
            file_id: enum_.read().file_id,
            ast: &ast,
            enum_: &enum_,
        };

        enumck.check();
    }
}

struct EnumCheck<'x> {
    sa: &'x SemAnalysis,
    file_id: SourceFileId,
    ast: &'x Arc<ast::Enum>,
    enum_: &'x RwLock<EnumDefinition>,
}

impl<'x> EnumCheck<'x> {
    fn check(&mut self) {
        let mut symtable = ModuleSymTable::new(self.sa, self.enum_.read().module_id);

        symtable.push_level();

        {
            let enum_ = self.enum_.read();

            for (id, name) in enum_.type_params().names() {
                symtable.insert(name, Sym::TypeParam(id));
            }
        }

        let mut variant_idx: usize = 0;
        let mut simple_enumeration = true;

        for value in &self.ast.variants {
            let mut types: Vec<SourceType> = Vec::new();

            if let Some(ref variant_types) = value.types {
                for ty in variant_types {
                    let variant_ty = read_type(
                        self.sa,
                        &symtable,
                        self.file_id.into(),
                        ty,
                        TypeParamContext::Enum(self.enum_.read().id()),
                        AllowSelf::No,
                    )
                    .unwrap_or(SourceType::Error);
                    types.push(variant_ty);
                }
            }

            if types.len() > 0 {
                simple_enumeration = false;
            }

            self.enum_.write().variants[variant_idx].types = types;
            variant_idx += 1;
        }

        self.enum_.write().simple_enumeration = simple_enumeration;

        symtable.pop_level();
    }
}

pub fn check_variants(sa: &SemAnalysis) {
    for enum_ in sa.enums.iter() {
        let mut enum_ = enum_.write();
        let ast = enum_.ast.clone();

        let mut enumck = EnumCheckVariants {
            sa,
            file_id: enum_.file_id,
            ast: &ast,
            enum_: &mut *enum_,
        };

        enumck.check();
    }
}

struct EnumCheckVariants<'x> {
    sa: &'x SemAnalysis,
    file_id: SourceFileId,
    ast: &'x Arc<ast::Enum>,
    enum_: &'x mut EnumDefinition,
}

impl<'x> EnumCheckVariants<'x> {
    fn check(&mut self) {
        let mut next_variant_id: u32 = 0;

        for value in &self.ast.variants {
            let variant = EnumVariant {
                id: next_variant_id as usize,
                name: value.name,
                types: Vec::new(),
            };

            self.enum_.variants.push(variant);
            let result = self.enum_.name_to_value.insert(value.name, next_variant_id);

            if result.is_some() {
                let name = self.sa.interner.str(value.name).to_string();
                self.sa.diag.lock().report(
                    self.enum_.file_id,
                    value.pos,
                    ErrorMessage::ShadowEnumVariant(name),
                );
            }

            next_variant_id += 1;
        }

        if self.ast.variants.is_empty() {
            self.sa.diag.lock().report(
                self.enum_.file_id,
                self.ast.pos,
                ErrorMessage::NoEnumVariant,
            );
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::language::error::msg::ErrorMessage;
    use crate::language::tests::*;

    #[test]
    fn enum_definitions() {
        err("enum Foo {}", pos(1, 1), ErrorMessage::NoEnumVariant);
        ok("enum Foo { A, B, C }");
        err(
            "enum Foo { A, A }",
            pos(1, 15),
            ErrorMessage::ShadowEnumVariant("A".into()),
        );
    }

    #[test]
    fn enum_with_argument() {
        ok("
            enum Foo { A(Int32), B(Float32), C}
            fun give_me_a(): Foo { Foo::A(1i32) }
            fun give_me_b(): Foo { Foo::B(2.0f32) }
            fun give_me_c(): Foo { Foo::C }

        ");
    }

    #[test]
    fn enum_wrong_type() {
        err(
            "
            enum Foo { A(Int32), B(Float32), C}
            fun give_me_a(): Foo { Foo::A(2.0f32) }

        ",
            pos(3, 42),
            ErrorMessage::EnumArgsIncompatible(
                "Foo".into(),
                "A".into(),
                vec!["Int32".into()],
                vec!["Float32".into()],
            ),
        );
    }

    #[test]
    fn enum_missing_args() {
        err(
            "
            enum Foo { A(Int32), B(Float32), C}
            fun give_me_a(): Foo { Foo::A }

        ",
            pos(3, 39),
            ErrorMessage::EnumArgsIncompatible(
                "Foo".into(),
                "A".into(),
                vec!["Int32".into()],
                Vec::new(),
            ),
        );
    }

    #[test]
    fn enum_unexpected_args() {
        err(
            "
            enum Foo { A(Int32), B(Float32), C}
            fun give_me_c(): Foo { Foo::C(12.0f32) }

        ",
            pos(3, 42),
            ErrorMessage::EnumArgsIncompatible(
                "Foo".into(),
                "C".into(),
                Vec::new(),
                vec!["Float32".into()],
            ),
        );
    }

    #[test]
    fn enum_parens_but_no_args() {
        err(
            "
            enum Foo { A(Int32), B(Float32), C}
            fun give_me_c(): Foo { Foo::C() }
        ",
            pos(3, 42),
            ErrorMessage::EnumArgsNoParens("Foo".into(), "C".into()),
        );
    }

    #[test]
    fn enum_copy() {
        ok("
            enum Foo { A(Int32), B(Float32), C}
            fun foo_test(y: Foo): Foo { let x: Foo = y; x }
        ");
    }

    #[test]
    fn enum_generic() {
        ok("
            enum Foo[T] { One(T), Two }
        ");
    }

    #[test]
    fn enum_with_type_param() {
        ok("trait SomeTrait {} enum MyOption[T: SomeTrait] { None, Some(T) }");
    }

    #[test]
    fn enum_generic_with_failures() {
        err(
            "enum MyOption[] { A, B }",
            pos(1, 1),
            ErrorMessage::TypeParamsExpected,
        );

        err(
            "enum MyOption[X, X] { A, B }",
            pos(1, 18),
            ErrorMessage::TypeParamNameNotUnique("X".into()),
        );

        err(
            "enum MyOption[X: NonExistingTrait] { A, B }",
            pos(1, 18),
            ErrorMessage::UnknownIdentifier("NonExistingTrait".into()),
        );
    }

    #[test]
    fn check_enum_type() {
        err(
            "
                enum MyOption[X] { A, B }
                fun foo(v: MyOption): Unit {}
            ",
            pos(3, 28),
            ErrorMessage::WrongNumberTypeParams(1, 0),
        );
    }

    #[test]
    fn check_enum_value() {
        ok("
            enum Foo { A(Int32), B }
            fun foo(): Foo { Foo::A(1i32) }
            fun bar(): Foo { Foo::B }
        ");

        err(
            "
            enum Foo { A(Int32), B }
            fun foo(): Foo { Foo::A(true) }
        ",
            pos(3, 36),
            ErrorMessage::EnumArgsIncompatible(
                "Foo".into(),
                "A".into(),
                vec!["Int32".into()],
                vec!["Bool".into()],
            ),
        );
    }

    #[test]
    fn check_enum_value_generic() {
        ok("
            enum Foo[T] { A, B }
            fun foo(): Unit { let tmp = Foo[String]::B; }
        ");

        err(
            "
            trait SomeTrait {}
            enum Foo[T: SomeTrait] { A, B }
            fun foo(): Unit { let tmp = Foo[String]::B; }
        ",
            pos(4, 52),
            ErrorMessage::TypeNotImplementingTrait("String".into(), "SomeTrait".into()),
        );
    }

    #[test]
    fn enum_with_generic_argument() {
        ok("
            enum Foo[T] { A(T), B }
            fun foo(): Unit { let tmp = Foo[Int32]::A(0i32); }
        ");

        err(
            "
            enum Foo[T] { A(T), B }
            fun foo(): Unit { let tmp = Foo[Int32]::A(true); }
        ",
            pos(3, 54),
            ErrorMessage::EnumArgsIncompatible(
                "Foo".into(),
                "A".into(),
                vec!["T".into()],
                vec!["Bool".into()],
            ),
        );
    }

    #[test]
    fn enum_move_generic() {
        ok("
            enum Foo[T] { A(T), B }
            fun foo(x: Foo[Int32]): Foo[Int32] { x }
        ");

        err(
            "
            enum Foo[T] { A(T), B }
            fun foo(x: Foo[Int32]): Foo[Float32] { x }
        ",
            pos(3, 50),
            ErrorMessage::ReturnType("Foo[Float32]".into(), "Foo[Int32]".into()),
        );
    }

    #[test]
    fn enum_nested() {
        ok("
            enum Foo { A(Foo), B }
        ");
    }
}
