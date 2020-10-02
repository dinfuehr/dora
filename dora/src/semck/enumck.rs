use std::collections::HashSet;

use dora_parser::ast::visit::{walk_file, Visitor};
use dora_parser::ast::{Ast, Enum, File, TypeParam};

use crate::error::msg::SemError;
use crate::semck;
use crate::sym::TypeSym;
use crate::ty::BuiltinType;
use crate::vm::{EnumId, EnumVariant, NodeMap, VM};

pub fn check<'ast>(vm: &mut VM<'ast>, ast: &'ast Ast, map_enum_defs: &NodeMap<EnumId>) {
    let mut enumck = EnumCheck {
        vm,
        ast,
        map_enum_defs,
        file_id: 0,

        enum_id: None,
    };

    enumck.check();
}

struct EnumCheck<'x, 'ast: 'x> {
    vm: &'x mut VM<'ast>,
    ast: &'ast Ast,
    map_enum_defs: &'x NodeMap<EnumId>,
    file_id: u32,

    enum_id: Option<EnumId>,
}

impl<'x, 'ast> EnumCheck<'x, 'ast> {
    fn check(&mut self) {
        self.visit_ast(self.ast);
    }

    fn check_type_params(&mut self, ast: &'ast Enum, type_params: &'ast [TypeParam]) {
        let enum_id = self.enum_id.expect("missing enum_id");
        let xenum = &self.vm.enums[enum_id];
        let mut xenum = xenum.write();

        if type_params.len() > 0 {
            let mut names = HashSet::new();
            let mut type_param_id = 0;
            let mut params = Vec::new();

            for type_param in type_params {
                if !names.insert(type_param.name) {
                    let name = self.vm.interner.str(type_param.name).to_string();
                    let msg = SemError::TypeParamNameNotUnique(name);
                    self.vm.diag.lock().report(xenum.file, type_param.pos, msg);
                }

                params.push(BuiltinType::TypeParam(type_param_id.into()));

                for bound in &type_param.bounds {
                    let ty = semck::read_type(self.vm, xenum.file, bound);

                    match ty {
                        Some(BuiltinType::TraitObject(trait_id)) => {
                            if !xenum.type_params[type_param_id]
                                .trait_bounds
                                .insert(trait_id)
                            {
                                let msg = SemError::DuplicateTraitBound;
                                self.vm.diag.lock().report(xenum.file, type_param.pos, msg);
                            }
                        }

                        None => {
                            // unknown type, error is already thrown
                        }

                        _ => {
                            let msg = SemError::BoundExpected;
                            self.vm.diag.lock().report(xenum.file, bound.pos(), msg);
                        }
                    }
                }

                let sym = TypeSym::SymTypeParam(type_param_id.into());
                self.vm.sym.lock().insert_type(type_param.name, sym);
                type_param_id += 1;
            }
        } else {
            let msg = SemError::TypeParamsExpected;
            self.vm.diag.lock().report(xenum.file, ast.pos, msg);
        }
    }
}

impl<'x, 'ast> Visitor<'ast> for EnumCheck<'x, 'ast> {
    fn visit_file(&mut self, f: &'ast File) {
        walk_file(self, f);
        self.file_id += 1;
    }

    fn visit_enum(&mut self, e: &'ast Enum) {
        let enum_id = *self.map_enum_defs.get(e.id).unwrap();
        self.enum_id = Some(enum_id);

        if let Some(ref type_params) = e.type_params {
            self.check_type_params(e, type_params);
        }

        let xenum = &self.vm.enums[enum_id];
        let mut xenum = xenum.write();

        let mut next_variant_id: u32 = 0;
        let mut simple_enumeration = true;

        for value in &e.variants {
            let mut types: Vec<BuiltinType> = Vec::new();

            if let Some(ref variant_types) = value.types {
                for ty in variant_types {
                    let variant_ty = semck::read_type(self.vm, self.file_id.into(), ty)
                        .unwrap_or(BuiltinType::Error);
                    types.push(variant_ty);
                }
            }

            if types.len() > 0 {
                simple_enumeration = false;
            }

            let variant = EnumVariant {
                name: value.name,
                types: types,
            };
            xenum.variants.push(variant);
            let result = xenum.name_to_value.insert(value.name, next_variant_id);

            if result.is_some() {
                let name = self.vm.interner.str(value.name).to_string();
                self.vm
                    .diag
                    .lock()
                    .report(xenum.file, value.pos, SemError::ShadowEnumValue(name));
            }

            next_variant_id += 1;
        }

        xenum.simple_enumeration = simple_enumeration;

        if e.variants.is_empty() {
            self.vm
                .diag
                .lock()
                .report(xenum.file, e.pos, SemError::NoEnumValue);
        }

        self.enum_id = None;
    }
}

#[cfg(test)]
mod tests {
    use crate::error::msg::SemError;
    use crate::semck::tests::*;

    #[test]
    fn enum_definitions() {
        err("enum Foo {}", pos(1, 1), SemError::NoEnumValue);
        ok("enum Foo { A, B, C }");
        err(
            "enum Foo { A, A }",
            pos(1, 15),
            SemError::ShadowEnumValue("A".into()),
        );
    }

    #[test]
    fn enum_with_argument() {
        ok("
            enum Foo { A(Int32), B(Float32), C}
            fun give_me_a(): Foo { Foo::A(1) }
            fun give_me_b(): Foo { Foo::B(2.0F) }
            fun give_me_c(): Foo { Foo::C }

        ");
    }

    #[test]
    fn enum_wrong_type() {
        err(
            "
            enum Foo { A(Int32), B(Float32), C}
            fun give_me_a(): Foo { Foo::A(2.0F) }

        ",
            pos(3, 42),
            SemError::EnumArgsIncompatible(
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
            SemError::EnumArgsIncompatible(
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
            fun give_me_c(): Foo { Foo::C(12.0F) }

        ",
            pos(3, 42),
            SemError::EnumArgsIncompatible(
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
            SemError::EnumArgsNoParens("Foo".into(), "C".into()),
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
            SemError::TypeParamsExpected,
        );

        err(
            "enum MyOption[X, X] { A, B }",
            pos(1, 18),
            SemError::TypeParamNameNotUnique("X".into()),
        );

        err(
            "enum MyOption[X: NonExistingTrait] { A, B }",
            pos(1, 18),
            SemError::UnknownType("NonExistingTrait".into()),
        );
    }

    #[test]
    fn check_enum_type() {
        err(
            "
                enum MyOption[X] { A, B }
                fun foo(v: MyOption) {}
            ",
            pos(3, 28),
            SemError::WrongNumberTypeParams(1, 0),
        );
    }

    #[test]
    fn check_enum_value() {
        ok("
            enum Foo { A(Int32), B }
            fun foo(): Foo { Foo::A(1) }
            fun bar(): Foo { Foo::B }
        ");

        err(
            "
            enum Foo { A(Int32), B }
            fun foo(): Foo { Foo::A(true) }
        ",
            pos(3, 36),
            SemError::EnumArgsIncompatible(
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
            fun foo() { let tmp = Foo[String]::B; }
        ");

        err(
            "
            trait SomeTrait {}
            enum Foo[T: SomeTrait] { A, B }
            fun foo() { let tmp = Foo[String]::B; }
        ",
            pos(4, 46),
            SemError::TraitBoundNotSatisfied("String".into(), "SomeTrait".into()),
        );
    }

    #[test]
    fn enum_with_generic_argument() {
        ok("
            enum Foo[T] { A(T), B }
            fun foo() { let tmp = Foo[Int32]::A(0); }
        ");

        err(
            "
            enum Foo[T] { A(T), B }
            fun foo() { let tmp = Foo[Int32]::A(true); }
        ",
            pos(3, 48),
            SemError::EnumArgsIncompatible(
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
            SemError::ReturnType("Foo[Float32]".into(), "Foo[Int32]".into()),
        );
    }
}
