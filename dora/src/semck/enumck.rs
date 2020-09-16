use dora_parser::ast::visit::{walk_file, Visitor};
use dora_parser::ast::{Ast, Enum, File, TypeParam};

use crate::error::msg::SemError;
use crate::semck;
use crate::ty::BuiltinType;
use crate::vm::{EnumId, EnumVariant, NodeMap, VM};

pub fn check<'ast>(vm: &mut VM<'ast>, ast: &'ast Ast, map_enum_defs: &NodeMap<EnumId>) {
    let mut enumck = EnumCheck {
        vm,
        ast,
        map_enum_defs,
        file_id: 0,
    };

    enumck.check();
}

struct EnumCheck<'x, 'ast: 'x> {
    vm: &'x mut VM<'ast>,
    ast: &'ast Ast,
    map_enum_defs: &'x NodeMap<EnumId>,
    file_id: u32,
}

impl<'x, 'ast> EnumCheck<'x, 'ast> {
    fn check(&mut self) {
        self.visit_ast(self.ast);
    }

    fn check_type_params(&mut self, _e: &'ast Enum, _type_params: &'ast [TypeParam]) {
        unimplemented!();
    }
}

impl<'x, 'ast> Visitor<'ast> for EnumCheck<'x, 'ast> {
    fn visit_file(&mut self, f: &'ast File) {
        walk_file(self, f);
        self.file_id += 1;
    }

    fn visit_enum(&mut self, e: &'ast Enum) {
        let enum_id = *self.map_enum_defs.get(e.id).unwrap();

        if let Some(ref type_params) = e.type_params {
            self.check_type_params(e, type_params);
        }

        let xenum = &self.vm.enums[enum_id];
        let mut xenum = xenum.write();

        let mut next_variant_id: u32 = 0;

        for value in &e.variants {
            let mut types: Vec<BuiltinType> = Vec::new();

            if let Some(ref variant_types) = value.types {
                for ty in variant_types {
                    let variant_ty = semck::read_type(self.vm, self.file_id.into(), ty)
                        .unwrap_or(BuiltinType::Error);
                    types.push(variant_ty);
                }
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

        if e.variants.is_empty() {
            self.vm
                .diag
                .lock()
                .report(xenum.file, e.pos, SemError::NoEnumValue);
        }
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
            fun give_me_a() -> Foo { Foo::A(1) }
            fun give_me_b() -> Foo { Foo::B(2.0F) }
            fun give_me_c() -> Foo { Foo::C }

        ");
    }

    #[test]
    fn enum_wrong_type() {
        err(
            "
            enum Foo { A(Int32), B(Float32), C}
            fun give_me_a() -> Foo { Foo::A(2.0F) }

        ",
            pos(3, 44),
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
            fun give_me_a() -> Foo { Foo::A }

        ",
            pos(3, 41),
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
            fun give_me_c() -> Foo { Foo::C(12.0F) }

        ",
            pos(3, 44),
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
            fun give_me_c() -> Foo { Foo::C() }
        ",
            pos(3, 44),
            SemError::EnumArgsNoParens("Foo".into(), "C".into()),
        );
    }

    #[test]
    fn enum_copy() {
        ok("
            enum Foo { A(Int32), B(Float32), C}
            fun foo_test(y: Foo) -> Foo { let x: Foo = y; x }
        ");
    }

    #[test]
    #[ignore]
    fn enum_generic() {
        ok("
            enum Foo[T] { One(T), Two }
        ");
    }
}
