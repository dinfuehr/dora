use dora_parser::ast::visit::Visitor;
use dora_parser::ast::{Ast, Enum};

use crate::error::msg::SemError;
use crate::vm::{EnumId, EnumVariant, NodeMap, VM};

pub fn check<'ast>(vm: &mut VM<'ast>, ast: &'ast Ast, map_enum_defs: &NodeMap<EnumId>) {
    let mut enumck = EnumCheck {
        vm,
        ast,
        map_enum_defs,
    };

    enumck.check();
}

struct EnumCheck<'x, 'ast: 'x> {
    vm: &'x mut VM<'ast>,
    ast: &'ast Ast,
    map_enum_defs: &'x NodeMap<EnumId>,
}

impl<'x, 'ast> EnumCheck<'x, 'ast> {
    fn check(&mut self) {
        self.visit_ast(self.ast);
    }
}

impl<'x, 'ast> Visitor<'ast> for EnumCheck<'x, 'ast> {
    fn visit_enum(&mut self, e: &'ast Enum) {
        let enum_id = *self.map_enum_defs.get(e.id).unwrap();

        let mut xenum = self.vm.enums[enum_id].write();
        let mut enum_value_int: u32 = 0;
        assert!(e.type_params.is_none());

        for value in &e.variants {
            let variant = EnumVariant {
                name: value.name,
                types: Vec::new(),
            };
            xenum.variants.push(variant);
            let result = xenum.name_to_value.insert(value.name, enum_value_int);
            assert!(value.types.is_none());

            if result.is_some() {
                let name = self.vm.interner.str(value.name).to_string();
                self.vm
                    .diag
                    .lock()
                    .report(xenum.file, value.pos, SemError::ShadowEnumValue(name));
            }

            enum_value_int += 1;
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
}
