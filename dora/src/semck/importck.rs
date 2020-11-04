use crate::error::msg::SemError;
use crate::semck::report_term_shadow;
use crate::sym::{TermSym, TypeSym};
use crate::vm::{ImportData, VM};

pub fn check<'a>(vm: &VM) {
    for import in &vm.imports {
        check_import(vm, import);
    }
}

fn check_import(vm: &VM, import: &ImportData) {
    let table = vm.namespace_table(import.namespace_id);

    let container_name = import.ast.container_name;
    let element_name = import.ast.element_name;

    let sym_type = table.read().get_type(container_name);

    match sym_type {
        Some(TypeSym::Enum(enum_id)) => {
            let xenum = vm.enums[enum_id].read();

            if let Some(&variant_id) = xenum.name_to_value.get(&element_name) {
                let sym = TermSym::EnumValue(enum_id, variant_id as usize);
                if let Some(sym) = table.write().insert_term(element_name, sym) {
                    report_term_shadow(
                        vm,
                        element_name,
                        import.file_id.into(),
                        import.ast.pos,
                        sym,
                    );
                }
            } else {
                let name = vm.interner.str(element_name).to_string();
                vm.diag.lock().report(
                    import.file_id.into(),
                    import.ast.pos,
                    SemError::UnknownEnumValue(name),
                );
            }
        }

        _ => {
            vm.diag.lock().report(
                import.file_id.into(),
                import.ast.pos,
                SemError::EnumExpected,
            );
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::error::msg::SemError;
    use crate::semck::tests::*;

    #[test]
    fn check_initializer() {
        ok("let a: Int32 = 0;");
        ok("let a: Int32 = 0; var b: Int32 = a + 1;");
        err(
            "var a: Int32 = foo;",
            pos(1, 16),
            SemError::UnknownIdentifier("foo".into()),
        );
    }

    #[test]
    fn check_type() {
        err(
            "var x: Foo = 0;",
            pos(1, 8),
            SemError::UnknownIdentifier("Foo".into()),
        );
    }
}
