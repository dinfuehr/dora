use crate::error::msg::SemError;
use crate::semck::report_term_shadow;
use crate::sym::{TermSym, TypeSym};
use crate::vm::{NamespaceId, NodeMap, VM};
use dora_parser::ast::visit::{walk_file, walk_namespace, Visitor};
use dora_parser::ast::{File, Import, Namespace};

pub fn check<'a>(vm: &mut VM, map_namespaces: &'a NodeMap<NamespaceId>) {
    let mut checker = ImportCheck {
        vm,
        file_id: 0,
        namespace_id: None,
        map_namespaces,
    };
    checker.check();
}

struct ImportCheck<'a> {
    vm: &'a mut VM,
    file_id: u32,
    namespace_id: Option<NamespaceId>,

    map_namespaces: &'a NodeMap<NamespaceId>,
}

impl<'a> ImportCheck<'a> {
    fn check(&mut self) {
        let files = self.vm.files.clone();
        let files = files.read();

        for file in files.iter() {
            self.visit_file(file);
        }
    }
}

impl<'a> Visitor for ImportCheck<'a> {
    fn visit_file(&mut self, f: &File) {
        walk_file(self, f);
        self.file_id += 1;
    }

    fn visit_namespace(&mut self, namespace: &Namespace) {
        let namespace_id = self.map_namespaces.get(namespace.id).cloned().unwrap();

        let old_namespace_id = self.namespace_id;
        self.namespace_id = Some(namespace_id);
        walk_namespace(self, namespace);
        self.namespace_id = old_namespace_id;
    }

    fn visit_import(&mut self, import: &Import) {
        let table = self.vm.namespace_table(self.namespace_id);

        let sym_type = table.read().get_type(import.container_name);

        match sym_type {
            Some(TypeSym::Enum(enum_id)) => {
                let xenum = self.vm.enums[enum_id].read();

                if let Some(&variant_id) = xenum.name_to_value.get(&import.element_name) {
                    let sym = TermSym::EnumValue(enum_id, variant_id as usize);
                    if let Some(sym) = table.write().insert_term(import.element_name, sym) {
                        report_term_shadow(
                            self.vm,
                            import.element_name,
                            self.file_id.into(),
                            import.pos,
                            sym,
                        );
                    }
                } else {
                    let name = self.vm.interner.str(import.element_name).to_string();
                    self.vm.diag.lock().report(
                        self.file_id.into(),
                        import.pos,
                        SemError::UnknownEnumValue(name),
                    );
                }
            }

            _ => {
                self.vm
                    .diag
                    .lock()
                    .report(self.file_id.into(), import.pos, SemError::EnumExpected);
            }
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
            SemError::UnknownType("Foo".into()),
        );
    }
}
