use std::collections::HashSet;

use crate::error::msg::SemError;
use crate::semck;
use crate::sym::{NestedSymTable, TypeSym};
use crate::ty::SourceType;
use crate::vm::{FileId, NamespaceId, StructFieldData, StructFieldId, StructId, VM};

use dora_parser::ast;
use dora_parser::interner::Name;

pub fn check(vm: &VM) {
    for xstruct in vm.structs.iter() {
        let (struct_id, file_id, ast, namespace_id) = {
            let xstruct = xstruct.read();
            (
                xstruct.id,
                xstruct.file_id,
                xstruct.ast.clone(),
                xstruct.namespace_id,
            )
        };

        let mut clsck = StructCheck {
            vm,
            struct_id,
            file_id,
            ast: &ast,
            namespace_id,
            symtable: NestedSymTable::new(vm, namespace_id),
            fields: HashSet::new(),
        };

        clsck.check();
    }
}

struct StructCheck<'x> {
    vm: &'x VM,
    struct_id: StructId,
    file_id: FileId,
    ast: &'x ast::Struct,
    namespace_id: NamespaceId,
    symtable: NestedSymTable<'x>,
    fields: HashSet<Name>,
}

impl<'x> StructCheck<'x> {
    fn check(&mut self) {
        self.symtable.push_level();

        if let Some(ref type_params) = self.ast.type_params {
            self.check_type_params(type_params);
        }

        for (idx, field) in self.ast.fields.iter().enumerate() {
            self.visit_struct_field(field, idx.into());
        }

        self.symtable.pop_level();
    }

    fn check_type_params(&mut self, type_params: &[ast::TypeParam]) {
        if type_params.len() > 0 {
            let mut names = HashSet::new();
            let mut type_param_id = 0;
            let mut params = Vec::new();

            for type_param in type_params {
                if !names.insert(type_param.name) {
                    let name = self.vm.interner.str(type_param.name).to_string();
                    let msg = SemError::TypeParamNameNotUnique(name);
                    self.vm
                        .diag
                        .lock()
                        .report(self.file_id, type_param.pos, msg);
                }

                params.push(SourceType::TypeParam(type_param_id.into()));

                for bound in &type_param.bounds {
                    let ty = semck::read_type_table(self.vm, &self.symtable, self.file_id, bound);

                    match ty {
                        Some(SourceType::TraitObject(trait_id)) => {
                            let xstruct = self.vm.structs.idx(self.struct_id);
                            let mut xstruct = xstruct.write();
                            if !xstruct.type_params[type_param_id]
                                .trait_bounds
                                .insert(trait_id)
                            {
                                let msg = SemError::DuplicateTraitBound;
                                self.vm
                                    .diag
                                    .lock()
                                    .report(self.file_id, type_param.pos, msg);
                            }
                        }

                        None => {
                            // unknown type, error is already thrown
                        }

                        _ => {
                            let msg = SemError::BoundExpected;
                            self.vm.diag.lock().report(self.file_id, bound.pos(), msg);
                        }
                    }
                }

                let sym = TypeSym::TypeParam(type_param_id.into());
                self.symtable.insert_type(type_param.name, sym);
                type_param_id += 1;
            }
        } else {
            let msg = SemError::TypeParamsExpected;
            self.vm.diag.lock().report(self.file_id, self.ast.pos, msg);
        }
    }

    fn visit_struct_field(&mut self, f: &ast::StructField, id: StructFieldId) {
        let ty = semck::read_type_table(self.vm, &self.symtable, self.file_id, &f.data_type)
            .unwrap_or(SourceType::Error);

        let xstruct = self.vm.structs.idx(self.struct_id);
        let mut xstruct = xstruct.write();

        if !self.fields.insert(f.name) {
            let name = self.vm.interner.str(f.name).to_string();
            self.vm
                .diag
                .lock()
                .report(self.file_id, f.pos, SemError::ShadowField(name));
            return;
        }

        let field = StructFieldData {
            id,
            pos: f.pos,
            name: f.name,
            ty,
        };

        xstruct.fields.push(field);
        let old = xstruct.field_names.insert(f.name, id);
        assert!(old.is_none());
    }
}

#[cfg(test)]
mod tests {
    use crate::error::msg::SemError;
    use crate::semck::tests::*;

    #[test]
    fn struct_field() {
        ok("struct Foo { a: Int32 }");
        ok("struct Foo { a: Int32, b: Int32 }");
        ok("struct Foo { a: Int32 } struct Bar { a: Int32 }");
        ok("struct Foo { a: Int32, bar: Bar } struct Bar { a: Int32 }");
        err(
            "struct Bar { a: Unknown }",
            pos(1, 17),
            SemError::UnknownIdentifier("Unknown".into()),
        );
        err(
            "struct Foo { a: Int32, a: Int32 }",
            pos(1, 24),
            SemError::ShadowField("a".into()),
        );
    }

    #[test]
    fn structs_generic() {
        ok("
            struct Foo[T] { f1: T, f2: Int32 }
        ");
    }

    #[test]
    fn struct_with_type_param() {
        ok("trait SomeTrait {} struct Foo[T: SomeTrait] { f1: T, f2: Int32 }");
    }

    #[test]
    fn struct_with_type_params_error() {
        err(
            "struct MyStruct[] { f1: Int32 }",
            pos(1, 1),
            SemError::TypeParamsExpected,
        );

        err(
            "struct MyStruct[X, X] { f1: X }",
            pos(1, 20),
            SemError::TypeParamNameNotUnique("X".into()),
        );

        err(
            "struct MyStruct[X: NonExistingTrait] { f1: X }",
            pos(1, 20),
            SemError::UnknownIdentifier("NonExistingTrait".into()),
        );
    }
}
