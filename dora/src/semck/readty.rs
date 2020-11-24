use parking_lot::RwLock;
use std::sync::Arc;

use crate::error::msg::SemError;
use crate::sym::{NestedSymTable, SymTable, TermSym, TypeSym};
use crate::ty::{SourceType, SourceTypeArray};
use crate::vm::{
    class_accessible_from, ensure_tuple, enum_accessible_from, struct_accessible_from,
    trait_accessible_from, ClassId, EnumId, FileId, StructId, TypeParam, VM,
};

use dora_parser::ast::{Type, TypeBasicType, TypeLambdaType, TypeTupleType};
use dora_parser::lexer::position::Position;

pub fn read_type_table(
    vm: &VM,
    table: &NestedSymTable,
    file_id: FileId,
    t: &Type,
) -> Option<SourceType> {
    read_type_common(vm, table, file_id, t)
}

fn read_type_common(vm: &VM, table: &NestedSymTable, file: FileId, t: &Type) -> Option<SourceType> {
    match *t {
        Type::This(_) => Some(SourceType::This),
        Type::Basic(ref basic) => read_type_basic(vm, table, file, basic),
        Type::Tuple(ref tuple) => read_type_tuple(vm, table, file, tuple),
        Type::Lambda(ref lambda) => read_type_lambda(vm, table, file, lambda),
    }
}

fn read_type_basic(
    vm: &VM,
    table: &NestedSymTable,
    file_id: FileId,
    basic: &TypeBasicType,
) -> Option<SourceType> {
    let sym = read_type_path(vm, table, file_id, basic);

    if sym.is_err() {
        return None;
    }

    let sym = sym.unwrap();

    if sym.is_none() {
        let name = vm
            .interner
            .str(basic.path.last().cloned().unwrap())
            .to_string();
        let msg = SemError::UnknownIdentifier(name);
        vm.diag.lock().report(file_id, basic.pos, msg);
        return None;
    }

    let sym = sym.unwrap();

    match sym {
        TypeSym::Class(cls_id) => read_type_class(vm, table, file_id, basic, cls_id),

        TypeSym::Trait(trait_id) => {
            if !trait_accessible_from(vm, trait_id, table.namespace_id()) {
                let xtrait = vm.traits[trait_id].read();
                let msg = SemError::NotAccessible(xtrait.name(vm));
                vm.diag.lock().report(file_id, basic.pos, msg);
            }

            if basic.params.len() > 0 {
                let msg = SemError::NoTypeParamsExpected;
                vm.diag.lock().report(file_id, basic.pos, msg);
            }

            Some(SourceType::TraitObject(trait_id))
        }

        TypeSym::Struct(struct_id) => read_type_struct(vm, table, file_id, basic, struct_id),

        TypeSym::Enum(enum_id) => read_type_enum(vm, table, file_id, basic, enum_id),

        TypeSym::TypeParam(type_param_id) => {
            if basic.params.len() > 0 {
                let msg = SemError::NoTypeParamsExpected;
                vm.diag.lock().report(file_id, basic.pos, msg);
            }

            Some(SourceType::TypeParam(type_param_id))
        }
    }
}

fn read_type_path(
    vm: &VM,
    table: &NestedSymTable,
    file_id: FileId,
    basic: &TypeBasicType,
) -> Result<Option<TypeSym>, ()> {
    if basic.path.len() > 1 {
        let first_name = basic.path.first().cloned().unwrap();
        let last_name = basic.path.last().cloned().unwrap();
        let mut namespace_table =
            table_for_namespace(vm, file_id, basic, table.get_term(first_name))?;

        for &name in &basic.path[1..basic.path.len() - 1] {
            let sym = namespace_table.read().get_term(name);
            namespace_table = table_for_namespace(vm, file_id, basic, sym)?;
        }

        let sym = namespace_table.read().get_type(last_name);
        Ok(sym)
    } else {
        let name = basic.path.last().cloned().unwrap();
        Ok(table.get_type(name))
    }
}

fn table_for_namespace(
    vm: &VM,
    file_id: FileId,
    basic: &TypeBasicType,
    sym: Option<TermSym>,
) -> Result<Arc<RwLock<SymTable>>, ()> {
    match sym {
        Some(TermSym::Namespace(namespace_id)) => {
            Ok(vm.namespaces[namespace_id.to_usize()].table.clone())
        }

        _ => {
            let msg = SemError::ExpectedNamespace;
            vm.diag.lock().report(file_id, basic.pos, msg);
            Err(())
        }
    }
}

fn read_type_enum(
    vm: &VM,
    table: &NestedSymTable,
    file_id: FileId,
    basic: &TypeBasicType,
    enum_id: EnumId,
) -> Option<SourceType> {
    if !enum_accessible_from(vm, enum_id, table.namespace_id()) {
        let xenum = vm.enums[enum_id].read();
        let msg = SemError::NotAccessible(xenum.name(vm));
        vm.diag.lock().report(file_id, basic.pos, msg);
    }

    let mut type_params = Vec::new();

    for param in &basic.params {
        let param = read_type_common(vm, table, file_id, param);

        if let Some(param) = param {
            type_params.push(param);
        } else {
            return None;
        }
    }

    let xenum = &vm.enums[enum_id];
    let xenum = xenum.read();

    if check_type_params(vm, &xenum.type_params, &type_params, file_id, basic.pos) {
        let list = SourceTypeArray::with(type_params);
        let list_id = vm.source_type_arrays.lock().insert(list);
        Some(SourceType::Enum(enum_id, list_id))
    } else {
        None
    }
}

fn read_type_struct(
    vm: &VM,
    table: &NestedSymTable,
    file_id: FileId,
    basic: &TypeBasicType,
    struct_id: StructId,
) -> Option<SourceType> {
    if !struct_accessible_from(vm, struct_id, table.namespace_id()) {
        let xstruct = vm.structs.idx(struct_id);
        let xstruct = xstruct.read();
        let msg = SemError::NotAccessible(xstruct.name(vm));
        vm.diag.lock().report(file_id, basic.pos, msg);
    }

    let mut type_params = Vec::new();

    for param in &basic.params {
        let param = read_type_common(vm, table, file_id, param);

        if let Some(param) = param {
            type_params.push(param);
        } else {
            return None;
        }
    }

    let xstruct = vm.structs.idx(struct_id);
    let xstruct = xstruct.read();

    if check_type_params(vm, &xstruct.type_params, &type_params, file_id, basic.pos) {
        let list = SourceTypeArray::with(type_params);
        let list_id = vm.source_type_arrays.lock().insert(list);
        Some(SourceType::Struct(struct_id, list_id))
    } else {
        None
    }
}

fn check_type_params(
    vm: &VM,
    tp_definitions: &[TypeParam],
    type_params: &[SourceType],
    file_id: FileId,
    pos: Position,
) -> bool {
    if tp_definitions.len() != type_params.len() {
        let msg = SemError::WrongNumberTypeParams(tp_definitions.len(), type_params.len());
        vm.diag.lock().report(file_id, pos, msg);
        return false;
    }

    let mut success = true;

    for (tp_definition, tp_ty) in tp_definitions.iter().zip(type_params.iter()) {
        let cls_id = if let Some(cls_id) = tp_ty.cls_id(vm) {
            cls_id
        } else {
            continue;
        };

        let cls = vm.classes.idx(cls_id);
        let cls = cls.read();

        for &trait_bound in &tp_definition.trait_bounds {
            if !cls.implements_trait(vm, trait_bound) {
                let bound = vm.traits[trait_bound].read();
                let name = tp_ty.name_cls(vm, &*cls);
                let trait_name = vm.interner.str(bound.name).to_string();
                let msg = SemError::TraitBoundNotSatisfied(name, trait_name);
                vm.diag.lock().report(file_id, pos, msg);
                success = false;
            }
        }
    }

    success
}

fn read_type_class(
    vm: &VM,
    table: &NestedSymTable,
    file_id: FileId,
    basic: &TypeBasicType,
    cls_id: ClassId,
) -> Option<SourceType> {
    if !class_accessible_from(vm, cls_id, table.namespace_id()) {
        let cls = vm.classes.idx(cls_id);
        let cls = cls.read();
        let msg = SemError::NotAccessible(cls.name(vm));
        vm.diag.lock().report(file_id, basic.pos, msg);
    }

    let mut type_params = Vec::new();

    for param in &basic.params {
        let param = read_type_common(vm, table, file_id, param);

        if let Some(param) = param {
            type_params.push(param);
        } else {
            return None;
        }
    }

    let cls = vm.classes.idx(cls_id);
    let cls = cls.read();

    if check_type_params(vm, &cls.type_params, &type_params, file_id, basic.pos) {
        if cls.type_params.is_empty() {
            Some(cls.ty.clone())
        } else {
            let list = SourceTypeArray::with(type_params);
            let list_id = vm.source_type_arrays.lock().insert(list);
            Some(SourceType::Class(cls_id, list_id))
        }
    } else {
        None
    }
}

fn read_type_tuple(
    vm: &VM,
    table: &NestedSymTable,
    file_id: FileId,
    tuple: &TypeTupleType,
) -> Option<SourceType> {
    if tuple.subtypes.len() == 0 {
        Some(SourceType::Unit)
    } else {
        let mut subtypes = Vec::new();

        for subtype in &tuple.subtypes {
            if let Some(ty) = read_type_common(vm, table, file_id, subtype) {
                subtypes.push(ty);
            } else {
                return None;
            }
        }

        let tuple_id = ensure_tuple(vm, subtypes);
        Some(SourceType::Tuple(tuple_id))
    }
}

fn read_type_lambda(
    vm: &VM,
    table: &NestedSymTable,
    file_id: FileId,
    lambda: &TypeLambdaType,
) -> Option<SourceType> {
    let mut params = vec![];

    for param in &lambda.params {
        if let Some(p) = read_type_common(vm, table, file_id, param) {
            params.push(p);
        } else {
            return None;
        }
    }

    let ret = if let Some(ret) = read_type_common(vm, table, file_id, &lambda.ret) {
        ret
    } else {
        return None;
    };

    let ty = vm.lambda_types.lock().insert(params, ret);
    let ty = SourceType::Lambda(ty);

    Some(ty)
}

#[cfg(test)]
mod tests {
    use crate::error::msg::SemError;
    use crate::semck::tests::*;

    #[test]
    fn namespace_class() {
        ok("
            fun f(x: foo::Foo) {}
            namespace foo { @pub class Foo }
        ");

        err(
            "
            fun f(x: foo::Foo) {}
            namespace foo { class Foo }
        ",
            pos(2, 22),
            SemError::NotAccessible("foo::Foo".into()),
        );
    }

    #[test]
    fn namespace_enum() {
        ok("
            fun f(x: foo::Foo) {}
            namespace foo { @pub enum Foo { A, B } }
        ");

        err(
            "
            fun f(x: foo::Foo) {}
            namespace foo { enum Foo { A, B } }
        ",
            pos(2, 22),
            SemError::NotAccessible("foo::Foo".into()),
        );
    }

    #[test]
    fn namespace_trait() {
        ok("
            fun f(x: foo::Foo) {}
            namespace foo { @pub trait Foo {} }
        ");

        err(
            "
            fun f(x: foo::Foo) {}
            namespace foo { trait Foo {} }
        ",
            pos(2, 22),
            SemError::NotAccessible("foo::Foo".into()),
        );
    }
}
