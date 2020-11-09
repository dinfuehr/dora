use parking_lot::RwLock;
use std::sync::Arc;

use crate::error::msg::SemError;
use crate::sym::{NestedSymTable, SymTable, TermSym, TypeSym};
use crate::ty::{SourceType, TypeList};
use crate::vm::{ensure_tuple, ClassId, EnumId, FileId, NamespaceId, VM};

use dora_parser::ast::{Type, TypeBasicType, TypeLambdaType, TypeTupleType};

pub fn read_type_table(
    vm: &VM,
    table: &NestedSymTable,
    file: FileId,
    t: &Type,
) -> Option<SourceType> {
    read_type_raw(vm, table, file, t)
}

pub fn read_type_namespace(
    vm: &VM,
    file: FileId,
    namespace_id: NamespaceId,
    t: &Type,
) -> Option<SourceType> {
    let symtable = NestedSymTable::new(vm, namespace_id);
    read_type_raw(vm, &symtable, file, t)
}

fn read_type_raw(vm: &VM, table: &NestedSymTable, file: FileId, t: &Type) -> Option<SourceType> {
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
    file: FileId,
    basic: &TypeBasicType,
) -> Option<SourceType> {
    let sym = read_type_path(vm, table, file, basic);

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
        vm.diag.lock().report(file, basic.pos, msg);
        return None;
    }

    let sym = sym.unwrap();

    match sym {
        TypeSym::Class(cls_id) => read_type_class(vm, table, file, basic, cls_id),

        TypeSym::Trait(trait_id) => {
            if basic.params.len() > 0 {
                let msg = SemError::NoTypeParamsExpected;
                vm.diag.lock().report(file, basic.pos, msg);
            }

            Some(SourceType::TraitObject(trait_id))
        }

        TypeSym::Struct(struct_id) => {
            if basic.params.len() > 0 {
                let msg = SemError::NoTypeParamsExpected;
                vm.diag.lock().report(file, basic.pos, msg);
            }

            let list_id = vm.lists.lock().insert(TypeList::empty());
            Some(SourceType::Struct(struct_id, list_id))
        }

        TypeSym::Enum(enum_id) => read_type_enum(vm, table, file, basic, enum_id),

        TypeSym::TypeParam(type_param_id) => {
            if basic.params.len() > 0 {
                let msg = SemError::NoTypeParamsExpected;
                vm.diag.lock().report(file, basic.pos, msg);
            }

            Some(SourceType::TypeParam(type_param_id))
        }
    }
}

fn read_type_path(
    vm: &VM,
    table: &NestedSymTable,
    file: FileId,
    basic: &TypeBasicType,
) -> Result<Option<TypeSym>, ()> {
    if basic.path.len() > 1 {
        let first_name = basic.path.first().cloned().unwrap();
        let last_name = basic.path.last().cloned().unwrap();
        let mut namespace_table = table_for_namespace(vm, file, basic, table.get_term(first_name))?;

        for &name in &basic.path[1..basic.path.len() - 1] {
            let sym = namespace_table.read().get_term(name);
            namespace_table = table_for_namespace(vm, file, basic, sym)?;
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
    file: FileId,
    basic: &TypeBasicType,
    sym: Option<TermSym>,
) -> Result<Arc<RwLock<SymTable>>, ()> {
    match sym {
        Some(TermSym::Namespace(namespace_id)) => {
            Ok(vm.namespaces[namespace_id.to_usize()].table.clone())
        }

        _ => {
            let msg = SemError::ExpectedNamespace;
            vm.diag.lock().report(file, basic.pos, msg);
            Err(())
        }
    }
}

fn read_type_enum(
    vm: &VM,
    table: &NestedSymTable,
    file: FileId,
    basic: &TypeBasicType,
    enum_id: EnumId,
) -> Option<SourceType> {
    let mut type_params = Vec::new();

    for param in &basic.params {
        let param = read_type_raw(vm, table, file, param);

        if let Some(param) = param {
            type_params.push(param);
        } else {
            return None;
        }
    }

    let xenum = &vm.enums[enum_id];
    let xenum = xenum.read();

    if xenum.type_params.len() != type_params.len() {
        let msg = SemError::WrongNumberTypeParams(xenum.type_params.len(), type_params.len());
        vm.diag.lock().report(file, basic.pos, msg);
        return None;
    }

    let mut failed = false;

    for (tp, ty) in xenum.type_params.iter().zip(type_params.iter()) {
        for &trait_bound in &tp.trait_bounds {
            if !ty.implements_trait(vm, trait_bound) {
                let bound = vm.traits[trait_bound].read();
                let trait_name = vm.interner.str(bound.name).to_string();
                let name = ty.name(vm);
                let msg = SemError::TraitBoundNotSatisfied(name, trait_name);
                vm.diag.lock().report(file, basic.pos, msg);
                failed = true;
            }
        }
    }

    if failed {
        return None;
    }

    let list = TypeList::with(type_params);
    let list_id = vm.lists.lock().insert(list);
    Some(SourceType::Enum(xenum.id, list_id))
}

fn read_type_class(
    vm: &VM,
    table: &NestedSymTable,
    file: FileId,
    basic: &TypeBasicType,
    cls_id: ClassId,
) -> Option<SourceType> {
    let mut type_params = Vec::new();

    for param in &basic.params {
        let param = read_type_raw(vm, table, file, param);

        if let Some(param) = param {
            type_params.push(param);
        } else {
            return None;
        }
    }

    let cls = vm.classes.idx(cls_id);
    let cls = cls.read();

    if cls.type_params.len() != type_params.len() {
        let msg = SemError::WrongNumberTypeParams(cls.type_params.len(), type_params.len());
        vm.diag.lock().report(file, basic.pos, msg);
        return None;
    }

    if type_params.len() == 0 {
        return Some(cls.ty.clone());
    }

    for (tp, ty) in cls.type_params.iter().zip(type_params.iter()) {
        let cls_id = if let Some(cls_id) = ty.cls_id(vm) {
            cls_id
        } else {
            continue;
        };

        let cls = vm.classes.idx(cls_id);
        let cls = cls.read();

        for &trait_bound in &tp.trait_bounds {
            if !cls.implements_trait(vm, trait_bound) {
                let bound = vm.traits[trait_bound].read();
                let name = ty.name_cls(vm, &*cls);
                let trait_name = vm.interner.str(bound.name).to_string();
                let msg = SemError::TraitBoundNotSatisfied(name, trait_name);
                vm.diag.lock().report(file, basic.pos, msg);
            }
        }
    }

    let list = TypeList::with(type_params);
    let list_id = vm.lists.lock().insert(list);
    Some(SourceType::Class(cls.id, list_id))
}

fn read_type_tuple(
    vm: &VM,
    table: &NestedSymTable,
    file: FileId,
    tuple: &TypeTupleType,
) -> Option<SourceType> {
    if tuple.subtypes.len() == 0 {
        Some(SourceType::Unit)
    } else {
        let mut subtypes = Vec::new();

        for subtype in &tuple.subtypes {
            if let Some(ty) = read_type_raw(vm, table, file, subtype) {
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
    file: FileId,
    lambda: &TypeLambdaType,
) -> Option<SourceType> {
    let mut params = vec![];

    for param in &lambda.params {
        if let Some(p) = read_type_raw(vm, table, file, param) {
            params.push(p);
        } else {
            return None;
        }
    }

    let ret = if let Some(ret) = read_type_raw(vm, table, file, &lambda.ret) {
        ret
    } else {
        return None;
    };

    let ty = vm.lambda_types.lock().insert(params, ret);
    let ty = SourceType::Lambda(ty);

    Some(ty)
}
