use crate::error::msg::SemError;
use crate::sym::SymTables;
use crate::sym::TypeSym::{SymClass, SymEnum, SymStruct, SymTrait, SymTypeParam};
use crate::ty::{SourceType, TypeList};
use crate::vm::{ensure_tuple, ClassId, EnumId, FileId, VM};
use dora_parser::ast::Type::{TypeBasic, TypeLambda, TypeSelf, TypeTuple};
use dora_parser::ast::{Type, TypeBasicType, TypeLambdaType, TypeTupleType};

pub fn read_type_table<'ast>(
    vm: &VM<'ast>,
    table: &SymTables,
    file: FileId,
    t: &'ast Type,
) -> Option<SourceType> {
    read_type_raw(vm, Some(table), file, t)
}

pub fn read_type<'ast>(vm: &VM<'ast>, file: FileId, t: &'ast Type) -> Option<SourceType> {
    read_type_raw(vm, None, file, t)
}

fn read_type_raw<'ast>(
    vm: &VM<'ast>,
    table: Option<&SymTables>,
    file: FileId,
    t: &'ast Type,
) -> Option<SourceType> {
    match *t {
        TypeSelf(_) => Some(SourceType::This),
        TypeBasic(ref basic) => read_type_basic(vm, table, file, basic),
        TypeTuple(ref tuple) => read_type_tuple(vm, table, file, tuple),
        TypeLambda(ref lambda) => read_type_lambda(vm, table, file, lambda),
    }
}

fn read_type_basic<'ast>(
    vm: &VM<'ast>,
    table: Option<&SymTables>,
    file: FileId,
    basic: &'ast TypeBasicType,
) -> Option<SourceType> {
    let sym = if let Some(table) = table {
        table.get_type(basic.name)
    } else {
        vm.sym.lock().get_type(basic.name)
    };

    if sym.is_none() {
        let name = vm.interner.str(basic.name).to_string();
        let msg = SemError::UnknownType(name);
        vm.diag.lock().report(file, basic.pos, msg);

        return None;
    }

    let sym = sym.unwrap();

    match sym {
        SymClass(cls_id) => read_type_class(vm, table, file, basic, cls_id),

        SymTrait(trait_id) => {
            if basic.params.len() > 0 {
                let msg = SemError::NoTypeParamsExpected;
                vm.diag.lock().report(file, basic.pos, msg);
            }

            Some(SourceType::TraitObject(trait_id))
        }

        SymStruct(struct_id) => {
            if basic.params.len() > 0 {
                let msg = SemError::NoTypeParamsExpected;
                vm.diag.lock().report(file, basic.pos, msg);
            }

            let list_id = vm.lists.lock().insert(TypeList::empty());
            Some(SourceType::Struct(struct_id, list_id))
        }

        SymEnum(enum_id) => read_type_enum(vm, table, file, basic, enum_id),

        SymTypeParam(type_param_id) => {
            if basic.params.len() > 0 {
                let msg = SemError::NoTypeParamsExpected;
                vm.diag.lock().report(file, basic.pos, msg);
            }

            Some(SourceType::TypeParam(type_param_id))
        }
    }
}

fn read_type_enum<'ast>(
    vm: &VM<'ast>,
    table: Option<&SymTables>,
    file: FileId,
    basic: &'ast TypeBasicType,
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

fn read_type_class<'ast>(
    vm: &VM<'ast>,
    table: Option<&SymTables>,
    file: FileId,
    basic: &'ast TypeBasicType,
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
        return Some(cls.ty);
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

fn read_type_tuple<'ast>(
    vm: &VM<'ast>,
    table: Option<&SymTables>,
    file: FileId,
    tuple: &'ast TypeTupleType,
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

fn read_type_lambda<'ast>(
    vm: &VM<'ast>,
    table: Option<&SymTables>,
    file: FileId,
    lambda: &'ast TypeLambdaType,
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
