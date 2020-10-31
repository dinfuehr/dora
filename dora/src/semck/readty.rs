use crate::error::msg::SemError;
use crate::sym::{SymTables, TypeSym};
use crate::ty::{SourceType, TypeList};
use crate::vm::{ensure_tuple, ClassId, EnumId, FileId, NamespaceId, VM};
use dora_parser::ast::Type::{TypeBasic, TypeLambda, TypeSelf, TypeTuple};
use dora_parser::ast::{Type, TypeBasicType, TypeLambdaType, TypeTupleType};

pub fn read_type_table(vm: &VM, table: &SymTables, file: FileId, t: &Type) -> Option<SourceType> {
    read_type_raw(vm, TypeContext::Table(table), file, t)
}

pub fn read_type_namespace(
    vm: &VM,
    file: FileId,
    namespace_id: Option<NamespaceId>,
    t: &Type,
) -> Option<SourceType> {
    read_type_raw(vm, TypeContext::Namespace(namespace_id), file, t)
}

#[derive(Clone)]
enum TypeContext<'a> {
    Table(&'a SymTables),
    Namespace(Option<NamespaceId>),
}

fn read_type_raw(vm: &VM, context: TypeContext, file: FileId, t: &Type) -> Option<SourceType> {
    match *t {
        TypeSelf(_) => Some(SourceType::This),
        TypeBasic(ref basic) => read_type_basic(vm, context, file, basic),
        TypeTuple(ref tuple) => read_type_tuple(vm, context, file, tuple),
        TypeLambda(ref lambda) => read_type_lambda(vm, context, file, lambda),
    }
}

fn read_type_basic(
    vm: &VM,
    context: TypeContext,
    file: FileId,
    basic: &TypeBasicType,
) -> Option<SourceType> {
    let sym = match context {
        TypeContext::Table(table) => table.get_type(basic.name),

        TypeContext::Namespace(namespace_id) => {
            vm.namespace_table(namespace_id).read().get_type(basic.name)
        }
    };

    if sym.is_none() {
        let name = vm.interner.str(basic.name).to_string();
        let msg = SemError::UnknownType(name);
        vm.diag.lock().report(file, basic.pos, msg);

        return None;
    }

    let sym = sym.unwrap();

    match sym {
        TypeSym::Class(cls_id) => read_type_class(vm, context, file, basic, cls_id),

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

        TypeSym::Enum(enum_id) => read_type_enum(vm, context, file, basic, enum_id),

        TypeSym::TypeParam(type_param_id) => {
            if basic.params.len() > 0 {
                let msg = SemError::NoTypeParamsExpected;
                vm.diag.lock().report(file, basic.pos, msg);
            }

            Some(SourceType::TypeParam(type_param_id))
        }
    }
}

fn read_type_enum(
    vm: &VM,
    context: TypeContext,
    file: FileId,
    basic: &TypeBasicType,
    enum_id: EnumId,
) -> Option<SourceType> {
    let mut type_params = Vec::new();

    for param in &basic.params {
        let param = read_type_raw(vm, context.clone(), file, param);

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
    context: TypeContext,
    file: FileId,
    basic: &TypeBasicType,
    cls_id: ClassId,
) -> Option<SourceType> {
    let mut type_params = Vec::new();

    for param in &basic.params {
        let param = read_type_raw(vm, context.clone(), file, param);

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
    context: TypeContext,
    file: FileId,
    tuple: &TypeTupleType,
) -> Option<SourceType> {
    if tuple.subtypes.len() == 0 {
        Some(SourceType::Unit)
    } else {
        let mut subtypes = Vec::new();

        for subtype in &tuple.subtypes {
            if let Some(ty) = read_type_raw(vm, context.clone(), file, subtype) {
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
    context: TypeContext,
    file: FileId,
    lambda: &TypeLambdaType,
) -> Option<SourceType> {
    let mut params = vec![];

    for param in &lambda.params {
        if let Some(p) = read_type_raw(vm, context.clone(), file, param) {
            params.push(p);
        } else {
            return None;
        }
    }

    let ret = if let Some(ret) = read_type_raw(vm, context.clone(), file, &lambda.ret) {
        ret
    } else {
        return None;
    };

    let ty = vm.lambda_types.lock().insert(params, ret);
    let ty = SourceType::Lambda(ty);

    Some(ty)
}
