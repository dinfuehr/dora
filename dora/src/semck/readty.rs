use parking_lot::RwLock;
use std::sync::Arc;

use crate::error::msg::SemError;
use crate::sym::{NestedSymTable, SymTable, TermSym, TypeSym};
use crate::ty::{implements_trait, SourceType, SourceTypeArray};
use crate::vm::{
    class_accessible_from, ensure_tuple, enum_accessible_from, struct_accessible_from,
    trait_accessible_from, ClassId, EnumId, ExtensionId, Fct, FileId, ImplData, StructId, TraitId,
    TypeParam, TypeParamId, VM,
};

use dora_parser::ast::{Type, TypeBasicType, TypeLambdaType, TypeTupleType};
use dora_parser::lexer::position::Position;

#[derive(Copy, Clone)]
pub enum TypeParamContext<'a> {
    Class(ClassId),
    Enum(EnumId),
    Struct(StructId),
    Fct(&'a Fct),
    Trait(TraitId),
    Impl(&'a ImplData),
    Extension(ExtensionId),
    None,
}

pub fn read_type(
    vm: &VM,
    table: &NestedSymTable,
    file_id: FileId,
    t: &Type,
    ctxt: TypeParamContext,
) -> Option<SourceType> {
    match *t {
        Type::This(_) => Some(SourceType::This),
        Type::Basic(ref basic) => read_type_basic(vm, table, file_id, basic, ctxt),
        Type::Tuple(ref tuple) => read_type_tuple(vm, table, file_id, tuple, ctxt),
        Type::Lambda(ref lambda) => read_type_lambda(vm, table, file_id, lambda, ctxt),
    }
}

fn read_type_basic(
    vm: &VM,
    table: &NestedSymTable,
    file_id: FileId,
    basic: &TypeBasicType,
    ctxt: TypeParamContext,
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
        TypeSym::Class(cls_id) => read_type_class(vm, table, file_id, basic, cls_id, ctxt),

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

        TypeSym::Struct(struct_id) => read_type_struct(vm, table, file_id, basic, struct_id, ctxt),

        TypeSym::Enum(enum_id) => read_type_enum(vm, table, file_id, basic, enum_id, ctxt),

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
    ctxt: TypeParamContext,
) -> Option<SourceType> {
    if !enum_accessible_from(vm, enum_id, table.namespace_id()) {
        let xenum = vm.enums[enum_id].read();
        let msg = SemError::NotAccessible(xenum.name(vm));
        vm.diag.lock().report(file_id, basic.pos, msg);
    }

    let mut type_params = Vec::new();

    for param in &basic.params {
        let param = read_type(vm, table, file_id, param, ctxt);

        if let Some(param) = param {
            type_params.push(param);
        } else {
            return None;
        }
    }

    let xenum = &vm.enums[enum_id];
    let xenum = xenum.read();

    if check_type_params(
        vm,
        &xenum.type_params,
        &type_params,
        file_id,
        basic.pos,
        ctxt,
    ) {
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
    ctxt: TypeParamContext,
) -> Option<SourceType> {
    if !struct_accessible_from(vm, struct_id, table.namespace_id()) {
        let xstruct = vm.structs.idx(struct_id);
        let xstruct = xstruct.read();
        let msg = SemError::NotAccessible(xstruct.name(vm));
        vm.diag.lock().report(file_id, basic.pos, msg);
    }

    let mut type_params = Vec::new();

    for param in &basic.params {
        let param = read_type(vm, table, file_id, param, ctxt);

        if let Some(param) = param {
            type_params.push(param);
        } else {
            return None;
        }
    }

    let xstruct = vm.structs.idx(struct_id);
    let xstruct = xstruct.read();

    if check_type_params(
        vm,
        &xstruct.type_params,
        &type_params,
        file_id,
        basic.pos,
        ctxt,
    ) {
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
    ctxt: TypeParamContext,
) -> bool {
    if tp_definitions.len() != type_params.len() {
        let msg = SemError::WrongNumberTypeParams(tp_definitions.len(), type_params.len());
        vm.diag.lock().report(file_id, pos, msg);
        return false;
    }

    let mut success = true;

    for (tp_definition, tp_ty) in tp_definitions.iter().zip(type_params.iter()) {
        match tp_ty {
            SourceType::TypeParam(tp_id) => check_bounds_for_type_param_id(
                vm,
                tp_definition,
                *tp_id,
                &mut success,
                file_id,
                pos,
                ctxt,
            ),

            SourceType::Tuple(_)
            | SourceType::Unit
            | SourceType::Module(_)
            | SourceType::TraitObject(_)
            | SourceType::Lambda(_) => fail_for_each_trait_bound(
                vm,
                tp_definition,
                tp_ty.clone(),
                &mut success,
                file_id,
                pos,
            ),
            SourceType::Error | SourceType::Ptr | SourceType::This | SourceType::Any => {
                unreachable!()
            }
            SourceType::Bool
            | SourceType::Char
            | SourceType::UInt8
            | SourceType::Int32
            | SourceType::Int64
            | SourceType::Float32
            | SourceType::Float64
            | SourceType::Class(_, _)
            | SourceType::Enum(_, _)
            | SourceType::Struct(_, _) => {
                for &trait_bound in &tp_definition.trait_bounds {
                    if !implements_trait(vm, tp_ty.clone(), trait_bound) {
                        let bound = vm.traits[trait_bound].read();
                        let name = tp_ty.name(vm);
                        let trait_name = vm.interner.str(bound.name).to_string();
                        let msg = SemError::TraitBoundNotSatisfied(name, trait_name);
                        vm.diag.lock().report(file_id, pos, msg);
                        success = false;
                    }
                }
            }
        }
    }

    success
}

fn check_bounds_for_type_param_id(
    vm: &VM,
    tp_definition: &TypeParam,
    tp_id: TypeParamId,
    success: &mut bool,
    file_id: FileId,
    pos: Position,
    ctxt: TypeParamContext,
) {
    match ctxt {
        TypeParamContext::Class(cls_id) => {
            let cls = vm.classes.idx(cls_id);
            let cls = cls.read();

            check_bounds_for_type_param(
                vm,
                tp_definition,
                cls.type_param(tp_id),
                success,
                file_id,
                pos,
                ctxt,
            )
        }

        TypeParamContext::Enum(enum_id) => {
            let xenum = &vm.enums[enum_id];
            let xenum = xenum.read();

            check_bounds_for_type_param(
                vm,
                tp_definition,
                xenum.type_param(tp_id),
                success,
                file_id,
                pos,
                ctxt,
            )
        }

        TypeParamContext::Struct(struct_id) => {
            let xstruct = &vm.structs.idx(struct_id);
            let xstruct = xstruct.read();

            check_bounds_for_type_param(
                vm,
                tp_definition,
                xstruct.type_param(tp_id),
                success,
                file_id,
                pos,
                ctxt,
            )
        }

        TypeParamContext::Impl(ximpl) => check_bounds_for_type_param(
            vm,
            tp_definition,
            ximpl.type_param(tp_id),
            success,
            file_id,
            pos,
            ctxt,
        ),

        TypeParamContext::Extension(extension_id) => {
            let extension = &vm.extensions[extension_id];
            let extension = extension.read();

            check_bounds_for_type_param(
                vm,
                tp_definition,
                extension.type_param(tp_id),
                success,
                file_id,
                pos,
                ctxt,
            )
        }

        TypeParamContext::Trait(trait_id) => {
            let xtrait = &vm.traits[trait_id];
            let xtrait = xtrait.read();

            check_bounds_for_type_param(
                vm,
                tp_definition,
                xtrait.type_param(tp_id),
                success,
                file_id,
                pos,
                ctxt,
            )
        }

        TypeParamContext::Fct(fct) => fct.type_param_id(vm, tp_id, |tp_definition_arg, _| {
            check_bounds_for_type_param(
                vm,
                tp_definition,
                tp_definition_arg,
                success,
                file_id,
                pos,
                ctxt,
            )
        }),

        TypeParamContext::None => unreachable!(),
    }
}

fn check_bounds_for_type_param(
    vm: &VM,
    tp_definition: &TypeParam,
    tp_definition_arg: &TypeParam,
    success: &mut bool,
    file_id: FileId,
    pos: Position,
    _ctxt: TypeParamContext,
) {
    for &trait_bound in &tp_definition.trait_bounds {
        if !tp_definition_arg.trait_bounds.contains(&trait_bound) {
            let bound = vm.traits[trait_bound].read();
            let name = vm.interner.str(tp_definition_arg.name).to_string();
            let trait_name = vm.interner.str(bound.name).to_string();
            let msg = SemError::TraitBoundNotSatisfied(name, trait_name);
            vm.diag.lock().report(file_id, pos, msg);
            *success = false;
        }
    }
}

fn fail_for_each_trait_bound(
    vm: &VM,
    tp_definition: &TypeParam,
    tp_ty: SourceType,
    success: &mut bool,
    file_id: FileId,
    pos: Position,
) {
    for &trait_bound in &tp_definition.trait_bounds {
        let bound = vm.traits[trait_bound].read();
        let name = tp_ty.name(vm);
        let trait_name = vm.interner.str(bound.name).to_string();
        let msg = SemError::TraitBoundNotSatisfied(name, trait_name);
        vm.diag.lock().report(file_id, pos, msg);
        *success = false;
    }
}

fn read_type_class(
    vm: &VM,
    table: &NestedSymTable,
    file_id: FileId,
    basic: &TypeBasicType,
    cls_id: ClassId,
    ctxt: TypeParamContext,
) -> Option<SourceType> {
    if !class_accessible_from(vm, cls_id, table.namespace_id()) {
        let cls = vm.classes.idx(cls_id);
        let cls = cls.read();
        let msg = SemError::NotAccessible(cls.name(vm));
        vm.diag.lock().report(file_id, basic.pos, msg);
    }

    let mut type_params = Vec::new();

    for param in &basic.params {
        let param = read_type(vm, table, file_id, param, ctxt);

        if let Some(param) = param {
            type_params.push(param);
        } else {
            return None;
        }
    }

    let cls = vm.classes.idx(cls_id);
    let cls = cls.read();

    if check_type_params(vm, &cls.type_params, &type_params, file_id, basic.pos, ctxt) {
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
    ctxt: TypeParamContext,
) -> Option<SourceType> {
    if tuple.subtypes.len() == 0 {
        Some(SourceType::Unit)
    } else {
        let mut subtypes = Vec::new();

        for subtype in &tuple.subtypes {
            if let Some(ty) = read_type(vm, table, file_id, subtype, ctxt) {
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
    ctxt: TypeParamContext,
) -> Option<SourceType> {
    let mut params = vec![];

    for param in &lambda.params {
        if let Some(p) = read_type(vm, table, file_id, param, ctxt) {
            params.push(p);
        } else {
            return None;
        }
    }

    let ret = if let Some(ret) = read_type(vm, table, file_id, &lambda.ret, ctxt) {
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
