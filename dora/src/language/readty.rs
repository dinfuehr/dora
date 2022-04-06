use parking_lot::RwLock;
use std::sync::Arc;

use crate::language::access::{
    class_accessible_from, enum_accessible_from, struct_accessible_from, trait_accessible_from,
};
use crate::language::error::msg::SemError;
use crate::language::sem_analysis::{
    ClassDefinitionId, StructDefinitionId, TypeParam, TypeParamId,
};
use crate::language::sym::{NestedSymTable, Sym, SymTable};
use crate::language::ty::{implements_trait, SourceType, SourceTypeArray};
use crate::vm::{
    ensure_tuple, EnumDefinitionId, ExtensionId, FctDefinition, FileId, ImplData, SemAnalysis,
    TraitDefinitionId,
};

use dora_parser::ast::{Type, TypeBasicType, TypeLambdaType, TypeTupleType};
use dora_parser::lexer::position::Position;

#[derive(Copy, Clone)]
pub enum TypeParamContext<'a> {
    Class(ClassDefinitionId),
    Enum(EnumDefinitionId),
    Struct(StructDefinitionId),
    Fct(&'a FctDefinition),
    Trait(TraitDefinitionId),
    Impl(&'a ImplData),
    Extension(ExtensionId),
    None,
}

#[derive(Copy, Clone)]
pub enum AllowSelf {
    Yes,
    No,
}

pub fn read_type(
    sa: &SemAnalysis,
    table: &NestedSymTable,
    file_id: FileId,
    t: &Type,
    ctxt: TypeParamContext,
    allow_self: AllowSelf,
) -> Option<SourceType> {
    match *t {
        Type::This(ref node) => match allow_self {
            AllowSelf::Yes => Some(SourceType::This),
            AllowSelf::No => {
                sa.diag
                    .lock()
                    .report(file_id, node.pos, SemError::SelfTypeUnavailable);

                None
            }
        },
        Type::Basic(ref basic) => read_type_basic(sa, table, file_id, basic, ctxt, allow_self),
        Type::Tuple(ref tuple) => read_type_tuple(sa, table, file_id, tuple, ctxt, allow_self),
        Type::Lambda(ref lambda) => read_type_lambda(sa, table, file_id, lambda, ctxt, allow_self),
    }
}

fn read_type_basic(
    sa: &SemAnalysis,
    table: &NestedSymTable,
    file_id: FileId,
    basic: &TypeBasicType,
    ctxt: TypeParamContext,
    allow_self: AllowSelf,
) -> Option<SourceType> {
    let sym = read_type_path(sa, table, file_id, basic);

    if sym.is_err() {
        return None;
    }

    let sym = sym.unwrap();

    if sym.is_none() {
        let name = sa
            .interner
            .str(basic.path.names.last().cloned().unwrap())
            .to_string();
        let msg = SemError::UnknownIdentifier(name);
        sa.diag.lock().report(file_id, basic.pos, msg);
        return None;
    }

    let sym = sym.unwrap();

    match sym {
        Sym::Class(cls_id) => read_type_class(sa, table, file_id, basic, cls_id, ctxt, allow_self),

        Sym::Trait(trait_id) => {
            if !trait_accessible_from(sa, trait_id, table.namespace_id()) {
                let xtrait = sa.traits[trait_id].read();
                let msg = SemError::NotAccessible(xtrait.name(sa));
                sa.diag.lock().report(file_id, basic.pos, msg);
            }

            if basic.params.len() > 0 {
                let msg = SemError::NoTypeParamsExpected;
                sa.diag.lock().report(file_id, basic.pos, msg);
            }

            Some(SourceType::Trait(trait_id, SourceTypeArray::empty()))
        }

        Sym::Struct(struct_id) => {
            read_type_struct(sa, table, file_id, basic, struct_id, ctxt, allow_self)
        }

        Sym::Enum(enum_id) => read_type_enum(sa, table, file_id, basic, enum_id, ctxt, allow_self),

        Sym::TypeParam(type_param_id) => {
            if basic.params.len() > 0 {
                let msg = SemError::NoTypeParamsExpected;
                sa.diag.lock().report(file_id, basic.pos, msg);
            }

            Some(SourceType::TypeParam(type_param_id))
        }

        _ => unreachable!(),
    }
}

fn read_type_path(
    sa: &SemAnalysis,
    table: &NestedSymTable,
    file_id: FileId,
    basic: &TypeBasicType,
) -> Result<Option<Sym>, ()> {
    let names = &basic.path.names;

    if names.len() > 1 {
        let first_name = names.first().cloned().unwrap();
        let last_name = names.last().cloned().unwrap();
        let mut namespace_table = table_for_namespace(sa, file_id, basic, table.get(first_name))?;

        for &name in &names[1..names.len() - 1] {
            let sym = namespace_table.read().get(name);
            namespace_table = table_for_namespace(sa, file_id, basic, sym)?;
        }

        let sym = namespace_table.read().get(last_name);
        Ok(sym)
    } else {
        let name = names.last().cloned().unwrap();
        Ok(table.get(name))
    }
}

fn table_for_namespace(
    sa: &SemAnalysis,
    file_id: FileId,
    basic: &TypeBasicType,
    sym: Option<Sym>,
) -> Result<Arc<RwLock<SymTable>>, ()> {
    match sym {
        Some(Sym::Namespace(namespace_id)) => {
            Ok(sa.namespaces[namespace_id.to_usize()].read().table.clone())
        }

        _ => {
            let msg = SemError::ExpectedNamespace;
            sa.diag.lock().report(file_id, basic.pos, msg);
            Err(())
        }
    }
}

fn read_type_enum(
    sa: &SemAnalysis,
    table: &NestedSymTable,
    file_id: FileId,
    basic: &TypeBasicType,
    enum_id: EnumDefinitionId,
    ctxt: TypeParamContext,
    allow_self: AllowSelf,
) -> Option<SourceType> {
    if !enum_accessible_from(sa, enum_id, table.namespace_id()) {
        let xenum = sa.enums[enum_id].read();
        let msg = SemError::NotAccessible(xenum.name(sa));
        sa.diag.lock().report(file_id, basic.pos, msg);
    }

    let mut type_params = Vec::new();

    for param in &basic.params {
        let param = read_type(sa, table, file_id, param, ctxt, allow_self);

        if let Some(param) = param {
            type_params.push(param);
        } else {
            return None;
        }
    }

    let xenum = &sa.enums[enum_id];
    let xenum = xenum.read();

    if check_type_params(
        sa,
        &xenum.type_params,
        &type_params,
        file_id,
        basic.pos,
        ctxt,
    ) {
        let type_params = SourceTypeArray::with(type_params);
        Some(SourceType::Enum(enum_id, type_params))
    } else {
        None
    }
}

fn read_type_struct(
    sa: &SemAnalysis,
    table: &NestedSymTable,
    file_id: FileId,
    basic: &TypeBasicType,
    struct_id: StructDefinitionId,
    ctxt: TypeParamContext,
    allow_self: AllowSelf,
) -> Option<SourceType> {
    if !struct_accessible_from(sa, struct_id, table.namespace_id()) {
        let xstruct = sa.structs.idx(struct_id);
        let xstruct = xstruct.read();
        let msg = SemError::NotAccessible(xstruct.name(sa));
        sa.diag.lock().report(file_id, basic.pos, msg);
    }

    let mut type_params = Vec::new();

    for param in &basic.params {
        let param = read_type(sa, table, file_id, param, ctxt, allow_self);

        if let Some(param) = param {
            type_params.push(param);
        } else {
            return None;
        }
    }

    let xstruct = sa.structs.idx(struct_id);
    let xstruct = xstruct.read();

    if check_type_params(
        sa,
        &xstruct.type_params,
        &type_params,
        file_id,
        basic.pos,
        ctxt,
    ) {
        if let Some(ref primitive_ty) = xstruct.primitive_ty {
            Some(primitive_ty.clone())
        } else {
            Some(SourceType::Struct(
                struct_id,
                SourceTypeArray::with(type_params),
            ))
        }
    } else {
        None
    }
}

fn check_type_params(
    sa: &SemAnalysis,
    tp_definitions: &[TypeParam],
    type_params: &[SourceType],
    file_id: FileId,
    pos: Position,
    ctxt: TypeParamContext,
) -> bool {
    if tp_definitions.len() != type_params.len() {
        let msg = SemError::WrongNumberTypeParams(tp_definitions.len(), type_params.len());
        sa.diag.lock().report(file_id, pos, msg);
        return false;
    }

    let mut success = true;

    for (tp_definition, tp_ty) in tp_definitions.iter().zip(type_params.iter()) {
        use_type_params(sa, ctxt, |check_type_param_defs| {
            for &trait_bound in &tp_definition.trait_bounds {
                if !implements_trait(sa, tp_ty.clone(), check_type_param_defs, trait_bound) {
                    let bound = sa.traits[trait_bound].read();
                    let name = tp_ty.name_with_params(sa, check_type_param_defs);
                    let trait_name = sa.interner.str(bound.name).to_string();
                    let msg = SemError::TypeNotImplementingTrait(name, trait_name);
                    sa.diag.lock().report(file_id, pos, msg);
                    success = false;
                }
            }
        });
    }

    success
}

fn use_type_params<F, R>(sa: &SemAnalysis, ctxt: TypeParamContext, callback: F) -> R
where
    F: FnOnce(&[TypeParam]) -> R,
{
    match ctxt {
        TypeParamContext::Class(cls_id) => {
            let cls = sa.classes.idx(cls_id);
            let cls = cls.read();

            callback(&cls.type_params)
        }

        TypeParamContext::Enum(enum_id) => {
            let xenum = &sa.enums[enum_id];
            let xenum = xenum.read();

            callback(&xenum.type_params)
        }

        TypeParamContext::Struct(struct_id) => {
            let xstruct = &sa.structs.idx(struct_id);
            let xstruct = xstruct.read();

            callback(&xstruct.type_params)
        }

        TypeParamContext::Impl(ximpl) => callback(&ximpl.type_params),

        TypeParamContext::Extension(extension_id) => {
            let extension = &sa.extensions[extension_id];
            let extension = extension.read();

            callback(&extension.type_params)
        }

        TypeParamContext::Trait(trait_id) => {
            let xtrait = &sa.traits[trait_id];
            let xtrait = xtrait.read();

            callback(&xtrait.type_params)
        }

        TypeParamContext::Fct(fct) => callback(&fct.type_params),
        TypeParamContext::None => callback(&[]),
    }
}

fn check_bounds_for_type_param_id(
    sa: &SemAnalysis,
    tp_definition: &TypeParam,
    tp_id: TypeParamId,
    success: &mut bool,
    file_id: FileId,
    pos: Position,
    ctxt: TypeParamContext,
) {
    match ctxt {
        TypeParamContext::Class(cls_id) => {
            let cls = sa.classes.idx(cls_id);
            let cls = cls.read();

            check_bounds_for_type_param(
                sa,
                tp_definition,
                cls.type_param(tp_id),
                success,
                file_id,
                pos,
                ctxt,
            )
        }

        TypeParamContext::Enum(enum_id) => {
            let xenum = &sa.enums[enum_id];
            let xenum = xenum.read();

            check_bounds_for_type_param(
                sa,
                tp_definition,
                xenum.type_param(tp_id),
                success,
                file_id,
                pos,
                ctxt,
            )
        }

        TypeParamContext::Struct(struct_id) => {
            let xstruct = &sa.structs.idx(struct_id);
            let xstruct = xstruct.read();

            check_bounds_for_type_param(
                sa,
                tp_definition,
                xstruct.type_param(tp_id),
                success,
                file_id,
                pos,
                ctxt,
            )
        }

        TypeParamContext::Impl(ximpl) => check_bounds_for_type_param(
            sa,
            tp_definition,
            ximpl.type_param(tp_id),
            success,
            file_id,
            pos,
            ctxt,
        ),

        TypeParamContext::Extension(extension_id) => {
            let extension = &sa.extensions[extension_id];
            let extension = extension.read();

            check_bounds_for_type_param(
                sa,
                tp_definition,
                extension.type_param(tp_id),
                success,
                file_id,
                pos,
                ctxt,
            )
        }

        TypeParamContext::Trait(trait_id) => {
            let xtrait = &sa.traits[trait_id];
            let xtrait = xtrait.read();

            check_bounds_for_type_param(
                sa,
                tp_definition,
                xtrait.type_param(tp_id),
                success,
                file_id,
                pos,
                ctxt,
            )
        }

        TypeParamContext::Fct(fct) => check_bounds_for_type_param(
            sa,
            tp_definition,
            fct.type_param(tp_id),
            success,
            file_id,
            pos,
            ctxt,
        ),

        TypeParamContext::None => unreachable!(),
    }
}

fn check_bounds_for_type_param(
    sa: &SemAnalysis,
    tp_definition: &TypeParam,
    tp_definition_arg: &TypeParam,
    success: &mut bool,
    file_id: FileId,
    pos: Position,
    _ctxt: TypeParamContext,
) {
    for &trait_bound in &tp_definition.trait_bounds {
        if !tp_definition_arg.trait_bounds.contains(&trait_bound) {
            let bound = sa.traits[trait_bound].read();
            let name = sa.interner.str(tp_definition_arg.name).to_string();
            let trait_name = sa.interner.str(bound.name).to_string();
            let msg = SemError::TypeNotImplementingTrait(name, trait_name);
            sa.diag.lock().report(file_id, pos, msg);
            *success = false;
        }
    }
}

fn fail_for_each_trait_bound(
    sa: &SemAnalysis,
    tp_definition: &TypeParam,
    tp_ty: SourceType,
    success: &mut bool,
    file_id: FileId,
    pos: Position,
) {
    for &trait_bound in &tp_definition.trait_bounds {
        let bound = sa.traits[trait_bound].read();
        let name = tp_ty.name(sa);
        let trait_name = sa.interner.str(bound.name).to_string();
        let msg = SemError::TypeNotImplementingTrait(name, trait_name);
        sa.diag.lock().report(file_id, pos, msg);
        *success = false;
    }
}

fn read_type_class(
    sa: &SemAnalysis,
    table: &NestedSymTable,
    file_id: FileId,
    basic: &TypeBasicType,
    cls_id: ClassDefinitionId,
    ctxt: TypeParamContext,
    allow_self: AllowSelf,
) -> Option<SourceType> {
    if !class_accessible_from(sa, cls_id, table.namespace_id()) {
        let cls = sa.classes.idx(cls_id);
        let cls = cls.read();
        let msg = SemError::NotAccessible(cls.name(sa));
        sa.diag.lock().report(file_id, basic.pos, msg);
    }

    let mut type_params = Vec::new();

    for param in &basic.params {
        let param = read_type(sa, table, file_id, param, ctxt, allow_self);

        if let Some(param) = param {
            type_params.push(param);
        } else {
            return None;
        }
    }

    let cls = sa.classes.idx(cls_id);
    let cls = cls.read();

    if check_type_params(sa, &cls.type_params, &type_params, file_id, basic.pos, ctxt) {
        if let Some(ref primitive_ty) = cls.primitive_type {
            assert!(type_params.is_empty());
            Some(primitive_ty.clone())
        } else {
            Some(SourceType::Class(
                cls_id,
                SourceTypeArray::with(type_params),
            ))
        }
    } else {
        None
    }
}

fn read_type_tuple(
    sa: &SemAnalysis,
    table: &NestedSymTable,
    file_id: FileId,
    tuple: &TypeTupleType,
    ctxt: TypeParamContext,
    allow_self: AllowSelf,
) -> Option<SourceType> {
    if tuple.subtypes.len() == 0 {
        Some(SourceType::Unit)
    } else {
        let mut subtypes = Vec::new();

        for subtype in &tuple.subtypes {
            if let Some(ty) = read_type(sa, table, file_id, subtype, ctxt, allow_self) {
                subtypes.push(ty);
            } else {
                return None;
            }
        }

        let tuple_id = ensure_tuple(sa, subtypes);
        Some(SourceType::Tuple(tuple_id))
    }
}

fn read_type_lambda(
    sa: &SemAnalysis,
    table: &NestedSymTable,
    file_id: FileId,
    lambda: &TypeLambdaType,
    ctxt: TypeParamContext,
    allow_self: AllowSelf,
) -> Option<SourceType> {
    let mut params = vec![];

    for param in &lambda.params {
        if let Some(p) = read_type(sa, table, file_id, param, ctxt, allow_self) {
            params.push(p);
        } else {
            return None;
        }
    }

    let ret = if let Some(ret) = read_type(sa, table, file_id, &lambda.ret, ctxt, allow_self) {
        ret
    } else {
        return None;
    };

    let ty = sa.lambda_types.lock().insert(params, ret);
    let ty = SourceType::Lambda(ty);

    Some(ty)
}

#[cfg(test)]
mod tests {
    use crate::language::error::msg::SemError;
    use crate::language::tests::*;

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
