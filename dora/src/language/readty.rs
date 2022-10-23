use parking_lot::RwLock;
use std::sync::Arc;

use crate::language::access::{
    class_accessible_from, enum_accessible_from, struct_accessible_from, trait_accessible_from,
};
use crate::language::error::msg::ErrorMessage;
use crate::language::sem_analysis::{
    implements_trait, ClassDefinitionId, EnumDefinitionId, ExtensionDefinitionId, FctDefinition,
    ImplDefinition, ModuleDefinitionId, SemAnalysis, SourceFileId, StructDefinitionId,
    TraitDefinitionId, TypeParamDefinition,
};
use crate::language::specialize::specialize_type;
use crate::language::sym::{ModuleSymTable, Sym, SymTable};
use crate::language::ty::{SourceType, SourceTypeArray};

use dora_parser::ast::{self, TypeBasicType, TypeLambdaType, TypeTupleType};
use dora_parser::lexer::position::Position;

#[derive(Copy, Clone)]
pub enum TypeParamContext<'a> {
    Class(ClassDefinitionId),
    Enum(EnumDefinitionId),
    Struct(StructDefinitionId),
    Fct(&'a FctDefinition),
    Trait(TraitDefinitionId),
    Impl(&'a ImplDefinition),
    Extension(ExtensionDefinitionId),
    None,
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum AllowSelf {
    Yes,
    No,
}

pub fn read_type_unchecked(
    sa: &SemAnalysis,
    table: &ModuleSymTable,
    file_id: SourceFileId,
    t: &ast::Type,
) -> SourceType {
    match *t {
        ast::Type::This(_) => SourceType::This,
        ast::Type::Basic(ref node) => read_type_basic_unchecked(sa, table, file_id, node),
        ast::Type::Tuple(ref node) => read_type_tuple_unchecked(sa, table, file_id, node),
        ast::Type::Lambda(ref node) => read_type_lambda_unchecked(sa, table, file_id, node),
    }
}

fn read_type_basic_unchecked(
    sa: &SemAnalysis,
    table: &ModuleSymTable,
    file_id: SourceFileId,
    node: &TypeBasicType,
) -> SourceType {
    let sym = read_type_path(sa, table, file_id, node);

    if sym.is_err() {
        return SourceType::Error;
    }

    let sym = sym.unwrap();

    if sym.is_none() {}

    let mut type_params = Vec::new();

    for param in &node.params {
        let ty = read_type_unchecked(sa, table, file_id, param);
        type_params.push(ty);
    }

    let type_params = SourceTypeArray::with(type_params);

    match sym {
        Some(Sym::Class(class_id)) => SourceType::Class(class_id, type_params),
        Some(Sym::Trait(trait_id)) => SourceType::Trait(trait_id, type_params),
        Some(Sym::Struct(struct_id)) => {
            let struct_ = sa.structs.idx(struct_id);
            let struct_ = struct_.read();

            if let Some(ref primitive_ty) = struct_.primitive_ty {
                if type_params.is_empty() {
                    primitive_ty.clone()
                } else {
                    let msg = ErrorMessage::WrongNumberTypeParams(0, type_params.len());
                    sa.diag.lock().report(file_id, node.pos, msg);
                    SourceType::Error
                }
            } else {
                SourceType::Struct(struct_id, type_params)
            }
        }
        Some(Sym::Enum(enum_id)) => SourceType::Enum(enum_id, type_params),
        Some(Sym::TypeParam(type_param_id)) => {
            if node.params.len() > 0 {
                let msg = ErrorMessage::NoTypeParamsExpected;
                sa.diag.lock().report(file_id, node.pos, msg);
            }

            SourceType::TypeParam(type_param_id)
        }

        Some(_) => {
            let name = sa
                .interner
                .str(node.path.names.last().cloned().unwrap())
                .to_string();
            let msg = ErrorMessage::UnknownType(name);
            sa.diag.lock().report(file_id, node.pos, msg);
            SourceType::Error
        }

        None => {
            let name = sa
                .interner
                .str(node.path.names.last().cloned().unwrap())
                .to_string();
            let msg = ErrorMessage::UnknownIdentifier(name);
            sa.diag.lock().report(file_id, node.pos, msg);
            SourceType::Error
        }
    }
}

fn read_type_lambda_unchecked(
    sa: &SemAnalysis,
    table: &ModuleSymTable,
    file_id: SourceFileId,
    node: &TypeLambdaType,
) -> SourceType {
    let mut params = vec![];

    for param in &node.params {
        let ty = read_type_unchecked(sa, table, file_id, param);
        params.push(ty);
    }

    let params = SourceTypeArray::with(params);
    let return_type = read_type_unchecked(sa, table, file_id, &node.ret);

    SourceType::Lambda(params, Box::new(return_type))
}

fn read_type_tuple_unchecked(
    sa: &SemAnalysis,
    table: &ModuleSymTable,
    file_id: SourceFileId,
    node: &TypeTupleType,
) -> SourceType {
    if node.subtypes.is_empty() {
        return SourceType::Unit;
    }

    let mut subtypes = Vec::new();

    for subtype in &node.subtypes {
        let ty = read_type_unchecked(sa, table, file_id, subtype);
        subtypes.push(ty);
    }

    let subtypes = SourceTypeArray::with(subtypes);
    SourceType::Tuple(subtypes)
}

pub fn verify_type(
    sa: &SemAnalysis,
    module_id: ModuleDefinitionId,
    file_id: SourceFileId,
    t: &ast::Type,
    ty: SourceType,
    ctxt: TypeParamContext,
    allow_self: AllowSelf,
) -> bool {
    match t {
        &ast::Type::This(ref node) => {
            assert_eq!(ty, SourceType::This);

            if allow_self == AllowSelf::No {
                sa.diag
                    .lock()
                    .report(file_id, node.pos, ErrorMessage::SelfTypeUnavailable);
                return false;
            }
        }

        &ast::Type::Basic(ref node) => {
            if !verify_type_basic(sa, module_id, file_id, node, ty, ctxt, allow_self) {
                return false;
            }
        }

        &ast::Type::Tuple(ref node) => {
            assert!(ty.is_tuple_or_unit());

            if ty.is_unit() {
                assert_eq!(node.subtypes.len(), 0);
                return true;
            }

            let subtypes = ty.tuple_subtypes();
            assert_eq!(subtypes.len(), node.subtypes.len());

            for (subtype, ast_param) in subtypes.iter().zip(node.subtypes.iter()) {
                if !verify_type(sa, module_id, file_id, ast_param, subtype, ctxt, allow_self) {
                    return false;
                }
            }
        }

        &ast::Type::Lambda(ref node) => {
            assert!(ty.is_lambda());

            let (params, return_type) = ty.to_lambda().expect("lambda expected");

            assert_eq!(params.len(), node.params.len());

            for (param, ast_param) in params.iter().zip(node.params.iter()) {
                if !verify_type(sa, module_id, file_id, ast_param, param, ctxt, allow_self) {
                    return false;
                }
            }

            if !verify_type(
                sa,
                module_id,
                file_id,
                &node.ret,
                return_type,
                ctxt,
                allow_self,
            ) {
                return false;
            }
        }
    }

    true
}

fn verify_type_basic(
    sa: &SemAnalysis,
    module_id: ModuleDefinitionId,
    file_id: SourceFileId,
    node: &ast::TypeBasicType,
    ty: SourceType,
    ctxt: TypeParamContext,
    allow_self: AllowSelf,
) -> bool {
    match ty {
        SourceType::TypeParam(_) => {}

        SourceType::Class(cls_id, type_params) => {
            let cls = sa.classes.idx(cls_id);
            let cls = cls.read();

            if !class_accessible_from(sa, cls_id, module_id) {
                let msg = ErrorMessage::NotAccessible(cls.name(sa));
                sa.diag.lock().report(file_id, node.pos, msg);
                return false;
            }

            for (type_param, ast_type_param) in type_params.iter().zip(node.params.iter()) {
                if !verify_type(
                    sa,
                    module_id,
                    file_id,
                    ast_type_param,
                    type_param,
                    ctxt,
                    allow_self,
                ) {
                    return false;
                }
            }

            if !check_type_params(
                sa,
                cls.type_params(),
                type_params.types(),
                file_id,
                node.pos,
                ctxt,
            ) {
                return false;
            }
        }

        SourceType::Enum(enum_id, type_params) => {
            let enum_ = sa.enums.idx(enum_id);
            let enum_ = enum_.read();

            if !enum_accessible_from(sa, enum_id, module_id) {
                let msg = ErrorMessage::NotAccessible(enum_.name(sa));
                sa.diag.lock().report(file_id, node.pos, msg);
                return false;
            }

            for (type_param, ast_type_param) in type_params.iter().zip(node.params.iter()) {
                if !verify_type(
                    sa,
                    module_id,
                    file_id,
                    ast_type_param,
                    type_param,
                    ctxt,
                    allow_self,
                ) {
                    return false;
                }
            }

            if !check_type_params(
                sa,
                enum_.type_params(),
                type_params.types(),
                file_id,
                node.pos,
                ctxt,
            ) {
                return false;
            }
        }

        SourceType::Unit
        | SourceType::Bool
        | SourceType::UInt8
        | SourceType::Char
        | SourceType::Int32
        | SourceType::Int64
        | SourceType::Float32
        | SourceType::Float64 => {
            let struct_id = ty
                .primitive_struct_id(sa)
                .expect("primitive struct expected");

            if !struct_accessible_from(sa, struct_id, module_id) {
                let struct_ = sa.structs.idx(struct_id);
                let struct_ = struct_.read();
                let msg = ErrorMessage::NotAccessible(struct_.name(sa));
                sa.diag.lock().report(file_id, node.pos, msg);
                return false;
            }
        }

        SourceType::Struct(struct_id, type_params) => {
            let struct_ = sa.structs.idx(struct_id);
            let struct_ = struct_.read();

            if !struct_accessible_from(sa, struct_id, module_id) {
                let msg = ErrorMessage::NotAccessible(struct_.name(sa));
                sa.diag.lock().report(file_id, node.pos, msg);
                return false;
            }

            for (type_param, ast_type_param) in type_params.iter().zip(node.params.iter()) {
                if !verify_type(
                    sa,
                    module_id,
                    file_id,
                    ast_type_param,
                    type_param,
                    ctxt,
                    allow_self,
                ) {
                    return false;
                }
            }

            if !check_type_params(
                sa,
                struct_.type_params(),
                type_params.types(),
                file_id,
                node.pos,
                ctxt,
            ) {
                return false;
            }
        }

        SourceType::Trait(trait_id, type_params) => {
            let trait_ = sa.traits.idx(trait_id);
            let trait_ = trait_.read();

            if !trait_accessible_from(sa, trait_id, module_id) {
                let msg = ErrorMessage::NotAccessible(trait_.name(sa));
                sa.diag.lock().report(file_id, node.pos, msg);
                return false;
            }

            for (type_param, ast_type_param) in type_params.iter().zip(node.params.iter()) {
                if !verify_type(
                    sa,
                    module_id,
                    file_id,
                    ast_type_param,
                    type_param,
                    ctxt,
                    allow_self,
                ) {
                    return false;
                }
            }

            if !check_type_params(
                sa,
                trait_.type_params(),
                type_params.types(),
                file_id,
                node.pos,
                ctxt,
            ) {
                return false;
            }
        }

        SourceType::Error => {
            return false;
        }

        _ => {
            println!("ty = {} {:?}", ty.name(sa), ty);
            unreachable!()
        }
    }

    true
}

pub fn read_type(
    sa: &SemAnalysis,
    table: &ModuleSymTable,
    file_id: SourceFileId,
    t: &ast::Type,
    ctxt: TypeParamContext,
    allow_self: AllowSelf,
) -> Option<SourceType> {
    let ty = read_type_unchecked(sa, table, file_id, t);

    if verify_type(
        sa,
        table.module_id(),
        file_id,
        t,
        ty.clone(),
        ctxt,
        allow_self,
    ) {
        Some(ty)
    } else {
        None
    }
}

fn read_type_path(
    sa: &SemAnalysis,
    table: &ModuleSymTable,
    file_id: SourceFileId,
    basic: &TypeBasicType,
) -> Result<Option<Sym>, ()> {
    let names = &basic.path.names;

    if names.len() > 1 {
        let first_name = names.first().cloned().unwrap();
        let last_name = names.last().cloned().unwrap();
        let mut module_table = table_for_module(sa, file_id, basic, table.get(first_name))?;

        for &name in &names[1..names.len() - 1] {
            let sym = module_table.read().get(name);
            module_table = table_for_module(sa, file_id, basic, sym)?;
        }

        let sym = module_table.read().get(last_name);
        Ok(sym)
    } else {
        let name = names.last().cloned().unwrap();
        Ok(table.get(name))
    }
}

fn table_for_module(
    sa: &SemAnalysis,
    file_id: SourceFileId,
    basic: &TypeBasicType,
    sym: Option<Sym>,
) -> Result<Arc<RwLock<SymTable>>, ()> {
    match sym {
        Some(Sym::Module(module_id)) => Ok(sa.modules[module_id].read().table.clone()),

        _ => {
            let msg = ErrorMessage::ExpectedModule;
            sa.diag.lock().report(file_id, basic.pos, msg);
            Err(())
        }
    }
}

fn check_type_params(
    sa: &SemAnalysis,
    tp_definitions: &TypeParamDefinition,
    type_params: &[SourceType],
    file_id: SourceFileId,
    pos: Position,
    ctxt: TypeParamContext,
) -> bool {
    if tp_definitions.len() != type_params.len() {
        let msg = ErrorMessage::WrongNumberTypeParams(tp_definitions.len(), type_params.len());
        sa.diag.lock().report(file_id, pos, msg);
        return false;
    }

    let type_params_sta = SourceTypeArray::with(type_params.to_vec());

    let mut success = true;

    use_type_params(sa, ctxt, |check_type_param_defs| {
        for bound in tp_definitions.bounds() {
            let tp_ty = bound.ty();
            let trait_ty = bound.trait_ty();
            let tp_ty = specialize_type(sa, tp_ty, &type_params_sta);

            if !implements_trait(sa, tp_ty.clone(), check_type_param_defs, trait_ty.clone()) {
                let name = tp_ty.name_with_type_params(sa, check_type_param_defs);
                let trait_name = trait_ty.name_with_type_params(sa, check_type_param_defs);
                let msg = ErrorMessage::TypeNotImplementingTrait(name, trait_name);
                sa.diag.lock().report(file_id, pos, msg);
                success = false;
            }
        }
    });

    success
}

fn use_type_params<F, R>(sa: &SemAnalysis, ctxt: TypeParamContext, callback: F) -> R
where
    F: FnOnce(&TypeParamDefinition) -> R,
{
    match ctxt {
        TypeParamContext::Class(cls_id) => {
            let cls = sa.classes.idx(cls_id);
            let cls = cls.read();

            callback(cls.type_params())
        }

        TypeParamContext::Enum(enum_id) => {
            let enum_ = &sa.enums[enum_id];
            let enum_ = enum_.read();

            callback(enum_.type_params())
        }

        TypeParamContext::Struct(struct_id) => {
            let struct_ = &sa.structs.idx(struct_id);
            let struct_ = struct_.read();

            callback(struct_.type_params())
        }

        TypeParamContext::Impl(impl_) => callback(impl_.type_params()),

        TypeParamContext::Extension(extension_id) => {
            let extension = &sa.extensions[extension_id];
            let extension = extension.read();

            callback(extension.type_params())
        }

        TypeParamContext::Trait(trait_id) => {
            let trait_ = &sa.traits[trait_id];
            let trait_ = trait_.read();

            callback(&trait_.type_params())
        }

        TypeParamContext::Fct(fct) => callback(&fct.type_params),
        TypeParamContext::None => callback(&TypeParamDefinition::new()),
    }
}

#[cfg(test)]
mod tests {
    use crate::language::error::msg::ErrorMessage;
    use crate::language::tests::*;

    #[test]
    fn module_class() {
        ok("
            fun f(x: foo::Foo): Unit {}
            mod foo { @pub class Foo }
        ");

        err(
            "
            fun f(x: foo::Foo): Unit {}
            mod foo { class Foo }
        ",
            pos(2, 22),
            ErrorMessage::NotAccessible("foo::Foo".into()),
        );
    }

    #[test]
    fn mod_enum() {
        ok("
            fun f(x: foo::Foo): Unit {}
            mod foo { @pub enum Foo { A, B } }
        ");

        err(
            "
            fun f(x: foo::Foo): Unit {}
            mod foo { enum Foo { A, B } }
        ",
            pos(2, 22),
            ErrorMessage::NotAccessible("foo::Foo".into()),
        );
    }

    #[test]
    fn mod_trait() {
        ok("
            fun f(x: foo::Foo): Unit {}
            mod foo { @pub trait Foo {} }
        ");

        err(
            "
            fun f(x: foo::Foo): Unit {}
            mod foo { trait Foo {} }
        ",
            pos(2, 22),
            ErrorMessage::NotAccessible("foo::Foo".into()),
        );
    }
}
