use id_arena::Id;

use crate::ModuleSymTable;
use crate::Name;
use crate::access::sym_accessible_from;
use crate::sema::{
    AliasDefinitionId, Element, ErrorMessage, ModuleDefinitionId, Sema, SourceFileId, TypeParamId,
    parent_element_or_self,
};
use crate::sym::SymbolKind;

use dora_parser::Span;
use dora_parser::ast::{self, SyntaxNodeBase};

pub type TypeRefId = Id<TypeRef>;

pub enum TypeRef {
    This,

    Path {
        path: Vec<Name>,
        type_arguments: Vec<TypeArgument>,
    },

    Assoc {
        name: Name,
    },

    Tuple {
        subtypes: Vec<TypeRefId>,
    },

    Lambda {
        params: Vec<TypeRefId>,
        return_ty: TypeRefId,
    },

    QualifiedPath {
        ty: TypeRefId,
        trait_ty: TypeRefId,
        name: Name,
    },

    Ref {
        ty: TypeRefId,
    },

    Error,
}

#[allow(unused)]
pub struct TypeArgument {
    pub name: Option<Name>,
    pub ty: TypeRefId,
}

#[allow(unused)]
pub(crate) fn lower_type(sa: &mut Sema, file_id: SourceFileId, node: ast::AstType) -> TypeRefId {
    let syntax_node_ptr = node.as_ptr();
    let type_ref = match node {
        ast::AstType::PathType(node) => lower_path_type(sa, file_id, node),
        ast::AstType::TupleType(node) => {
            let mut subtypes = Vec::new();

            for ast_subtype in node.subtypes() {
                subtypes.push(lower_type(sa, file_id, ast_subtype));
            }

            TypeRef::Tuple { subtypes }
        }
        ast::AstType::LambdaType(node) => {
            let mut params = Vec::new();

            for ast_param in node.params() {
                params.push(lower_type(sa, file_id, ast_param));
            }

            let return_ty = if let Some(ast_ret) = node.ret() {
                lower_type(sa, file_id, ast_ret)
            } else {
                unit_ty(sa)
            };

            TypeRef::Lambda { params, return_ty }
        }
        ast::AstType::QualifiedPathType(node) => {
            let ty = lower_type(sa, file_id, node.ty());
            let trait_ty = lower_type(sa, file_id, node.trait_ty());

            if let Some(name) = node.name() {
                let name = sa.interner.intern(name.token().text());
                TypeRef::QualifiedPath { ty, trait_ty, name }
            } else {
                TypeRef::Error
            }
        }
        ast::AstType::RefType(node) => {
            let ty = lower_type(sa, file_id, node.ty());
            TypeRef::Ref { ty }
        }
        ast::AstType::Error { .. } => TypeRef::Error,
    };

    sa.alloc_type_ref(type_ref, Some(syntax_node_ptr))
}

fn unit_ty(sa: &mut Sema) -> TypeRefId {
    sa.alloc_type_ref(
        TypeRef::Tuple {
            subtypes: Vec::new(),
        },
        None,
    )
}

fn lower_path_type(sa: &mut Sema, file_id: SourceFileId, node: ast::AstPathType) -> TypeRef {
    let ast_path_data = node.path();

    if ast_path_data.segments().next().unwrap().is_upcase_this() {
        return lower_assoc_type(sa, file_id, node);
    }

    let mut path = Vec::new();
    let mut type_arguments = Vec::new();

    for segment in ast_path_data.segments() {
        if let ast::AstPathSegment::Name(n) = segment {
            path.push(sa.interner.intern(n.token().text()));
        } else {
            sa.report(file_id, segment.span(), ErrorMessage::InvalidType);
            return TypeRef::Error;
        }
    }

    for ast_type_argument in node.params() {
        let name = ast_type_argument
            .name()
            .map(|n| sa.interner.intern(n.token().text()));
        let ty = lower_type_opt(sa, file_id, ast_type_argument.ty());
        type_arguments.push(TypeArgument { name, ty });
    }

    TypeRef::Path {
        path,
        type_arguments,
    }
}

fn lower_assoc_type(sa: &mut Sema, file_id: SourceFileId, node: ast::AstPathType) -> TypeRef {
    let ast_path_data = node.path();
    let mut segments = ast_path_data.segments();
    let _ = segments.next();

    if let Some(ast::AstPathSegment::Name(n)) = segments.next() {
        if segments.next().is_none() {
            return TypeRef::Assoc {
                name: sa.interner.intern(n.token().text()),
            };
        }
    } else {
        return TypeRef::This;
    }

    sa.report(file_id, node.span(), ErrorMessage::InvalidType);
    TypeRef::Error
}

fn lower_type_opt(sa: &mut Sema, file_id: SourceFileId, node: Option<ast::AstType>) -> TypeRefId {
    if let Some(node) = node {
        lower_type(sa, file_id, node)
    } else {
        sa.alloc_type_ref(TypeRef::Error, None)
    }
}

#[allow(unused)]
pub fn parse_type_ref(
    sa: &Sema,
    table: &ModuleSymTable,
    file_id: SourceFileId,
    element: &dyn Element,
    allow_self: bool,
    type_ref_id: TypeRefId,
) {
    match &sa.type_refs[type_ref_id] {
        TypeRef::Path {
            path,
            type_arguments,
        } => {
            if let Some(sym) = resolve_path_symbol(sa, table, element, path, file_id, type_ref_id) {
                sa.set_type_ref_symbol(type_ref_id, sym);
            }

            for arg in type_arguments {
                parse_type_ref(sa, table, file_id, element, allow_self, arg.ty);
            }
        }
        TypeRef::QualifiedPath { ty, trait_ty, name } => {
            parse_type_ref(sa, table, file_id, element, allow_self, *ty);
            parse_type_ref(sa, table, file_id, element, allow_self, *trait_ty);

            if let Some(SymbolKind::Trait(trait_id)) = sa.type_ref_symbol(*trait_ty) {
                let trait_ = sa.trait_(trait_id);
                if let Some(alias_id) = trait_.alias_names().get(name) {
                    sa.set_type_ref_symbol(type_ref_id, SymbolKind::Alias(*alias_id));
                }
            }
        }
        TypeRef::Assoc { name } => {
            if let Some(alias_id) = lookup_alias_on_self(sa, element, *name) {
                sa.set_type_ref_symbol(type_ref_id, SymbolKind::Alias(alias_id));
            } else {
                sa.report(
                    file_id,
                    type_ref_span(sa, file_id, type_ref_id),
                    ErrorMessage::UnknownAssoc,
                );
            }
        }
        TypeRef::Tuple { subtypes } => {
            for subtype in subtypes {
                parse_type_ref(sa, table, file_id, element, allow_self, *subtype);
            }
        }
        TypeRef::Lambda { params, return_ty } => {
            for param in params {
                parse_type_ref(sa, table, file_id, element, allow_self, *param);
            }
            parse_type_ref(sa, table, file_id, element, allow_self, *return_ty);
        }
        TypeRef::Ref { ty } => {
            parse_type_ref(sa, table, file_id, element, allow_self, *ty);
        }
        TypeRef::This => {
            if !allow_self {
                sa.report(
                    file_id,
                    type_ref_span(sa, file_id, type_ref_id),
                    ErrorMessage::SelfTypeUnavailable,
                );
            }
        }
        TypeRef::Error => {}
    }
}

fn type_ref_span(sa: &Sema, file_id: SourceFileId, type_ref_id: TypeRefId) -> Span {
    sa.type_ref_syntax_node_ptr(type_ref_id)
        .map(|ptr| sa.syntax::<ast::AstType>(file_id, ptr).span())
        .expect("missing SyntaxNodePtr for TypeRefId")
}

fn resolve_path_symbol(
    sa: &Sema,
    table: &ModuleSymTable,
    element: &dyn Element,
    path: &[Name],
    file_id: SourceFileId,
    type_ref_id: TypeRefId,
) -> Option<SymbolKind> {
    let mut iter = path.iter();
    let first = match iter.next() {
        Some(first) => first,
        None => return None,
    };
    let mut sym = match table.get(*first) {
        Some(sym) => sym,
        None => {
            report_unknown_symbol(sa, file_id, type_ref_id, *first);
            return None;
        }
    };

    while let Some(name) = iter.next() {
        match sym {
            SymbolKind::Module(module_id) => {
                let module = sa.module(module_id);
                let current_sym = match module.table().get(*name) {
                    Some(sym) => sym,
                    None => {
                        report_unknown_symbol(sa, file_id, type_ref_id, *name);
                        return None;
                    }
                };
                if !sym_accessible_from(sa, current_sym.clone(), module_id) {
                    report_inaccessible_symbol(sa, file_id, type_ref_id, module_id, *name);
                    return None;
                }
                sym = current_sym;
            }
            SymbolKind::TypeParam(id) => {
                if iter.next().is_some() {
                    sa.report(
                        file_id,
                        type_ref_span(sa, file_id, type_ref_id),
                        ErrorMessage::ExpectedPath,
                    );
                    return None;
                }

                if let Some(alias_id) = lookup_alias_on_type_param(sa, element, id, *name) {
                    return Some(SymbolKind::Alias(alias_id));
                }

                report_unknown_symbol(sa, file_id, type_ref_id, *name);
                return None;
            }
            _ => {
                sa.report(
                    file_id,
                    type_ref_span(sa, file_id, type_ref_id),
                    ErrorMessage::ExpectedPath,
                );
                return None;
            }
        }
    }

    match sym {
        SymbolKind::Trait(..)
        | SymbolKind::Class(..)
        | SymbolKind::Struct(..)
        | SymbolKind::Enum(..)
        | SymbolKind::Alias(..)
        | SymbolKind::TypeParam(..) => Some(sym),
        _ => {
            sa.report(
                file_id,
                type_ref_span(sa, file_id, type_ref_id),
                ErrorMessage::ExpectedTypeName,
            );
            None
        }
    }
}

fn report_unknown_symbol(sa: &Sema, file_id: SourceFileId, type_ref_id: TypeRefId, name: Name) {
    let name = sa.interner.str(name).to_string();
    sa.report(
        file_id,
        type_ref_span(sa, file_id, type_ref_id),
        ErrorMessage::UnknownIdentifier(name),
    );
}

fn report_inaccessible_symbol(
    sa: &Sema,
    file_id: SourceFileId,
    type_ref_id: TypeRefId,
    module_id: ModuleDefinitionId,
    name: Name,
) {
    let module = sa.module(module_id);
    let module_name = module.name(sa);
    let name = sa.interner.str(name).to_string();
    sa.report(
        file_id,
        type_ref_span(sa, file_id, type_ref_id),
        ErrorMessage::NotAccessibleInModule(module_name, name),
    );
}

fn lookup_alias_on_self(sa: &Sema, element: &dyn Element, name: Name) -> Option<AliasDefinitionId> {
    let element = parent_element_or_self(sa, element);

    if let Some(trait_) = element.to_trait() {
        if let Some(alias_id) = trait_.alias_names().get(&name) {
            return Some(*alias_id);
        }

        for bound in trait_.type_param_definition.bounds_for_self() {
            let trait_id = bound.trait_id;
            let trait_ = sa.trait_(trait_id);

            if let Some(id) = trait_.alias_names().get(&name) {
                return Some(*id);
            }
        }
    } else if let Some(impl_) = element.to_impl() {
        if let Some(trait_id) = impl_.parsed_trait_ty().trait_id() {
            let trait_ = sa.trait_(trait_id);
            return trait_.alias_names().get(&name).cloned();
        }
    }

    None
}

fn lookup_alias_on_type_param(
    sa: &Sema,
    element: &dyn Element,
    id: TypeParamId,
    name: Name,
) -> Option<AliasDefinitionId> {
    let type_param_definition = element.type_param_definition();
    let mut results = Vec::with_capacity(2);

    for bound in type_param_definition.bounds_for_type_param(id) {
        let trait_id = bound.trait_id;
        let trait_ = sa.trait_(trait_id);

        if let Some(id) = trait_.alias_names().get(&name) {
            results.push(*id);
        }
    }

    if results.len() == 1 {
        results.pop()
    } else {
        None
    }
}
