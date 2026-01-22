use crate::args;
use crate::error::diagnostics::INVALID_TYPE;
use crate::sema::{Sema, SourceFileId, TypeRefArenaBuilder};

use dora_parser::ast::{self, SyntaxNodeBase};

use super::{TypeArgument, TypeRef, TypeRefId};

pub(crate) fn lower_type(
    sa: &Sema,
    type_refs: &mut TypeRefArenaBuilder,
    file_id: SourceFileId,
    node: ast::AstType,
) -> TypeRefId {
    let syntax_node_ptr = node.as_ptr();
    let type_ref = match node {
        ast::AstType::PathType(node) => lower_path_type_in_arena(sa, type_refs, file_id, node),
        ast::AstType::TupleType(node) => {
            let mut subtypes = Vec::new();

            for ast_subtype in node.subtypes() {
                subtypes.push(lower_type(sa, type_refs, file_id, ast_subtype));
            }

            TypeRef::Tuple { subtypes }
        }
        ast::AstType::LambdaType(node) => {
            let mut params = Vec::new();

            for ast_param in node.params() {
                params.push(lower_type(sa, type_refs, file_id, ast_param));
            }

            let return_ty = if let Some(ast_ret) = node.ret() {
                lower_type(sa, type_refs, file_id, ast_ret)
            } else {
                unit_ty_in_arena(type_refs)
            };

            TypeRef::Lambda { params, return_ty }
        }
        ast::AstType::QualifiedPathType(node) => {
            let ty = lower_type(sa, type_refs, file_id, node.ty());
            let trait_ty = lower_type(sa, type_refs, file_id, node.trait_ty());

            if let Some(name) = node.name() {
                let name = sa.interner.intern(name.text());
                TypeRef::QualifiedPath { ty, trait_ty, name }
            } else {
                TypeRef::Error
            }
        }
        ast::AstType::RefType(node) => {
            let ty = lower_type(sa, type_refs, file_id, node.ty());
            TypeRef::Ref { ty }
        }
        ast::AstType::Error { .. } => TypeRef::Error,
    };

    type_refs.alloc(type_ref, Some(syntax_node_ptr))
}

fn unit_ty_in_arena(type_refs: &mut TypeRefArenaBuilder) -> TypeRefId {
    type_refs.alloc(
        TypeRef::Tuple {
            subtypes: Vec::new(),
        },
        None,
    )
}

fn lower_path_type_in_arena(
    sa: &Sema,
    type_refs: &mut TypeRefArenaBuilder,
    file_id: SourceFileId,
    node: ast::AstPathType,
) -> TypeRef {
    let ast_path_data = node.path();

    if ast_path_data.segments().next().unwrap().is_upcase_this() {
        return lower_assoc_type(sa, file_id, node);
    }

    let mut path = Vec::new();
    let mut type_arguments = Vec::new();

    for segment in ast_path_data.segments() {
        if let ast::TypePathSegment::Name(n) = segment {
            path.push(sa.interner.intern(n.text()));
        } else {
            sa.report(file_id, segment.span(), &INVALID_TYPE, args!());
            return TypeRef::Error;
        }
    }

    for ast_type_argument in node.params() {
        let name = ast_type_argument
            .name()
            .map(|n| sa.interner.intern(n.text()));
        let ty = lower_type_opt(sa, type_refs, file_id, ast_type_argument.ty());
        type_arguments.push(TypeArgument { name, ty });
    }

    TypeRef::Path {
        path,
        type_arguments,
    }
}

fn lower_assoc_type(sa: &Sema, file_id: SourceFileId, node: ast::AstPathType) -> TypeRef {
    let ast_path_data = node.path();
    let mut segments = ast_path_data.segments();
    let _ = segments.next();

    if let Some(ast::TypePathSegment::Name(n)) = segments.next() {
        if segments.next().is_none() {
            return TypeRef::Assoc {
                name: sa.interner.intern(n.text()),
            };
        }
    } else {
        return TypeRef::This;
    }

    sa.report(file_id, node.span(), &INVALID_TYPE, args!());
    TypeRef::Error
}

pub(crate) fn lower_type_opt(
    sa: &Sema,
    type_refs: &mut TypeRefArenaBuilder,
    file_id: SourceFileId,
    node: Option<ast::AstType>,
) -> TypeRefId {
    if let Some(node) = node {
        lower_type(sa, type_refs, file_id, node)
    } else {
        type_refs.alloc(TypeRef::Error, None)
    }
}
