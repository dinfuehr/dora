use id_arena::Id;

use crate::Name;
use crate::sema::{ErrorMessage, Sema, SourceFileId};

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
