use id_arena::Id;

use crate::Name;
use crate::sema::Sema;

use dora_parser::ast;

pub type TypeRefId = Id<TypeRef>;

pub enum TypeRef {
    This,

    Regular {
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
pub(crate) fn parse_type(sa: &mut Sema, node: ast::AstType) -> TypeRefId {
    match node {
        ast::AstType::RegularType(node) => {
            let path = Vec::new();
            let mut type_arguments = Vec::new();

            let ast_path_data = node.path();

            for segment in ast_path_data.segments() {
                unimplemented!()
            }

            for ast_type_argument in node.params() {
                let name = ast_type_argument
                    .name()
                    .map(|n| sa.interner.intern(n.token().text()));
                let ty = parse_type_opt(sa, ast_type_argument.ty());
                type_arguments.push(TypeArgument { name, ty });
            }

            sa.type_refs.alloc(TypeRef::Regular {
                path,
                type_arguments,
            })
        }
        ast::AstType::TupleType(node) => {
            let mut subtypes = Vec::new();

            for ast_subtype in node.subtypes() {
                subtypes.push(parse_type(sa, ast_subtype));
            }

            sa.type_refs.alloc(TypeRef::Tuple { subtypes })
        }
        ast::AstType::LambdaType(node) => {
            let mut params = Vec::new();

            for ast_param in node.params() {
                params.push(parse_type(sa, ast_param));
            }

            let return_ty = if let Some(ast_ret) = node.ret() {
                parse_type(sa, ast_ret)
            } else {
                sa.type_refs.alloc(TypeRef::Tuple {
                    subtypes: Vec::new(),
                })
            };

            sa.type_refs.alloc(TypeRef::Lambda { params, return_ty })
        }
        ast::AstType::QualifiedPathType(node) => {
            let ty = parse_type(sa, node.ty());
            let trait_ty = parse_type(sa, node.trait_ty());

            if let Some(name) = node.name() {
                let name = sa.interner.intern(name.token().text());
                sa.type_refs
                    .alloc(TypeRef::QualifiedPath { ty, trait_ty, name })
            } else {
                sa.type_refs.alloc(TypeRef::Error)
            }
        }
        ast::AstType::RefType(node) => {
            let ty = parse_type(sa, node.ty());
            sa.type_refs.alloc(TypeRef::Ref { ty })
        }
        ast::AstType::Error { .. } => sa.type_refs.alloc(TypeRef::Error),
    }
}

pub(crate) fn parse_type_opt(sa: &mut Sema, node: Option<ast::AstType>) -> TypeRefId {
    if let Some(node) = node {
        parse_type(sa, node)
    } else {
        sa.type_refs.alloc(TypeRef::Error)
    }
}
