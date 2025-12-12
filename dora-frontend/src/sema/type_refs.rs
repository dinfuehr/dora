use id_arena::Id;

use crate::Name;
use crate::sema::Sema;

use dora_parser::ast;

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
pub(crate) fn lower_type(sa: &mut Sema, node: ast::AstType) -> TypeRefId {
    let type_ref = match node {
        ast::AstType::PathType(node) => lower_path_type(sa, node),
        ast::AstType::TupleType(node) => {
            let mut subtypes = Vec::new();

            for ast_subtype in node.subtypes() {
                subtypes.push(lower_type(sa, ast_subtype));
            }

            TypeRef::Tuple { subtypes }
        }
        ast::AstType::LambdaType(node) => {
            let mut params = Vec::new();

            for ast_param in node.params() {
                params.push(lower_type(sa, ast_param));
            }

            let return_ty = if let Some(ast_ret) = node.ret() {
                lower_type(sa, ast_ret)
            } else {
                unit_ty(sa)
            };

            TypeRef::Lambda { params, return_ty }
        }
        ast::AstType::QualifiedPathType(node) => {
            let ty = lower_type(sa, node.ty());
            let trait_ty = lower_type(sa, node.trait_ty());

            if let Some(name) = node.name() {
                let name = sa.interner.intern(name.token().text());
                TypeRef::QualifiedPath { ty, trait_ty, name }
            } else {
                TypeRef::Error
            }
        }
        ast::AstType::RefType(node) => {
            let ty = lower_type(sa, node.ty());
            TypeRef::Ref { ty }
        }
        ast::AstType::Error { .. } => TypeRef::Error,
    };

    sa.type_refs.alloc(type_ref)
}

fn unit_ty(sa: &mut Sema) -> TypeRefId {
    sa.type_refs.alloc(TypeRef::Tuple {
        subtypes: Vec::new(),
    })
}

fn lower_path_type(sa: &mut Sema, node: ast::AstPathType) -> TypeRef {
    let ast_path_data = node.path();

    if ast_path_data.segments().next().unwrap().is_upcase_this() {
        return lower_assoc_type(sa, node);
    }

    let path = Vec::new();
    let mut type_arguments = Vec::new();

    for _segment in ast_path_data.segments() {
        unimplemented!()
    }

    for ast_type_argument in node.params() {
        let name = ast_type_argument
            .name()
            .map(|n| sa.interner.intern(n.token().text()));
        let ty = lower_type_opt(sa, ast_type_argument.ty());
        type_arguments.push(TypeArgument { name, ty });
    }

    TypeRef::Path {
        path,
        type_arguments,
    }
}

fn lower_assoc_type(_sa: &mut Sema, _node: ast::AstPathType) -> TypeRef {
    unimplemented!()
}

fn lower_type_opt(sa: &mut Sema, node: Option<ast::AstType>) -> TypeRefId {
    if let Some(node) = node {
        lower_type(sa, node)
    } else {
        sa.type_refs.alloc(TypeRef::Error)
    }
}
