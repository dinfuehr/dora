use id_arena::Id;

use crate::Name;
use crate::sema::Sema;

use dora_parser::ast;

pub type TypeRefId = Id<TypeRef>;

pub enum TypeRef {
    This,

    Regular {
        path: Vec<Name>,
        type_arguments: Vec<TypeRefId>,
    },

    Assoc {
        name: Name,
    },

    Tuple {
        subtypes: Vec<TypeRefId>,
    },

    Lambda {
        params: Vec<TypeRefId>,
        return_ty: Option<TypeRefId>,
    },

    QualifiedPath {
        ty: Box<TypeRefId>,
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

            for ast_type_argument in node.params() {}

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
        ast::AstType::LambdaType(_node) => {
            unimplemented!()
        }
        ast::AstType::QualifiedPathType(_node) => {
            unimplemented!()
        }
        ast::AstType::RefType(node) => {
            let ty = parse_type(sa, node.ty());
            sa.type_refs.alloc(TypeRef::Ref { ty })
        }
        ast::AstType::Error { .. } => sa.type_refs.alloc(TypeRef::Error),
    }
}
