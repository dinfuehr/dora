use id_arena::Id;

use crate::Name;
use crate::sema::{Sema, SourceFileId};

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

pub struct TypeArgument {
    pub name: Option<Name>,
    pub ty: TypeRefId,
}

mod check;
mod lower;
mod parse;

#[allow(unused_imports)]
pub(crate) use check::check_type_ref;
pub(crate) use lower::lower_type;
#[allow(unused_imports)]
pub(crate) use parse::parse_type_ref;

#[allow(dead_code)]
pub(crate) fn type_ref_span(sa: &Sema, file_id: SourceFileId, type_ref_id: TypeRefId) -> Span {
    sa.type_ref_syntax_node_ptr(type_ref_id)
        .map(|ptr| sa.syntax::<ast::AstType>(file_id, ptr).span())
        .expect("missing SyntaxNodePtr for TypeRefId")
}
