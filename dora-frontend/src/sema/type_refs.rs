use std::cell::RefCell;
use std::collections::HashMap;
use id_arena::{Arena, Id};

use crate::sema::{Sema, SourceFileId};
use crate::{Name, SymbolKind};

use dora_parser::Span;
use dora_parser::ast::{self, SyntaxNodeBase, SyntaxNodePtr};

pub type TypeRefId = Id<TypeRef>;

pub struct TypeRefArena {
    arena: Arena<TypeRef>,
    syntax_nodes: Vec<Option<SyntaxNodePtr>>,
    symbols: RefCell<HashMap<TypeRefId, SymbolKind>>,
}

impl TypeRefArena {
    pub fn new() -> TypeRefArena {
        TypeRefArena {
            arena: Arena::new(),
            syntax_nodes: Vec::new(),
            symbols: RefCell::new(HashMap::new()),
        }
    }

    pub fn alloc(
        &mut self,
        type_ref: TypeRef,
        syntax_node_ptr: Option<SyntaxNodePtr>,
    ) -> TypeRefId {
        let id = self.arena.alloc(type_ref);
        self.syntax_nodes.push(syntax_node_ptr);
        debug_assert_eq!(id.index(), self.syntax_nodes.len() - 1);
        id
    }

    pub fn syntax_node_ptr(&self, id: TypeRefId) -> Option<SyntaxNodePtr> {
        self.syntax_nodes[id.index()]
    }

    pub fn set_symbol(&self, id: TypeRefId, sym: SymbolKind) {
        self.symbols.borrow_mut().insert(id, sym);
    }

    pub fn symbol(&self, id: TypeRefId) -> Option<SymbolKind> {
        self.symbols.borrow().get(&id).cloned()
    }

    pub fn type_ref(&self, id: TypeRefId) -> &TypeRef {
        &self.arena[id]
    }
}

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
mod convert;
mod lower;
mod parse;

#[allow(unused_imports)]
pub(crate) use check::check_type_ref;
pub(crate) use convert::convert_type_ref;
pub(crate) use lower::lower_type;
#[allow(unused_imports)]
pub(crate) use parse::parse_type_ref;

#[allow(dead_code)]
pub(crate) fn type_ref_span(sa: &Sema, file_id: SourceFileId, type_ref_id: TypeRefId) -> Span {
    sa.type_refs()
        .syntax_node_ptr(type_ref_id)
        .map(|ptr| sa.syntax::<ast::AstType>(file_id, ptr).span())
        .expect("missing SyntaxNodePtr for TypeRefId")
}
