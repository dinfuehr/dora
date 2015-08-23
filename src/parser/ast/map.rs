use std::default::Default;
use parser::ast::*;

use self::Entry::*;

enum Entry<'a> {
    EmptyEntry,

    EntryElem(NodeId, &'a Elem),
    EntryParam(NodeId, &'a Param),
    EntryType(NodeId, &'a Type),
    EntryStmt(NodeId, &'a Stmt),
    EntryExpr(NodeId, &'a Expr),
}

impl<'a> Default for Entry<'a> {
    fn default() -> Entry<'a> {
        EmptyEntry
    }
}

struct AstMap<'a> {
    ast: &'a Ast,
    map: Vec<Entry<'a>>,
}

impl<'a> AstMap<'a> {
    fn new<'b>(ast: &'b Ast) -> AstMap<'b> {
        AstMap {
            ast: ast,
            map: Vec::new()
        }
    }
}
