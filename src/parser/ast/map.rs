use std::default::Default;
use parser::ast::*;
use parser::ast::visit::Visitor;
use parser::interner::Interner;

use self::Entry::*;

pub fn build<'a>(ast: &'a Ast, interner: &'a Interner) -> AstMap<'a> {
    let ast_map = AstMap {
        ast: ast,
        entries: Vec::new()
    };

    let mut visitor = MapVisitor {
        interner: interner
    };

    visitor.visit_ast(ast);

    ast_map
}

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

pub struct AstMap<'a> {
    ast: &'a Ast,
    entries: Vec<Entry<'a>>,
}

pub struct MapVisitor<'a> {
    interner: &'a Interner
}

impl<'a> visit::Visitor<'a> for MapVisitor<'a> {
    fn visit_fct(&mut self, f: &'a Function) {
        println!("fn {}", self.interner.str(f.name));
    }
}
