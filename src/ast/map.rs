use std::default::Default;
use std::iter::repeat;

use ast::*;
use ast::visit::Visitor;
use parser::interner::Interner;

use self::Entry::*;

pub fn build<'a>(ast: &'a Ast, interner: &Interner) -> Map<'a> {
    let mut map = Map {
        ast: ast,
        entries: Vec::new()
    };

    {
        let mut visitor = MapVisitor {
            interner: interner,
            parent_id: NodeId(0),
            map: &mut map
        };

        visitor.visit_ast(ast);
    }

    map
}

#[derive(Clone)]
enum Entry<'a> {
    EmptyEntry,

    EntryFct(NodeId, &'a Function),
    EntryParam(NodeId, &'a Param),
    EntryType(NodeId, &'a Type),
    EntryStmt(NodeId, &'a Stmt),
    EntryExpr(NodeId, &'a Expr),
}

impl<'a> Entry<'a> {
    pub fn parent_id(&self) -> NodeId {
        match *self {
            EntryFct(id, _) => id,
            EntryParam(id, _) => id,
            EntryType(id, _) => id,
            EntryStmt(id, _) => id,
            EntryExpr(id, _) => id,
            _ => unreachable!()
        }
    }

    pub fn is_empty(&self) -> bool {
        match *self {
            EmptyEntry => true,
            _ => false
        }
    }

    pub fn to_fct(&self) -> Option<&Function> {
        match *self {
            EntryFct(_, fct) => Some(fct),
            _ => None
        }
    }

    pub fn to_type(&self) -> Option<&Type> {
        match *self {
            EntryType(_, ty) => Some(ty),
            _ => None
        }
    }

    pub fn to_stmt(&self) -> Option<&Stmt> {
        match *self {
            EntryStmt(_, stmt) => Some(stmt),
            _ => None
        }
    }

    pub fn to_expr(&self) -> Option<&Expr> {
        match *self {
            EntryExpr(_, expr) => Some(expr),
            _ => None
        }
    }

    pub fn to_param(&self) -> Option<&Param> {
        match *self {
            EntryParam(_, param) => Some(param),
            _ => None
        }
    }
}

impl<'a> Default for Entry<'a> {
    fn default() -> Entry<'a> {
        EmptyEntry
    }
}

pub struct Map<'ast> {
    ast: &'ast Ast,
    entries: Vec<Entry<'ast>>,
}

impl<'ast> Map<'ast> {
    pub fn entry(&self, id: NodeId) -> &Entry<'ast> {
        &self.entries[id.0]
    }

    pub fn insert(&mut self, id: NodeId, entry: Entry<'ast>) {
        let len = self.entries.len();
        let idx = id.0;

        if idx >= len {
            let allocate = idx - len + 1;
            self.entries.extend(repeat(EmptyEntry).take(allocate));
        }

        self.entries[id.0] = entry;
    }
}

pub struct MapVisitor<'a, 'ast> where 'ast: 'a {
    interner: &'a Interner,
    parent_id: NodeId,
    map: &'a mut Map<'ast>
}

impl<'a, 'ast> MapVisitor<'a, 'ast>{
    fn use_parent<F>(&mut self, new_id: NodeId, fct: F)
            where F: FnOnce(&mut MapVisitor<'a, 'ast>) -> () {
        let old = self.parent_id;
        self.parent_id = new_id;

        fct(self);

        self.parent_id = old;
    }
}

impl<'a, 'ast> visit::Visitor<'ast> for MapVisitor<'a, 'ast> where 'ast :'a {
    fn visit_fct(&mut self, f: &'ast Function) {
        self.map.insert(f.id, EntryFct(self.parent_id, f));
        self.use_parent(f.id, |this| visit::walk_fct(this, f));
    }

    fn visit_param(&mut self, p: &'ast Param) {
        self.map.insert(p.id, EntryParam(self.parent_id, p));
        self.use_parent(p.id, |this| visit::walk_param(this, p));
    }

    fn visit_type(&mut self, t: &'ast Type) {
        let id = t.id();

        self.map.insert(id, EntryType(self.parent_id, t));
        self.use_parent(id, |this| visit::walk_type(this, t));
    }

    fn visit_stmt(&mut self, s: &'ast Stmt) {
        let id = s.id();

        self.map.insert(id, EntryStmt(self.parent_id, s));
        self.use_parent(id, |this| visit::walk_stmt(this, s));
    }

    fn visit_expr(&mut self, e: &'ast Expr) {
        let id = e.id();

        self.map.insert(id, EntryExpr(self.parent_id, e));
        self.use_parent(id, |this| visit::walk_expr(this, e));
    }
}
