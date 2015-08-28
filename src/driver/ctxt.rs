use std::cell::RefCell;
use std::collections::HashMap;

use driver::cmd::Args;
use error::diag::Diagnostic;

use parser::ast::{Ast, NodeId};
use parser::ast::map::Map;
use parser::interner::Interner;

use sym::SymTable;
use sym::BuiltinType;

pub struct Context<'a, 'ast> where 'ast: 'a {
    pub args: &'a Args,
    pub interner: &'a Interner,
    pub map: &'a Map<'ast>,
    pub ast: &'a Ast,
    pub diag: RefCell<Diagnostic>,
    pub sym: RefCell<SymTable>,
    pub types: RefCell<HashMap<NodeId, BuiltinType>>,
    pub var_uses: RefCell<HashMap<NodeId, i32>>,
    pub vars: RefCell<HashMap<NodeId, Vec<i32>>>,
}
