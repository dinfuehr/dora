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

    // saves type of element
    // used for functions, params and variables
    pub types: RefCell<HashMap<NodeId, BuiltinType>>,

    // points to the definition of variable/function from its usage
    pub defs: RefCell<HashMap<NodeId, NodeId>>,

    pub vars: RefCell<HashMap<NodeId, Vec<i32>>>,
}

impl<'a, 'ast> Context<'a, 'ast> {
    pub fn new(args: &'a Args, interner: &'a Interner,
           map: &'a Map<'ast>, ast: &'a Ast) -> Context<'a, 'ast> {
        Context {
            args: args,
            interner: interner,
            map: map,
            ast: ast,
            diag: RefCell::new(Diagnostic::new()),
            sym: RefCell::new(SymTable::new()),
            types: RefCell::new(HashMap::new()),
            defs: RefCell::new(HashMap::new()),
            vars: RefCell::new(HashMap::new())
        }
    }
}
