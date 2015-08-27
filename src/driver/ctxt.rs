use std::cell::RefCell;

use driver::cmd::Args;
use error::diag::Diagnostic;

use parser::ast::Ast;
use parser::ast::map::Map;
use parser::interner::Interner;
use sym::SymTable;

pub struct Context<'a, 'ast> where 'ast: 'a {
    pub args: &'a Args,
    pub interner: &'a Interner,
    pub map: &'a Map<'ast>,
    pub ast: &'a Ast,
    pub diagnostic: RefCell<Diagnostic>,
    pub sym: RefCell<SymTable>
}
