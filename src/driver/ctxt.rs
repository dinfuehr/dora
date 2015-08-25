use driver::cmd::Args;
use parser::ast::Ast;
use parser::ast::map::Map;
use parser::interner::Interner;

pub struct Context<'a, 'ast> where 'ast: 'a {
    pub args: &'a Args,
    pub interner: &'a Interner,
    pub map: &'a Map<'ast>,
    pub ast: &'a Ast
}
