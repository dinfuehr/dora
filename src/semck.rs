use ast::Ast;
use ast::Function;
use ast::Elem::ElemFunction;

use parser::Parser;

use sym::SymbolTable;

pub struct SemCheck<'a> {
    ast: &'a Ast,
    symbols: SymbolTable,
}

impl<'a> SemCheck<'a> {
    pub fn new(ast: &Ast) -> SemCheck {
        SemCheck {
            ast: ast,
            symbols: SymbolTable::new()
        }
    }

    pub fn check(&mut self) {
        for elem in &self.ast.elements {
            match *elem {
                ElemFunction(ref fct) => self.add_function_header(fct),
                _ => unreachable!()
            }
        }
    }

    fn add_function_header(&mut self, fct: &Function) {
    }

    fn check_function(&mut self, fct: &Function) {
        println!("check_function");
    }
}

#[test]
fn test_empty_file() {
    let prog = Parser::from_str("").parse().unwrap();

    let mut check = SemCheck::new(&prog);
    check.check();
}

#[test]
fn test_function() {
    let prog = Parser::from_str("fn main() { }").parse().unwrap();
}

