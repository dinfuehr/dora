use ast::Ast;
use ast::Function;
use ast::Elem::ElemFunction;

use parser::Parser;

pub struct SemCheck<'a> {
    ast: &'a Ast,
}

impl<'a> SemCheck<'a> {
    pub fn new(ast: &Ast) -> SemCheck {
        SemCheck {
            ast: ast
        }
    }

    pub fn check(&mut self) {
        for elem in &self.ast.elements {
            match *elem {
                ElemFunction(ref fct) => self.check_function(fct),
                _ => unreachable!()
            }
        }
    }

    pub fn check_function(&mut self, fct: &Function) {
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

