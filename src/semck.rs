use std::default::Default;

use ast::Program;
use ast::Stmt;
use ast::StmtType::*;
use ast::visit::Visitor;
use error::ErrorCode;
use error::ParseError;
use lexer::position::Position;
use parser::Parser;
use sym::Sym;
use sym::Sym::*;
use sym::SymbolTable;

struct SemCheck<'a> {
    program: &'a Program,
    global: SymbolTable,
}

impl<'a> Visitor for SemCheck<'a> {
    type Returns = ();

    fn visit_stmt(&mut self, s: &Stmt) -> Result<(), ParseError> {
        match s.node {
            StmtVar(ref ident, ref ty, ref expr) => {
                if let None = self.global.get(ident) {
                    self.global.insert(ident.clone(), SymLocalVar);

                    Ok(())
                } else {
                    Err(ParseError {
                        position: s.pos,
                        message: "unsupported element".to_string(),
                        code: ErrorCode::MainDefinition
                    })
                }
            }

            StmtBlock(ref stmts) => {
                for stmt in stmts {
                    try!(self.visit_stmt(stmt));
                }

                Ok(())
            }

            _ => Err(ParseError {
                position: s.pos,
                message: "unsupported element".to_string(),
                code: ErrorCode::MainDefinition
            })
        }
    }
}

impl<'a> SemCheck<'a> {
    pub fn new(prog: &Program) -> SemCheck {
        SemCheck { program: prog, global: SymbolTable::new() }
    }

    pub fn check(&mut self) -> Result<(), ParseError> {
        self.check_main()
    }

    fn check_main(&mut self) -> Result<(), ParseError> {
        let fct = self.program.get_function("main");

        if fct.is_none() {
            return Err(ParseError {
                position: Position::new(1, 1),
                message: "main not found".to_string(),
                code: ErrorCode::MainDefinition
            })
        }

        let fct = fct.unwrap();

        if !fct.type_params.empty() || fct.params.len() > 0 ||
            !fct.return_type.is_unit() {
            return Err(ParseError {
                position: fct.position,
                message: "definition of main not correct".to_string(),
                code: ErrorCode::MainDefinition
            })
        }

        try!(self.visit_stmt(&fct.block));

        Ok(())
    }
}

#[cfg(test)]
fn ck(code: &'static str) -> Result<(), ParseError> {
    let prog = Parser::from_str(code).parse().unwrap();

    SemCheck::new(&prog).check()
}

#[test]
fn test_main_undefined() {
    assert!(ck("fn foo() {}").is_err());
}

#[test]
fn test_main_definition_invalid() {
    assert!(ck("fn main(x:int) {}").is_err());
}

#[test]
fn test_main() {
    assert!(ck("fn main() {}").is_ok());
}

#[test]
fn test_var() {
    assert!(ck("fn main() { var x=0; }").is_ok());
    assert!(ck("fn main() { var x=0; var x=0; }").is_err());
    assert!(ck("fn main() { var x=0; var y=0; }").is_ok());
}
