use ast::Ast;
use ast::ElemType::*;
use ast::Expr;
use ast::ExprType::*;
use ast::Function;
use ast::Stmt;
use ast::StmtType::*;
use ast::visit::Visitor;

use error::err;
use error::unimplemented;
use error::ErrorCode;
use error::ParseError;

use lexer::position::Position;

use parser::Parser;

use sym::Sym::*;
use sym::SymbolTable;

type SemResult = Result<(), ParseError>;

struct SemCheck {
    global: SymbolTable,
}

impl Visitor for SemCheck {
    type Returns = ();

    fn visit_stmt(&mut self, s: &Stmt) -> SemResult {
        match s.node {
            StmtReturn(ref expr) => {
                if expr.is_none() {
                    return err(s.pos, "no return value given".to_string(),
                        ErrorCode::ExpectedValue);
                }

                try!(self.visit_expr(&expr.as_ref().unwrap()));

                Ok(())
            }

            StmtBlock(ref stmts) => {
                for stmt in stmts {
                    try!(self.visit_stmt(stmt));
                }

                Ok(())
            }

            _ => unimplemented(s.pos)
        }
    }

    fn visit_expr(&mut self, e: &Expr) -> SemResult {
        match e.node {
            ExprLitInt(val) => {
                Ok(())
            }

            _ => unimplemented(e.pos)
        }
    }
}

impl SemCheck {
    pub fn new() -> SemCheck {
        SemCheck { global: SymbolTable::new() }
    }

    pub fn check_program(&mut self, prog: &Ast) -> SemResult {
        // check all functions
        try!(self.check_fcts(prog));

        // check main definition
        self.check_main_fct(prog)
    }

    fn check_main_fct(&mut self, prog: &Ast) -> SemResult {
        let fct = prog.function("main");

        if fct.is_none() {
            return err(Position::new(1, 1), "main function not found".to_string(),
                ErrorCode::MainDefinition)
        }

        let fct = fct.unwrap();

        if !fct.type_params.empty() {
            return err(fct.pos,
                "main function is not allowed to have any type parameters".to_string(),
                ErrorCode::MainDefinition)
        }

        if fct.params.len() > 0 {
            return err(fct.pos,
                "main function is not allowed to have any parameters".to_string(),
                ErrorCode::MainDefinition)
        }

        if !fct.return_type.is_int() {
            return err(fct.pos,
                "main function needs to return `int`".to_string(),
                ErrorCode::MainDefinition)
        }

        Ok(())
    }

    fn check_fcts(&mut self, prog: &Ast) -> SemResult {
        for elem in &prog.elements {
            try!(match elem.node {
                // only allow fct's as top level element
                ElemFunction(ref fct) => self.check_fct(fct),

                _ => unimplemented(elem.pos)
            })
        }

        Ok(())
    }

    fn check_fct(&mut self, fct: &Function) -> SemResult {
        self.visit_stmt(&fct.block)
    }
}

#[cfg(test)]
fn ck(code: &'static str) -> SemResult {
    let prog = Parser::from_str(code).parse().unwrap();

    SemCheck::new().check_program(&prog)
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
fn test_main_no_return_value() {
    assert!(ck("fn main() {}").is_err());
    assert!(ck("fn main() -> int { return; }").is_err());
}

#[test]
fn test_main() {
    assert!(ck("fn main() -> int { return 0; }").is_ok());
}

