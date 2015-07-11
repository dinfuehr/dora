use ast::Ast;
use ast::ElemType::*;
use ast::Expr;
use ast::ExprType::*;
use ast::Function;
use ast::Stmt;
use ast::StmtType::*;
use ast::TypeInfo;
use ast::visit::Visitor;

use error::err;
use error::unimplemented;
use error::ErrorCode;
use error::ParseError;

use lexer::position::Position;

use parser::Parser;

use sym::Sym::*;
use sym::SymbolTable;

use ty::Ty;
use ty::Ty::*;

type SemResult = Result<(), ParseError>;

struct SemCheck<'a> {
    global: SymbolTable,
    ast: &'a Ast,
}

impl<'a> Visitor<'a> for SemCheck<'a> {
    type Returns = ();

    fn visit_fct(&mut self, fct: &'a Function) -> SemResult {
        self.visit_stmt(&fct.block)
    }

    fn visit_stmt(&mut self, s: &'a Stmt) -> SemResult {
        match s.node {
            StmtReturn(ref expr) => {
                if let Some(ref expr) = *expr {
                    self.visit_expr(&expr)
                } else {
                    err(s.pos, "no return value given".to_string(),
                        ErrorCode::ExpectedValue)
                }
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
            ExprLitInt(_) => {
                Ok(())
            }

            _ => unimplemented(e.pos)
        }
    }
}

impl<'a> SemCheck<'a> {
    pub fn new(ast: &'a Ast) -> SemCheck {
        SemCheck {
            global: SymbolTable::new(),
            ast: ast,
        }
    }

    pub fn check(&mut self) -> SemResult {
        // check all functions
        try!(self.visit_ast(self.ast));

        // check main definition
        self.check_main_fct(self.ast)
    }

    fn check_main_fct(&mut self, prog: &Ast) -> SemResult {
        let fct = prog.function("main");

        if fct.is_none() {
            return err(Position::new(1, 1), "main function not found".to_string(),
                ErrorCode::MainDefinition)
        }

        let fct = fct.unwrap();

        if fct.params.len() > 0 {
            return err(fct.pos,
                "main function is not allowed to have any parameters".to_string(),
                ErrorCode::MainDefinition)
        }

        if !self.is_int(&fct.return_type) {
            return err(fct.pos,
                "main function needs to return `int`".to_string(),
                ErrorCode::MainDefinition)
        }

        Ok(())
    }

    fn check_type(&self, ty: &TypeInfo) -> Result<Ty, ParseError> {
        match *ty {
            TypeInfo::Unit => Ok(TyUnit),
            TypeInfo::Basic(name) if self.ast.interner.str(name) == "int" => Ok(TyInt),
            _ => err(Position::new(1, 1),
                "unkown type definition".to_string(),
                ErrorCode::ExpectedType)
        }
    }

    fn is_int(&self, ty: &TypeInfo) -> bool {
        if let TypeInfo::Basic(name) = *ty {
            self.ast.interner.str(name) == "int"
        } else {
            false
        }
    }
}

#[cfg(test)]
fn ck(code: &'static str) -> SemResult {
    let ast = Parser::from_str(code).parse().unwrap();

    SemCheck::new(&ast).check()
}

#[test]
fn test_main_undefined() {
    assert!(ck("").is_err());
}

#[test]
fn test_main() {
    assert!(ck("fn main() -> int { return 0; }").is_ok());
}

