use parser::ast::Ast;
use parser::ast::Elem::ElemFunction;
use parser::ast::Expr::{self, ExprUn, ExprBin, ExprLitInt, ExprLitStr, ExprLitBool,
                ExprAssign, ExprIdent};
use parser::ast::ExprUnType;
use parser::ast::ExprBinType;
use parser::ast::ExprLitIntType;
use parser::ast::ExprLitStrType;
use parser::ast::ExprLitBoolType;
use parser::ast::ExprIdentType;
use parser::ast::ExprAssignType;
use parser::ast::Function;
use parser::ast::Param;
use parser::ast::Stmt::{self, StmtBlock, StmtBreak, StmtContinue, StmtExpr,
                StmtIf, StmtLoop, StmtReturn, StmtVar, StmtWhile};
use parser::ast::StmtBlockType;
use parser::ast::StmtBreakType;
use parser::ast::StmtContinueType;
use parser::ast::StmtExprType;
use parser::ast::StmtIfType;
use parser::ast::StmtLoopType;
use parser::ast::StmtReturnType;
use parser::ast::StmtVarType;
use parser::ast::StmtWhileType;
use parser::ast::Type;
use parser::interner::{Interner, Name};

macro_rules! dump {
    ($self_:ident, $($x:expr),*) => {{
        for _ in 0..($self_.indent*2) {
            print!(" ");
        }

        println!($($x,)*);
    }};
}

pub fn dump<'a>(ast: &'a Ast, interner: &'a Interner) {
    AstDumper::new(ast, interner).dump();
}

struct AstDumper<'a> {
    ast: &'a Ast,
    interner: &'a Interner,
    indent: u32,
}

impl<'a> AstDumper<'a> {
    fn new<'b>(ast: &'b Ast, interner: &'b Interner) -> AstDumper<'b> {
        AstDumper {
            ast: ast,
            interner: interner,
            indent: 0
        }
    }

    fn dump(&mut self) {
        for el in &self.ast.elements {
            match *el {
                ElemFunction(ref fct) => self.dump_fct(fct),
                _ => unreachable!()
            }
        }
    }

    fn dump_fct(&mut self, fct: &Function) {
        dump!(self, "fct {} @ {} {}", self.str(fct.name), fct.pos, fct.id);

        self.indent(|d| {
            dump!(d, "params");
            d.indent(|d| {
                if fct.params.is_empty() {
                    dump!(d, "no params");
                } else {
                    for param in &fct.params {
                        d.dump_param(&param);
                    }
                }
            });

            dump!(d, "returns");

            if let Some(ref ty) = fct.return_type {
                d.indent(|d| d.dump_type(ty));
            } else {
                d.indent(|d| dump!(d, "<no return type>"))
            }

            dump!(d, "executes");
            d.indent(|d| d.dump_stmt(&fct.block));
        });
    }

    fn dump_param(&mut self, param: &Param) {
        dump!(self, "param {} @ {} {}", self.str(param.name), param.pos, param.id);
        self.indent(|d| d.dump_type(&param.data_type));
    }

    fn dump_type(&mut self, ty: &Type) {
        dump!(self, "type `{}` @ {:?} {}", ty.to_string(self.interner), ty.pos(), ty.id());
    }

    fn dump_stmt(&mut self, stmt: &Stmt) {
        match *stmt {
            StmtBlock(ref block) => self.dump_stmt_block(block),
            StmtReturn(ref ret) => self.dump_stmt_return(ret),
            StmtBreak(ref stmt) => self.dump_stmt_break(stmt),
            StmtContinue(ref stmt) => self.dump_stmt_continue(stmt),
            StmtExpr(ref expr) => self.dump_stmt_expr(expr),
            StmtIf(ref stmt) => self.dump_stmt_if(stmt),
            StmtVar(ref stmt) => self.dump_stmt_var(stmt),
            StmtWhile(ref stmt) => self.dump_stmt_while(stmt),
            StmtLoop(ref stmt) => self.dump_stmt_loop(stmt),
        }
    }

    fn dump_stmt_var(&mut self, stmt: &StmtVarType) {
        dump!(self, "var {} @ {} {}", self.str(stmt.name), stmt.pos, stmt.id);

        self.indent(|d| {
            dump!(d, "type");
            d.indent(|d| {
                if let Some(ref ty) = stmt.data_type {
                    d.dump_type(ty);
                } else {
                    dump!(d, "<no type given>");
                }
            });

            dump!(d, "expr");
            d.indent(|d| {
                if let Some(ref expr) = stmt.expr {
                    d.dump_expr(expr);
                } else {
                    dump!(d, "<no expr given>");
                }
            });
        });
    }

    fn dump_stmt_while(&mut self, stmt: &StmtWhileType) {
        dump!(self, "while @ {} {}", stmt.pos, stmt.id);

        self.indent(|d| {
            dump!(d, "cond");
            d.indent(|d| { d.dump_expr(&stmt.cond); });

            dump!(d, "body");
            d.indent(|d| { d.dump_stmt(&stmt.block); });
        });
    }

    fn dump_stmt_loop(&mut self, stmt: &StmtLoopType) {
        dump!(self, "loop @ {} {}", stmt.pos, stmt.id);
        self.indent(|d| { d.dump_stmt(&stmt.block); });
    }

    fn dump_stmt_if(&mut self, stmt: &StmtIfType) {
        dump!(self, "if @ {} {}", stmt.pos, stmt.id);

        self.indent(|d| {
            d.indent(|d| { d.dump_expr(&stmt.cond); });
            dump!(d, "then");
            d.indent(|d| { d.dump_stmt(&stmt.then_block); });
            dump!(d, "else");
            d.indent(|d| { d.dump_stmt(&stmt.then_block); });
        });
    }

    fn dump_stmt_expr(&mut self, stmt: &StmtExprType) {
        dump!(self, "expr stmt @ {} {}", stmt.pos, stmt.id);
        self.indent(|d| { d.dump_expr(&stmt.expr); });
    }

    fn dump_stmt_block(&mut self, block: &StmtBlockType) {
        dump!(self, "block ({} statement(s)) @ {} {}",
            block.stmts.len(), block.pos, block.id);

        self.indent(|d| {
            if block.stmts.is_empty() {
                dump!(d, "no statements");
            } else {
                for stmt in &block.stmts {
                    d.dump_stmt(stmt);
                }
            }
        });

        dump!(self, "block end");
    }

    fn dump_stmt_return(&mut self, ret: &StmtReturnType) {
        dump!(self, "return @ {} {}", ret.pos, ret.id);

        self.indent(|d| {
            if let Some(ref expr) = ret.expr {
                d.dump_expr(expr);
            } else {
                dump!(d, "<nothing>");
            }
        });
    }

    fn dump_stmt_break(&mut self, stmt: &StmtBreakType) {
        dump!(self, "break @ {} {}", stmt.pos, stmt.id);
    }

    fn dump_stmt_continue(&mut self, stmt: &StmtContinueType) {
        dump!(self, "continue @ {} {}", stmt.pos, stmt.id);
    }

    fn dump_expr(&mut self, expr: &Expr) {
        match *expr {
            ExprUn(ref un) => self.dump_expr_un(un),
            ExprBin(ref bin) => self.dump_expr_bin(bin),
            ExprLitInt(ref lit) => self.dump_expr_lit_int(lit),
            ExprLitStr(ref lit) => self.dump_expr_lit_str(lit),
            ExprLitBool(ref lit) => self.dump_expr_lit_bool(lit),
            ExprIdent(ref ident) => self.dump_expr_ident(ident),
            ExprAssign(ref assign) => self.dump_expr_assign(assign),
        }
    }

    fn dump_expr_lit_int(&mut self, lit: &ExprLitIntType) {
        dump!(self, "lit int {} @ {} {}", lit.value, lit.pos, lit.id);
    }

    fn dump_expr_lit_str(&mut self, lit: &ExprLitStrType) {
        dump!(self, "lit string {:?} @ {} {}", lit.value, lit.pos, lit.id);
    }

    fn dump_expr_lit_bool(&mut self, lit: &ExprLitBoolType) {
        dump!(self, "lit bool {} @ {} {}", lit.value, lit.pos, lit.id);
    }

    fn dump_expr_ident(&mut self, ident: &ExprIdentType) {
        dump!(self, "ident {} @ {} {}", self.str(ident.name), ident.pos, ident.id);
    }

    fn dump_expr_un(&mut self, expr: &ExprUnType) {
        dump!(self, "unary {:?} @ {} {}", expr.op, expr.pos, expr.id);
        self.indent(|d| d.dump_expr(&expr.opnd));
    }

    fn dump_expr_bin(&mut self, expr: &ExprBinType) {
        self.indent(|d| d.dump_expr(&expr.rhs));
        dump!(self, "binary {:?} @ {} {}", expr.op, expr.pos, expr.id);
        self.indent(|d| d.dump_expr(&expr.lhs));
    }

    fn dump_expr_assign(&mut self, expr: &ExprAssignType) {
        self.indent(|d| d.dump_expr(&expr.rhs));
        dump!(self, "assign (=) @ {} {}", expr.pos, expr.id);
        self.indent(|d| d.dump_expr(&expr.lhs));
    }

    fn indent<F>(&mut self, fct: F) where F: Fn(&mut AstDumper) -> () {
        let old = self.indent;
        self.indent = old+1;

        fct(self);

        self.indent = old;
    }

    fn str(&self, name: Name) -> &str {
        self.interner.str(name)
    }
}

