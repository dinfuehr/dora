use ast::*;
use ast::Elem::*;
use ast::Expr::*;
use ast::Stmt::*;
use interner::{Interner, Name, RcStr};

macro_rules! dump {
    ($self_:ident, $($message:tt)*) => {{
        for _ in 0..($self_.indent*2) {
            print!(" ");
        }

        println!($($message)*);
    }};
}

pub fn dump(ast: &Ast, interner: &Interner) {
    let mut dumper = AstDumper {
        interner: interner,
        indent: 0
    };

    dumper.dump_ast(ast);
}

pub fn dump_expr<'a>(expr: &'a Expr, interner: &'a Interner) {
    let mut dumper = AstDumper {
        interner: interner,
        indent: 0
    };

    dumper.dump_expr(expr);
}

struct AstDumper<'a> {
    interner: &'a Interner,
    indent: u32,
}

impl<'a> AstDumper<'a> {
    fn dump_ast(&mut self, ast: &'a Ast) {
        for el in &ast.elements {
            match *el {
                ElemFunction(ref fct) => self.dump_fct(fct),
                ElemClass(ref cls) => self.dump_class(cls),
            }
        }
    }

    fn dump_class(&mut self, cls: &Class) {
        dump!(self, "class {} @ {} {}", self.str(cls.name), cls.pos, cls.id);

        if let Some(ref parent_class) = cls.parent_class {
            self.indent(|d| {
                dump!(d, "super (name={} @ {})", d.str(parent_class.name), parent_class.pos);
            });
        }

        for ctor in &cls.ctors {
            self.indent(|d| {
                dump!(d, "ctor");

                d.indent(|d| d.dump_fct(ctor));
            })
        }

        self.indent(|d| {
            dump!(d, "methods");

            d.indent(|d| {
                if cls.methods.is_empty() {
                    dump!(d, "no methods");
                } else {
                    for mtd in &cls.methods {
                        d.dump_fct(mtd);
                    }
                }
            });
        });

        self.indent(|d| {
            dump!(d, "fields");

            d.indent(|d| {
                if cls.fields.is_empty() {
                    dump!(d, "no fields");
                } else {
                    for field in &cls.fields {
                        d.dump_field(field);
                    }
                }
            });
        });
    }

    fn dump_field(&mut self, field: &Field) {
        dump!(self, "field {} @ {} {}", self.str(field.name), field.pos, field.id);
        self.indent(|d| d.dump_type(&field.data_type));
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
                        d.dump_param(param);
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
        dump!(self, "param {} @ {} {}",
            self.str(param.name), param.pos, param.id);

        if !param.data_type.is_self() {
            self.indent(|d| d.dump_type(&param.data_type));
        }
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
            StmtThrow(ref stmt) => self.dump_stmt_throw(stmt),
            StmtTry(ref stmt) => self.dump_stmt_try(stmt),
        }
    }

    fn dump_stmt_var(&mut self, stmt: &StmtVarType) {
        dump!(self, "let {} @ {} {}", self.str(stmt.name), stmt.pos, stmt.id);

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

    fn dump_stmt_throw(&mut self, stmt: &StmtThrowType) {
        dump!(self, "throw @ {} {}", stmt.pos, stmt.id);
        self.indent(|d| d.dump_expr(&stmt.expr));
    }

    fn dump_stmt_try(&mut self, stmt: &StmtTryType) {
        dump!(self, "try @ {} {}", stmt.pos, stmt.id);
        self.indent(|d| d.dump_stmt(&stmt.try_block));

        for catch in &stmt.catch_blocks {
            dump!(self, "catch (var={})", self.str(catch.name));
            self.indent(|d| {
                d.dump_type(&catch.data_type);
                d.dump_stmt(&catch.block);
            });
        }

        if let Some(ref finally_block) = stmt.finally_block {
            dump!(self, "finally");
            self.dump_stmt(&finally_block.block);
        }
    }

    fn dump_expr(&mut self, expr: &Expr) {
        match *expr {
            ExprUn(ref un) => self.dump_expr_un(un),
            ExprBin(ref bin) => self.dump_expr_bin(bin),
            ExprField(ref field) => self.dump_expr_field(field),
            ExprArray(ref array) => self.dump_expr_array(array),
            ExprLitInt(ref lit) => self.dump_expr_lit_int(lit),
            ExprLitStr(ref lit) => self.dump_expr_lit_str(lit),
            ExprLitBool(ref lit) => self.dump_expr_lit_bool(lit),
            ExprIdent(ref ident) => self.dump_expr_ident(ident),
            ExprAssign(ref assign) => self.dump_expr_assign(assign),
            ExprCall(ref call) => self.dump_expr_call(call),
            ExprSuperCall(ref call) => self.dump_expr_super_call(call),
            ExprSelf(ref selfie) => self.dump_expr_self(selfie),
            ExprNil(ref nil) => self.dump_expr_nil(nil),
        }
    }

    fn dump_expr_super_call(&mut self, expr: &ExprSuperCallType) {
        dump!(self, "super @ {} {}", expr.pos, expr.id);

        self.indent(|d| {
            for arg in &expr.args {
                d.dump_expr(arg);
            }
        });
    }

    fn dump_expr_self(&mut self, selfie: &ExprSelfType) {
        dump!(self, "self @ {} {}", selfie.pos, selfie.id);
    }

    fn dump_expr_nil(&mut self, nil: &ExprNilType) {
        dump!(self, "nil @ {} {}", nil.pos, nil.id);
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

    fn dump_expr_array(&mut self, expr: &ExprArrayType) {
        self.indent(|d| d.dump_expr(&expr.object));
        dump!(self, "[] @ {} {}", expr.pos, expr.id);
        self.indent(|d| d.dump_expr(&expr.index));
    }

    fn dump_expr_field(&mut self, field: &ExprFieldType) {
        dump!(self, "field {} @ {} {}", self.str(field.name), field.pos, field.id);
        self.indent(|d| d.dump_expr(&field.object));
    }

    fn dump_expr_assign(&mut self, expr: &ExprAssignType) {
        self.indent(|d| d.dump_expr(&expr.rhs));
        dump!(self, "assign (=) @ {} {}", expr.pos, expr.id);
        self.indent(|d| d.dump_expr(&expr.lhs));
    }

    fn dump_expr_call(&mut self, expr: &ExprCallType) {
        dump!(self, "call {} (with self={}) @ {} {}", self.str(expr.name),
              expr.with_self, expr.pos, expr.id);

        self.indent(|d| {
            for arg in &expr.args {
                d.dump_expr(arg);
            }
        });
    }

    fn indent<F>(&mut self, fct: F) where F: Fn(&mut AstDumper) -> () {
        let old = self.indent;
        self.indent = old+1;

        fct(self);

        self.indent = old;
    }

    fn str(&self, name: Name) -> RcStr {
        self.interner.str(name)
    }
}
