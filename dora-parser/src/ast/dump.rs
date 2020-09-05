use crate::ast::*;

use crate::ast::Expr::*;
use crate::ast::Stmt::*;
use crate::interner::{ArcStr, Interner, Name};

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
        interner,
        indent: 0,
    };

    dumper.dump_ast(ast);
}

pub fn dump_fct(fct: &Function, interner: &Interner) {
    let mut dumper = AstDumper {
        interner,
        indent: 0,
    };

    dumper.dump_fct(fct);
}

pub fn dump_expr<'a>(expr: &'a Expr, interner: &'a Interner) {
    let mut dumper = AstDumper {
        interner,
        indent: 0,
    };

    dumper.dump_expr(expr);
}

pub fn dump_stmt<'a>(stmt: &'a Stmt, interner: &'a Interner) {
    let mut dumper = AstDumper {
        interner,
        indent: 0,
    };

    dumper.dump_stmt(stmt);
}

struct AstDumper<'a> {
    interner: &'a Interner,
    indent: u32,
}

impl<'a> AstDumper<'a> {
    fn dump_ast(&mut self, ast: &Ast) {
        for f in &ast.files {
            dump!(self, "file {}", &f.path);

            self.indent(|d| {
                d.dump_file(f);
            })
        }
    }

    fn dump_file(&mut self, f: &File) {
        for el in &f.elements {
            match *el {
                ElemFunction(ref fct) => self.dump_fct(fct),
                ElemClass(ref cls) => self.dump_class(cls),
                ElemStruct(ref struc) => self.dump_struct(struc),
                ElemTrait(ref xtrait) => self.dump_trait(xtrait),
                ElemImpl(ref ximpl) => self.dump_impl(ximpl),
                ElemModule(ref module) => self.dump_module(module),
                ElemGlobal(ref global) => self.dump_global(global),
                ElemConst(ref xconst) => self.dump_const(xconst),
                ElemEnum(ref xenum) => self.dump_enum(xenum),
                ElemAlias(ref alias) => self.dump_alias(alias),
            }
        }
    }

    fn dump_global(&mut self, global: &Global) {
        dump!(
            self,
            "global {} @ {} {}",
            self.str(global.name),
            global.pos,
            global.id
        );

        self.indent(|d| {
            d.dump_type(&global.data_type);

            if let Some(ref initializer) = global.initializer {
                d.dump_fct(initializer);
            } else {
                dump!(d, "<no expr given>");
            }
        });
    }

    fn dump_const(&mut self, xconst: &Const) {
        dump!(
            self,
            "const {} @ {} {}",
            self.str(xconst.name),
            xconst.pos,
            xconst.id
        );

        self.indent(|d| {
            d.dump_type(&xconst.data_type);
            d.dump_expr(&xconst.expr);
        });
    }

    fn dump_alias(&mut self, alias: &Alias) {
        dump!(
            self,
            "alias {} @ {} {}",
            self.str(alias.name),
            alias.pos,
            alias.id
        );

        self.indent(|d| {
            d.dump_type(&alias.ty);
        });
    }

    fn dump_enum(&mut self, xenum: &Enum) {
        dump!(
            self,
            "enum {} @ {} {}",
            self.str(xenum.name),
            xenum.pos,
            xenum.id
        );

        self.indent(|d| {
            for value in &xenum.variants {
                d.dump_enum_value(value);
            }
        });
    }

    fn dump_enum_value(&mut self, value: &EnumVariant) {
        dump!(self, "{} {} {}", value.pos, value.id, self.str(value.name));

        if let Some(ref types) = value.types {
            self.indent(|d| {
                for ty in types {
                    d.dump_type(ty);
                }
            });
        }
    }

    fn dump_impl(&mut self, ximpl: &Impl) {
        dump!(self, "impl @ {} {}", ximpl.pos, ximpl.id);

        self.indent(|d| {
            if let Some(trait_type) = ximpl.trait_type.as_ref() {
                d.dump_type(trait_type);
                dump!(d, "for");
            }

            d.dump_type(&ximpl.class_type);

            for mtd in &ximpl.methods {
                d.dump_fct(mtd);
            }
        });
    }

    fn dump_struct(&mut self, struc: &Struct) {
        dump!(
            self,
            "struct {} @ {} {}",
            self.str(struc.name),
            struc.pos,
            struc.id
        );

        self.indent(|d| {
            for field in &struc.fields {
                d.dump_struct_field(field);
            }
        });
    }

    fn dump_struct_field(&mut self, field: &StructField) {
        dump!(
            self,
            "field {} @ {} {}",
            self.str(field.name),
            field.pos,
            field.id
        );
        self.indent(|d| d.dump_type(&field.data_type));
    }

    fn dump_trait(&mut self, t: &Trait) {
        dump!(self, "trait {} @ {} {}", self.str(t.name), t.pos, t.id);
        self.indent(|d| {
            for m in &t.methods {
                d.dump_fct(m);
            }
        });
    }

    fn dump_class(&mut self, cls: &Class) {
        dump!(
            self,
            "class {} @ {} {}",
            self.str(cls.name),
            cls.pos,
            cls.id
        );

        self.indent(|d| {
            dump!(d, "open = {}", cls.has_open);

            if let Some(ref parent_class) = cls.parent_class {
                dump!(
                    d,
                    "super (name={} @ {})",
                    d.str(parent_class.name),
                    parent_class.pos
                );
            }

            dump!(d, "fields");

            d.indent(|d| {
                for field in &cls.fields {
                    d.dump_field(field);
                }
            });

            dump!(d, "constructor");
            if let Some(ctor) = &cls.constructor {
                d.indent(|d| d.dump_fct(ctor));
            }

            dump!(d, "methods");
            d.indent(|d| {
                for mtd in &cls.methods {
                    d.dump_fct(mtd);
                }
            });
        });
    }

    fn dump_module(&mut self, modu: &Module) {
        dump!(
            self,
            "module {} @ {} {}",
            self.str(modu.name),
            modu.pos,
            modu.id
        );

        self.indent(|d| {
            if let Some(ref parent_class) = modu.parent_class {
                dump!(
                    d,
                    "super (name={} @ {})",
                    d.str(parent_class.name),
                    parent_class.pos
                );
            }

            dump!(d, "fields");

            d.indent(|d| {
                for field in &modu.fields {
                    d.dump_field(field);
                }
            });

            dump!(d, "constructor");
            if let Some(ctor) = &modu.constructor {
                d.indent(|d| d.dump_fct(ctor));
            }

            dump!(d, "methods");
            d.indent(|d| {
                for mtd in &modu.methods {
                    d.dump_fct(mtd);
                }
            });
        });
    }

    fn dump_field(&mut self, field: &Field) {
        dump!(
            self,
            "field {} @ {} {}",
            self.str(field.name),
            field.pos,
            field.id
        );
        self.indent(|d| d.dump_type(&field.data_type));
    }

    fn dump_fct(&mut self, fct: &Function) {
        dump!(self, "fct {} @ {} {}", self.str(fct.name), fct.pos, fct.id);

        self.indent(|d| {
            dump!(d, "open = {}", fct.has_open);
            dump!(d, "override = {}", fct.has_override);
            dump!(d, "final = {}", fct.has_final);
            dump!(d, "internal = {}", fct.internal);
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

            if let Some(ref block) = fct.block {
                d.indent(|d| d.dump_expr_block(block));
            }
        });
    }

    fn dump_param(&mut self, param: &Param) {
        dump!(
            self,
            "param {} @ {} {}",
            self.str(param.name),
            param.pos,
            param.id
        );

        self.indent(|d| d.dump_type(&param.data_type));
    }

    fn dump_type(&mut self, ty: &Type) {
        dump!(
            self,
            "type `{}` @ {:?} {}",
            ty.to_string(self.interner),
            ty.pos(),
            ty.id()
        );
    }

    fn dump_stmt(&mut self, stmt: &Stmt) {
        match *stmt {
            StmtReturn(ref ret) => self.dump_stmt_return(ret),
            StmtBreak(ref stmt) => self.dump_stmt_break(stmt),
            StmtContinue(ref stmt) => self.dump_stmt_continue(stmt),
            StmtExpr(ref expr) => self.dump_stmt_expr(expr),
            StmtLet(ref stmt) => self.dump_stmt_let(stmt),
            StmtWhile(ref stmt) => self.dump_stmt_while(stmt),
            StmtFor(ref stmt) => self.dump_stmt_for(stmt),
        }
    }

    fn dump_stmt_let(&mut self, stmt: &StmtLetType) {
        dump!(self, "let @ {} {}", stmt.pos, stmt.id);

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

    fn dump_stmt_for(&mut self, stmt: &StmtForType) {
        dump!(self, "for @ {} {}", stmt.pos, stmt.id);

        self.indent(|d| {
            dump!(d, "name {:?}", stmt.name);
            dump!(d, "cond");
            d.indent(|d| {
                d.dump_expr(&stmt.expr);
            });
            dump!(d, "body");
            d.indent(|d| {
                d.dump_stmt(&stmt.block);
            });
        });
    }

    fn dump_stmt_while(&mut self, stmt: &StmtWhileType) {
        dump!(self, "while @ {} {}", stmt.pos, stmt.id);

        self.indent(|d| {
            dump!(d, "cond");
            d.indent(|d| {
                d.dump_expr(&stmt.cond);
            });

            dump!(d, "body");
            d.indent(|d| {
                d.dump_stmt(&stmt.block);
            });
        });
    }

    fn dump_stmt_expr(&mut self, stmt: &StmtExprType) {
        dump!(self, "expr stmt @ {} {}", stmt.pos, stmt.id);
        self.indent(|d| {
            d.dump_expr(&stmt.expr);
        });
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
            ExprDot(ref field) => self.dump_expr_dot(field),
            ExprLitChar(ref lit) => self.dump_expr_lit_char(lit),
            ExprLitInt(ref lit) => self.dump_expr_lit_int(lit),
            ExprLitFloat(ref lit) => self.dump_expr_lit_float(lit),
            ExprLitStr(ref lit) => self.dump_expr_lit_str(lit),
            ExprTemplate(ref tmpl) => self.dump_expr_template(tmpl),
            ExprLitBool(ref lit) => self.dump_expr_lit_bool(lit),
            ExprIdent(ref ident) => self.dump_expr_ident(ident),
            ExprCall(ref call) => self.dump_expr_call(call),
            ExprTypeParam(ref expr) => self.dump_expr_type_param(expr),
            ExprPath(ref path) => self.dump_expr_path(path),
            ExprDelegation(ref call) => self.dump_expr_delegation(call),
            ExprSelf(ref selfie) => self.dump_expr_self(selfie),
            ExprSuper(ref expr) => self.dump_expr_super(expr),
            ExprNil(ref nil) => self.dump_expr_nil(nil),
            ExprConv(ref expr) => self.dump_expr_conv(expr),
            ExprLambda(ref expr) => self.dump_expr_lambda(expr),
            ExprBlock(ref expr) => self.dump_expr_block(expr),
            ExprIf(ref expr) => self.dump_expr_if(expr),
            ExprTuple(ref expr) => self.dump_expr_tuple(expr),
            ExprParen(ref expr) => self.dump_expr_paren(expr),
        }
    }

    fn dump_expr_block(&mut self, block: &ExprBlockType) {
        dump!(
            self,
            "block ({} statement(s)) @ {} {}",
            block.stmts.len(),
            block.pos,
            block.id
        );

        self.indent(|d| {
            if block.stmts.is_empty() {
                dump!(d, "no statements");
            } else {
                for stmt in &block.stmts {
                    d.dump_stmt(stmt);
                }
            }

            if let Some(ref expr) = block.expr {
                dump!(d, "value");
                d.dump_expr(expr);
            }
        });

        dump!(self, "block end");
    }

    fn dump_expr_if(&mut self, expr: &ExprIfType) {
        dump!(self, "if @ {} {}", expr.pos, expr.id);

        self.indent(|d| {
            d.indent(|d| {
                d.dump_expr(&expr.cond);
            });
            dump!(d, "then");
            d.indent(|d| {
                d.dump_expr(&expr.then_block);
            });
            dump!(d, "else");
            d.indent(|d| {
                d.dump_expr(&expr.then_block);
            });
        });
    }

    fn dump_expr_conv(&mut self, expr: &ExprConvType) {
        self.indent(|d| d.dump_expr(&expr.object));
        let op = if expr.is { "is" } else { "as" };
        dump!(self, "{} @ {} {}", op, expr.pos, expr.id);
        self.indent(|d| d.dump_type(&expr.data_type));
    }

    fn dump_expr_delegation(&mut self, expr: &ExprDelegationType) {
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

    fn dump_expr_super(&mut self, selfie: &ExprSuperType) {
        dump!(self, "super @ {} {}", selfie.pos, selfie.id);
    }

    fn dump_expr_nil(&mut self, nil: &ExprNilType) {
        dump!(self, "nil @ {} {}", nil.pos, nil.id);
    }

    fn dump_expr_lit_char(&mut self, lit: &ExprLitCharType) {
        dump!(
            self,
            "lit char {} {} @ {} {}",
            lit.value,
            lit.value as u32,
            lit.pos,
            lit.id
        );
    }

    fn dump_expr_lit_int(&mut self, lit: &ExprLitIntType) {
        dump!(self, "lit int {} @ {} {}", lit.value, lit.pos, lit.id);
    }

    fn dump_expr_lit_float(&mut self, lit: &ExprLitFloatType) {
        dump!(self, "lit float {} @ {} {}", lit.value, lit.pos, lit.id);
    }

    fn dump_expr_lit_str(&mut self, lit: &ExprLitStrType) {
        dump!(self, "lit string {:?} @ {} {}", lit.value, lit.pos, lit.id);
    }

    fn dump_expr_template(&mut self, tmpl: &ExprTemplateType) {
        dump!(self, "template @ {} {}", tmpl.pos, tmpl.id);
        self.indent(|d| {
            for part in &tmpl.parts {
                d.dump_expr(part)
            }
        });
    }

    fn dump_expr_lit_bool(&mut self, lit: &ExprLitBoolType) {
        dump!(self, "lit bool {} @ {} {}", lit.value, lit.pos, lit.id);
    }

    fn dump_expr_ident(&mut self, ident: &ExprIdentType) {
        dump!(
            self,
            "ident {} @ {} {}",
            self.str(ident.name),
            ident.pos,
            ident.id
        );
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

    fn dump_expr_lambda(&mut self, expr: &ExprLambdaType) {
        dump!(self, "lambda @ {} {}", expr.pos, expr.id);
        self.indent(|d| d.dump_stmt(&expr.block));
    }

    fn dump_expr_tuple(&mut self, expr: &ExprTupleType) {
        dump!(self, "tuple @ {} {}", expr.pos, expr.id);
        self.indent(|d| {
            for expr in &expr.values {
                d.dump_expr(expr);
            }
        });
    }

    fn dump_expr_dot(&mut self, expr: &ExprDotType) {
        self.indent(|d| d.dump_expr(&expr.rhs));
        dump!(self, "dot @ {} {}", expr.pos, expr.id);
        self.indent(|d| d.dump_expr(&expr.lhs));
    }

    fn dump_expr_path(&mut self, expr: &ExprPathType) {
        self.indent(|d| d.dump_expr(&expr.rhs));
        dump!(self, "path (::) @ {} {}", expr.pos, expr.id);
        self.indent(|d| d.dump_expr(&expr.lhs));
    }

    fn dump_expr_call(&mut self, expr: &ExprCallType) {
        dump!(self, "call @ {} {}", expr.pos, expr.id);

        self.indent(|d| {
            dump!(d, "callee");
            d.indent(|d| d.dump_expr(&expr.callee));

            for arg in &expr.args {
                d.dump_expr(arg);
            }
        });
    }

    fn dump_expr_paren(&mut self, expr: &ExprParenType) {
        dump!(self, "paren @ {} {}", expr.pos, expr.id);
        self.indent(|d| {
            d.dump_expr(&expr.expr);
        });
    }

    fn dump_expr_type_param(&mut self, expr: &ExprTypeParamType) {
        dump!(self, "type param @ {} {}", expr.pos, expr.id);

        self.indent(|d| {
            dump!(d, "callee");
            d.indent(|d| d.dump_expr(&expr.callee));

            for arg in &expr.args {
                d.dump_type(arg);
            }
        });
    }

    fn indent<F>(&mut self, fct: F)
    where
        F: Fn(&mut AstDumper) -> (),
    {
        let old = self.indent;
        self.indent = old + 1;

        fct(self);

        self.indent = old;
    }

    fn str(&self, name: Name) -> ArcStr {
        self.interner.str(name)
    }
}
