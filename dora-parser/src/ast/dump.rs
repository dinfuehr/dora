use std::sync::Arc;

use crate::ast::*;

use crate::interner::{ArcStr, Interner, Name};

macro_rules! dump {
    ($self_:ident, $($message:tt)*) => {{
        for _ in 0..($self_.indent*2) {
            print!(" ");
        }

        println!($($message)*);
    }};
}

pub fn dump_file(ast: &Arc<File>, interner: &Interner) {
    let mut dumper = AstDumper {
        interner,
        indent: 0,
    };

    dumper.dump_file(ast);
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
    fn dump_file(&mut self, f: &File) {
        for el in &f.elements {
            self.dump_elem(el);
        }
    }

    fn dump_elem(&mut self, el: &Elem) {
        match *el {
            Elem::Function(ref fct) => self.dump_fct(fct),
            Elem::Class(ref cls) => self.dump_class(cls),
            Elem::Struct(ref struc) => self.dump_struct(struc),
            Elem::Trait(ref xtrait) => self.dump_trait(xtrait),
            Elem::Impl(ref ximpl) => self.dump_impl(ximpl),
            Elem::Module(ref module) => self.dump_module(module),
            Elem::Annotation(ref annotation) => self.dump_annotation(annotation),
            Elem::Global(ref global) => self.dump_global(global),
            Elem::Const(ref xconst) => self.dump_const(xconst),
            Elem::Enum(ref xenum) => self.dump_enum(xenum),
            Elem::Alias(ref alias) => self.dump_alias(alias),
            Elem::Namespace(ref namespace) => self.dump_namespace(namespace),
            Elem::Import(ref import) => self.dump_import(import),
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

    fn dump_import(&mut self, import: &Import) {
        dump!(self, "import @ {} {}", import.pos, import.id);
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

    fn dump_namespace(&mut self, namespace: &Namespace) {
        dump!(
            self,
            "namespace {} @ {} {}",
            self.str(namespace.name),
            namespace.pos,
            namespace.id
        );

        self.indent(|d| {
            for e in &namespace.elements {
                d.dump_elem(e);
            }
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

    fn dump_annotation(&mut self, annotation: &Annotation) {
        dump!(
            self,
            "annotation {} @ {} {}",
            self.str(annotation.name),
            annotation.pos,
            annotation.id
        );

        self.indent(|d| {
            dump!(d, "params");
            if let Some(params) = &annotation.term_params {
                for param in params {
                    d.dump_annotation_param(param);
                }
            }
        });
    }

    fn dump_annotation_param(&mut self, param: &AnnotationParam) {
        dump!(self, "param {} @ {}", self.str(param.name), param.pos);

        self.indent(|d| d.dump_type(&param.data_type));
    }

    #[allow(dead_code)]
    fn dump_annotation_usages(&self, annotation_usages: &AnnotationUsages) {
        for annotation_usage in annotation_usages.iter() {
            dump!(
                self,
                "@{} {}",
                self.str(annotation_usage.name),
                annotation_usage.pos
            );
        }
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
        dump!(self, "type @ {:?} {}", ty.pos(), ty.id());
    }

    fn dump_stmt(&mut self, stmt: &Stmt) {
        match *stmt {
            Stmt::Return(ref ret) => self.dump_stmt_return(ret),
            Stmt::Break(ref stmt) => self.dump_stmt_break(stmt),
            Stmt::Continue(ref stmt) => self.dump_stmt_continue(stmt),
            Stmt::Expr(ref expr) => self.dump_stmt_expr(expr),
            Stmt::Let(ref stmt) => self.dump_stmt_let(stmt),
            Stmt::While(ref stmt) => self.dump_stmt_while(stmt),
            Stmt::For(ref stmt) => self.dump_stmt_for(stmt),
        }
    }

    fn dump_stmt_let(&mut self, stmt: &StmtLetType) {
        dump!(self, "let @ {} {}", stmt.pos, stmt.id);

        self.indent(|d| {
            d.dump_stmt_let_pattern(&stmt.pattern);
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

    fn dump_stmt_let_pattern(&mut self, pattern: &LetPattern) {
        match pattern {
            LetPattern::Ident(ref ident) => dump!(self, "ident {:?}", ident.name),
            LetPattern::Underscore(_) => dump!(self, "_"),
            LetPattern::Tuple(ref tuple) => {
                dump!(self, "tuple");
                self.indent(|d| {
                    for part in &tuple.parts {
                        d.dump_stmt_let_pattern(part);
                    }
                });
            }
        }
    }

    fn dump_stmt_for(&mut self, stmt: &StmtForType) {
        dump!(self, "for @ {} {}", stmt.pos, stmt.id);

        self.indent(|d| {
            d.dump_stmt_let_pattern(&stmt.pattern);
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
            Expr::Un(ref un) => self.dump_expr_un(un),
            Expr::Bin(ref bin) => self.dump_expr_bin(bin),
            Expr::Dot(ref field) => self.dump_expr_dot(field),
            Expr::LitChar(ref lit) => self.dump_expr_lit_char(lit),
            Expr::LitInt(ref lit) => self.dump_expr_lit_int(lit),
            Expr::LitFloat(ref lit) => self.dump_expr_lit_float(lit),
            Expr::LitStr(ref lit) => self.dump_expr_lit_str(lit),
            Expr::Template(ref tmpl) => self.dump_expr_template(tmpl),
            Expr::LitBool(ref lit) => self.dump_expr_lit_bool(lit),
            Expr::Ident(ref ident) => self.dump_expr_ident(ident),
            Expr::Call(ref call) => self.dump_expr_call(call),
            Expr::TypeParam(ref expr) => self.dump_expr_type_param(expr),
            Expr::Path(ref path) => self.dump_expr_path(path),
            Expr::Delegation(ref call) => self.dump_expr_delegation(call),
            Expr::This(ref selfie) => self.dump_expr_self(selfie),
            Expr::Super(ref expr) => self.dump_expr_super(expr),
            Expr::Conv(ref expr) => self.dump_expr_conv(expr),
            Expr::Lambda(ref expr) => self.dump_expr_lambda(expr),
            Expr::Block(ref expr) => self.dump_expr_block(expr),
            Expr::If(ref expr) => self.dump_expr_if(expr),
            Expr::Tuple(ref expr) => self.dump_expr_tuple(expr),
            Expr::Paren(ref expr) => self.dump_expr_paren(expr),
            Expr::Match(ref expr) => self.dump_expr_match(expr),
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

    fn dump_expr_match(&mut self, expr: &ExprMatchType) {
        dump!(self, "match @ {} {}", expr.pos, expr.id);
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
