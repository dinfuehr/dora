use std::sync::Arc;

use crate::ast::*;

macro_rules! dump {
    ($self_:ident, $($message:tt)*) => {{
        for _ in 0..($self_.indent*2) {
            print!(" ");
        }

        println!($($message)*);
    }};
}

pub fn dump_file(ast: &Arc<File>) {
    let mut dumper = AstDumper {
        indent: 0,
        f: ast.as_ref(),
    };
    dumper.dump_file();
}

pub fn dump_fct(f: &Arc<File>, fct: &Function) {
    let mut dumper = AstDumper { indent: 0, f };
    dumper.dump_fct(fct);
}

struct AstDumper<'a> {
    indent: u32,
    f: &'a File,
}

impl<'a> AstDumper<'a> {
    fn dump_file(&mut self) {
        for &element_id in &self.f.elements {
            let element = self.f.node(element_id);
            self.dump_elem(element);
        }
    }

    fn dump_elem(&mut self, el: &ElemData) {
        match *el {
            ElemData::Function(ref node) => self.dump_fct(node),
            ElemData::Class(ref node) => self.dump_class(node),
            ElemData::Struct(ref node) => self.dump_struct(node),
            ElemData::Trait(ref node) => self.dump_trait(node),
            ElemData::Impl(ref node) => self.dump_impl(node),
            ElemData::Global(ref node) => self.dump_global(node),
            ElemData::Const(ref node) => self.dump_const(node),
            ElemData::Enum(ref node) => self.dump_enum(node),
            ElemData::Module(ref node) => self.dump_module(node),
            ElemData::Use(ref node) => self.dump_use(node),
            ElemData::Extern(ref node) => self.dump_extern(node),
            ElemData::Alias(ref node) => self.dump_associated_type(node),
            ElemData::Error { id, span } => {
                dump!(self, "error @ {} {}", span, id);
            }
        }
    }

    fn dump_global(&mut self, global: &Global) {
        dump!(self, "global @ {} {}", global.span, global.id);
        self.dump_maybe_ident(&global.name);

        self.indent(|d| {
            d.dump_type(&global.data_type);

            if let Some(ref initial_value) = global.initial_value {
                d.dump_expr(initial_value);
            } else {
                dump!(d, "<no expr given>");
            }
        });
    }

    fn dump_extern(&mut self, stmt: &ExternPackage) {
        dump!(self, "extern package @ {} {}", stmt.span, stmt.id);
        self.dump_maybe_ident(&stmt.name);
        self.dump_maybe_ident(&stmt.identifier);
    }

    fn dump_const(&mut self, const_: &Const) {
        dump!(self, "const @ {} {}", const_.span, const_.id);
        self.dump_maybe_ident(&const_.name);

        self.indent(|d| {
            d.dump_type(&const_.data_type);
            d.dump_expr(&const_.expr);
        });
    }

    fn dump_use(&mut self, node: &Use) {
        dump!(self, "use @ {} {}", node.span, node.id);
    }

    fn dump_module(&mut self, module: &Module) {
        dump!(self, "module @ {} {}", module.span, module.id);
        self.dump_maybe_ident(&module.name);

        self.indent(|d| {
            if let Some(ref elements) = module.elements {
                for &element_id in elements {
                    let element = self.f.node(element_id);
                    d.dump_elem(element);
                }
            }
        });
    }

    fn dump_enum(&mut self, enum_: &Enum) {
        dump!(self, "enum @ {} {}", enum_.span, enum_.id);
        self.dump_maybe_ident(&enum_.name);

        self.indent(|d| {
            for value in &enum_.variants {
                d.dump_enum_value(value);
            }
        });
    }

    fn dump_enum_value(&mut self, value: &EnumVariant) {
        dump!(self, "enum variant @ {} {}", value.span, value.id);
        self.dump_maybe_ident(&value.name);

        self.indent(|d| {
            for field in &value.fields {
                d.dump_type(&field.data_type);
            }
        });
    }

    fn dump_impl(&mut self, impl_: &Impl) {
        dump!(self, "impl @ {} {}", impl_.span, impl_.id);

        self.indent(|d| {
            if let Some(trait_type) = impl_.trait_type.as_ref() {
                d.dump_type(trait_type);
                dump!(d, "for");
            }

            d.dump_type(&impl_.extended_type);

            for &element_id in &impl_.methods {
                let element = self.f.node(element_id);
                d.dump_elem(element);
            }
        });
    }

    fn dump_struct(&mut self, struc: &Struct) {
        dump!(self, "struct @ {} {}", struc.span, struc.id);
        self.dump_maybe_ident(&struc.name);

        self.indent(|d| {
            for field in &struc.fields {
                d.dump_field(field);
            }
        });
    }

    fn dump_field(&mut self, field: &Arc<Field>) {
        dump!(self, "field @ {} {}", field.span, field.id);
        self.dump_maybe_ident(&field.name);
        self.indent(|d| d.dump_type(&field.data_type));
    }

    fn dump_trait(&mut self, t: &Trait) {
        dump!(self, "trait @ {} {}", t.span, t.id);
        self.indent(|d| {
            d.dump_maybe_ident(&t.name);
            for &element_id in &t.methods {
                let element = self.f.node(element_id);
                d.dump_elem(element);
            }
        });
    }

    fn dump_associated_type(&mut self, t: &Alias) {
        dump!(self, "trait @ {} {}", t.span, t.id);
        self.indent(|d| {
            d.dump_maybe_ident(&t.name);
        });
    }

    fn dump_class(&mut self, cls: &Class) {
        dump!(self, "class @ {} {}", cls.span, cls.id);
        self.dump_maybe_ident(&cls.name);

        self.indent(|d| {
            dump!(d, "fields");

            d.indent(|d| {
                for field in &cls.fields {
                    d.dump_field(field);
                }
            });
        });
    }

    fn dump_fct(&mut self, fct: &Function) {
        dump!(self, "fct @ {} {}", fct.span, fct.id);
        self.dump_maybe_ident(&fct.name);

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

            if let Some(ref block) = fct.block {
                d.indent(|d| d.dump_expr(block));
            }
        });
    }

    fn dump_param(&mut self, param: &Param) {
        dump!(self, "param @ {} {}", param.span, param.id);
        self.dump_type(&param.data_type);

        self.indent(|d| d.dump_type(&param.data_type));
    }

    fn dump_type(&mut self, ty: &TypeData) {
        dump!(self, "type @ {:?} {}", ty.span(), ty.id());
    }

    fn dump_stmt(&mut self, stmt: &StmtData) {
        match *stmt {
            StmtData::Expr(ref expr) => self.dump_stmt_expr(expr),
            StmtData::Let(ref stmt) => self.dump_stmt_let(stmt),
        }
    }

    fn dump_stmt_let(&mut self, stmt: &StmtLetType) {
        dump!(self, "let @ {} {}", stmt.span, stmt.id);

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

    fn dump_expr_for(&mut self, stmt: &ExprForType) {
        dump!(self, "for @ {} {}", stmt.span, stmt.id);

        self.indent(|d| {
            dump!(d, "cond");
            d.indent(|d| {
                d.dump_expr(&stmt.expr);
            });
            dump!(d, "body");
            d.indent(|d| {
                d.dump_expr(&stmt.block);
            });
        });
    }

    fn dump_expr_while(&mut self, expr: &ExprWhileType) {
        dump!(self, "while @ {} {}", expr.span, expr.id);

        self.indent(|d| {
            dump!(d, "cond");
            d.indent(|d| {
                d.dump_expr(&expr.cond);
            });

            dump!(d, "body");
            d.indent(|d| {
                d.dump_expr(&expr.block);
            });
        });
    }

    fn dump_stmt_expr(&mut self, stmt: &StmtExprType) {
        dump!(self, "expr stmt @ {} {}", stmt.span, stmt.id);
        self.indent(|d| {
            d.dump_expr(&stmt.expr);
        });
    }

    fn dump_expr_return(&mut self, ret: &ExprReturnType) {
        dump!(self, "return @ {} {}", ret.span, ret.id);

        self.indent(|d| {
            if let Some(ref expr) = ret.expr {
                d.dump_expr(expr);
            } else {
                dump!(d, "<nothing>");
            }
        });
    }

    fn dump_expr_break(&mut self, stmt: &ExprBreakType) {
        dump!(self, "break @ {} {}", stmt.span, stmt.id);
    }

    fn dump_expr_continue(&mut self, stmt: &ExprContinueType) {
        dump!(self, "continue @ {} {}", stmt.span, stmt.id);
    }

    fn dump_expr(&mut self, expr: &ExprData) {
        match *expr {
            ExprData::Un(ref un) => self.dump_expr_un(un),
            ExprData::Bin(ref bin) => self.dump_expr_bin(bin),
            ExprData::Dot(ref field) => self.dump_expr_dot(field),
            ExprData::LitChar(ref lit) => self.dump_expr_lit_char(lit),
            ExprData::LitInt(ref lit) => self.dump_expr_lit_int(lit),
            ExprData::LitFloat(ref lit) => self.dump_expr_lit_float(lit),
            ExprData::LitStr(ref lit) => self.dump_expr_lit_str(lit),
            ExprData::Template(ref tmpl) => self.dump_expr_template(tmpl),
            ExprData::LitBool(ref lit) => self.dump_expr_lit_bool(lit),
            ExprData::Ident(ref ident) => self.dump_expr_ident(ident),
            ExprData::Call(ref call) => self.dump_expr_call(call),
            ExprData::TypeParam(ref expr) => self.dump_expr_type_param(expr),
            ExprData::Path(ref path) => self.dump_expr_path(path),
            ExprData::This(ref selfie) => self.dump_expr_self(selfie),
            ExprData::Conv(ref expr) => self.dump_expr_conv(expr),
            ExprData::Is(ref expr) => self.dump_expr_is(expr),
            ExprData::Lambda(ref expr) => self.dump_expr_lambda(expr),
            ExprData::Block(ref expr) => self.dump_expr_block(expr),
            ExprData::If(ref expr) => self.dump_expr_if(expr),
            ExprData::Tuple(ref expr) => self.dump_expr_tuple(expr),
            ExprData::Paren(ref expr) => self.dump_expr_paren(expr),
            ExprData::Match(ref expr) => self.dump_expr_match(expr),
            ExprData::For(ref expr) => self.dump_expr_for(expr),
            ExprData::While(ref expr) => self.dump_expr_while(expr),
            ExprData::Break(ref expr) => self.dump_expr_break(expr),
            ExprData::Continue(ref expr) => self.dump_expr_continue(expr),
            ExprData::Return(ref ret) => self.dump_expr_return(ret),
            ExprData::Error { id, span } => {
                dump!(self, "error @ {} {}", span, id);
            }
        }
    }

    fn dump_expr_block(&mut self, block: &ExprBlockType) {
        dump!(
            self,
            "block ({} statement(s)) @ {} {}",
            block.stmts.len(),
            block.span,
            block.id
        );

        self.indent(|d| {
            if block.stmts.is_empty() {
                dump!(d, "<no statements>");
            } else {
                for (idx, stmt) in block.stmts.iter().enumerate() {
                    dump!(d, "<block stmt {}>", idx);
                    d.indent(|d| {
                        d.dump_stmt(stmt);
                    });
                }
            }

            if let Some(ref expr) = block.expr {
                dump!(d, "<block result>");
                d.indent(|d| {
                    d.dump_expr(expr);
                });
            }
        });

        dump!(self, "block end");
    }

    fn dump_expr_if(&mut self, expr: &ExprIfType) {
        dump!(self, "if @ {} {}", expr.span, expr.id);

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
        dump!(self, "as @ {} {}", expr.span, expr.id);
        self.indent(|d| d.dump_type(&expr.data_type));
    }

    fn dump_expr_is(&mut self, expr: &ExprIsType) {
        self.indent(|d| d.dump_expr(&expr.value));
        dump!(self, "is @ {} {}", expr.span, expr.id);
    }

    fn dump_expr_self(&mut self, selfie: &ExprSelfType) {
        dump!(self, "self @ {} {}", selfie.span, selfie.id);
    }

    fn dump_expr_lit_char(&mut self, lit: &ExprLitCharType) {
        dump!(self, "lit char {} @ {} {}", lit.value, lit.span, lit.id);
    }

    fn dump_expr_lit_int(&mut self, lit: &ExprLitIntType) {
        dump!(self, "lit int {} @ {} {}", lit.value, lit.span, lit.id);
    }

    fn dump_expr_lit_float(&mut self, lit: &ExprLitFloatType) {
        dump!(self, "lit float {} @ {} {}", lit.value, lit.span, lit.id);
    }

    fn dump_expr_lit_str(&mut self, lit: &ExprLitStrType) {
        dump!(self, "lit string {:?} @ {} {}", lit.value, lit.span, lit.id);
    }

    fn dump_expr_template(&mut self, tmpl: &ExprTemplateType) {
        dump!(self, "template @ {} {}", tmpl.span, tmpl.id);
        self.indent(|d| {
            for part in &tmpl.parts {
                d.dump_expr(part)
            }
        });
    }

    fn dump_expr_lit_bool(&mut self, lit: &ExprLitBoolType) {
        dump!(self, "lit bool {} @ {} {}", lit.value, lit.span, lit.id);
    }

    fn dump_expr_ident(&mut self, ident: &ExprIdentType) {
        dump!(self, "ident {} @ {} {}", ident.name, ident.span, ident.id);
    }

    fn dump_expr_un(&mut self, expr: &ExprUnType) {
        dump!(self, "unary {:?} @ {} {}", expr.op, expr.span, expr.id);
        self.indent(|d| d.dump_expr(&expr.opnd));
    }

    fn dump_expr_bin(&mut self, expr: &ExprBinType) {
        self.indent(|d| d.dump_expr(&expr.rhs));
        dump!(self, "binary {:?} @ {} {}", expr.op, expr.span, expr.id);
        self.indent(|d| d.dump_expr(&expr.lhs));
    }

    fn dump_expr_lambda(&mut self, fct: &Arc<Function>) {
        dump!(self, "lambda @ {} {}", fct.span, fct.id);
        self.indent(|d| d.dump_fct(fct));
    }

    fn dump_expr_tuple(&mut self, expr: &ExprTupleType) {
        dump!(self, "tuple @ {} {}", expr.span, expr.id);
        self.indent(|d| {
            for expr in &expr.values {
                d.dump_expr(expr);
            }
        });
    }

    fn dump_expr_dot(&mut self, expr: &ExprDotType) {
        self.indent(|d| d.dump_expr(&expr.rhs));
        dump!(self, "dot @ {} {}", expr.span, expr.id);
        self.indent(|d| d.dump_expr(&expr.lhs));
    }

    fn dump_expr_path(&mut self, expr: &ExprPathType) {
        self.indent(|d| d.dump_expr(&expr.rhs));
        dump!(self, "path (::) @ {} {}", expr.span, expr.id);
        self.indent(|d| d.dump_expr(&expr.lhs));
    }

    fn dump_expr_call(&mut self, expr: &ExprCallType) {
        dump!(self, "call @ {} {}", expr.span, expr.id);

        self.indent(|d| {
            dump!(d, "callee");
            d.indent(|d| d.dump_expr(&expr.callee));

            for arg in &expr.args {
                if let Some(ref name) = arg.name {
                    d.dump_ident(name);
                }
                d.dump_expr(&arg.expr);
            }
        });
    }

    fn dump_expr_paren(&mut self, expr: &ExprParenType) {
        dump!(self, "paren @ {} {}", expr.span, expr.id);
        self.indent(|d| {
            d.dump_expr(&expr.expr);
        });
    }

    fn dump_expr_match(&mut self, expr: &ExprMatchType) {
        dump!(self, "match @ {} {}", expr.span, expr.id);
        self.indent(|d| {
            d.dump_expr(&expr.expr);
        });
    }

    fn dump_expr_type_param(&mut self, expr: &ExprTypeParamType) {
        dump!(self, "type param @ {} {}", expr.span, expr.id);

        self.indent(|d| {
            dump!(d, "callee");
            d.indent(|d| d.dump_expr(&expr.callee));

            for arg in &expr.args {
                d.dump_type(arg);
            }
        });
    }

    fn dump_maybe_ident(&self, ident: &Option<Ident>) {
        if let Some(ident) = ident {
            dump!(self, "ident {}", ident.name_as_string);
        } else {
            dump!(self, "missing ident");
        }
    }

    fn dump_ident(&self, ident: &Ident) {
        dump!(self, "ident {}", ident.name_as_string);
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
}
