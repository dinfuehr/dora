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

pub fn dump_node(f: &Arc<File>, id: AstId) {
    let mut dumper = AstDumper { indent: 0, f };
    dumper.dump_node_id(id);
}

struct AstDumper<'a> {
    indent: u32,
    f: &'a File,
}

impl<'a> AstDumper<'a> {
    fn dump_file(&mut self) {
        for &element_id in &self.f.elements {
            self.dump_node_id(element_id);
        }
    }

    fn dump_node_id(&mut self, id: AstId) {
        self.dump_node(id, self.f.node(id));
    }

    fn dump_node(&mut self, id: AstId, el: &Ast) {
        match *el {
            Ast::Function(ref node) => self.dump_fct(id, node),
            Ast::Class(ref node) => self.dump_class(id, node),
            Ast::Struct(ref node) => self.dump_struct(id, node),
            Ast::WhereClause(ref node) => self.dump_where(id, node),
            Ast::WhereClauseItem(ref node) => self.dump_where_clause(id, node),
            Ast::Field(ref node) => self.dump_field(id, node),
            Ast::Trait(ref node) => self.dump_trait(id, node),
            Ast::Impl(ref node) => self.dump_impl(id, node),
            Ast::Global(ref node) => self.dump_global(id, node),
            Ast::Const(ref node) => self.dump_const(id, node),
            Ast::Enum(ref node) => self.dump_enum(id, node),
            Ast::Module(ref node) => self.dump_module(id, node),
            Ast::Use(ref node) => self.dump_use(id, node),
            Ast::UsePath(ref node) => self.dump_use_path(id, node),
            Ast::UseGroup(ref node) => self.dump_use_group(id, node),
            Ast::UseTargetName(ref node) => self.dump_use_target_name(id, node),
            Ast::Extern(ref node) => self.dump_extern(id, node),
            Ast::Alias(ref node) => self.dump_associated_type(id, node),
            Ast::Argument(ref node) => self.dump_argument(id, node),
            Ast::Param(ref node) => self.dump_param(id, node),
            Ast::RegularType(ref node) => self.dump_regular_type(id, node),
            Ast::TupleType(ref node) => self.dump_tuple_type(id, node),
            Ast::LambdaType(ref node) => self.dump_lambda_type(id, node),
            Ast::QualifiedPathType(ref node) => self.dump_qualified_path_type(id, node),
            Ast::ExprStmt(ref expr) => self.dump_stmt_expr(id, expr),
            Ast::LetStmt(ref stmt) => self.dump_stmt_let(id, stmt),
            Ast::Un(ref un) => self.dump_expr_un(id, un),
            Ast::Bin(ref bin) => self.dump_expr_bin(id, bin),
            Ast::Dot(ref field) => self.dump_expr_dot(id, field),
            Ast::LitChar(ref lit) => self.dump_expr_lit_char(id, lit),
            Ast::LitInt(ref lit) => self.dump_expr_lit_int(id, lit),
            Ast::LitFloat(ref lit) => self.dump_expr_lit_float(id, lit),
            Ast::LitStr(ref lit) => self.dump_expr_lit_str(id, lit),
            Ast::Template(ref tmpl) => self.dump_expr_template(id, tmpl),
            Ast::LitBool(ref lit) => self.dump_expr_lit_bool(id, lit),
            Ast::Ident(ref ident) => self.dump_ident(id, ident),
            Ast::Call(ref call) => self.dump_expr_call(id, call),
            Ast::TypeParam(ref expr) => self.dump_expr_type_param(id, expr),
            Ast::Path(ref path) => self.dump_expr_path(id, path),
            Ast::PathData(ref node) => self.dump_path_data(id, node),
            Ast::This(ref selfie) => self.dump_expr_self(id, selfie),
            Ast::UpcaseThis(ref selfie) => self.dump_upcase_self(id, selfie),
            Ast::Conv(ref expr) => self.dump_expr_conv(id, expr),
            Ast::Is(ref expr) => self.dump_expr_is(id, expr),
            Ast::Lambda(ref expr) => self.dump_expr_lambda(id, expr),
            Ast::Block(ref expr) => self.dump_expr_block(id, expr),
            Ast::If(ref expr) => self.dump_expr_if(id, expr),
            Ast::Tuple(ref expr) => self.dump_expr_tuple(id, expr),
            Ast::Paren(ref expr) => self.dump_expr_paren(id, expr),
            Ast::Match(ref expr) => self.dump_expr_match(id, expr),
            Ast::MatchArm(ref node) => self.dump_expr_match_arm(id, node),
            Ast::For(ref expr) => self.dump_expr_for(id, expr),
            Ast::While(ref expr) => self.dump_expr_while(id, expr),
            Ast::Break(ref expr) => self.dump_expr_break(id, expr),
            Ast::Continue(ref expr) => self.dump_expr_continue(id, expr),
            Ast::Return(ref ret) => self.dump_expr_return(id, ret),
            Ast::TypeArgument(ref node) => self.dump_type_argument(id, node),
            Ast::Underscore(ref node) => self.dump_underscore(id, node),
            Ast::LitPattern(ref node) => self.dump_lit_pattern(id, node),
            Ast::IdentPattern(ref node) => self.dump_ident_pattern(id, node),
            Ast::TuplePattern(ref node) => self.dump_tuple_pattern(id, node),
            Ast::ConstructorPattern(ref node) => self.dump_constructor_pattern(id, node),
            Ast::ConstructorField(ref node) => self.dump_constructor_field(id, node),
            Ast::Rest(ref node) => self.dump_rest(id, node),
            Ast::Alt(ref node) => self.dump_alt(id, node),
            Ast::Error(ref node) => {
                dump!(self, "error @ {} {}", node.span, node.id);
            }
        }
    }

    fn dump_global(&mut self, id: AstId, global: &Global) {
        dump!(self, "global @ {} {} {:?}", global.span, global.id, id);
        self.dump_maybe_ident(global.name);

        self.indent(|d| {
            d.dump_node_id(global.data_type);

            if let Some(initial_value) = global.initial_value {
                d.dump_node_id(initial_value);
            } else {
                dump!(d, "<no expr given>");
            }
        });
    }

    fn dump_extern(&mut self, id: AstId, stmt: &ExternPackage) {
        dump!(self, "extern package @ {} {} {:?}", stmt.span, stmt.id, id);
        self.dump_maybe_ident(stmt.name);
        self.dump_maybe_ident(stmt.identifier);
    }

    fn dump_const(&mut self, id: AstId, const_: &Const) {
        dump!(self, "const @ {} {} {:?}", const_.span, const_.id, id);
        self.dump_maybe_ident(const_.name);

        self.indent(|d| {
            d.dump_node_id(const_.data_type);
            d.dump_node_id(const_.expr);
        });
    }

    fn dump_use(&mut self, id: AstId, node: &Use) {
        dump!(self, "use @ {} {} {:?}", node.span, node.id, id);
        self.indent(|d| {
            d.dump_node_id(node.path);
        })
    }

    fn dump_use_path(&mut self, id: AstId, node: &UsePath) {
        dump!(self, "use path @ {} {} {:?}", node.span, node.id, id);
        self.indent(|d| match node.target {
            UsePathDescriptor::Default => dump!(d, "default"),
            UsePathDescriptor::Error => dump!(d, "error"),
            UsePathDescriptor::As(use_target_name_id) => d.dump_node_id(use_target_name_id),
            UsePathDescriptor::Group(use_group_id) => {
                d.dump_node_id(use_group_id);
            }
        });
    }

    fn dump_use_group(&mut self, id: AstId, node: &UseGroup) {
        dump!(self, "use group @ {} {} {:?}", node.span, node.id, id);
        self.indent(|d| {
            for &target in &node.targets {
                d.dump_node_id(target);
            }
        })
    }

    fn dump_use_target_name(&mut self, id: AstId, node: &UseTargetName) {
        dump!(self, "use target name @ {} {} {:?}", node.span, node.id, id);
        self.indent(|d| {
            if let Some(id) = node.name {
                d.dump_node_id(id);
            }
        })
    }

    fn dump_module(&mut self, id: AstId, module: &Module) {
        dump!(self, "module @ {} {} {:?}", module.span, module.id, id);
        self.dump_maybe_ident(module.name);

        self.indent(|d| {
            if let Some(ref elements) = module.elements {
                for &element_id in elements {
                    d.dump_node_id(element_id);
                }
            }
        });
    }

    fn dump_enum(&mut self, id: AstId, enum_: &Enum) {
        dump!(self, "enum @ {} {} {:?}", enum_.span, enum_.id, id);
        self.dump_maybe_ident(enum_.name);

        self.indent(|d| {
            for value in &enum_.variants {
                d.dump_enum_value(value);
            }
        });
    }

    fn dump_enum_value(&mut self, value: &EnumVariant) {
        dump!(self, "enum variant @ {} {}", value.span, value.id);
        self.dump_maybe_ident(value.name);

        self.indent(|d| {
            for &field_id in &value.fields {
                d.dump_node_id(field_id);
            }
        });
    }

    fn dump_impl(&mut self, id: AstId, impl_: &Impl) {
        dump!(self, "impl @ {} {} {:?}", impl_.span, impl_.id, id);

        self.indent(|d| {
            if let Some(trait_type) = impl_.trait_type {
                d.dump_node_id(trait_type);
                dump!(d, "for");
            }

            d.dump_node_id(impl_.extended_type);

            for &element_id in &impl_.methods {
                d.dump_node_id(element_id);
            }
        });
    }

    fn dump_struct(&mut self, id: AstId, node: &Struct) {
        dump!(self, "struct @ {} {} {:?}", node.span, node.id, id);
        self.dump_maybe_ident(node.name);

        self.indent(|d| {
            for &field_id in &node.fields {
                d.dump_node_id(field_id);
            }
        });
    }

    fn dump_where(&mut self, id: AstId, node: &WhereClause) {
        dump!(self, "where @ {} {} {:?}", node.span, node.id, id);

        self.indent(|d| {
            for &clause_id in &node.clauses {
                d.dump_node_id(clause_id);
            }
        });
    }

    fn dump_where_clause(&mut self, id: AstId, node: &WhereClauseItem) {
        dump!(self, "where clause @ {} {} {:?}", node.span, node.id, id);

        self.indent(|d| {
            d.dump_node_id(node.ty);
            for &clause_id in &node.bounds {
                d.dump_node_id(clause_id);
            }
        });
    }

    fn dump_field(&mut self, id: AstId, field: &Field) {
        dump!(self, "field @ {} {} {:?}", field.span, field.id, id);
        self.dump_maybe_ident(field.name);
        self.indent(|d| d.dump_node_id(field.data_type));
    }

    fn dump_trait(&mut self, id: AstId, node: &Trait) {
        dump!(self, "trait @ {} {} {:?}", node.span, node.id, id);
        self.indent(|d| {
            d.dump_maybe_ident(node.name);
            for &element_id in &node.methods {
                d.dump_node_id(element_id);
            }
        });
    }

    fn dump_associated_type(&mut self, id: AstId, node: &Alias) {
        dump!(self, "trait @ {} {} {:?}", node.span, node.id, id);
        self.indent(|d| {
            d.dump_maybe_ident(node.name);
        });
    }

    fn dump_class(&mut self, id: AstId, cls: &Class) {
        dump!(self, "class @ {} {} {:?}", cls.span, cls.id, id);
        self.dump_maybe_ident(cls.name);

        self.indent(|d| {
            dump!(d, "fields");

            d.indent(|d| {
                for &field_id in &cls.fields {
                    d.dump_node_id(field_id);
                }
            });
        });
    }

    fn dump_fct(&mut self, id: AstId, fct: &Function) {
        dump!(self, "fct @ {} {} {:?}", fct.span, fct.id, id);
        self.dump_maybe_ident(fct.name);

        self.indent(|d| {
            dump!(d, "params");
            d.indent(|d| {
                if fct.params.is_empty() {
                    dump!(d, "no params");
                } else {
                    for &param_id in &fct.params {
                        d.dump_node_id(param_id);
                    }
                }
            });

            dump!(d, "returns");

            if let Some(ty) = fct.return_type {
                d.indent(|d| d.dump_node_id(ty));
            } else {
                d.indent(|d| dump!(d, "<no return type>"))
            }

            dump!(d, "executes");

            if let Some(block) = fct.block {
                d.indent(|d| d.dump_node_id(block));
            }
        });
    }

    fn dump_param(&mut self, id: AstId, param: &Param) {
        dump!(self, "param @ {} {} {:?}", param.span, param.id, id);
        self.dump_node_id(param.data_type);
    }

    fn dump_regular_type(&mut self, id: AstId, node: &TypeRegularType) {
        dump!(self, "regular type @ {} {} {:?}", node.span, node.id, id);

        self.indent(|d| {
            d.dump_node_id(node.path);

            for &arg_id in &node.params {
                d.dump_node_id(arg_id);
            }
        });
    }

    fn dump_type_argument(&mut self, id: AstId, node: &TypeArgument) {
        dump!(self, "argument type @ {} {} {:?}", node.span, node.id, id);

        self.indent(|d| {
            d.dump_maybe_ident(node.name);
            d.dump_node_id(node.ty);
        });
    }

    fn dump_tuple_type(&mut self, id: AstId, ty: &TypeTupleType) {
        dump!(self, "tuple type @ {} {} {:?}", ty.span, ty.id, id);
    }

    fn dump_lambda_type(&mut self, id: AstId, ty: &TypeLambdaType) {
        dump!(self, "lmabda type @ {} {} {:?}", ty.span, ty.id, id);
    }

    fn dump_qualified_path_type(&mut self, id: AstId, ty: &TypeQualifiedPathType) {
        dump!(self, "qualified path type @ {} {} {:?}", ty.span, ty.id, id);
    }

    fn dump_stmt_let(&mut self, id: AstId, stmt: &StmtLetType) {
        dump!(self, "let @ {} {} {:?}", stmt.span, stmt.id, id);

        self.indent(|d| {
            dump!(d, "type");
            d.indent(|d| {
                if let Some(ty) = stmt.data_type {
                    d.dump_node_id(ty);
                } else {
                    dump!(d, "<no type given>");
                }
            });

            dump!(d, "expr");
            d.indent(|d| {
                if let Some(expr) = stmt.expr {
                    d.dump_node_id(expr);
                } else {
                    dump!(d, "<no expr given>");
                }
            });
        });
    }

    fn dump_expr_for(&mut self, id: AstId, stmt: &ExprForType) {
        dump!(self, "for @ {} {} {:?}", stmt.span, stmt.id, id);

        self.indent(|d| {
            dump!(d, "cond");
            d.indent(|d| {
                d.dump_node_id(stmt.expr);
            });
            dump!(d, "body");
            d.indent(|d| {
                d.dump_node_id(stmt.block);
            });
        });
    }

    fn dump_expr_while(&mut self, id: AstId, expr: &ExprWhileType) {
        dump!(self, "while @ {} {} {:?}", expr.span, expr.id, id);

        self.indent(|d| {
            dump!(d, "cond");
            d.indent(|d| {
                d.dump_node_id(expr.cond);
            });

            dump!(d, "body");
            d.indent(|d| {
                d.dump_node_id(expr.block);
            });
        });
    }

    fn dump_stmt_expr(&mut self, id: AstId, stmt: &StmtExprType) {
        dump!(self, "expr stmt @ {} {} {:?}", stmt.span, stmt.id, id);
        self.indent(|d| {
            d.dump_node_id(stmt.expr);
        });
    }

    fn dump_expr_return(&mut self, id: AstId, ret: &ExprReturnType) {
        dump!(self, "return @ {} {} {:?}", ret.span, ret.id, id);

        self.indent(|d| {
            if let Some(expr) = ret.expr {
                d.dump_node_id(expr);
            } else {
                dump!(d, "<nothing>");
            }
        });
    }

    fn dump_expr_break(&mut self, id: AstId, stmt: &ExprBreakType) {
        dump!(self, "break @ {} {} {:?}", stmt.span, stmt.id, id);
    }

    fn dump_expr_continue(&mut self, id: AstId, stmt: &ExprContinueType) {
        dump!(self, "continue @ {} {} {:?}", stmt.span, stmt.id, id);
    }

    fn dump_expr_block(&mut self, id: AstId, block: &ExprBlockType) {
        dump!(
            self,
            "block ({} statement(s)) @ {} {} {:?}",
            block.stmts.len(),
            block.span,
            block.id,
            id
        );

        self.indent(|d| {
            if block.stmts.is_empty() {
                dump!(d, "<no statements>");
            } else {
                for (idx, &stmt_id) in block.stmts.iter().enumerate() {
                    dump!(d, "<block stmt {}>", idx);
                    d.indent(|d| {
                        d.dump_node_id(stmt_id);
                    });
                }
            }

            if let Some(expr) = block.expr {
                dump!(d, "<block result>");
                d.indent(|d| {
                    d.dump_node_id(expr);
                });
            }
        });

        dump!(self, "block end");
    }

    fn dump_expr_if(&mut self, id: AstId, expr: &ExprIfType) {
        dump!(self, "if @ {} {} {:?}", expr.span, expr.id, id);

        self.indent(|d| {
            d.indent(|d| {
                d.dump_node_id(expr.cond);
            });
            dump!(d, "then");
            d.indent(|d| {
                d.dump_node_id(expr.then_block);
            });
            if let Some(else_block) = expr.else_block {
                dump!(d, "else");
                d.indent(|d| {
                    d.dump_node_id(else_block);
                });
            } else {
                dump!(d, "no else");
            }
        });
    }

    fn dump_expr_conv(&mut self, id: AstId, expr: &ExprConvType) {
        self.indent(|d| d.dump_node_id(expr.object));
        dump!(self, "as @ {} {} {:?}", expr.span, expr.id, id);
        self.indent(|d| {
            d.dump_node_id(expr.data_type);
        });
    }

    fn dump_expr_is(&mut self, id: AstId, expr: &ExprIsType) {
        self.indent(|d| d.dump_node_id(expr.value));
        dump!(self, "is @ {} {} {:?}", expr.span, expr.id, id);
    }

    fn dump_expr_self(&mut self, id: AstId, selfie: &ExprSelfType) {
        dump!(self, "self @ {} {} {:?}", selfie.span, selfie.id, id);
    }

    fn dump_upcase_self(&mut self, id: AstId, selfie: &UpcaseThis) {
        dump!(self, "Self @ {} {} {:?}", selfie.span, selfie.id, id);
    }

    fn dump_expr_lit_char(&mut self, id: AstId, lit: &ExprLitCharType) {
        dump!(
            self,
            "lit char {} @ {} {} {:?}",
            lit.value,
            lit.span,
            lit.id,
            id
        );
    }

    fn dump_expr_lit_int(&mut self, id: AstId, lit: &ExprLitIntType) {
        dump!(
            self,
            "lit int {} @ {} {} {:?}",
            lit.value,
            lit.span,
            lit.id,
            id
        );
    }

    fn dump_expr_lit_float(&mut self, id: AstId, lit: &ExprLitFloatType) {
        dump!(
            self,
            "lit float {} @ {} {} {:?}",
            lit.value,
            lit.span,
            lit.id,
            id
        );
    }

    fn dump_expr_lit_str(&mut self, id: AstId, lit: &ExprLitStrType) {
        dump!(
            self,
            "lit string {:?} @ {} {} {:?}",
            lit.value,
            lit.span,
            lit.id,
            id
        );
    }

    fn dump_expr_template(&mut self, id: AstId, tmpl: &ExprTemplateType) {
        dump!(self, "template @ {} {} {:?}", tmpl.span, tmpl.id, id);
        self.indent(|d| {
            for &part in &tmpl.parts {
                d.dump_node_id(part)
            }
        });
    }

    fn dump_expr_lit_bool(&mut self, id: AstId, lit: &ExprLitBoolType) {
        dump!(
            self,
            "lit bool {} @ {} {} {:?}",
            lit.value,
            lit.span,
            lit.id,
            id
        );
    }

    fn dump_ident(&mut self, id: AstId, ident: &Ident) {
        dump!(
            self,
            "ident {} @ {} {} {:?}",
            ident.name,
            ident.span,
            ident.id,
            id
        );
    }

    fn dump_expr_un(&mut self, id: AstId, expr: &ExprUnType) {
        dump!(
            self,
            "unary {:?} @ {} {} {:?}",
            expr.op,
            expr.span,
            expr.id,
            id
        );
        self.indent(|d| d.dump_node_id(expr.opnd));
    }

    fn dump_expr_bin(&mut self, id: AstId, expr: &ExprBinType) {
        self.indent(|d| d.dump_node_id(expr.rhs));
        dump!(
            self,
            "binary {:?} @ {} {} {:?}",
            expr.op,
            expr.span,
            expr.id,
            id
        );
        self.indent(|d| d.dump_node_id(expr.lhs));
    }

    fn dump_expr_lambda(&mut self, id: AstId, node: &ExprLambdaType) {
        dump!(self, "lambda @ {} {} {:?}", node.span, node.id, id);
        let fct = self
            .f
            .node(node.fct_id)
            .to_function()
            .expect("fct expected");
        self.indent(|d| d.dump_fct(node.fct_id, fct));
    }

    fn dump_expr_tuple(&mut self, id: AstId, expr: &ExprTupleType) {
        dump!(self, "tuple expr @ {} {} {:?}", expr.span, expr.id, id);
        self.indent(|d| {
            for &expr in &expr.values {
                d.dump_node_id(expr);
            }
        });
    }

    fn dump_expr_dot(&mut self, id: AstId, expr: &ExprDotType) {
        self.indent(|d| d.dump_node_id(expr.rhs));
        dump!(self, "dot @ {} {} {:?}", expr.span, expr.id, id);
        self.indent(|d| d.dump_node_id(expr.lhs));
    }

    fn dump_expr_path(&mut self, id: AstId, expr: &ExprPathType) {
        self.indent(|d| d.dump_node_id(expr.rhs));
        dump!(self, "path (::) @ {} {} {:?}", expr.span, expr.id, id);
        self.indent(|d| d.dump_node_id(expr.lhs));
    }

    fn dump_path_data(&mut self, id: AstId, node: &PathData) {
        dump!(self, "path data @ {} {} {:?}", node.span, node.id, id);
        self.indent(|d| {
            for &segment_id in &node.segments {
                d.dump_node_id(segment_id);
            }
        });
    }

    fn dump_expr_call(&mut self, id: AstId, expr: &ExprCallType) {
        dump!(self, "call @ {} {} {:?}", expr.span, expr.id, id);

        self.indent(|d| {
            dump!(d, "callee");
            d.indent(|d| d.dump_node_id(expr.callee));

            for &arg_id in &expr.args {
                d.dump_node_id(arg_id);
            }
        });
    }

    fn dump_argument(&mut self, id: AstId, node: &Argument) {
        dump!(self, "argument @ {} {} {:?}", node.span, node.id, id);
        self.dump_maybe_ident(node.name);
        self.dump_node_id(node.expr);
    }

    fn dump_expr_paren(&mut self, id: AstId, expr: &ExprParenType) {
        dump!(self, "paren @ {} {} {:?}", expr.span, expr.id, id);
        self.indent(|d| {
            d.dump_node_id(expr.expr);
        });
    }

    fn dump_expr_match(&mut self, id: AstId, expr: &ExprMatchType) {
        dump!(self, "match @ {} {} {:?}", expr.span, expr.id, id);
        self.indent(|d| {
            d.dump_node_id(expr.expr);

            for &arm_id in &expr.arms {
                d.dump_node_id(arm_id);
            }
        });
    }

    fn dump_expr_match_arm(&mut self, id: AstId, node: &MatchArmType) {
        dump!(self, "match arm @ {} {} {:?}", node.span, node.id, id);
        self.indent(|d| {
            if let Some(cond_id) = node.cond {
                d.dump_node_id(cond_id);
            }

            d.dump_node_id(node.value);
        });
    }

    fn dump_expr_type_param(&mut self, id: AstId, expr: &ExprTypeParamType) {
        dump!(self, "type param @ {} {} {:?}", expr.span, expr.id, id);

        self.indent(|d| {
            dump!(d, "callee");
            d.indent(|d| d.dump_node_id(expr.callee));

            for &arg_id in &expr.args {
                d.dump_node_id(arg_id);
            }
        });
    }

    fn dump_maybe_ident(&mut self, ident: Option<AstId>) {
        if let Some(ident_id) = ident {
            self.dump_node_id(ident_id);
        } else {
            dump!(self, "missing ident");
        }
    }

    fn dump_underscore(&mut self, id: AstId, node: &PatternUnderscore) {
        dump!(self, "underscore @ {} {} {:?}", node.span, node.id, id);
    }

    fn dump_lit_pattern(&mut self, id: AstId, node: &PatternLit) {
        dump!(self, "PatternLit @ {} {} {:?}", node.span, node.id, id);
        self.dump_node_id(node.expr);
    }

    fn dump_ident_pattern(&mut self, id: AstId, node: &PatternIdent) {
        dump!(self, "PatternIdent @ {} {} {:?}", node.span, node.id, id);
        self.dump_node_id(node.name);
    }

    fn dump_tuple_pattern(&mut self, id: AstId, node: &PatternTuple) {
        dump!(self, "PatternTuple @ {} {} {:?}", node.span, node.id, id);
    }

    fn dump_constructor_pattern(&mut self, id: AstId, node: &PatternConstructor) {
        dump!(
            self,
            "PatternConstructor @ {} {} {:?}",
            node.span,
            node.id,
            id
        );
    }

    fn dump_constructor_field(&mut self, id: AstId, node: &PatternField) {
        dump!(self, "PatternField @ {} {} {:?}", node.span, node.id, id);
    }

    fn dump_rest(&mut self, id: AstId, node: &PatternRest) {
        dump!(self, "rest @ {} {} {:?}", node.span, node.id, id);
    }

    fn dump_alt(&mut self, id: AstId, node: &PatternAlt) {
        dump!(self, "PatternAlt @ {} {} {:?}", node.span, node.id, id);
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
