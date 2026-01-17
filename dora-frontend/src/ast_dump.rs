use dora_parser::GreenId;
use dora_parser::ast::*;
use dora_parser::{Span, TokenKind, compute_line_column};

use crate::sema::{AnalysisData, FctDefinition, Sema, SourceFile};

macro_rules! dump {
    ($self_:ident, $($message:tt)*) => {{
        for _ in 0..($self_.indent*2) {
            print!(" ");
        }

        println!($($message)*);
    }};
}

pub fn dump_file(file: &SourceFile) {
    let mut dumper = AstDumper {
        indent: 0,
        file,
        analysis: None,
    };
    dumper.dump_file();
}

pub fn dump_node(file: &SourceFile, node: SyntaxNode) {
    let mut dumper = AstDumper {
        indent: 0,
        file,
        analysis: None,
    };
    dumper.dump_node(node);
}

pub fn dump_function(sa: &Sema, file: &SourceFile, fct: &FctDefinition) {
    let mut dumper = AstDumper {
        indent: 0,
        file,
        analysis: Some(fct.analysis()),
    };
    dumper.dump_node(fct.ast(sa).unwrap());
}

struct AstDumper<'a> {
    indent: u32,
    file: &'a SourceFile,
    analysis: Option<&'a AnalysisData>,
}

impl<'a> AstDumper<'a> {
    fn dump_file(&mut self) {
        self.dump_node(self.file.ast().root());
    }

    fn format_span(&self, span: Span) -> String {
        let (start_line, start_col) = compute_line_column(&self.file.line_starts, span.start());
        let (end_line, end_col) = compute_line_column(&self.file.line_starts, span.end());

        format!(
            "L{}:{}({})-L{}:{}({})",
            start_line,
            start_col,
            span.start(),
            end_line,
            end_col,
            span.end()
        )
    }

    fn node_extra_info(&self, node: &SyntaxNode) -> Option<String> {
        match node.syntax_kind() {
            TokenKind::LIT_CHAR_EXPR => Some(node.as_lit_char_expr().token_as_string()),
            TokenKind::LIT_INT_EXPR => Some(node.as_lit_int_expr().token_as_string()),
            TokenKind::LIT_FLOAT_EXPR => Some(node.as_lit_float_expr().token_as_string()),
            TokenKind::LIT_BOOL_EXPR => Some(node.as_lit_bool_expr().value().to_string()),
            TokenKind::BIN_EXPR => Some(node.as_bin_expr().op().as_str().to_string()),
            TokenKind::UN_EXPR => Some(node.as_un_expr().op().as_str().to_string()),
            _ => None,
        }
    }

    fn dump_token(&mut self, token: SyntaxToken) {
        if token.syntax_kind() == TokenKind::STRING_LITERAL {
            dump!(self, "{} t {}", token.syntax_kind(), token.text());
        } else {
            dump!(self, "{} t {:?}", token.syntax_kind(), token.text());
        }
    }

    fn dump_node(&mut self, node: SyntaxNode) {
        let kind = node.syntax_kind();
        let span = self.format_span(node.span());
        let id_str = node.id();

        if let Some(extra) = self.node_extra_info(&node) {
            dump!(self, "{} {} #{} {}", kind, extra, id_str, span);
        } else {
            dump!(self, "{} #{} {}", kind, id_str, span);
        }

        if let Some(analysis) = self.analysis {
            self.dump_analysis_info(node.id(), analysis);
        }

        self.indent(|d| {
            for element in node.children_with_tokens() {
                match element {
                    SyntaxElement::Token(token) => {
                        d.dump_token(token);
                    }

                    SyntaxElement::Node(node) => {
                        d.dump_node(node);
                    }
                }
            }
        });
    }

    fn dump_analysis_info(&mut self, id: GreenId, analysis: &AnalysisData) {
        let mut values = Vec::new();

        if let Some(value) = analysis.get_call_type(id) {
            values.push(format!("call={:?}", value));
        }

        if let Some(value) = analysis.get_ident(id).as_ref() {
            values.push(format!("ident={:?}", value));
        }

        if let Some(value) = analysis.ty_opt(id) {
            values.push(format!("ty={:?}", value));
        }

        if let Some(value) = analysis.get_var_id(id) {
            values.push(format!("var={:?}", value));
        }

        if let Some(value) = analysis.get_const_value(id) {
            values.push(format!("const={:?}", value));
        }

        if let Some(value) = analysis.get_for_type_info(id) {
            values.push(format!("for={:?}", value));
        }

        if let Some(value) = analysis.get_lambda(id) {
            values.push(format!("lambda={:?}", value));
        }

        if let Some(value) = analysis.get_template(id) {
            values.push(format!("template={:?}", value));
        }

        if let Some(value) = analysis.get_block_context(id) {
            values.push(format!("block_context={:?}", value));
        }

        if let Some(value) = analysis.get_argument(id) {
            values.push(format!("argument={}", value));
        }

        if let Some(value) = analysis.get_field_id(id) {
            values.push(format!("field_id={}", value));
        }

        if let Some(value) = analysis.get_array_assignment(id) {
            values.push(format!("array_assignment={:?}", value));
        }

        if !values.is_empty() {
            dump!(self, "  ⮡ {}", values.join(", "));
        }
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
