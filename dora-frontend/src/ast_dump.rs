use dora_parser::ast::*;
use dora_parser::{Span, compute_line_column};

use crate::sema::{AnalysisData, FctDefinition, SourceFile};

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

pub fn dump_function(file: &SourceFile, fct: &FctDefinition) {
    let mut dumper = AstDumper {
        indent: 0,
        file,
        analysis: Some(fct.analysis()),
    };
    dumper.dump_node_id(fct.ast_id());
}

struct AstDumper<'a> {
    indent: u32,
    file: &'a SourceFile,
    analysis: Option<&'a AnalysisData>,
}

impl<'a> AstDumper<'a> {
    fn dump_file(&mut self) {
        self.dump_node_id(self.file.ast().root_id);
    }

    fn dump_node_id(&mut self, id: AstId) {
        self.dump_node(id, self.file.node(id));
    }

    fn format_span(&self, span: Span) -> String {
        let (start_line, start_col) = compute_line_column(&self.file.line_starts, span.start());
        let (end_line, end_col) = compute_line_column(&self.file.line_starts, span.end());
        format!("L{}:{}-L{}:{}", start_line, start_col, end_line, end_col)
    }

    fn node_extra_info(&self, el: &Ast) -> Option<String> {
        match el {
            Ast::LitChar(lit) => Some(lit.value.clone()),
            Ast::LitInt(lit) => Some(lit.value.clone()),
            Ast::LitFloat(lit) => Some(lit.value.clone()),
            Ast::LitStr(lit) => Some(format!("{:?}", lit.value)),
            Ast::LitBool(lit) => Some(lit.value.to_string()),
            Ast::Ident(ident) => Some(ident.name.clone()),
            Ast::Bin(bin) => Some(bin.op.as_str().to_string()),
            Ast::Un(un) => Some(un.op.as_str().to_string()),
            _ => None,
        }
    }

    fn dump_node(&mut self, id: AstId, el: &Ast) {
        let name = el.name();
        let span = self.format_span(el.span());
        let id_str = id.index();

        if let Some(extra) = self.node_extra_info(el) {
            dump!(self, "{} {} #{} {}", name, extra, id_str, span);
        } else {
            dump!(self, "{} #{} {}", name, id_str, span);
        }

        if let Some(analysis) = self.analysis {
            self.dump_analysis_info(id, analysis);
        }

        let children = el.children();
        if !children.is_empty() {
            self.indent(|d| {
                for &child_id in &children {
                    d.dump_node_id(child_id);
                }
            });
        }
    }

    fn dump_analysis_info(&mut self, id: AstId, analysis: &AnalysisData) {
        let mut values = Vec::new();

        if let Some(value) = analysis.map_calls.get(id) {
            values.push(format!("call={:?}", value));
        }

        if let Some(value) = analysis.map_idents.get(id) {
            values.push(format!("ident={:?}", value));
        }

        if let Some(value) = analysis.map_tys.get(id) {
            values.push(format!("ty={:?}", value));
        }

        if let Some(value) = analysis.map_vars.get(id) {
            values.push(format!("var={:?}", value));
        }

        if let Some(value) = analysis.map_consts.get(id) {
            values.push(format!("const={:?}", value));
        }

        if let Some(value) = analysis.map_fors.get(id) {
            values.push(format!("for={:?}", value));
        }

        if let Some(value) = analysis.map_lambdas.get(id) {
            values.push(format!("lambda={:?}", value));
        }

        if let Some(value) = analysis.map_templates.get(id) {
            values.push(format!("template={:?}", value));
        }

        if let Some(value) = analysis.map_block_contexts.get(id) {
            values.push(format!("block_context={:?}", value));
        }

        if let Some(value) = analysis.map_argument.get(id) {
            values.push(format!("argument={}", value));
        }

        if let Some(value) = analysis.map_field_ids.get(id) {
            values.push(format!("field_id={}", value));
        }

        if let Some(value) = analysis.map_array_assignments.get(id) {
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
