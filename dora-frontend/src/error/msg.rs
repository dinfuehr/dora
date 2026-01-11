use crate::error::DescriptorArgs;
use crate::error::diagnostics::{DiagnosticDescriptor, format_message};
use crate::sema::{Sema, SourceFileId};
use dora_parser::{Span, compute_line_column};

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum ErrorLevel {
    Warn,
    Error,
}

#[derive(Clone, Debug)]
pub struct ErrorDescriptor {
    pub file_id: Option<SourceFileId>,
    pub span: Option<Span>,
    pub level: ErrorLevel,
    pub desc: &'static DiagnosticDescriptor,
    pub args: DescriptorArgs,
}

impl ErrorDescriptor {
    pub fn new(
        file: SourceFileId,
        span: Span,
        level: ErrorLevel,
        desc: &'static DiagnosticDescriptor,
        args: DescriptorArgs,
    ) -> ErrorDescriptor {
        ErrorDescriptor {
            file_id: Some(file),
            span: Some(span),
            level,
            desc,
            args,
        }
    }

    pub fn new_without_location(
        level: ErrorLevel,
        desc: &'static DiagnosticDescriptor,
        args: DescriptorArgs,
    ) -> ErrorDescriptor {
        ErrorDescriptor {
            file_id: None,
            span: None,
            level,
            desc,
            args,
        }
    }

    pub fn message(&self, sa: &Sema) -> String {
        format_message(self.desc.message, &self.args, sa)
    }

    pub fn line_column(&self, sa: &Sema) -> Option<(u32, u32)> {
        if let Some(file_id) = self.file_id {
            let file = sa.file(file_id);
            let span = self.span.expect("missing location");
            Some(compute_line_column(&file.line_starts, span.start()))
        } else {
            None
        }
    }
}
