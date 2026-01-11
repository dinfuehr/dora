use std::cmp::Ordering;

use crate::error::DescriptorArgs;
use crate::error::diagnostics::DiagnosticDescriptor;
use crate::error::msg::ErrorDescriptor;
use crate::sema::{Sema, SourceFileId};

use dora_parser::Span;

pub struct Diagnostic {
    pub errors: Vec<ErrorDescriptor>,
    pub warnings: Vec<ErrorDescriptor>,
}

impl Diagnostic {
    pub fn new() -> Diagnostic {
        Diagnostic {
            errors: Vec::new(),
            warnings: Vec::new(),
        }
    }

    pub fn errors(&self) -> &[ErrorDescriptor] {
        &self.errors
    }

    pub fn warnings(&self) -> &[ErrorDescriptor] {
        &self.warnings
    }

    pub fn report(
        &mut self,
        file: SourceFileId,
        span: Span,
        desc: &'static DiagnosticDescriptor,
        args: DescriptorArgs,
    ) {
        self.errors.push(ErrorDescriptor::new(
            file,
            span,
            desc.level.clone(),
            desc,
            args,
        ));
    }

    pub fn warn(
        &mut self,
        file: SourceFileId,
        span: Span,
        desc: &'static DiagnosticDescriptor,
        args: DescriptorArgs,
    ) {
        use crate::error::msg::ErrorLevel;
        self.warnings.push(ErrorDescriptor::new(
            file,
            span,
            ErrorLevel::Warn,
            desc,
            args,
        ));
    }

    pub fn report_without_location(
        &mut self,
        desc: &'static DiagnosticDescriptor,
        args: DescriptorArgs,
    ) {
        self.errors.push(ErrorDescriptor::new_without_location(
            desc.level.clone(),
            desc,
            args,
        ));
    }

    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }

    pub fn has_errors_or_warnings(&self) -> bool {
        !self.errors.is_empty() || !self.warnings.is_empty()
    }

    pub fn dump(&mut self, sa: &Sema, report_all_warnings: bool) {
        self.sort();

        for err in &self.errors {
            eprintln!("{}", message_for_error(err, "error", sa));
        }

        for err in &self.warnings {
            if let Some(file_id) = err.file_id {
                let file = sa.file(file_id);
                if !report_all_warnings && file.package_id != sa.program_package_id() {
                    // Do not report warnings by default in standard library or boots for now.
                    continue;
                }
            }
            eprintln!("{}", message_for_error(err, "warning", sa));
        }

        if !self.errors.is_empty() {
            let no_errors = self.errors.len();

            if no_errors == 1 {
                eprintln!("{} error found.", no_errors);
            } else {
                eprintln!("{} errors found.", no_errors);
            }
        }
    }

    pub fn sort(&mut self) {
        self.errors.sort_by(sort_by);
        self.warnings.sort_by(sort_by);
    }
}

pub fn sort_by(el1: &ErrorDescriptor, el2: &ErrorDescriptor) -> Ordering {
    if el1.file_id.is_none() {
        return Ordering::Less;
    }

    if el2.file_id.is_none() {
        return Ordering::Greater;
    }

    let el1_file = el1.file_id.expect("missing location");
    let el1_span = el1.span.expect("missing span");

    let el2_file = el2.file_id.expect("missing location");
    let el2_span = el2.span.expect("missing span");

    let result = el1_file.cmp(&el2_file);

    if result.is_eq() {
        el1_span.start().cmp(&el2_span.start())
    } else {
        result
    }
}

pub fn message_for_error(err: &ErrorDescriptor, kind: &str, sa: &Sema) -> String {
    if let Some(file) = err.file_id {
        let file = sa.file(file);
        let (line, column) = err.line_column(sa).expect("missing location");

        format!(
            "{} in {:?} at {}:{}: {}",
            kind,
            file.path,
            line,
            column,
            err.message(sa)
        )
    } else {
        assert!(err.span.is_none());
        format!("{}: {}", kind, err.message(sa))
    }
}
