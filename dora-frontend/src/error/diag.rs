use std::cmp::Ordering;

use crate::error::msg::{ErrorDescriptor, ErrorMessage};
use crate::sema::{Sema, SourceFileId};

use dora_parser::Span;

pub struct Diagnostic {
    errors: Vec<ErrorDescriptor>,
    warnings: Vec<ErrorDescriptor>,
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

    pub fn report(&mut self, file: SourceFileId, span: Span, msg: ErrorMessage) {
        self.errors.push(ErrorDescriptor::new(file, span, msg));
    }

    pub fn warn(&mut self, file: SourceFileId, span: Span, msg: ErrorMessage) {
        self.warnings.push(ErrorDescriptor::new(file, span, msg));
    }

    pub fn report_without_location(&mut self, msg: ErrorMessage) {
        self.errors.push(ErrorDescriptor::new_without_location(msg));
    }

    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }

    pub fn has_errors_or_warnings(&self) -> bool {
        !self.errors.is_empty() || !self.warnings.is_empty()
    }

    pub fn dump(&mut self, sa: &Sema) {
        self.sort();

        for err in &self.errors {
            eprintln!("{}", message_for_error(err, "error", sa));
        }

        for err in &self.warnings {
            eprintln!("{}", message_for_error(err, "warning", sa));
        }

        let no_errors = self.errors.len();

        if no_errors == 1 {
            eprintln!("{} error found.", no_errors);
        } else {
            eprintln!("{} errors found.", no_errors);
        }
    }

    pub fn sort(&mut self) {
        let criteria = |el1: &ErrorDescriptor, el2: &ErrorDescriptor| {
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
        };

        self.errors.sort_by(criteria);
        self.warnings.sort_by(criteria);
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
            err.msg.message()
        )
    } else {
        assert!(err.span.is_none());
        format!("{}: {}", kind, err.msg.message())
    }
}
