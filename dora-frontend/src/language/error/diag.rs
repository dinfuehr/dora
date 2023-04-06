use crate::language::error::msg::{ErrorDescriptor, ErrorMessage};
use crate::language::sem_analysis::{SemAnalysis, SourceFileId};

use dora_parser::Span;

pub struct Diagnostic {
    errors: Vec<ErrorDescriptor>,
}

impl Diagnostic {
    pub fn new() -> Diagnostic {
        Diagnostic { errors: Vec::new() }
    }

    pub fn errors(&self) -> &[ErrorDescriptor] {
        &self.errors
    }

    pub fn report(&mut self, file: SourceFileId, span: Span, msg: ErrorMessage) {
        self.errors.push(ErrorDescriptor::new(file, span, msg));
    }

    pub fn report_without_location(&mut self, msg: ErrorMessage) {
        self.errors.push(ErrorDescriptor::new_without_location(msg));
    }

    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }

    pub fn dump(&self, sa: &SemAnalysis) {
        for err in &self.errors {
            eprintln!("{}", &err.message(sa));
        }
    }
}
