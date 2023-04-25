use std::cmp::Ordering;

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

    pub fn dump(&mut self, sa: &SemAnalysis) {
        self.sort();

        for err in &self.errors {
            eprintln!("{}", &err.message(sa));
        }
    }

    fn sort(&mut self) {
        self.errors.sort_by(|el1, el2| {
            if el1.file.is_none() {
                return Ordering::Less;
            }

            if el2.file.is_none() {
                return Ordering::Greater;
            }

            let el1_file = el1.file.expect("missing location");
            let el1_span = el1.span.expect("missing span");

            let el2_file = el2.file.expect("missing location");
            let el2_span = el2.span.expect("missing span");

            let result = el1_file.cmp(&el2_file);

            if result.is_eq() {
                el1_span.start().cmp(&el2_span.start())
            } else {
                result
            }
        });
    }
}
