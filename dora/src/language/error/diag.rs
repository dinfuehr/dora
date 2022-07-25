use crate::language::error::msg::{ErrorDescriptor, ErrorMessage};
use crate::language::sem_analysis::{SemAnalysis, SourceFileId};

use dora_parser::lexer::position::Position;

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

    pub fn report(&mut self, file: SourceFileId, pos: Position, msg: ErrorMessage) {
        self.errors.push(ErrorDescriptor::new(file, pos, msg));
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
