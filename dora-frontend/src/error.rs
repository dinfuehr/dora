use dora_parser::{Span, compute_line_column};

use crate::sema::{Sema, SourceFileId};

pub mod diag;
pub mod diagnostics;
pub mod msg;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum DescriptorArg {
    String(String),
    Usize(usize),
    Location(Location),
}

impl DescriptorArg {
    pub fn to_string(&self, sa: &Sema) -> String {
        match self {
            DescriptorArg::String(s) => s.clone(),
            DescriptorArg::Usize(n) => format!("{}", n),
            DescriptorArg::Location(loc) => {
                let file = sa.file(loc.file_id);
                let (line, column) = compute_line_column(&file.line_starts, loc.span.start());
                format!("{}:{}:{}", file.path.display(), line, column)
            }
        }
    }
}

impl From<String> for DescriptorArg {
    fn from(s: String) -> Self {
        DescriptorArg::String(s)
    }
}

impl From<usize> for DescriptorArg {
    fn from(n: usize) -> Self {
        DescriptorArg::Usize(n)
    }
}

impl From<Location> for DescriptorArg {
    fn from(loc: Location) -> Self {
        DescriptorArg::Location(loc)
    }
}

pub type DescriptorArgs = Vec<DescriptorArg>;

#[macro_export]
macro_rules! args {
    () => {
        vec![]
    };
    ($($arg:expr),+ $(,)?) => {
        vec![$($crate::error::DescriptorArg::from($arg)),+]
    };
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Location {
    pub file_id: SourceFileId,
    pub span: Span,
}
