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
            DescriptorArg::Location(loc) => loc.to_string(sa),
        }
    }
}

impl From<String> for DescriptorArg {
    fn from(s: String) -> Self {
        DescriptorArg::String(s)
    }
}

impl From<&str> for DescriptorArg {
    fn from(s: &str) -> Self {
        DescriptorArg::String(s.to_string())
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

impl Location {
    pub fn to_string(&self, sa: &Sema) -> String {
        let file = sa.file(self.file_id);
        let (line, column) = compute_line_column(&file.line_starts, self.span.start());

        // Get the file name of the top level module (e.g. boots/boots.dora).
        // Then use that directory for building relative paths.
        let package = sa.package(file.package_id);
        let top_level_module = sa.module(package.top_level_module_id());
        let base_dir = top_level_module
            .file_id
            .get()
            .and_then(|fid| sa.file(*fid).path.parent());

        let path = if let Some(base) = base_dir {
            file.path.strip_prefix(base).unwrap_or(&file.path)
        } else {
            &file.path
        };
        format!("{}:{}:{}", path.display(), line, column)
    }
}
