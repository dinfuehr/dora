use std::cmp::Ordering;
use std::fmt::Write;
use std::io::{self, Write as IoWrite};

use crate::error::DescriptorArgs;
use crate::error::diagnostics::DiagnosticDescriptor;
use crate::error::msg::ErrorDescriptor;
use crate::sema::{Sema, SourceFileId};

use dora_parser::Span;

struct StderrWriter;

impl Write for StderrWriter {
    fn write_str(&mut self, s: &str) -> std::fmt::Result {
        io::stderr()
            .write_all(s.as_bytes())
            .map_err(|_| std::fmt::Error)
    }
}

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
        self.dump_to_stderr(sa, report_all_warnings);
    }

    pub fn dump_to_stderr(&mut self, sa: &Sema, report_all_warnings: bool) {
        self.dump_to_write(&mut StderrWriter, sa, report_all_warnings);
    }

    pub fn dump_to_string(&mut self, sa: &Sema, report_all_warnings: bool) -> String {
        let mut output = String::new();
        self.dump_to_write(&mut output, sa, report_all_warnings);
        output
    }

    pub fn dump_to_write(&mut self, w: &mut dyn Write, sa: &Sema, report_all_warnings: bool) {
        self.sort();

        for err in &self.errors {
            write!(w, "{}", message_for_error(err, "error", sa)).unwrap();
        }

        for err in &self.warnings {
            if let Some(file_id) = err.file_id {
                let file = sa.file(file_id);
                if !report_all_warnings && file.package_id != sa.program_package_id() {
                    // Do not report warnings by default in standard library or boots for now.
                    continue;
                }
            }
            write!(w, "{}", message_for_error(err, "warning", sa)).unwrap();
        }

        if !self.errors.is_empty() {
            let no_errors = self.errors.len();

            if no_errors == 1 {
                write!(w, "{} error found.\n", no_errors).unwrap();
            } else {
                write!(w, "{} errors found.\n", no_errors).unwrap();
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
    if let Some(file_id) = err.file_id {
        let file = sa.file(file_id);
        let span = err.span.expect("missing span");
        let (start_line, start_column) = err.line_column(sa).expect("missing location");

        let mut result = format!("{}: {}\n", kind, err.message(sa));

        let (end_line, end_column) = if span.len() > 0 {
            dora_parser::compute_line_column(&file.line_starts, span.end())
        } else {
            (start_line, start_column)
        };

        // Print location header
        if start_line == end_line {
            write!(
                result,
                "--> {}:{}:{}\n",
                file.path.display(),
                start_line,
                start_column
            )
            .unwrap();
        } else {
            write!(
                result,
                "--> {}:{}:{} - {}:{}\n",
                file.path.display(),
                start_line,
                start_column,
                end_line,
                end_column
            )
            .unwrap();
        }

        // If the span is on a single line, show context lines with underline
        if start_line == end_line {
            let total_lines = file.line_starts.len();
            let error_line_idx = start_line as usize - 1;

            // Show up to 2 lines before the error line
            let context_start = error_line_idx.saturating_sub(2);

            // Show up to 2 lines after the error line
            let context_end = (error_line_idx + 3).min(total_lines);

            for line_idx in context_start..context_end {
                let line_content =
                    dora_parser::get_line_content(&file.content, &file.line_starts, line_idx);
                let line_content = line_content.trim_end_matches(['\r', '\n']);

                result.push_str(" | ");
                result.push_str(line_content);
                result.push('\n');

                // Add underline for the error line
                if line_idx == error_line_idx {
                    result.push_str("   ");
                    for _ in 1..start_column {
                        result.push(' ');
                    }
                    if span.len() == 0 {
                        result.push('^');
                    } else {
                        for _ in 0..span.len() {
                            result.push('~');
                        }
                    }
                    result.push('\n');
                }
            }
        }

        result
    } else {
        assert!(err.span.is_none());
        format!("{}: {}\n", kind, err.message(sa))
    }
}
