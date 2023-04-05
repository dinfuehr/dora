pub use self::lexer::position::{Position, Span};
pub use self::parser::Parser;

pub mod ast;
mod builder;
pub mod error;
pub mod interner;
pub mod lexer;
pub mod parser;

pub fn compute_line_starts(content: &str) -> Vec<u32> {
    let mut pos: u32 = 0;
    let mut line_starts = vec![0];
    for ch in content.chars() {
        if ch == '\n' {
            line_starts.push(pos + 1);
        }
        pos += 1;
    }
    line_starts
}

pub fn compute_line_column(line_starts: &[u32], offset: u32) -> (u32, u32) {
    let result = line_starts.binary_search(&offset);
    match result {
        Ok(idx) => {
            let idx: u32 = idx.try_into().expect("overflow");
            (idx + 1, 1)
        }
        Err(idx) => {
            let line_start = line_starts[idx - 1];
            (idx.try_into().expect("overflow"), offset - line_start + 1)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{compute_line_column, compute_line_starts};

    #[test]
    fn test_line_starts() {
        assert_eq!(compute_line_starts("abc"), vec![0]);
        assert_eq!(compute_line_starts("a\nc\nd"), vec![0, 2, 4]);
        assert_eq!(compute_line_starts("\n\n"), vec![0, 1, 2]);
    }

    #[test]
    fn test_compute_line_column() {
        let content = "a\nb\nc";
        let line_starts = compute_line_starts(content);
        assert_eq!((1, 1), compute_line_column(&line_starts, 0));
        assert_eq!((1, 2), compute_line_column(&line_starts, 1));
        assert_eq!((2, 1), compute_line_column(&line_starts, 2));
        assert_eq!((2, 2), compute_line_column(&line_starts, 3));
        assert_eq!((3, 1), compute_line_column(&line_starts, 4));
        assert_eq!((3, 2), compute_line_column(&line_starts, 5));
        assert_eq!((3, 3), compute_line_column(&line_starts, 6));
    }
}
