pub use self::green::{GreenElement, GreenNode, GreenToken};

mod green;
pub use self::error::{ParseError, ParseErrorWithLocation};
pub use self::lexer::lex;
pub use self::parser::Parser;
pub use self::span::Span;
pub use self::token::{TokenKind, TokenSet};

pub mod ast;
pub mod error;
pub mod lexer;
pub mod parser;
pub mod span;
pub mod token;

pub fn compute_line_starts(content: &str) -> Vec<u32> {
    let mut pos: u32 = 0;
    let mut line_starts = vec![0];
    let mut chars = content.chars().peekable();

    while let Some(ch) = chars.next() {
        let ch_len = ch.len_utf8() as u32;

        if ch == '\n' {
            debug_assert_eq!(ch_len, 1);
            line_starts.push(pos + 1);
        } else if ch == '\r' {
            debug_assert_eq!(ch_len, 1);
            if chars.peek() == Some(&'\n') {
                assert_eq!('\n', chars.next().unwrap());
                debug_assert_eq!('\n'.len_utf8(), 1);
                line_starts.push(pos + 2);
                pos += 1;
            } else {
                line_starts.push(pos + 1);
            }
        }

        pos += ch_len;
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

pub fn get_line_content<'a>(content: &'a str, line_starts: &[u32], line_number: usize) -> &'a str {
    if line_number >= line_starts.len() {
        return "";
    }

    let start = line_starts[line_number] as usize;
    let end = if line_number + 1 < line_starts.len() {
        line_starts[line_number + 1] as usize
    } else {
        content.len()
    };

    &content[start..end]
}

#[cfg(test)]
mod tests {
    use super::{compute_line_column, compute_line_starts, get_line_content};

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

    #[test]
    fn test_line_starts_with_multibyte_utf8() {
        // Test with 2-byte UTF-8 character (ä = 0xC3 0xA4)
        assert_eq!(compute_line_starts("ä\nb"), vec![0, 3]);

        // Test with 3-byte UTF-8 character (€ = 0xE2 0x82 0xAC)
        assert_eq!(compute_line_starts("€\nb"), vec![0, 4]);

        // Test with 4-byte UTF-8 character (emoji 😀 = 0xF0 0x9F 0x98 0x80)
        assert_eq!(compute_line_starts("😀\nb"), vec![0, 5]);

        // Test mixed content
        assert_eq!(compute_line_starts("a😀b\nc"), vec![0, 7]);
    }

    #[test]
    fn test_compute_line_column_with_multibyte_utf8() {
        // String: "😀\nb" where 😀 is 4 bytes, \n is 1 byte, b is 1 byte
        let content = "😀\nb";
        let line_starts = compute_line_starts(content);

        // First line starts at byte 0
        assert_eq!((1, 1), compute_line_column(&line_starts, 0));
        // Byte 4 is still on first line (the newline at byte 4)
        assert_eq!((1, 5), compute_line_column(&line_starts, 4));
        // Second line starts at byte 5
        assert_eq!((2, 1), compute_line_column(&line_starts, 5));
        // Byte 6 is the end of 'b'
        assert_eq!((2, 2), compute_line_column(&line_starts, 6));
    }

    #[test]
    fn test_line_starts_with_cr() {
        assert_eq!(compute_line_starts("a\rb\rc"), vec![0, 2, 4]);
        assert_eq!(compute_line_starts("\r\r"), vec![0, 1, 2]);
        assert_eq!(compute_line_starts("abc\r"), vec![0, 4]);
    }

    #[test]
    fn test_line_starts_with_crlf() {
        assert_eq!(compute_line_starts("a\r\nb\r\nc"), vec![0, 3, 6]);
        assert_eq!(compute_line_starts("\r\n\r\n"), vec![0, 2, 4]);
        assert_eq!(compute_line_starts("abc\r\n"), vec![0, 5]);
    }

    #[test]
    fn test_line_starts_with_mixed_line_breaks() {
        assert_eq!(compute_line_starts("a\nb\rc\r\nd"), vec![0, 2, 4, 7]);
        assert_eq!(compute_line_starts("\n\r\r\n"), vec![0, 1, 2, 4]);
    }

    #[test]
    fn test_line_starts_with_crlf_and_multibyte_utf8() {
        // Test CRLF with multi-byte UTF-8 characters
        // "ä\r\nb" where ä is 2 bytes, \r\n is 2 bytes, b is 1 byte
        assert_eq!(compute_line_starts("ä\r\nb"), vec![0, 4]);

        // "😀\r\nb" where 😀 is 4 bytes, \r\n is 2 bytes, b is 1 byte
        assert_eq!(compute_line_starts("😀\r\nb"), vec![0, 6]);
    }

    #[test]
    fn test_compute_line_column_with_crlf() {
        let content = "a\r\nb\r\nc";
        let line_starts = compute_line_starts(content);

        // Line 1: "a\r\n" (positions 0-2)
        assert_eq!((1, 1), compute_line_column(&line_starts, 0)); // 'a'
        assert_eq!((1, 2), compute_line_column(&line_starts, 1)); // '\r'
        assert_eq!((1, 3), compute_line_column(&line_starts, 2)); // '\n'

        // Line 2: "b\r\n" (positions 3-5)
        assert_eq!((2, 1), compute_line_column(&line_starts, 3)); // 'b'
        assert_eq!((2, 2), compute_line_column(&line_starts, 4)); // '\r'
        assert_eq!((2, 3), compute_line_column(&line_starts, 5)); // '\n'

        // Line 3: "c" (position 6)
        assert_eq!((3, 1), compute_line_column(&line_starts, 6)); // 'c'
    }

    #[test]
    fn test_compute_line_column_with_cr() {
        let content = "a\rb\rc";
        let line_starts = compute_line_starts(content);

        // Line 1: "a\r" (positions 0-1)
        assert_eq!((1, 1), compute_line_column(&line_starts, 0)); // 'a'
        assert_eq!((1, 2), compute_line_column(&line_starts, 1)); // '\r'

        // Line 2: "b\r" (positions 2-3)
        assert_eq!((2, 1), compute_line_column(&line_starts, 2)); // 'b'
        assert_eq!((2, 2), compute_line_column(&line_starts, 3)); // '\r'

        // Line 3: "c" (position 4)
        assert_eq!((3, 1), compute_line_column(&line_starts, 4)); // 'c'
    }

    #[test]
    fn test_get_line_content() {
        let content = "line1\nline2\nline3";
        let line_starts = compute_line_starts(content);

        // Test getting each line (zero-based indexing)
        assert_eq!(get_line_content(content, &line_starts, 0), "line1\n");
        assert_eq!(get_line_content(content, &line_starts, 1), "line2\n");
        assert_eq!(get_line_content(content, &line_starts, 2), "line3");

        // Test out of bounds
        assert_eq!(get_line_content(content, &line_starts, 3), "");
        assert_eq!(get_line_content(content, &line_starts, 100), "");
    }

    #[test]
    fn test_get_line_content_with_crlf() {
        let content = "line1\r\nline2\r\nline3";
        let line_starts = compute_line_starts(content);

        // Test getting each line with CRLF
        assert_eq!(get_line_content(content, &line_starts, 0), "line1\r\n");
        assert_eq!(get_line_content(content, &line_starts, 1), "line2\r\n");
        assert_eq!(get_line_content(content, &line_starts, 2), "line3");
    }

    #[test]
    fn test_get_line_content_with_cr() {
        let content = "line1\rline2\rline3";
        let line_starts = compute_line_starts(content);

        // Test getting each line with CR
        assert_eq!(get_line_content(content, &line_starts, 0), "line1\r");
        assert_eq!(get_line_content(content, &line_starts, 1), "line2\r");
        assert_eq!(get_line_content(content, &line_starts, 2), "line3");
    }

    #[test]
    fn test_get_line_content_with_mixed_line_breaks() {
        let content = "line1\nline2\rline3\r\nline4";
        let line_starts = compute_line_starts(content);

        assert_eq!(get_line_content(content, &line_starts, 0), "line1\n");
        assert_eq!(get_line_content(content, &line_starts, 1), "line2\r");
        assert_eq!(get_line_content(content, &line_starts, 2), "line3\r\n");
        assert_eq!(get_line_content(content, &line_starts, 3), "line4");
    }

    #[test]
    fn test_get_line_content_with_multibyte_utf8() {
        let content = "Hello 世界\n你好 World\n😀🎉";
        let line_starts = compute_line_starts(content);

        assert_eq!(get_line_content(content, &line_starts, 0), "Hello 世界\n");
        assert_eq!(get_line_content(content, &line_starts, 1), "你好 World\n");
        assert_eq!(get_line_content(content, &line_starts, 2), "😀🎉");
    }

    #[test]
    fn test_get_line_content_empty_lines() {
        let content = "\n\ntext\n";
        let line_starts = compute_line_starts(content);

        assert_eq!(get_line_content(content, &line_starts, 0), "\n");
        assert_eq!(get_line_content(content, &line_starts, 1), "\n");
        assert_eq!(get_line_content(content, &line_starts, 2), "text\n");
        assert_eq!(get_line_content(content, &line_starts, 3), "");
    }

    #[test]
    fn test_get_line_content_single_line() {
        let content = "single line no newline";
        let line_starts = compute_line_starts(content);

        assert_eq!(
            get_line_content(content, &line_starts, 0),
            "single line no newline"
        );
        assert_eq!(get_line_content(content, &line_starts, 1), "");
    }

    #[test]
    fn test_get_line_content_empty_string() {
        let content = "";
        let line_starts = compute_line_starts(content);

        assert_eq!(get_line_content(content, &line_starts, 0), "");
        assert_eq!(get_line_content(content, &line_starts, 1), "");
    }
}
