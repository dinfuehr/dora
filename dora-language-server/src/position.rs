use lsp_types::{Position, Range};

use dora_parser::Span;

pub fn span_to_range(content: &str, line_starts: &[u32], span: Span) -> Range {
    let start = utf8_offset_to_utf16_position(content, line_starts, span.start());
    let end = utf8_offset_to_utf16_position(content, line_starts, span.end());

    Range { start, end }
}

#[allow(unused)]
pub fn range_to_span(content: &str, line_starts: &[u32], range: Range) -> Span {
    let start = utf16_position_to_utf8_offset(content, line_starts, range.start);
    let end = utf16_position_to_utf8_offset(content, line_starts, range.end);

    Span::new(start, end - start)
}

pub fn utf8_offset_to_utf16_position(content: &str, line_starts: &[u32], offset: u32) -> Position {
    // Find which line the offset is on
    let result = line_starts.binary_search(&offset);
    let (line_idx, line_start) = match result {
        Ok(idx) => {
            // Offset points exactly to the start of a line
            (idx, offset)
        }
        Err(idx) => {
            // Offset is somewhere within a line
            // idx is the insertion point, so the line is idx - 1
            (idx - 1, line_starts[idx - 1])
        }
    };

    // Get the portion of the line from line start to our offset
    let line_prefix = &content[line_start as usize..offset as usize];

    // Count UTF-16 code units in the line prefix
    let utf16_column: u32 = line_prefix.encode_utf16().count() as u32;

    Position::new(line_idx as u32, utf16_column)
}

#[allow(unused)]
pub fn utf16_position_to_utf8_offset(
    content: &str,
    line_starts: &[u32],
    position: Position,
) -> u32 {
    let line = position.line as usize;
    let utf16_column = position.character as usize;

    // Get the start offset of the requested line
    if line >= line_starts.len() {
        // Position is beyond the end of the file
        return content.len() as u32;
    }

    let line_start = line_starts[line] as usize;

    // Get the end of the line (either the start of the next line or end of content)
    let line_end = if line + 1 < line_starts.len() {
        line_starts[line + 1] as usize
    } else {
        content.len()
    };

    // Get the line content
    let line_content = &content[line_start..line_end];

    // If the column is 0, return the line start
    if utf16_column == 0 {
        return line_start as u32;
    }

    // Iterate through the line content, counting UTF-16 code units
    let mut current_utf16_offset = 0;
    let mut current_utf8_offset = 0;

    for ch in line_content.chars() {
        if current_utf16_offset >= utf16_column {
            break;
        }

        current_utf16_offset += ch.len_utf16();
        current_utf8_offset += ch.len_utf8();
    }

    (line_start + current_utf8_offset) as u32
}

#[cfg(test)]
mod tests {
    use super::*;
    use dora_parser::{Span, compute_line_starts};

    #[test]
    fn test_position_from_offset_simple() {
        let content = "line1\nline2\nline3";
        let line_starts = compute_line_starts(content);

        // First line, first character (0-based in LSP)
        assert_eq!(
            utf8_offset_to_utf16_position(content, &line_starts, 0),
            Position::new(0, 0)
        );

        // First line, second character
        assert_eq!(
            utf8_offset_to_utf16_position(content, &line_starts, 1),
            Position::new(0, 1)
        );

        // Second line, first character
        assert_eq!(
            utf8_offset_to_utf16_position(content, &line_starts, 6),
            Position::new(1, 0)
        );

        // Third line, first character
        assert_eq!(
            utf8_offset_to_utf16_position(content, &line_starts, 12),
            Position::new(2, 0)
        );
    }

    #[test]
    fn test_position_from_offset_with_crlf() {
        let content = "line1\r\nline2\r\nline3";
        let line_starts = compute_line_starts(content);

        // First line, first character
        assert_eq!(
            utf8_offset_to_utf16_position(content, &line_starts, 0),
            Position::new(0, 0)
        );

        // First line, at \r
        assert_eq!(
            utf8_offset_to_utf16_position(content, &line_starts, 5),
            Position::new(0, 5)
        );

        // Second line, first character
        assert_eq!(
            utf8_offset_to_utf16_position(content, &line_starts, 7),
            Position::new(1, 0)
        );

        // Third line, first character
        assert_eq!(
            utf8_offset_to_utf16_position(content, &line_starts, 14),
            Position::new(2, 0)
        );
    }

    #[test]
    fn test_position_from_offset_with_multibyte_utf8() {
        // "Hello 世界\n你好 World"
        // "Hello " = 6 bytes (6 UTF-16 code units)
        // "世" = 3 bytes (1 UTF-16 code unit)
        // "界" = 3 bytes (1 UTF-16 code unit)
        // "\n" = 1 byte (1 UTF-16 code unit)
        // Line 1: bytes 0-12 (13 bytes total)
        // Line 2: bytes 13-...
        let content = "Hello 世界\n你好 World";
        let line_starts = compute_line_starts(content);

        // First line, first character
        assert_eq!(
            utf8_offset_to_utf16_position(content, &line_starts, 0),
            Position::new(0, 0)
        );

        // First line, at '世' (byte offset 6, UTF-16 column 6)
        // "Hello " is 6 bytes and 6 UTF-16 code units
        assert_eq!(
            utf8_offset_to_utf16_position(content, &line_starts, 6),
            Position::new(0, 6)
        );

        // Second line, first character (byte offset 13, after "Hello 世界\n")
        assert_eq!(
            utf8_offset_to_utf16_position(content, &line_starts, 13),
            Position::new(1, 0)
        );
    }

    #[test]
    fn test_range_from_span_simple() {
        let content = "fn foo() {}";
        let line_starts = compute_line_starts(content);

        // Span covering "foo" (start at byte 3, length 3)
        let span = Span::new(3, 3);
        let range = span_to_range(content, &line_starts, span);

        assert_eq!(range.start, Position::new(0, 3));
        assert_eq!(range.end, Position::new(0, 6));
    }

    #[test]
    fn test_range_from_span_multiline() {
        let content = "line1\nline2\nline3";
        let line_starts = compute_line_starts(content);

        // Span from start of line2 (byte 6) with length 6 (to byte 12)
        let span = Span::new(6, 6);
        let range = span_to_range(content, &line_starts, span);

        assert_eq!(range.start, Position::new(1, 0));
        assert_eq!(range.end, Position::new(2, 0));
    }

    #[test]
    fn test_range_from_span_with_crlf() {
        let content = "line1\r\nline2\r\nline3";
        let line_starts = compute_line_starts(content);

        // Span covering "line2" (start at byte 7, length 5)
        let span = Span::new(7, 5);
        let range = span_to_range(content, &line_starts, span);

        assert_eq!(range.start, Position::new(1, 0));
        assert_eq!(range.end, Position::new(1, 5));
    }

    #[test]
    fn test_range_from_span_with_multibyte_utf8() {
        let content = "fn 你好() {}";
        let line_starts = compute_line_starts(content);

        // Span covering "你好" (start at byte 3, length 6 bytes for two Chinese characters)
        // "fn " = 3 bytes (3 UTF-16 code units)
        // "你好" = 6 bytes (2 UTF-16 code units)
        let span = Span::new(3, 6);
        let range = span_to_range(content, &line_starts, span);

        assert_eq!(range.start, Position::new(0, 3));
        assert_eq!(range.end, Position::new(0, 5)); // 3 + 2 = 5 UTF-16 code units
    }

    #[test]
    fn test_position_from_offset_with_emoji() {
        // Emoji like 😀 take 4 UTF-8 bytes but 2 UTF-16 code units (surrogate pair)
        // "Hello 😀 World"
        // "Hello " = 6 bytes (6 UTF-16 code units)
        // "😀" = 4 bytes (2 UTF-16 code units)
        // " World" = 6 bytes (6 UTF-16 code units)
        let content = "Hello 😀 World";
        let line_starts = compute_line_starts(content);

        // First line, first character
        assert_eq!(
            utf8_offset_to_utf16_position(content, &line_starts, 0),
            Position::new(0, 0)
        );

        // At the emoji (byte offset 6)
        // UTF-16 position should be 6 (after "Hello ")
        assert_eq!(
            utf8_offset_to_utf16_position(content, &line_starts, 6),
            Position::new(0, 6)
        );

        // After the emoji (byte offset 10 = 6 + 4)
        // UTF-16 position should be 8 (6 for "Hello " + 2 for emoji)
        assert_eq!(
            utf8_offset_to_utf16_position(content, &line_starts, 10),
            Position::new(0, 8)
        );

        // At 'W' of "World" (byte offset 11)
        // UTF-16 position should be 9 (6 + 2 + 1)
        assert_eq!(
            utf8_offset_to_utf16_position(content, &line_starts, 11),
            Position::new(0, 9)
        );
    }

    #[test]
    fn test_offset_from_position_simple() {
        let content = "line1\nline2\nline3";
        let line_starts = compute_line_starts(content);

        // First line, first character (0-based in LSP)
        assert_eq!(
            utf16_position_to_utf8_offset(content, &line_starts, Position::new(0, 0)),
            0
        );

        // First line, second character
        assert_eq!(
            utf16_position_to_utf8_offset(content, &line_starts, Position::new(0, 1)),
            1
        );

        // Second line, first character
        assert_eq!(
            utf16_position_to_utf8_offset(content, &line_starts, Position::new(1, 0)),
            6
        );

        // Third line, first character
        assert_eq!(
            utf16_position_to_utf8_offset(content, &line_starts, Position::new(2, 0)),
            12
        );
    }

    #[test]
    fn test_offset_from_position_with_crlf() {
        let content = "line1\r\nline2\r\nline3";
        let line_starts = compute_line_starts(content);

        // First line, first character
        assert_eq!(
            utf16_position_to_utf8_offset(content, &line_starts, Position::new(0, 0)),
            0
        );

        // First line, at \r (UTF-16 column 5)
        assert_eq!(
            utf16_position_to_utf8_offset(content, &line_starts, Position::new(0, 5)),
            5
        );

        // Second line, first character
        assert_eq!(
            utf16_position_to_utf8_offset(content, &line_starts, Position::new(1, 0)),
            7
        );

        // Third line, first character
        assert_eq!(
            utf16_position_to_utf8_offset(content, &line_starts, Position::new(2, 0)),
            14
        );
    }

    #[test]
    fn test_offset_from_position_with_multibyte_utf8() {
        // "Hello 世界\n你好 World"
        // "Hello " = 6 bytes (6 UTF-16 code units)
        // "世" = 3 bytes (1 UTF-16 code unit)
        // "界" = 3 bytes (1 UTF-16 code unit)
        // "\n" = 1 byte (1 UTF-16 code unit)
        // Line 1: bytes 0-12 (13 bytes total)
        // Line 2: bytes 13-...
        let content = "Hello 世界\n你好 World";
        let line_starts = compute_line_starts(content);

        // First line, first character
        assert_eq!(
            utf16_position_to_utf8_offset(content, &line_starts, Position::new(0, 0)),
            0
        );

        // First line, at '世' (UTF-16 column 6)
        // "Hello " is 6 bytes and 6 UTF-16 code units
        assert_eq!(
            utf16_position_to_utf8_offset(content, &line_starts, Position::new(0, 6)),
            6
        );

        // First line, at '界' (UTF-16 column 7)
        // "Hello 世" is 6 + 3 = 9 bytes, but 6 + 1 = 7 UTF-16 code units
        assert_eq!(
            utf16_position_to_utf8_offset(content, &line_starts, Position::new(0, 7)),
            9
        );

        // Second line, first character (after "Hello 世界\n")
        assert_eq!(
            utf16_position_to_utf8_offset(content, &line_starts, Position::new(1, 0)),
            13
        );

        // Second line, at '你' (UTF-16 column 0 on line 2, byte offset 13)
        assert_eq!(
            utf16_position_to_utf8_offset(content, &line_starts, Position::new(1, 0)),
            13
        );
    }

    #[test]
    fn test_offset_from_position_with_emoji() {
        // Emoji like 😀 take 4 UTF-8 bytes but 2 UTF-16 code units (surrogate pair)
        // "Hello 😀 World"
        // "Hello " = 6 bytes (6 UTF-16 code units)
        // "😀" = 4 bytes (2 UTF-16 code units)
        // " World" = 6 bytes (6 UTF-16 code units)
        let content = "Hello 😀 World";
        let line_starts = compute_line_starts(content);

        // First line, first character
        assert_eq!(
            utf16_position_to_utf8_offset(content, &line_starts, Position::new(0, 0)),
            0
        );

        // At the emoji (UTF-16 column 6, byte offset 6)
        assert_eq!(
            utf16_position_to_utf8_offset(content, &line_starts, Position::new(0, 6)),
            6
        );

        // After the emoji (UTF-16 column 8, byte offset 10 = 6 + 4)
        assert_eq!(
            utf16_position_to_utf8_offset(content, &line_starts, Position::new(0, 8)),
            10
        );

        // At 'W' of "World" (UTF-16 column 9, byte offset 11)
        assert_eq!(
            utf16_position_to_utf8_offset(content, &line_starts, Position::new(0, 9)),
            11
        );
    }

    #[test]
    fn test_roundtrip_conversion_simple() {
        let content = "line1\nline2\nline3";
        let line_starts = compute_line_starts(content);

        for offset in 0..content.len() as u32 {
            let position = utf8_offset_to_utf16_position(content, &line_starts, offset);
            let back = utf16_position_to_utf8_offset(content, &line_starts, position);
            assert_eq!(offset, back, "Roundtrip failed for offset {}", offset);
        }
    }

    #[test]
    fn test_roundtrip_conversion_with_multibyte() {
        let content = "Hello 世界\n你好 World";
        let line_starts = compute_line_starts(content);

        for offset in 0..content.len() as u32 {
            // Skip middle of multibyte characters
            if !content.is_char_boundary(offset as usize) {
                continue;
            }

            let position = utf8_offset_to_utf16_position(content, &line_starts, offset);
            let back = utf16_position_to_utf8_offset(content, &line_starts, position);
            assert_eq!(offset, back, "Roundtrip failed for offset {}", offset);
        }
    }

    #[test]
    fn test_roundtrip_conversion_with_emoji() {
        let content = "Hello 😀 World\n🎉 Test 🚀";
        let line_starts = compute_line_starts(content);

        for offset in 0..content.len() as u32 {
            // Skip middle of multibyte characters
            if !content.is_char_boundary(offset as usize) {
                continue;
            }

            let position = utf8_offset_to_utf16_position(content, &line_starts, offset);
            let back = utf16_position_to_utf8_offset(content, &line_starts, position);
            assert_eq!(offset, back, "Roundtrip failed for offset {}", offset);
        }
    }

    #[test]
    fn test_span_from_range_simple() {
        let content = "fn foo() {}";
        let line_starts = compute_line_starts(content);

        // Range covering "foo" (UTF-16 positions 3-6)
        let range = Range {
            start: Position::new(0, 3),
            end: Position::new(0, 6),
        };
        let span = range_to_span(content, &line_starts, range);

        assert_eq!(span.start(), 3);
        assert_eq!(span.len(), 3);
        assert_eq!(span.end(), 6);
    }

    #[test]
    fn test_span_from_range_multiline() {
        let content = "line1\nline2\nline3";
        let line_starts = compute_line_starts(content);

        // Range from start of line2 to start of line3
        let range = Range {
            start: Position::new(1, 0),
            end: Position::new(2, 0),
        };
        let span = range_to_span(content, &line_starts, range);

        assert_eq!(span.start(), 6);
        assert_eq!(span.len(), 6);
        assert_eq!(span.end(), 12);
    }

    #[test]
    fn test_span_from_range_with_crlf() {
        let content = "line1\r\nline2\r\nline3";
        let line_starts = compute_line_starts(content);

        // Range covering "line2"
        let range = Range {
            start: Position::new(1, 0),
            end: Position::new(1, 5),
        };
        let span = range_to_span(content, &line_starts, range);

        assert_eq!(span.start(), 7);
        assert_eq!(span.len(), 5);
        assert_eq!(span.end(), 12);
    }

    #[test]
    fn test_span_from_range_with_multibyte_utf8() {
        let content = "fn 你好() {}";
        let line_starts = compute_line_starts(content);

        // Range covering "你好" (UTF-16 positions 3-5)
        // "fn " = 3 bytes (3 UTF-16 code units)
        // "你好" = 6 bytes (2 UTF-16 code units)
        let range = Range {
            start: Position::new(0, 3),
            end: Position::new(0, 5),
        };
        let span = range_to_span(content, &line_starts, range);

        assert_eq!(span.start(), 3);
        assert_eq!(span.len(), 6);
        assert_eq!(span.end(), 9);
    }

    #[test]
    fn test_span_from_range_with_emoji() {
        // "Hello 😀 World"
        // "Hello " = 6 bytes (6 UTF-16 code units)
        // "😀" = 4 bytes (2 UTF-16 code units)
        // " World" = 6 bytes (6 UTF-16 code units)
        let content = "Hello 😀 World";
        let line_starts = compute_line_starts(content);

        // Range covering the emoji (UTF-16 positions 6-8)
        let range = Range {
            start: Position::new(0, 6),
            end: Position::new(0, 8),
        };
        let span = range_to_span(content, &line_starts, range);

        assert_eq!(span.start(), 6);
        assert_eq!(span.len(), 4);
        assert_eq!(span.end(), 10);
    }

    #[test]
    fn test_roundtrip_span_range_simple() {
        let content = "line1\nline2\nline3";
        let line_starts = compute_line_starts(content);

        // Test various spans
        let test_spans = vec![
            Span::new(0, 5),  // "line1"
            Span::new(6, 5),  // "line2"
            Span::new(12, 5), // "line3"
            Span::new(0, 11), // "line1\nline2"
            Span::new(0, 17), // entire content
        ];

        for span in test_spans {
            let range = span_to_range(content, &line_starts, span);
            let back = range_to_span(content, &line_starts, range);
            assert_eq!(
                span.start(),
                back.start(),
                "Roundtrip failed for span start {:?}",
                span
            );
            assert_eq!(
                span.len(),
                back.len(),
                "Roundtrip failed for span length {:?}",
                span
            );
        }
    }

    #[test]
    fn test_roundtrip_span_range_with_multibyte() {
        let content = "Hello 世界\n你好 World";
        let line_starts = compute_line_starts(content);

        // Test various spans covering multibyte characters
        let test_spans = vec![
            Span::new(0, 6),  // "Hello "
            Span::new(6, 3),  // "世"
            Span::new(9, 3),  // "界"
            Span::new(6, 6),  // "世界"
            Span::new(0, 12), // "Hello 世界"
            Span::new(13, 6), // "你好"
        ];

        for span in test_spans {
            let range = span_to_range(content, &line_starts, span);
            let back = range_to_span(content, &line_starts, range);
            assert_eq!(
                span.start(),
                back.start(),
                "Roundtrip failed for span start {:?}",
                span
            );
            assert_eq!(
                span.len(),
                back.len(),
                "Roundtrip failed for span length {:?}",
                span
            );
        }
    }

    #[test]
    fn test_roundtrip_span_range_with_emoji() {
        // "Hello 😀 World\n🎉 Test 🚀"
        // Byte positions (from the debug output):
        // "Hello " = bytes 0-6
        // "😀" = bytes 6-10
        // " World" = bytes 10-16
        // "\n" = byte 16-17
        // "🎉" = bytes 17-21
        // " Test " = bytes 21-27
        // "🚀" = bytes 27-31
        let content = "Hello 😀 World\n🎉 Test 🚀";
        let line_starts = compute_line_starts(content);

        // Test various spans covering emoji
        let test_spans = vec![
            Span::new(0, 6),   // "Hello "
            Span::new(6, 4),   // "😀"
            Span::new(10, 6),  // " World"
            Span::new(0, 16),  // "Hello 😀 World"
            Span::new(17, 4),  // "🎉"
            Span::new(27, 4),  // "🚀"
            Span::new(17, 10), // "🎉 Test "
        ];

        for span in test_spans {
            let range = span_to_range(content, &line_starts, span);
            let back = range_to_span(content, &line_starts, range);
            assert_eq!(
                span.start(),
                back.start(),
                "Roundtrip failed for span start {:?}",
                span
            );
            assert_eq!(
                span.len(),
                back.len(),
                "Roundtrip failed for span length {:?}",
                span
            );
        }
    }
}
