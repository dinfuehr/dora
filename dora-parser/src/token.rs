pub struct TokenSet(u128);

impl TokenSet {
    pub const fn new(kinds: &[TokenKind]) -> TokenSet {
        let mut value = 0;
        let mut i = 0;

        while i < kinds.len() {
            value |= 1 << (kinds[i] as u8);
            i += 1;
        }

        TokenSet(value)
    }

    pub const fn union(&self, other: TokenSet) -> TokenSet {
        TokenSet(self.0 | other.0)
    }

    pub fn contains(&self, kind: TokenKind) -> bool {
        self.0 & (1 << (kind as u8)) != 0
    }
}

pub const EXPRESSION_FIRST: TokenSet = TokenSet::new(&[
    TokenKind::TRUE,
    TokenKind::FALSE,
    TokenKind::STRING_LITERAL,
    TokenKind::TEMPLATE_LITERAL,
    TokenKind::CHAR_LITERAL,
    TokenKind::INT_LITERAL,
    TokenKind::FLOAT_LITERAL,
    TokenKind::IDENTIFIER,
    TokenKind::IF,
    TokenKind::MATCH,
    TokenKind::L_BRACE,
    TokenKind::L_PAREN,
    TokenKind::THIS,
    TokenKind::OR,
    TokenKind::OR_OR,
    TokenKind::NOT,
    TokenKind::SUB,
    TokenKind::ADD,
    TokenKind::FOR,
    TokenKind::WHILE,
    TokenKind::BREAK,
    TokenKind::CONTINUE,
    TokenKind::RETURN,
]);

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Copy, Clone)]
#[allow(non_camel_case_types)]
#[repr(u8)]
pub enum TokenKind {
    // literals
    STRING_LITERAL,
    TEMPLATE_LITERAL,
    TEMPLATE_END_LITERAL,
    CHAR_LITERAL,
    INT_LITERAL,
    FLOAT_LITERAL,
    IDENTIFIER,
    TRUE,
    FALSE,

    // "big" shapes
    CLASS,
    ENUM,
    STRUCT,
    TRAIT,
    IMPL,
    MOD,
    USE,
    PACKAGE,
    EXTERN,

    // "small" shapes
    FN,
    LET,
    MUT,
    CONST,

    // control flow
    RETURN,
    IF,
    ELSE,
    WHILE,
    FOR,
    IN,
    BREAK,
    CONTINUE,
    MATCH,

    // qualifiers
    THIS,
    SUPER,
    PUB,
    STATIC,

    // casting
    AS,

    // operators – numbers
    ADD,
    SUB,
    MUL,
    DIV,
    MODULO,

    // operators – logic
    NOT,
    OR,
    AND,
    CARET,
    AND_AND,
    OR_OR,

    // operators – comparisons
    EQ_EQ,
    NOT_EQ,
    EQ_EQ_EQ,
    NOT_EQ_EQ,
    LT,
    LE,
    GT,
    GE,

    // operators – shifts
    GT_GT,
    GT_GT_GT,
    LT_LT,

    // basic syntax
    EQ,
    COMMA,
    SEMICOLON,
    DOT,
    DOT_DOT_DOT,
    COLON,
    COLON_COLON,
    AT,
    ARROW,
    DOUBLE_ARROW,

    // brackets
    L_PAREN,
    R_PAREN,
    L_BRACKET,
    R_BRACKET,
    L_BRACE,
    R_BRACE,

    // unused
    TYPE,
    ALIAS,
    CAPITAL_THIS,
    UNDERSCORE,

    // trivia
    WHITESPACE,
    LINE_COMMENT,
    MULTILINE_COMMENT,

    // unknown character
    UNKNOWN,

    // End-of-file. This is the last token - see LAST_TOKEN.
    EOF,

    // Syntax tree nodes
    SOURCE_FILE,
}

pub const LAST_TOKEN: TokenKind = TokenKind::EOF;

impl TokenKind {
    pub fn is_trivia(self) -> bool {
        match self {
            TokenKind::LINE_COMMENT | TokenKind::MULTILINE_COMMENT | TokenKind::WHITESPACE => true,
            _ => false,
        }
    }

    pub fn is_eof(self) -> bool {
        self == TokenKind::EOF
    }
}

impl From<TokenKind> for rowan::SyntaxKind {
    fn from(value: TokenKind) -> Self {
        rowan::SyntaxKind(value as u16)
    }
}
