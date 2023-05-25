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
    TokenKind::IF_KW,
    TokenKind::MATCH_KW,
    TokenKind::L_BRACE,
    TokenKind::L_PAREN,
    TokenKind::SELF_KW,
    TokenKind::OR,
    TokenKind::OR_OR,
    TokenKind::NOT,
    TokenKind::SUB,
    TokenKind::ADD,
    TokenKind::FOR_KW,
    TokenKind::WHILE_KW,
    TokenKind::BREAK_KW,
    TokenKind::CONTINUE_KW,
    TokenKind::RETURN_KW,
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
    CLASS_KW,
    ENUM_KW,
    STRUCT_KW,
    TRAIT_KW,
    IMPL_KW,
    MOD_KW,
    USE_KW,
    PACKAGE_KW,
    EXTERN_KW,

    // "small" shapes
    FN_KW,
    LET_KW,
    MUT_KW,
    CONST_KW,

    // control flow
    RETURN_KW,
    IF_KW,
    ELSE_KW,
    WHILE_KW,
    FOR_KW,
    IN_KW,
    BREAK_KW,
    CONTINUE_KW,
    MATCH_KW,

    // qualifiers
    SELF_KW,
    SUPER_KW,
    PUB_KW,
    STATIC_KW,

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
    TYPE_KW,
    ALIAS_KW,
    UPCASE_SELF_KW,
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

    ALIAS,
    FN,
    STRUCT,
    CLASS,
    USE,
    USE_PATH,
    EXTERN,
    ENUM,
    MODULE,
    CONST,
    IMPL,
    GLOBAL,
    TRAIT,
    LIST,
    IDENT,
    TYPE_PARAMS,
    TYPE_PARAM,

    // Types
    SELF_TYPE,
    REGULAR_TYPE,
    LAMBDA_TYPE,
    TUPLE_TYPE,

    // Expressions
    TUPLE_EXPR,
    PAREN_EXPR,
    CHAR_LIT_EXPR,
    INT_LIT_EXPR,
    FLOAT_LIT_EXPR,
    STRING_LIT_EXPR,
    TEMPLATE_EXPR,
    BLOCK_EXPR,
    IF_EXPR,
    IDENT_EXPR,
    BOOL_LIT_EXPR,
    THIS_EXPR,
    LAMBDA_EXPR,
    FOR_EXPR,
    WHILE_EXPR,
    MATCH_EXPR,
    BREAK_EXPR,
    CONTINUE_EXPR,
    RETURN_EXPR,
    UNARY_EXPR,
    POSTFIX_EXPR,
    BINARY_EXPR,
    CONV_EXPR,
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
