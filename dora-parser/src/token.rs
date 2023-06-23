use TokenKind::*;

#[derive(Copy, Clone)]
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
    TRUE,
    FALSE,
    STRING_LITERAL,
    TEMPLATE_LITERAL,
    CHAR_LITERAL,
    INT_LITERAL,
    FLOAT_LITERAL,
    IDENTIFIER,
    IF_KW,
    MATCH_KW,
    L_BRACE,
    L_PAREN,
    SELF_KW,
    OR,
    OR_OR,
    NOT,
    SUB,
    ADD,
    FOR_KW,
    WHILE_KW,
    BREAK_KW,
    CONTINUE_KW,
    RETURN_KW,
]);

pub const PARAM_LIST_RS: TokenSet = TokenSet::new(&[FN_KW, OR, L_BRACE]);
pub const ENUM_VARIANT_RS: TokenSet = EMPTY;
pub const ENUM_VARIANT_ARGUMENT_RS: TokenSet = EMPTY;
pub const TYPE_PARAM_RS: TokenSet = EMPTY;
pub const USE_PATH_ATOM_FIRST: TokenSet =
    TokenSet::new(&[SELF_KW, PACKAGE_KW, SUPER_KW, IDENTIFIER]);
pub const USE_PATH_FIRST: TokenSet =
    USE_PATH_ATOM_FIRST.union(TokenSet::new(&[TokenKind::L_BRACE]));
pub const MODIFIER_FIRST: TokenSet = TokenSet::new(&[AT, PUB_KW, STATIC_KW]);
pub const FIELD_FIRST: TokenSet = TokenSet::new(&[IDENTIFIER, COLON]).union(MODIFIER_FIRST);
pub const FIELD_RS: TokenSet = ELEM_FIRST;
pub const LET_PATTERN_FIRST: TokenSet = TokenSet::new(&[L_PAREN, MUT_KW, UNDERSCORE, IDENTIFIER]);
pub const LET_PATTERN_RS: TokenSet = TokenSet::new(&[EQ]);
pub const MATCH_PATTERN_FIRST: TokenSet = TokenSet::new(&[UNDERSCORE, MUT_KW, IDENTIFIER]);
pub const MATCH_PATTERN_RS: TokenSet = TokenSet::new(&[DOUBLE_ARROW]);

pub const ELEM_FIRST: TokenSet = TokenSet::new(&[
    FN_KW, CLASS_KW, STRUCT_KW, TRAIT_KW, IMPL_KW, ALIAS_KW, LET_KW, CONST_KW, ENUM_KW, MOD_KW,
    USE_KW, EXTERN_KW,
]);

pub const EMPTY: TokenSet = TokenSet::new(&[]);

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
    STRUCT_FIELD,
    CLASS,
    CLASS_FIELD,
    USE,
    USE_GROUP,
    USE_PATH,
    USE_COMPONENT,
    USE_RENAME,
    EXTERN,
    ENUM,
    ENUM_VARIANT_LIST,
    ENUM_VARIANT,
    ENUM_VARIANT_ARGUMENT_LIST,
    MODULE,
    CONST,
    IMPL,
    GLOBAL,
    TRAIT,
    LIST,
    IDENT,
    TYPE_PARAMS,
    TYPE_PARAM,
    TYPE_LIST,
    EXPR_LIST,
    PATTERN_LIST,

    PARAM_LIST,

    MODIFIERS,
    MODIFIER,

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

    ERROR,
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
