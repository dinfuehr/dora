use TokenKind::*;

#[derive(Copy, Clone)]
pub struct TokenSet(u128);

impl TokenSet {
    pub const fn new(kinds: &[TokenKind]) -> TokenSet {
        let mut value = 0;
        let mut i = 0;

        while i < kinds.len() {
            let kind = kinds[i] as u16;
            assert!(kind <= EOF as u16);
            value |= 1 << kind;
            i += 1;
        }

        TokenSet(value)
    }

    pub const fn union(&self, other: TokenSet) -> TokenSet {
        TokenSet(self.0 | other.0)
    }

    pub fn contains(&self, kind: TokenKind) -> bool {
        self.0 & (1 << (kind as u16)) != 0
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
pub const TYPE_FIRST: TokenSet = TokenSet::new(&[IDENTIFIER, UPCASE_SELF_KW, L_PAREN]);
pub const UNNAMED_FIELD_FIRST: TokenSet = TYPE_FIRST.union(MODIFIER_FIRST);
pub const FIELD_RS: TokenSet = ELEM_FIRST;
pub const LET_PATTERN_FIRST: TokenSet = TokenSet::new(&[L_PAREN, MUT_KW, UNDERSCORE, IDENTIFIER]);
pub const LET_PATTERN_RS: TokenSet = TokenSet::new(&[EQ]);
pub const PATTERN_FIRST: TokenSet = TokenSet::new(&[
    UNDERSCORE,
    MUT_KW,
    L_PAREN,
    TRUE,
    FALSE,
    DOT_DOT,
    IDENTIFIER,
    CHAR_LITERAL,
    STRING_LITERAL,
    INT_LITERAL,
    FLOAT_LITERAL,
    SUB,
]);
pub const PATTERN_RS: TokenSet = TokenSet::new(&[DOUBLE_ARROW]);

pub const ELEM_FIRST: TokenSet = TokenSet::new(&[
    FN_KW, CLASS_KW, STRUCT_KW, TRAIT_KW, IMPL_KW, LET_KW, CONST_KW, ENUM_KW, MOD_KW, USE_KW,
    EXTERN_KW,
]);

pub const EMPTY: TokenSet = TokenSet::new(&[]);

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Copy, Clone, Hash)]
#[allow(non_camel_case_types)]
#[repr(u16)]
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

    // Renames in use.
    AS_KW,
    // Pattern matching.
    IS_KW,

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

    // compound assignment operators
    ADD_EQ,
    SUB_EQ,
    MUL_EQ,
    DIV_EQ,
    MOD_EQ,
    OR_EQ,
    AND_EQ,
    CARET_EQ,
    GT_GT_EQ,
    GT_GT_GT_EQ,
    LT_LT_EQ,

    // operators – shifts
    GT_GT,
    GT_GT_GT,
    LT_LT,

    // basic syntax
    EQ,
    COMMA,
    SEMICOLON,
    DOT,
    DOT_DOT,
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

    // types
    TYPE_KW,
    WHERE_KW,
    UPCASE_SELF_KW,
    UNDERSCORE,
    REF_KW,

    // trivia
    WHITESPACE,
    LINE_COMMENT,
    MULTILINE_COMMENT,

    // unknown character
    UNKNOWN,

    // End-of-file. This is the last token - see LAST_TOKEN.
    EOF,

    // Node kinds
    ALIAS,
    ALT,
    ARGUMENT,
    ARGUMENT_LIST,
    BIN,
    BLOCK,
    BREAK,
    CALL,
    CLASS,
    CONST,
    CONTINUE,
    CONV,
    CTOR_FIELD,
    CTOR_FIELD_LIST,
    CTOR_PATTERN,
    DOT_EXPR,
    ELEMENT_LIST,
    ENUM,
    ENUM_VARIANT,
    ERROR,
    ERROR_ELEM,
    ERROR_EXPR,
    ERROR_PATH_SEGMENT,
    ERROR_PATTERN,
    ERROR_STMT,
    ERROR_TYPE,
    ERROR_USE_TARGET,
    EXPR_STMT,
    EXTERN,
    FIELD,
    FOR,
    FUNCTION,
    GLOBAL,
    IDENT_PATTERN,
    IF,
    IMPL,
    IS,
    LAMBDA,
    LAMBDA_TYPE,
    LET,
    LIT_BOOL,
    LIT_CHAR,
    LIT_FLOAT,
    LIT_INT,
    LIT_PATTERN,
    LIT_STR,
    MATCH,
    MATCH_ARM,
    METHOD_CALL_EXPR,
    MODIFIER,
    MODIFIER_LIST,
    MODULE,
    NAME,
    NAME_EXPR,
    PARAM,
    PAREN,
    PATH,
    PATH_DATA,
    PLAIN,
    QUALIFIED_PATH_TYPE,
    REF_TYPE,
    REGULAR_TYPE,
    REST,
    RETURN,
    STRUCT,
    TEMPLATE,
    THIS,
    TRAIT,
    TUPLE,
    TUPLE_PATTERN,
    TUPLE_TYPE,
    TYPE_ARGUMENT_LIST,
    TYPE_ARGUMENT,
    TYPE_BOUNDS,
    TYPED_EXPR,
    TYPE_PARAM,
    TYPE_PARAM_LIST,
    UN,
    UNDERSCORE_PATTERN,
    UPCASE_THIS,
    USE,
    USE_AS,
    USE_ATOM,
    USE_GROUP,
    USE_PATH,
    WHERE_CLAUSE,
    WHERE_CLAUSE_ITEM,
    WHILE,
}

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

impl std::fmt::Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}
