use id_arena::Id;
use smol_str::SmolStr;

use dora_parser::ast::{self, SyntaxNodeBase};

use crate::sema::{Name, PatternArenaBuilder, Sema, SourceFileId};

pub type PatternId = Id<Pattern>;

pub enum Pattern {
    Alt(AltPattern),
    Ctor(CtorPattern),
    Ident(IdentPattern),
    LitBool(bool),
    LitChar(SmolStr),
    LitInt(SmolStr),
    LitFloat(SmolStr),
    LitStr(SmolStr),
    Rest,
    Tuple(TuplePattern),
    Underscore,
    Error,
}

impl Pattern {
    pub fn as_alt(&self) -> &AltPattern {
        match self {
            Pattern::Alt(pattern) => pattern,
            _ => unreachable!(),
        }
    }

    pub fn to_alt(&self) -> Option<&AltPattern> {
        match self {
            Pattern::Alt(pattern) => Some(pattern),
            _ => None,
        }
    }

    pub fn as_ctor(&self) -> &CtorPattern {
        match self {
            Pattern::Ctor(pattern) => pattern,
            _ => unreachable!(),
        }
    }

    pub fn to_ctor(&self) -> Option<&CtorPattern> {
        match self {
            Pattern::Ctor(pattern) => Some(pattern),
            _ => None,
        }
    }

    pub fn as_ident(&self) -> &IdentPattern {
        match self {
            Pattern::Ident(pattern) => pattern,
            _ => unreachable!(),
        }
    }

    pub fn to_ident(&self) -> Option<&IdentPattern> {
        match self {
            Pattern::Ident(pattern) => Some(pattern),
            _ => None,
        }
    }

    pub fn as_lit_bool(&self) -> bool {
        match self {
            Pattern::LitBool(value) => *value,
            _ => unreachable!(),
        }
    }

    pub fn to_lit_bool(&self) -> Option<bool> {
        match self {
            Pattern::LitBool(value) => Some(*value),
            _ => None,
        }
    }

    pub fn as_lit_char(&self) -> &SmolStr {
        match self {
            Pattern::LitChar(value) => value,
            _ => unreachable!(),
        }
    }

    pub fn to_lit_char(&self) -> Option<&SmolStr> {
        match self {
            Pattern::LitChar(value) => Some(value),
            _ => None,
        }
    }

    pub fn as_lit_int(&self) -> &SmolStr {
        match self {
            Pattern::LitInt(value) => value,
            _ => unreachable!(),
        }
    }

    pub fn to_lit_int(&self) -> Option<&SmolStr> {
        match self {
            Pattern::LitInt(value) => Some(value),
            _ => None,
        }
    }

    pub fn as_lit_float(&self) -> &SmolStr {
        match self {
            Pattern::LitFloat(value) => value,
            _ => unreachable!(),
        }
    }

    pub fn to_lit_float(&self) -> Option<&SmolStr> {
        match self {
            Pattern::LitFloat(value) => Some(value),
            _ => None,
        }
    }

    pub fn as_lit_str(&self) -> &SmolStr {
        match self {
            Pattern::LitStr(value) => value,
            _ => unreachable!(),
        }
    }

    pub fn to_lit_str(&self) -> Option<&SmolStr> {
        match self {
            Pattern::LitStr(value) => Some(value),
            _ => None,
        }
    }

    pub fn as_rest(&self) {
        match self {
            Pattern::Rest => {}
            _ => unreachable!(),
        }
    }

    pub fn to_rest(&self) -> Option<()> {
        match self {
            Pattern::Rest => Some(()),
            _ => None,
        }
    }

    pub fn as_tuple(&self) -> &TuplePattern {
        match self {
            Pattern::Tuple(pattern) => pattern,
            _ => unreachable!(),
        }
    }

    pub fn to_tuple(&self) -> Option<&TuplePattern> {
        match self {
            Pattern::Tuple(pattern) => Some(pattern),
            _ => None,
        }
    }

    pub fn as_underscore(&self) {
        match self {
            Pattern::Underscore => {}
            _ => unreachable!(),
        }
    }

    pub fn to_underscore(&self) -> Option<()> {
        match self {
            Pattern::Underscore => Some(()),
            _ => None,
        }
    }

    pub fn as_error(&self) {
        match self {
            Pattern::Error => {}
            _ => unreachable!(),
        }
    }

    pub fn to_error(&self) -> Option<()> {
        match self {
            Pattern::Error => Some(()),
            _ => None,
        }
    }
}

pub struct AltPattern {
    pub patterns: Vec<PatternId>,
}

pub struct CtorPattern {
    pub path: Vec<Name>,
    pub fields: Vec<CtorPatternField>,
}

pub struct CtorPatternField {
    pub name: Option<Name>,
    pub pattern: Option<PatternId>,
}

pub struct IdentPattern {
    pub name: Name,
    pub mutable: bool,
}

pub struct TuplePattern {
    pub patterns: Vec<PatternId>,
}

pub(crate) fn lower_pattern_opt(
    sa: &mut Sema,
    arena: &mut PatternArenaBuilder,
    file_id: SourceFileId,
    pattern: Option<ast::AstPattern>,
) -> PatternId {
    pattern
        .map(|pattern| lower_pattern(sa, arena, file_id, pattern))
        .unwrap_or_else(|| arena.alloc_pattern(Pattern::Error, None, None, None))
}

pub(crate) fn lower_pattern(
    sa: &mut Sema,
    arena: &mut PatternArenaBuilder,
    file_id: SourceFileId,
    pattern: ast::AstPattern,
) -> PatternId {
    let syntax_node_ptr = pattern.as_ptr();
    let syntax_node_id = pattern.as_syntax_node_id();
    let green_id = Some(pattern.id());

    let pattern = match pattern {
        ast::AstPattern::Alt(node) => Pattern::Alt(AltPattern {
            patterns: node
                .alts()
                .map(|pattern| lower_pattern(sa, arena, file_id, pattern))
                .collect(),
        }),
        ast::AstPattern::CtorPattern(node) => {
            let path = match lower_ctor_path(sa, node.path()) {
                Some(path) => path,
                None => {
                    return arena.alloc_pattern(
                        Pattern::Error,
                        Some(syntax_node_id),
                        Some(syntax_node_ptr),
                        green_id,
                    );
                }
            };
            let fields = node
                .param_list()
                .map(|list| {
                    list.items()
                        .map(|field| CtorPatternField {
                            name: field.ident().map(|name| sa.interner.intern(name.text())),
                            pattern: field
                                .pattern()
                                .map(|pattern| lower_pattern(sa, arena, file_id, pattern)),
                        })
                        .collect()
                })
                .unwrap_or_default();
            Pattern::Ctor(CtorPattern { path, fields })
        }
        ast::AstPattern::IdentPattern(node) => Pattern::Ident(IdentPattern {
            name: sa.interner.intern(node.name().text()),
            mutable: node.mutable(),
        }),
        ast::AstPattern::LitPatternBool(node) => {
            Pattern::LitBool(node.expr().as_lit_bool_expr().value())
        }
        ast::AstPattern::LitPatternChar(node) => Pattern::LitChar(lit_expr_text(node.expr())),
        ast::AstPattern::LitPatternInt(node) => Pattern::LitInt(lit_expr_text(node.expr())),
        ast::AstPattern::LitPatternFloat(node) => Pattern::LitFloat(lit_expr_text(node.expr())),
        ast::AstPattern::LitPatternStr(node) => Pattern::LitStr(lit_expr_text(node.expr())),
        ast::AstPattern::Rest(..) => Pattern::Rest,
        ast::AstPattern::TuplePattern(node) => Pattern::Tuple(TuplePattern {
            patterns: node
                .params()
                .map(|pattern| lower_pattern(sa, arena, file_id, pattern))
                .collect(),
        }),
        ast::AstPattern::UnderscorePattern(..) => Pattern::Underscore,
        ast::AstPattern::Error(..) => Pattern::Error,
    };

    arena.alloc_pattern(
        pattern,
        Some(syntax_node_id),
        Some(syntax_node_ptr),
        green_id,
    )
}

fn lower_ctor_path(sa: &mut Sema, node: ast::AstPathData) -> Option<Vec<Name>> {
    let mut path = Vec::new();

    for segment in node.segments() {
        match segment {
            ast::TypePathSegment::Name(token) | ast::TypePathSegment::UpcaseThis(token) => {
                path.push(sa.interner.intern(token.text()));
            }
            ast::TypePathSegment::Error(_) => return None,
        }
    }

    Some(path)
}

fn lit_expr_text(expr: ast::AstExpr) -> SmolStr {
    match expr {
        ast::AstExpr::LitCharExpr(node) => node.token_as_string().into(),
        ast::AstExpr::LitFloatExpr(node) => node.token_as_string().into(),
        ast::AstExpr::LitIntExpr(node) => node.token_as_string().into(),
        ast::AstExpr::LitStrExpr(node) => node.token_as_string().into(),
        ast::AstExpr::UnExpr(node) => {
            let opnd = node.opnd();
            if opnd.is_lit_int_expr() {
                format!("-{}", opnd.as_lit_int_expr().token_as_string()).into()
            } else if opnd.is_lit_float_expr() {
                format!("-{}", opnd.as_lit_float_expr().token_as_string()).into()
            } else {
                unreachable!()
            }
        }
        _ => unreachable!(),
    }
}
