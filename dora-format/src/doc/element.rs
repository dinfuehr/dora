use std::iter::Peekable;

use dora_parser::TokenKind;
use dora_parser::TokenKind::*;
use dora_parser::TokenKind::{LINE_COMMENT, MULTILINE_COMMENT, WHITESPACE};
use dora_parser::ast::{
    AstAlias, AstArgument, AstArgumentList, AstClass, AstConst, AstEnum, AstEnumVariant, AstExpr,
    AstExtern, AstField, AstFunction, AstGlobal, AstImpl, AstModifier, AstModifierList, AstModule,
    AstParam, AstStruct, AstTrait, AstType, AstTypeArgument, AstTypeArgumentList, AstTypeBounds,
    AstTypeParam, AstTypeParamList, AstUse, AstUseAs, AstUseAtom, AstUseGroup, AstUseName,
    AstUsePath, AstWhereClause, AstWhereClauseItem, SyntaxElement, SyntaxNodeBase,
};

use crate::doc::utils::{Options, if_node, if_token, print_node, print_token, print_token_opt};
use crate::doc::{BLOCK_INDENT, Formatter};
use crate::with_iter;

pub(crate) fn format_class(node: AstClass, f: &mut Formatter) {
    format_struct_like(node, f, CLASS_KW);
}

pub(crate) fn format_struct(node: AstStruct, f: &mut Formatter) {
    format_struct_like(node, f, STRUCT_KW);
}

pub(crate) fn format_alias(node: AstAlias, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        if if_node::<AstModifierList, _>(f, &mut iter) {
            print_node::<AstModifierList, _>(f, &mut iter);
        }

        print_token(f, &mut iter, TYPE_KW, &opt);
        f.text(" ");
        print_token(f, &mut iter, IDENTIFIER, &opt);

        if if_node::<AstTypeParamList, _>(f, &mut iter) {
            print_node::<AstTypeParamList, _>(f, &mut iter);
        }

        format_type_bounds_opt(f, &mut iter, &opt);

        if if_token(f, &mut iter, EQ) {
            f.text(" ");
            print_token(f, &mut iter, EQ, &opt);
            f.text(" ");
            print_node::<AstType, _>(f, &mut iter);
        }

        if if_node::<AstWhereClause, _>(f, &mut iter) {
            f.text(" ");
            print_node::<AstWhereClause, _>(f, &mut iter);
        }

        print_token(f, &mut iter, SEMICOLON, &opt);
    });
}

pub(crate) fn format_argument_list(node: AstArgumentList, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        print_token(f, &mut iter, L_PAREN, &opt);
        while !if_token(f, &mut iter, R_PAREN) {
            print_node::<AstArgument, _>(f, &mut iter);
            if if_token(f, &mut iter, COMMA) {
                print_token(f, &mut iter, COMMA, &opt);
                f.text(" ");
            }
        }
        print_token(f, &mut iter, R_PAREN, &opt);
    });
}

pub(crate) fn format_argument(node: AstArgument, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        if if_token(f, &mut iter, IDENTIFIER) {
            print_token(f, &mut iter, IDENTIFIER, &opt);
            if if_token(f, &mut iter, EQ) {
                f.text(" ");
                print_token(f, &mut iter, EQ, &opt);
                f.text(" ");
            }
        }
        print_node::<AstExpr, _>(f, &mut iter);
    });
}

pub(crate) fn format_field(node: AstField, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        if if_node::<AstModifierList, _>(f, &mut iter) {
            print_node::<AstModifierList, _>(f, &mut iter);
        }

        if if_token(f, &mut iter, IDENTIFIER) {
            print_token(f, &mut iter, IDENTIFIER, &opt);
            print_token(f, &mut iter, COLON, &opt);
            f.text(" ");
        }

        print_node::<AstType, _>(f, &mut iter);
    });
}

pub(crate) fn format_param(node: AstParam, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        print_node::<dora_parser::ast::AstPattern, _>(f, &mut iter);
        print_token(f, &mut iter, COLON, &opt);
        f.text(" ");
        print_node::<AstType, _>(f, &mut iter);
        if if_token(f, &mut iter, DOT_DOT_DOT) {
            print_token(f, &mut iter, DOT_DOT_DOT, &opt);
        }
    });
}

pub(crate) fn format_const(node: AstConst, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        if if_node::<AstModifierList, _>(f, &mut iter) {
            print_node::<AstModifierList, _>(f, &mut iter);
        }

        print_token(f, &mut iter, CONST_KW, &opt);
        f.text(" ");
        print_token(f, &mut iter, IDENTIFIER, &opt);
        print_token(f, &mut iter, COLON, &opt);
        f.text(" ");
        print_node::<AstType, _>(f, &mut iter);
        f.text(" ");
        print_token(f, &mut iter, EQ, &opt);
        f.text(" ");
        print_node::<AstExpr, _>(f, &mut iter);
        print_token(f, &mut iter, SEMICOLON, &opt);
    });
}

pub(crate) fn format_type_param_list(node: AstTypeParamList, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        print_token(f, &mut iter, L_BRACKET, &opt);
        while !if_token(f, &mut iter, R_BRACKET) {
            print_node::<AstTypeParam, _>(f, &mut iter);
            if if_token(f, &mut iter, COMMA) {
                print_token(f, &mut iter, COMMA, &opt);
                f.text(" ");
            }
        }
        print_token(f, &mut iter, R_BRACKET, &opt);
    });
}

pub(crate) fn format_type_param(node: AstTypeParam, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        print_token(f, &mut iter, IDENTIFIER, &opt);
        format_type_bounds_opt(f, &mut iter, &opt);
    });
}

pub(crate) fn format_type_bounds(node: AstTypeBounds, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        print_node::<AstType, _>(f, &mut iter);
        while if_token(f, &mut iter, ADD) {
            f.text(" ");
            print_token(f, &mut iter, ADD, &opt);
            f.text(" ");
            print_node::<AstType, _>(f, &mut iter);
        }
    });
}

pub(crate) fn format_type_argument_list(node: AstTypeArgumentList, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        print_token(f, &mut iter, L_BRACKET, &opt);
        while !if_token(f, &mut iter, R_BRACKET) {
            print_node::<AstTypeArgument, _>(f, &mut iter);
            if if_token(f, &mut iter, COMMA) {
                print_token(f, &mut iter, COMMA, &opt);
                f.text(" ");
            }
        }
        print_token(f, &mut iter, R_BRACKET, &opt);
    });
}

pub(crate) fn format_type_argument(node: AstTypeArgument, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        if if_token(f, &mut iter, IDENTIFIER) {
            print_token(f, &mut iter, IDENTIFIER, &opt);
            print_token(f, &mut iter, EQ, &opt);
        }

        print_node::<AstType, _>(f, &mut iter);
    });
}

pub(crate) fn format_enum(node: AstEnum, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        if if_node::<AstModifierList, _>(f, &mut iter) {
            print_node::<AstModifierList, _>(f, &mut iter);
        }

        print_token(f, &mut iter, ENUM_KW, &opt);
        f.text(" ");
        print_token(f, &mut iter, IDENTIFIER, &opt);

        if if_node::<AstTypeParamList, _>(f, &mut iter) {
            print_node::<AstTypeParamList, _>(f, &mut iter);
        }

        if if_node::<AstWhereClause, _>(f, &mut iter) {
            f.text(" ");
            print_node::<AstWhereClause, _>(f, &mut iter);
        }

        f.text(" ");
        print_token(f, &mut iter, L_BRACE, &opt);
        if if_token(f, &mut iter, R_BRACE) {
            print_token(f, &mut iter, R_BRACE, &opt);
        } else {
            f.hard_line();
            f.nest(BLOCK_INDENT, |f| {
                while !if_token(f, &mut iter, R_BRACE) {
                    print_node::<AstEnumVariant, _>(f, &mut iter);
                    print_token_opt(f, &mut iter, COMMA, &opt);
                    f.hard_line();
                }
            });
            print_token(f, &mut iter, R_BRACE, &opt);
        }
    });
}

pub(crate) fn format_enum_variant(node: AstEnumVariant, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        print_token(f, &mut iter, IDENTIFIER, &opt);

        if if_token(f, &mut iter, L_PAREN) {
            format_positional_fields(f, &mut iter, &opt);
        } else if if_token(f, &mut iter, L_BRACE) {
            f.text(" ");
            format_named_fields(f, &mut iter, &opt);
        }
    });
}

pub(crate) fn format_extern(node: AstExtern, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        if if_node::<AstModifierList, _>(f, &mut iter) {
            print_node::<AstModifierList, _>(f, &mut iter);
        }

        print_token(f, &mut iter, EXTERN_KW, &opt);
        f.text(" ");
        print_token(f, &mut iter, PACKAGE_KW, &opt);
        f.text(" ");
        print_token(f, &mut iter, IDENTIFIER, &opt);

        if if_token(f, &mut iter, AS_KW) {
            f.text(" ");
            print_token(f, &mut iter, AS_KW, &opt);
            f.text(" ");
            print_token(f, &mut iter, IDENTIFIER, &opt);
        }

        print_token(f, &mut iter, SEMICOLON, &opt);
    });
}

pub(crate) fn format_function(node: AstFunction, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        if if_node::<AstModifierList, _>(f, &mut iter) {
            print_node::<AstModifierList, _>(f, &mut iter);
        }

        print_token(f, &mut iter, FN_KW, &opt);
        f.text(" ");
        print_token(f, &mut iter, IDENTIFIER, &opt);

        if if_node::<AstTypeParamList, _>(f, &mut iter) {
            print_node::<AstTypeParamList, _>(f, &mut iter);
        }

        format_param_list(f, &mut iter, &opt);

        if if_token(f, &mut iter, COLON) {
            print_token(f, &mut iter, COLON, &opt);
            f.text(" ");
            print_node::<AstType, _>(f, &mut iter);
        }

        if if_node::<AstWhereClause, _>(f, &mut iter) {
            f.text(" ");
            print_node::<AstWhereClause, _>(f, &mut iter);
        }

        if if_token(f, &mut iter, SEMICOLON) {
            print_token(f, &mut iter, SEMICOLON, &opt);
        } else {
            f.text(" ");
            print_node::<dora_parser::ast::AstBlock, _>(f, &mut iter);
        }
    });
}

pub(crate) fn format_global(node: AstGlobal, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        if if_node::<AstModifierList, _>(f, &mut iter) {
            print_node::<AstModifierList, _>(f, &mut iter);
        }

        print_token(f, &mut iter, LET_KW, &opt);
        f.text(" ");
        if if_token(f, &mut iter, MUT_KW) {
            print_token(f, &mut iter, MUT_KW, &opt);
            f.text(" ");
        }
        print_token(f, &mut iter, IDENTIFIER, &opt);
        print_token(f, &mut iter, COLON, &opt);
        f.text(" ");
        print_node::<AstType, _>(f, &mut iter);

        if if_token(f, &mut iter, EQ) {
            f.text(" ");
            print_token(f, &mut iter, EQ, &opt);
            f.text(" ");
            print_node::<AstExpr, _>(f, &mut iter);
        }

        print_token(f, &mut iter, SEMICOLON, &opt);
    });
}

pub(crate) fn format_where_clause(node: AstWhereClause, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        print_token(f, &mut iter, WHERE_KW, &opt);
        f.text(" ");
        while if_node::<AstWhereClauseItem, _>(f, &mut iter) {
            print_node::<AstWhereClauseItem, _>(f, &mut iter);
            if if_token(f, &mut iter, COMMA) {
                print_token(f, &mut iter, COMMA, &opt);
                f.text(" ");
            }
        }
    });
}

pub(crate) fn format_where_clause_item(node: AstWhereClauseItem, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        print_node::<AstType, _>(f, &mut iter);
        print_token(f, &mut iter, COLON, &opt);
        f.text(" ");
        print_node::<AstType, _>(f, &mut iter);
        while if_token(f, &mut iter, ADD) {
            f.text(" ");
            print_token(f, &mut iter, ADD, &opt);
            f.text(" ");
            print_node::<AstType, _>(f, &mut iter);
        }
    });
}

pub(crate) fn format_impl(node: AstImpl, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        if if_node::<AstModifierList, _>(f, &mut iter) {
            print_node::<AstModifierList, _>(f, &mut iter);
        }

        print_token(f, &mut iter, IMPL_KW, &opt);

        if if_node::<AstTypeParamList, _>(f, &mut iter) {
            print_node::<AstTypeParamList, _>(f, &mut iter);
        }

        f.text(" ");
        print_node::<AstType, _>(f, &mut iter);

        if if_token(f, &mut iter, FOR_KW) {
            f.text(" ");
            print_token(f, &mut iter, FOR_KW, &opt);
            f.text(" ");
            print_node::<AstType, _>(f, &mut iter);
        }

        if if_node::<AstWhereClause, _>(f, &mut iter) {
            f.text(" ");
            print_node::<AstWhereClause, _>(f, &mut iter);
        }

        if if_node::<dora_parser::ast::AstElementList, _>(f, &mut iter) {
            f.text(" ");
            print_node::<dora_parser::ast::AstElementList, _>(f, &mut iter);
        }
    });
}

pub(crate) fn format_module(node: AstModule, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        if if_node::<AstModifierList, _>(f, &mut iter) {
            print_node::<AstModifierList, _>(f, &mut iter);
        }

        print_token(f, &mut iter, MOD_KW, &opt);
        f.text(" ");
        print_token(f, &mut iter, IDENTIFIER, &opt);

        if if_node::<dora_parser::ast::AstElementList, _>(f, &mut iter) {
            f.text(" ");
            print_node::<dora_parser::ast::AstElementList, _>(f, &mut iter);
        } else if if_token(f, &mut iter, SEMICOLON) {
            print_token(f, &mut iter, SEMICOLON, &opt);
        }
    });
}

pub(crate) fn format_modifier_list(node: AstModifierList, f: &mut Formatter) {
    with_iter!(node, f, |iter, _opt| {
        while if_node::<AstModifier, _>(f, &mut iter) {
            print_node::<AstModifier, _>(f, &mut iter);
            f.text(" ");
        }
    });
}

pub(crate) fn format_modifier(node: AstModifier, f: &mut Formatter) {
    for item in node.children_with_tokens() {
        match item {
            SyntaxElement::Token(token) => match token.syntax_kind() {
                WHITESPACE => {}
                LINE_COMMENT => {
                    f.token(token);
                    f.hard_line();
                }
                MULTILINE_COMMENT => {
                    f.token(token);
                }
                AT => {
                    f.token(token);
                }
                _ => {
                    f.token(token);
                }
            },
            SyntaxElement::Node(node) => {
                crate::doc::format_node(node, f);
            }
        }
    }
}

pub(crate) fn format_trait(node: AstTrait, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        if if_node::<AstModifierList, _>(f, &mut iter) {
            print_node::<AstModifierList, _>(f, &mut iter);
        }

        print_token(f, &mut iter, TRAIT_KW, &opt);
        f.text(" ");
        print_token(f, &mut iter, IDENTIFIER, &opt);

        if if_node::<AstTypeParamList, _>(f, &mut iter) {
            print_node::<AstTypeParamList, _>(f, &mut iter);
        }

        format_type_bounds_opt(f, &mut iter, &opt);

        if if_node::<AstWhereClause, _>(f, &mut iter) {
            f.text(" ");
            print_node::<AstWhereClause, _>(f, &mut iter);
        }

        if if_node::<dora_parser::ast::AstElementList, _>(f, &mut iter) {
            f.text(" ");
            print_node::<dora_parser::ast::AstElementList, _>(f, &mut iter);
        }
    });
}

pub(crate) fn format_use(node: AstUse, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        if if_node::<AstModifierList, _>(f, &mut iter) {
            print_node::<AstModifierList, _>(f, &mut iter);
        }

        print_token(f, &mut iter, USE_KW, &opt);
        f.text(" ");

        if if_node::<AstUseAtom, _>(f, &mut iter) {
            print_node::<AstUseAtom, _>(f, &mut iter);
        }

        print_token(f, &mut iter, COLON_COLON, &opt);
        print_node::<AstUsePath, _>(f, &mut iter);
        print_token(f, &mut iter, SEMICOLON, &opt);
    });
}

pub(crate) fn format_use_atom(node: AstUseAtom, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        if if_token(f, &mut iter, SELF_KW) {
            print_token(f, &mut iter, SELF_KW, &opt);
        } else if if_token(f, &mut iter, PACKAGE_KW) {
            print_token(f, &mut iter, PACKAGE_KW, &opt);
        } else if if_token(f, &mut iter, SUPER_KW) {
            print_token(f, &mut iter, SUPER_KW, &opt);
        } else {
            print_token(f, &mut iter, IDENTIFIER, &opt);
        }
    });
}

pub(crate) fn format_use_name(node: AstUseName, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        print_token(f, &mut iter, IDENTIFIER, &opt);
    });
}

pub(crate) fn format_use_as(node: AstUseAs, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        print_token(f, &mut iter, IDENTIFIER, &opt);
        f.text(" ");
        print_token(f, &mut iter, AS_KW, &opt);
        f.text(" ");
        if if_token(f, &mut iter, UNDERSCORE) {
            print_token(f, &mut iter, UNDERSCORE, &opt);
        } else {
            print_token(f, &mut iter, IDENTIFIER, &opt);
        }
    });
}

pub(crate) fn format_use_group(node: AstUseGroup, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        print_token(f, &mut iter, L_BRACE, &opt);

        if if_token(f, &mut iter, R_BRACE) {
            print_token(f, &mut iter, R_BRACE, &opt);
            return;
        }

        while !if_token(f, &mut iter, R_BRACE) {
            print_node::<AstUsePath, _>(f, &mut iter);
            if if_token(f, &mut iter, COMMA) {
                print_token(f, &mut iter, COMMA, &opt);
                f.text(" ");
            }
        }

        print_token(f, &mut iter, R_BRACE, &opt);
    });
}

pub(crate) fn format_use_path(node: AstUsePath, f: &mut Formatter) {
    for item in node.children_with_tokens() {
        match item {
            SyntaxElement::Token(token) => match token.syntax_kind() {
                WHITESPACE => {}
                LINE_COMMENT => {
                    f.token(token);
                    f.hard_line();
                }
                MULTILINE_COMMENT => {
                    f.token(token);
                }
                COLON_COLON => {
                    f.token(token);
                }
                _ => {
                    f.token(token);
                }
            },
            SyntaxElement::Node(node) => {
                crate::doc::format_node(node, f);
            }
        }
    }
}

fn format_struct_like<T>(node: T, f: &mut Formatter, keyword: TokenKind)
where
    T: dora_parser::ast::SyntaxNodeBase,
{
    with_iter!(node, f, |iter, opt| {
        if if_node::<AstModifierList, _>(f, &mut iter) {
            print_node::<AstModifierList, _>(f, &mut iter);
        }

        print_token(f, &mut iter, keyword, &opt);
        f.text(" ");
        print_token(f, &mut iter, IDENTIFIER, &opt);

        if if_node::<AstTypeParamList, _>(f, &mut iter) {
            print_node::<AstTypeParamList, _>(f, &mut iter);
        }

        let mut had_where_clause = false;
        if if_node::<AstWhereClause, _>(f, &mut iter) {
            f.text(" ");
            print_node::<AstWhereClause, _>(f, &mut iter);
            had_where_clause = true;
        }

        if if_token(f, &mut iter, L_PAREN) {
            if had_where_clause {
                f.text(" ");
            }
            format_positional_fields(f, &mut iter, &opt);
        } else if if_token(f, &mut iter, L_BRACE) {
            f.text(" ");
            format_named_fields(f, &mut iter, &opt);
        }
    });
}

fn format_named_fields<I>(f: &mut Formatter, iter: &mut Peekable<I>, opt: &Options)
where
    I: Iterator<Item = SyntaxElement>,
{
    print_token(f, iter, L_BRACE, opt);
    if if_token(f, iter, R_BRACE) {
        print_token(f, iter, R_BRACE, opt);
        return;
    }

    f.hard_line();
    f.nest(BLOCK_INDENT, |f| {
        while !if_token(f, iter, R_BRACE) {
            print_node::<AstField, _>(f, iter);
            print_token_opt(f, iter, COMMA, opt);
            f.hard_line();
        }
    });
    print_token(f, iter, R_BRACE, opt);
}

fn format_positional_fields<I>(f: &mut Formatter, iter: &mut Peekable<I>, opt: &Options)
where
    I: Iterator<Item = SyntaxElement>,
{
    print_token(f, iter, L_PAREN, opt);
    while !if_token(f, iter, R_PAREN) {
        print_node::<AstField, _>(f, iter);
        if if_token(f, iter, COMMA) {
            print_token(f, iter, COMMA, opt);
            f.text(" ");
        }
    }
    print_token(f, iter, R_PAREN, opt);
}

fn format_param_list<I>(f: &mut Formatter, iter: &mut Peekable<I>, opt: &Options)
where
    I: Iterator<Item = SyntaxElement>,
{
    print_token(f, iter, L_PAREN, opt);
    while !if_token(f, iter, R_PAREN) {
        print_node::<AstParam, _>(f, iter);
        if if_token(f, iter, COMMA) {
            print_token(f, iter, COMMA, opt);
            f.text(" ");
        }
    }
    print_token(f, iter, R_PAREN, opt);
}

fn format_type_bounds_opt<I>(f: &mut Formatter, iter: &mut Peekable<I>, opt: &Options)
where
    I: Iterator<Item = SyntaxElement>,
{
    if if_token(f, iter, COLON) {
        print_token(f, iter, COLON, opt);
        f.text(" ");
        if if_node::<AstTypeBounds, _>(f, iter) {
            print_node::<AstTypeBounds, _>(f, iter);
        } else {
            print_node::<AstType, _>(f, iter);
            while if_token(f, iter, ADD) {
                f.text(" ");
                print_token(f, iter, ADD, opt);
                f.text(" ");
                print_node::<AstType, _>(f, iter);
            }
        }
    } else if if_node::<AstTypeBounds, _>(f, iter) {
        print_node::<AstTypeBounds, _>(f, iter);
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use dora_parser::Parser;

    use crate::doc;
    use crate::render;

    fn format_to_string(input: &str) -> String {
        let content = Arc::new(input.to_string());
        let parser = Parser::from_shared_string(content);
        let (file, errors) = parser.parse();
        assert!(errors.is_empty(), "unexpected parse errors: {:?}", errors);

        let root = file.root();
        let (arena, root_id) = doc::format(root);
        render::render_doc(&arena, root_id)
    }

    #[test]
    fn formats_struct_named_fields() {
        let input = "struct  Point  {  x : Int32 , y : Int32 }";
        let expected = "struct Point {\n    x: Int32,\n    y: Int32\n}\n";
        assert_eq!(format_to_string(input), expected);
    }

    #[test]
    fn formats_struct_positional_fields_with_type_params() {
        let input = "struct  Foo [ T ] ( T , String )";
        let expected = "struct Foo[T](T, String)\n";
        assert_eq!(format_to_string(input), expected);
    }

    #[test]
    fn formats_class_named_fields() {
        let input = "class  Foo  {  bar : Int32 }";
        let expected = "class Foo {\n    bar: Int32\n}\n";
        assert_eq!(format_to_string(input), expected);
    }

    #[test]
    fn formats_const_definition() {
        let input = "const  Foo  :  Int32  =  1 ;";
        let expected = "const Foo: Int32 = 1;\n";
        assert_eq!(format_to_string(input), expected);
    }

    #[test]
    fn formats_global_definition() {
        let input = "let  mut  bar  :  Int32  =  2 ;";
        let expected = "let mut bar: Int32 = 2;\n";
        assert_eq!(format_to_string(input), expected);
    }

    #[test]
    fn formats_alias_with_bounds() {
        let input = "type  Foo [ T ] : Bar  =  Baz ;";
        let expected = "type Foo[T]: Bar = Baz;\n";
        assert_eq!(format_to_string(input), expected);
    }

    #[test]
    fn formats_enum_variants() {
        let input = "enum  Foo {  A ( Int32 ) , B }";
        let expected = "enum Foo {\n    A(Int32),\n    B\n}\n";
        assert_eq!(format_to_string(input), expected);
    }

    #[test]
    fn formats_extern_package() {
        let input = "extern  package  foo  as  bar ;";
        let expected = "extern package foo as bar;\n";
        assert_eq!(format_to_string(input), expected);
    }

    #[test]
    fn formats_function_with_type_params() {
        let input = "fn  foo [ T ] ( a : T ) : Int where T : Bar { }";
        let expected = "fn foo[T](a: T): Int where T: Bar {}\n";
        assert_eq!(format_to_string(input), expected);
    }

    #[test]
    fn formats_impl_with_type_params() {
        let input = "impl [ T ] Foo [ T ] for Bar { }";
        let expected = "impl[T] Foo[T] for Bar {}\n";
        assert_eq!(format_to_string(input), expected);
    }

    #[test]
    fn formats_module_with_body() {
        let input = "mod  foo { fn  bar ( ) { } }";
        let expected = "mod foo {\n    fn bar() {}\n}\n";
        assert_eq!(format_to_string(input), expected);
    }

    #[test]
    fn formats_module_without_body() {
        let input = "mod  foo ;";
        let expected = "mod foo;\n";
        assert_eq!(format_to_string(input), expected);
    }

    #[test]
    fn formats_trait_with_method() {
        let input = "trait  Foo : Bar { fn  test ( ) : Int32 ; }";
        let expected = "trait Foo: Bar {\n    fn test(): Int32;\n}\n";
        assert_eq!(format_to_string(input), expected);
    }

    #[test]
    fn formats_use_group() {
        let input = "use  self  :: { C , A , B } ;";
        let expected = "use self::{C, A, B};\n";
        assert_eq!(format_to_string(input), expected);
    }
}
