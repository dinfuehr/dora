use dora_parser::TokenKind;
use dora_parser::TokenKind::*;
use dora_parser::TokenKind::{LINE_COMMENT, MULTILINE_COMMENT, NEWLINE, WHITESPACE};
use dora_parser::ast::{
    AstAlias, AstClass, AstConst, AstEnum, AstEnumVariant, AstEnumVariantList, AstExpr, AstExtern,
    AstFieldDecl, AstFunction, AstGlobal, AstImpl, AstModifier, AstModifierList, AstModule,
    AstNamedFieldList, AstParam, AstParamList, AstStruct, AstTrait, AstType, AstTypeArgument,
    AstTypeArgumentList, AstTypeBounds, AstTypeParam, AstTypeParamList, AstUnnamedFieldList,
    AstWhereClause, AstWhereClauseItem, SyntaxElement, SyntaxNodeBase,
};

use crate::doc::utils::{
    Iter, Options, is_node, is_token, print_comma_list_grouped, print_next_token, print_node,
    print_token, print_trivia,
};
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
        if is_node::<AstModifierList>(&iter) {
            print_node::<AstModifierList>(f, &mut iter, &opt);
        }

        print_token(f, &mut iter, TYPE_KW, &opt);
        f.text(" ");
        print_token(f, &mut iter, IDENTIFIER, &opt);

        if is_node::<AstTypeParamList>(&iter) {
            print_node::<AstTypeParamList>(f, &mut iter, &opt);
        }

        format_type_bounds_opt(f, &mut iter, &opt);

        if is_token(&mut iter, EQ) {
            f.text(" ");
            print_token(f, &mut iter, EQ, &opt);
            f.text(" ");
            print_node::<AstType>(f, &mut iter, &opt);
        }

        if is_node::<AstWhereClause>(&iter) {
            f.text(" ");
            print_node::<AstWhereClause>(f, &mut iter, &opt);
        }

        print_token(f, &mut iter, SEMICOLON, &opt);
    });
}

pub(crate) fn format_field(node: AstFieldDecl, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        if is_node::<AstModifierList>(&iter) {
            print_node::<AstModifierList>(f, &mut iter, &opt);
        }

        if is_token(&mut iter, IDENTIFIER) {
            print_token(f, &mut iter, IDENTIFIER, &opt);
            print_token(f, &mut iter, COLON, &opt);
            f.text(" ");
        }

        print_node::<AstType>(f, &mut iter, &opt);
    });
}

pub(crate) fn format_param(node: AstParam, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        print_node::<dora_parser::ast::AstPattern>(f, &mut iter, &opt);
        print_token(f, &mut iter, COLON, &opt);
        f.text(" ");
        print_node::<AstType>(f, &mut iter, &opt);
        if is_token(&mut iter, DOT_DOT_DOT) {
            print_token(f, &mut iter, DOT_DOT_DOT, &opt);
        }
    });
}

pub(crate) fn format_param_list(node: AstParamList, f: &mut Formatter) {
    let opt = Options::new();
    print_comma_list_grouped(f, &node, &opt);
}

pub(crate) fn format_const(node: AstConst, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        if is_node::<AstModifierList>(&iter) {
            print_node::<AstModifierList>(f, &mut iter, &opt);
        }

        print_token(f, &mut iter, CONST_KW, &opt);
        f.text(" ");
        print_token(f, &mut iter, IDENTIFIER, &opt);
        print_token(f, &mut iter, COLON, &opt);
        f.text(" ");
        print_node::<AstType>(f, &mut iter, &opt);
        f.text(" ");
        print_token(f, &mut iter, EQ, &opt);
        f.text(" ");
        print_node::<AstExpr>(f, &mut iter, &opt);
        print_token(f, &mut iter, SEMICOLON, &opt);
    });
}

pub(crate) fn format_type_param_list(node: AstTypeParamList, f: &mut Formatter) {
    let opt = Options::new();
    print_comma_list_grouped(f, &node, &opt);
}

pub(crate) fn format_type_param(node: AstTypeParam, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        print_token(f, &mut iter, IDENTIFIER, &opt);
        format_type_bounds_opt(f, &mut iter, &opt);
    });
}

pub(crate) fn format_type_bounds(node: AstTypeBounds, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        print_node::<AstType>(f, &mut iter, &opt);
        while is_token(&mut iter, ADD) {
            f.text(" ");
            print_token(f, &mut iter, ADD, &opt);
            f.text(" ");
            print_node::<AstType>(f, &mut iter, &opt);
        }
    });
}

pub(crate) fn format_type_argument_list(node: AstTypeArgumentList, f: &mut Formatter) {
    let opt = Options::new();
    print_comma_list_grouped(f, &node, &opt);
}

pub(crate) fn format_type_argument(node: AstTypeArgument, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        if is_token(&mut iter, IDENTIFIER) {
            print_token(f, &mut iter, IDENTIFIER, &opt);
            print_token(f, &mut iter, EQ, &opt);
        }

        print_node::<AstType>(f, &mut iter, &opt);
    });
}

pub(crate) fn format_enum(node: AstEnum, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        if is_node::<AstModifierList>(&iter) {
            print_node::<AstModifierList>(f, &mut iter, &opt);
        }

        print_token(f, &mut iter, ENUM_KW, &opt);
        f.text(" ");
        print_token(f, &mut iter, IDENTIFIER, &opt);

        if is_node::<AstTypeParamList>(&iter) {
            print_node::<AstTypeParamList>(f, &mut iter, &opt);
        }

        if is_node::<AstWhereClause>(&iter) {
            f.text(" ");
            print_node::<AstWhereClause>(f, &mut iter, &opt);
        }

        if is_node::<AstEnumVariantList>(&iter) {
            f.text(" ");
            print_node::<AstEnumVariantList>(f, &mut iter, &opt);
        }
    });
}

pub(crate) fn format_enum_variant(node: AstEnumVariant, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        print_token(f, &mut iter, IDENTIFIER, &opt);

        if is_node::<AstUnnamedFieldList>(&iter) {
            print_node::<AstUnnamedFieldList>(f, &mut iter, &opt);
        } else if is_node::<AstNamedFieldList>(&iter) {
            f.text(" ");
            print_node::<AstNamedFieldList>(f, &mut iter, &opt);
        }
    });
}

pub(crate) fn format_extern(node: AstExtern, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        if is_node::<AstModifierList>(&iter) {
            print_node::<AstModifierList>(f, &mut iter, &opt);
        }

        print_token(f, &mut iter, EXTERN_KW, &opt);
        f.text(" ");
        print_token(f, &mut iter, PACKAGE_KW, &opt);
        f.text(" ");
        print_token(f, &mut iter, IDENTIFIER, &opt);

        if is_token(&mut iter, AS_KW) {
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
        if is_node::<AstModifierList>(&iter) {
            print_node::<AstModifierList>(f, &mut iter, &opt);
        }

        print_token(f, &mut iter, FN_KW, &opt);
        f.text(" ");
        print_token(f, &mut iter, IDENTIFIER, &opt);

        if is_node::<AstTypeParamList>(&iter) {
            print_node::<AstTypeParamList>(f, &mut iter, &opt);
        }

        print_node::<AstParamList>(f, &mut iter, &opt);

        if is_token(&mut iter, COLON) {
            print_token(f, &mut iter, COLON, &opt);
            f.text(" ");
            print_node::<AstType>(f, &mut iter, &opt);
        }

        if is_node::<AstWhereClause>(&iter) {
            f.text(" ");
            print_node::<AstWhereClause>(f, &mut iter, &opt);
        }

        if is_token(&mut iter, SEMICOLON) {
            print_token(f, &mut iter, SEMICOLON, &opt);
        } else {
            f.text(" ");
            print_node::<dora_parser::ast::AstBlockExpr>(f, &mut iter, &opt);
        }
    });
}

pub(crate) fn format_global(node: AstGlobal, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        if is_node::<AstModifierList>(&iter) {
            print_node::<AstModifierList>(f, &mut iter, &opt);
        }

        print_token(f, &mut iter, LET_KW, &opt);
        f.text(" ");
        if is_token(&mut iter, MUT_KW) {
            print_token(f, &mut iter, MUT_KW, &opt);
            f.text(" ");
        }
        print_token(f, &mut iter, IDENTIFIER, &opt);
        print_token(f, &mut iter, COLON, &opt);
        f.text(" ");
        print_node::<AstType>(f, &mut iter, &opt);

        if is_token(&mut iter, EQ) {
            f.text(" ");
            print_token(f, &mut iter, EQ, &opt);
            f.text(" ");
            print_node::<AstExpr>(f, &mut iter, &opt);
        }

        print_token(f, &mut iter, SEMICOLON, &opt);
    });
}

pub(crate) fn format_where_clause(node: AstWhereClause, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        print_token(f, &mut iter, WHERE_KW, &opt);
        f.text(" ");
        while is_node::<AstWhereClauseItem>(&mut iter) {
            print_node::<AstWhereClauseItem>(f, &mut iter, &opt);
            if is_token(&mut iter, COMMA) {
                print_token(f, &mut iter, COMMA, &opt);
                f.text(" ");
            }
        }
    });
}

pub(crate) fn format_where_clause_item(node: AstWhereClauseItem, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        print_node::<AstType>(f, &mut iter, &opt);
        print_token(f, &mut iter, COLON, &opt);
        f.text(" ");
        print_node::<AstType>(f, &mut iter, &opt);
        while is_token(&mut iter, ADD) {
            f.text(" ");
            print_token(f, &mut iter, ADD, &opt);
            f.text(" ");
            print_node::<AstType>(f, &mut iter, &opt);
        }
    });
}

pub(crate) fn format_impl(node: AstImpl, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        if is_node::<AstModifierList>(&iter) {
            print_node::<AstModifierList>(f, &mut iter, &opt);
        }

        print_token(f, &mut iter, IMPL_KW, &opt);

        if is_node::<AstTypeParamList>(&iter) {
            print_node::<AstTypeParamList>(f, &mut iter, &opt);
        }

        f.text(" ");
        print_node::<AstType>(f, &mut iter, &opt);

        if is_token(&mut iter, FOR_KW) {
            f.text(" ");
            print_token(f, &mut iter, FOR_KW, &opt);
            f.text(" ");
            print_node::<AstType>(f, &mut iter, &opt);
        }

        if is_node::<AstWhereClause>(&iter) {
            f.text(" ");
            print_node::<AstWhereClause>(f, &mut iter, &opt);
        }

        if is_node::<dora_parser::ast::AstElementList>(&iter) {
            f.text(" ");
            print_node::<dora_parser::ast::AstElementList>(f, &mut iter, &opt);
        }
    });
}

pub(crate) fn format_module(node: AstModule, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        if is_node::<AstModifierList>(&iter) {
            print_node::<AstModifierList>(f, &mut iter, &opt);
        }

        print_token(f, &mut iter, MOD_KW, &opt);
        f.text(" ");
        print_token(f, &mut iter, IDENTIFIER, &opt);

        if is_node::<dora_parser::ast::AstElementList>(&iter) {
            f.text(" ");
            print_node::<dora_parser::ast::AstElementList>(f, &mut iter, &opt);
        } else if is_token(&mut iter, SEMICOLON) {
            print_token(f, &mut iter, SEMICOLON, &opt);
        }
    });
}

pub(crate) fn format_modifier_list(node: AstModifierList, f: &mut Formatter) {
    with_iter!(node, f, |iter, opt| {
        while is_node::<AstModifier>(&iter) {
            print_node::<AstModifier>(f, &mut iter, &opt);
            f.text(" ");
        }
    });
}

pub(crate) fn format_modifier(node: AstModifier, f: &mut Formatter) {
    for item in node.children_with_tokens() {
        match item {
            SyntaxElement::Token(token) => match token.syntax_kind() {
                NEWLINE | WHITESPACE => {}
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
        if is_node::<AstModifierList>(&mut iter) {
            print_node::<AstModifierList>(f, &mut iter, &opt);
        }

        print_token(f, &mut iter, TRAIT_KW, &opt);
        f.text(" ");
        print_token(f, &mut iter, IDENTIFIER, &opt);

        if is_node::<AstTypeParamList>(&mut iter) {
            print_node::<AstTypeParamList>(f, &mut iter, &opt);
        }

        format_type_bounds_opt(f, &mut iter, &opt);

        if is_node::<AstWhereClause>(&iter) {
            f.text(" ");
            print_node::<AstWhereClause>(f, &mut iter, &opt);
        }

        if is_node::<dora_parser::ast::AstElementList>(&iter) {
            f.text(" ");
            print_node::<dora_parser::ast::AstElementList>(f, &mut iter, &opt);
        }
    });
}

fn format_struct_like<T>(node: T, f: &mut Formatter, keyword: TokenKind)
where
    T: dora_parser::ast::SyntaxNodeBase,
{
    with_iter!(node, f, |iter, opt| {
        if is_node::<AstModifierList>(&mut iter) {
            print_node::<AstModifierList>(f, &mut iter, &opt);
        }

        print_token(f, &mut iter, keyword, &opt);
        f.text(" ");
        print_token(f, &mut iter, IDENTIFIER, &opt);

        if is_node::<AstTypeParamList>(&mut iter) {
            print_node::<AstTypeParamList>(f, &mut iter, &opt);
        }

        let mut had_where_clause = false;
        if is_node::<AstWhereClause>(&iter) {
            f.text(" ");
            print_node::<AstWhereClause>(f, &mut iter, &opt);
            had_where_clause = true;
        }

        if is_node::<AstUnnamedFieldList>(&iter) {
            if had_where_clause {
                f.text(" ");
            }
            print_node::<AstUnnamedFieldList>(f, &mut iter, &opt);
        } else if is_node::<AstNamedFieldList>(&iter) {
            f.text(" ");
            print_node::<AstNamedFieldList>(f, &mut iter, &opt);
        }
    });
}

pub(crate) fn format_named_field_list(node: AstNamedFieldList, f: &mut Formatter) {
    let opt = Options::keep_empty_lines();
    let iter = &mut node.children_with_tokens();
    print_next_token(f, iter, &opt);

    if is_token(iter, LIST_ITEM) {
        f.hard_line();
        f.nest(BLOCK_INDENT, |f| {
            while is_token(iter, LIST_ITEM) {
                print_trivia(f, iter, &opt);
                let list_item_node = iter
                    .next()
                    .expect("missing list item")
                    .to_node()
                    .expect("expected list item node");

                let mut list_item_iter = list_item_node.children_with_tokens();
                print_trivia(f, &mut list_item_iter, &opt);
                print_node::<AstFieldDecl>(f, &mut list_item_iter, &opt);
                f.text(",");
                f.hard_line();
            }
        });
    }

    print_next_token(f, iter, &opt);
}

pub(crate) fn format_enum_variant_list(node: AstEnumVariantList, f: &mut Formatter) {
    let opt = Options::keep_empty_lines();
    let iter = &mut node.children_with_tokens();
    print_next_token(f, iter, &opt);

    if is_token(iter, LIST_ITEM) {
        f.hard_line();
        f.nest(BLOCK_INDENT, |f| {
            while is_token(iter, LIST_ITEM) {
                print_trivia(f, iter, &opt);
                let list_item_node = iter
                    .next()
                    .expect("missing list item")
                    .to_node()
                    .expect("expected list item node");

                let mut list_item_iter = list_item_node.children_with_tokens();
                print_trivia(f, &mut list_item_iter, &opt);
                print_node::<AstEnumVariant>(f, &mut list_item_iter, &opt);
                f.text(",");
                f.hard_line();
            }
        });
    }

    print_next_token(f, iter, &opt);
}

pub(crate) fn format_unnamed_field_list(node: AstUnnamedFieldList, f: &mut Formatter) {
    let opt = Options::new();
    print_comma_list_grouped(f, &node, &opt);
}

fn format_type_bounds_opt(f: &mut Formatter, iter: &mut Iter<'_>, opt: &Options) {
    if is_token(iter, COLON) {
        print_token(f, iter, COLON, opt);
        f.text(" ");
        if is_node::<AstTypeBounds>(iter) {
            print_node::<AstTypeBounds>(f, iter, &opt);
        } else {
            print_node::<AstType>(f, iter, &opt);
            while is_token(iter, ADD) {
                f.text(" ");
                print_token(f, iter, ADD, opt);
                f.text(" ");
                print_node::<AstType>(f, iter, &opt);
            }
        }
    } else if is_node::<AstTypeBounds>(iter) {
        print_node::<AstTypeBounds>(f, iter, &opt);
    }
}

#[cfg(test)]
mod tests {
    use crate::test_utils::assert_source;

    #[test]
    fn formats_struct_named_fields() {
        let input = "struct  Point  {  x : Int32 , y : Int32 }";
        let expected = "struct Point {\n    x: Int32,\n    y: Int32,\n}\n";
        assert_source(input, expected);
    }

    #[test]
    fn formats_struct_positional_fields_with_type_params() {
        let input = "struct  Foo [ T ] ( T , String )";
        let expected = "struct Foo[T](T, String)\n";
        assert_source(input, expected);
    }

    #[test]
    fn formats_class_named_fields() {
        let input = "class  Foo  {  bar : Int32 }";
        let expected = "class Foo {\n    bar: Int32,\n}\n";
        assert_source(input, expected);
    }

    #[test]
    fn formats_const_definition() {
        let input = "const  Foo  :  Int32  =  1 ;";
        let expected = "const Foo: Int32 = 1;\n";
        assert_source(input, expected);
    }

    #[test]
    fn formats_global_definition() {
        let input = "let  mut  bar  :  Int32  =  2 ;";
        let expected = "let mut bar: Int32 = 2;\n";
        assert_source(input, expected);
    }

    #[test]
    fn formats_alias_with_bounds() {
        let input = "type  Foo [ T ] : Bar  =  Baz ;";
        let expected = "type Foo[T]: Bar = Baz;\n";
        assert_source(input, expected);
    }

    #[test]
    fn formats_enum_variants() {
        let input = "enum  Foo {  A ( Int32 ) , B }";
        let expected = "enum Foo {\n    A(Int32),\n    B,\n}\n";
        assert_source(input, expected);
    }

    #[test]
    fn formats_enum_with_gap_lines() {
        let input = "enum Foo { A,\n\nB,\n\nC }";
        let expected = "enum Foo {\n    A,\n\n    B,\n\n    C,\n}\n";
        assert_source(input, expected);
    }

    #[test]
    fn formats_struct_with_gap_lines() {
        let input = "struct Foo { a: Int32,\n\nb: Int32 }";
        let expected = "struct Foo {\n    a: Int32,\n\n    b: Int32,\n}\n";
        assert_source(input, expected);
    }

    #[test]
    fn formats_class_with_gap_lines() {
        let input = "class Foo { a: Int32,\n\nb: Int32 }";
        let expected = "class Foo {\n    a: Int32,\n\n    b: Int32,\n}\n";
        assert_source(input, expected);
    }

    #[test]
    fn formats_extern_package() {
        let input = "extern  package  foo  as  bar ;";
        let expected = "extern package foo as bar;\n";
        assert_source(input, expected);
    }

    #[test]
    fn formats_function_with_type_params() {
        let input = "fn  foo [ T ] ( a : T ) : Int where T : Bar { }";
        let expected = "fn foo[T](a: T): Int where T: Bar {}\n";
        assert_source(input, expected);
    }

    #[test]
    fn formats_impl_with_type_params() {
        let input = "impl [ T ] Foo [ T ] for Bar { }";
        let expected = "impl[T] Foo[T] for Bar {}\n";
        assert_source(input, expected);
    }

    #[test]
    fn formats_module_with_body() {
        let input = "mod  foo { fn  bar ( ) { } }";
        let expected = "mod foo {\n    fn bar() {}\n}\n";
        assert_source(input, expected);
    }

    #[test]
    fn formats_module_without_body() {
        let input = "mod  foo ;";
        let expected = "mod foo;\n";
        assert_source(input, expected);
    }

    #[test]
    fn formats_trait_with_method() {
        let input = "trait  Foo : Bar { fn  test ( ) : Int32 ; }";
        let expected = "trait Foo: Bar {\n    fn test(): Int32;\n}\n";
        assert_source(input, expected);
    }

    #[test]
    fn formats_empty_input() {
        let input = "";
        assert_source(input, "");
    }

    #[test]
    fn formats_empty_main() {
        let input = "fn  main (  ) {  }";
        assert_source(input, "fn main() {}\n");
    }

    #[test]
    fn formats_empty_with_comment() {
        let input = "fn  main (  ) { // test\n  }";
        assert_source(input, "fn main() {\n    // test\n}\n");
    }

    #[test]
    fn formats_fct_with_simple_let() {
        let input = "fn  main (  ) {  let  x  =  1 ; }";
        assert_source(input, "fn main() {\n    let x = 1;\n}\n");
    }

    #[test]
    fn formats_fct_with_literal_types() {
        let input = "fn  main (  ) {  let  f  =  1.5 ; let  s  =  \"hi\" ; let  b  =  true ; }";
        assert_source(
            input,
            "fn main() {\n    let f = 1.5;\n    let s = \"hi\";\n    let b = true;\n}\n",
        );
    }

    #[test]
    fn formats_fct_with_multiple_stmts() {
        let input = "fn  main (  ) {  1;2;3;4 }";
        assert_source(input, "fn main() {\n    1;\n    2;\n    3;\n    4\n}\n");
    }

    #[test]
    fn formats_fct_on_same_line() {
        let input = "fn f(){} fn g(){}";
        assert_source(input, "fn f() {}\n\nfn g() {}\n");
    }
}
