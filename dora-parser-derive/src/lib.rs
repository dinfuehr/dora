use proc_macro::TokenStream;
use quote::quote;
use syn::{Data, DataEnum, DeriveInput, Fields, Ident, Path, parse_macro_input, parse_quote};

#[proc_macro_derive(AstEnum)]
pub fn derive_ast_enum(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let data_enum = match &input.data {
        Data::Enum(data) => data,
        _ => panic!("AstEnum can only be derived for enums"),
    };

    // Verify this is a unit variant enum (NodeKind style)
    let is_unit_variant_enum = data_enum
        .variants
        .iter()
        .all(|v| matches!(v.fields, Fields::Unit));
    if !is_unit_variant_enum {
        panic!("AstEnum requires unit variants (e.g., `Alias` not `Alias(Alias)`)");
    }

    // Generate the full Ast enum and all implementations from NodeKind
    generate_from_node_kind(data_enum)
}

struct EnumVariantInfo {
    name: Ident,
    token_kind: Path,
}

fn collect_variant_info(data_enum: &DataEnum) -> Vec<EnumVariantInfo> {
    data_enum
        .variants
        .iter()
        .map(|variant| {
            let name = variant.ident.clone();

            let token_kind_variant =
                Ident::new(&to_upper_snake_case(&name.to_string()), name.span());
            let token_kind = parse_quote!(TokenKind::#token_kind_variant);

            EnumVariantInfo { name, token_kind }
        })
        .collect()
}

fn generate_from_node_kind(data_enum: &DataEnum) -> TokenStream {
    let variant_info = collect_variant_info(data_enum);

    // Generate SyntaxNode::is_XXX(), SyntaxNode::to_XXX() and SyntaxNode::as_XXX() methods.
    let syntax_node_methods = generate_syntax_node_methods_per_variant(&variant_info);

    // Generate visitor pattern code
    let visitor_code = generate_visitor_pattern(&variant_info);

    // Generate wrappers for extra AST nodes declared via attribute.
    let ast_wrappers = generate_ast_wrappers(&variant_info);

    let expanded = quote! {
        #syntax_node_methods
        #visitor_code
        #ast_wrappers
    };

    TokenStream::from(expanded)
}

fn to_snake_case(s: &str) -> String {
    let mut result = String::new();
    let mut chars = s.chars().peekable();

    while let Some(ch) = chars.next() {
        if ch.is_uppercase() {
            if !result.is_empty() {
                result.push('_');
            }
            result.push(ch.to_lowercase().next().unwrap());
        } else {
            result.push(ch);
        }
    }

    result
}

fn to_upper_snake_case(s: &str) -> String {
    to_snake_case(s).to_uppercase()
}

fn generate_syntax_node_methods_per_variant(
    variant_info: &[EnumVariantInfo],
) -> proc_macro2::TokenStream {
    let as_methods: Vec<_> = variant_info
        .iter()
        .map(|info| {
            let variant_name = &info.name;
            let method_name_str = to_snake_case(&variant_name.to_string());
            let is_method_name =
                syn::Ident::new(&format!("is_{}", method_name_str), variant_name.span());
            let to_method_name =
                syn::Ident::new(&format!("to_{}", method_name_str), variant_name.span());
            let as_method_name =
                syn::Ident::new(&format!("as_{}", method_name_str), variant_name.span());
            let ast_type_name =
                syn::Ident::new(&format!("Ast{}", variant_name), variant_name.span());
            let token_kind = &info.token_kind;

            quote! {
                pub fn #is_method_name(&self) -> bool {
                    self.syntax_kind() == #token_kind
                }

                pub fn #to_method_name(&self) -> Option<#ast_type_name> {
                    if self.#is_method_name() {
                        Some(#ast_type_name(self.clone()))
                    } else {
                        None
                    }
                }

                pub fn #as_method_name(&self) -> #ast_type_name {
                    self.#to_method_name().expect("wrong node kind")
                }
            }
        })
        .collect();

    quote! {
        impl SyntaxNode {
            #(#as_methods)*
        }
    }
}

fn generate_visitor_pattern(variant_info: &[EnumVariantInfo]) -> proc_macro2::TokenStream {
    let trait_methods: Vec<_> = variant_info
        .iter()
        .map(|info| {
            let variant_name = &info.name;
            let method_name_str = to_snake_case(&variant_name.to_string());
            let visit_method =
                syn::Ident::new(&format!("visit_{}", method_name_str), variant_name.span());
            let ast_type_name =
                syn::Ident::new(&format!("Ast{}", variant_name), variant_name.span());

            quote! {
                fn #visit_method(&mut self, _ast_node: #ast_type_name) {
                    walk_children(self, _ast_node);
                }
            }
        })
        .collect();

    let visit_match_arms: Vec<_> = variant_info
        .iter()
        .map(|info| {
            let variant_name = &info.name;
            let method_name_str = to_snake_case(&variant_name.to_string());
            let visit_method =
                syn::Ident::new(&format!("visit_{}", method_name_str), variant_name.span());
            let as_method =
                syn::Ident::new(&format!("as_{}", method_name_str), variant_name.span());
            let token_kind = &info.token_kind;

            quote! {
                #token_kind => v.#visit_method(node.clone().#as_method()),
            }
        })
        .collect();

    quote! {
        pub trait Visitor: Sized {
            #(#trait_methods)*
        }

        pub fn visit_node<V: Visitor>(v: &mut V, node: SyntaxNode) {
            match node.syntax_kind() {
                #(#visit_match_arms)*
                _ => unreachable!("unhandled node {}", node.syntax_kind()),
            }
        }
    }
}

fn generate_ast_wrappers(variant_info: &[EnumVariantInfo]) -> proc_macro2::TokenStream {
    let wrappers: Vec<_> = variant_info
        .iter()
        .map(|info| {
            let name = &info.name;
            let ast_name = syn::Ident::new(&format!("Ast{}", name), name.span());
            let token_kind = &info.token_kind;

            quote! {
                #[derive(Clone, Debug)]
                #[repr(transparent)]
                pub struct #ast_name(SyntaxNode);

                impl #ast_name {
                    pub fn syntax_kind() -> TokenKind {
                        #token_kind
                    }
                }

                impl SyntaxNodeBase for #ast_name {
                    fn cast(node: SyntaxNode) -> Option<Self> {
                        if node.syntax_kind() == #token_kind {
                            Some(Self(node))
                        } else {
                            None
                        }
                    }

                    fn syntax_node(&self) -> &SyntaxNode {
                        &self.0
                    }

                    fn unwrap(self) -> SyntaxNode {
                        self.0
                    }
                }
            }
        })
        .collect();

    quote! {
        #(#wrappers)*
    }
}

#[proc_macro_derive(AstUnion, attributes(ast_union_kind))]
pub fn derive_ast_union(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let enum_name = &input.ident;

    let data_enum = match &input.data {
        Data::Enum(data) => data,
        _ => panic!("AstUnion can only be derived for enums"),
    };

    // Verify this is NOT a unit variant enum (opposite of AstEnum)
    let has_data_variants = data_enum
        .variants
        .iter()
        .any(|v| !matches!(v.fields, Fields::Unit));

    if !has_data_variants {
        panic!(
            "AstUnion requires data variants (e.g., `RegularType(AstRegularType)`). Use #[derive(AstEnum)] for unit variant enums instead."
        );
    }

    // Generate SyntaxNodeBase implementation
    generate_union_impl(enum_name, data_enum)
}

fn generate_union_impl(enum_name: &syn::Ident, data_enum: &DataEnum) -> TokenStream {
    let variant_info: Vec<_> = data_enum
        .variants
        .iter()
        .map(|variant| {
            let variant_name = variant.ident.clone();
            let inner_type = extract_single_unnamed_field_type(variant);
            let token_kind_variant = ast_union_token_kind_override(variant);
            (variant_name, inner_type, token_kind_variant)
        })
        .collect();

    // Generate match arms for each method
    let id_arms: Vec<_> = variant_info
        .iter()
        .map(|(variant_name, _, _)| quote! { #enum_name::#variant_name(inner) => inner.id() })
        .collect();

    // Generate cast method match arms
    let cast_arms: Vec<_> = variant_info
        .iter()
        .map(|(variant_name, inner_type, token_kind_variant)| {
            quote! { TokenKind::#token_kind_variant => Some(#enum_name::#variant_name(#inner_type::cast(node).unwrap())) }
        })
        .collect();

    // Generate is_XXX methods
    let is_methods: Vec<_> = variant_info
        .iter()
        .map(|(variant_name, _, _)| {
            let method_name =
                syn::Ident::new(&format!("is_{}", to_snake_case(&variant_name.to_string())), variant_name.span());
            quote! { pub fn #method_name(&self) -> bool { matches!(self, #enum_name::#variant_name(_)) } }
        })
        .collect();

    // Generate to_XXX methods
    let to_methods: Vec<_> = variant_info
        .iter()
        .map(|(variant_name, inner_type, _)| {
            let method_name =
                syn::Ident::new(&format!("to_{}", to_snake_case(&variant_name.to_string())), variant_name.span());
            quote! { pub fn #method_name(self) -> Option<#inner_type> { match self { #enum_name::#variant_name(value) => Some(value), _ => None, } } }
        })
        .collect();

    // Generate as_XXX methods
    let as_methods: Vec<_> = variant_info
        .iter()
        .map(|(variant_name, inner_type, _)| {
            let method_name =
                syn::Ident::new(&format!("as_{}", to_snake_case(&variant_name.to_string())), variant_name.span());
            quote! { pub fn #method_name(self) -> #inner_type { match self { #enum_name::#variant_name(value) => value, _ => unreachable!(), } } }
        })
        .collect();

    let from_impls: Vec<_> = variant_info
        .iter()
        .map(|(variant_name, inner_type, _)| {
            quote! { impl From<#inner_type> for #enum_name { fn from(value: #inner_type) -> Self { #enum_name::#variant_name(value) } } }
        })
        .collect();

    let span_arms: Vec<_> = variant_info
        .iter()
        .map(|(variant_name, _, _)| quote! { #enum_name::#variant_name(inner) => inner.span() })
        .collect();

    let full_span_arms: Vec<_> = variant_info
        .iter()
        .map(
            |(variant_name, _, _)| quote! { #enum_name::#variant_name(inner) => inner.full_span() },
        )
        .collect();

    let text_length_arms: Vec<_> = variant_info
        .iter()
        .map(|(variant_name, _, _)| quote! { #enum_name::#variant_name(inner) => inner.text_length() })
        .collect();

    let file_arms: Vec<_> = variant_info
        .iter()
        .map(|(variant_name, _, _)| quote! { #enum_name::#variant_name(inner) => inner.file() })
        .collect();

    let children_arms: Vec<_> = variant_info
        .iter()
        .map(|(variant_name, _, _)| quote! { #enum_name::#variant_name(inner) => inner.children().collect() })
        .collect();

    let children_with_tokens_arms: Vec<_> = variant_info
        .iter()
        .map(|(variant_name, _, _)| quote! { #enum_name::#variant_name(inner) => inner.children_with_tokens() })
        .collect();

    let syntax_kind_arms: Vec<_> = variant_info
        .iter()
        .map(|(variant_name, _, _)| quote! { #enum_name::#variant_name(inner) => inner.syntax_kind() })
        .collect();

    let as_ptr_arms: Vec<_> = variant_info
        .iter()
        .map(|(variant_name, _, _)| quote! { #enum_name::#variant_name(inner) => inner.as_ptr() })
        .collect();

    let syntax_node_arms: Vec<_> = variant_info
        .iter()
        .map(|(variant_name, _, _)| quote! { #enum_name::#variant_name(inner) => SyntaxNodeBase::syntax_node(inner) })
        .collect();

    let parent_arms: Vec<_> = variant_info
        .iter()
        .map(|(variant_name, _, _)| quote! { #enum_name::#variant_name(inner) => inner.parent() })
        .collect();

    let offset_arms: Vec<_> = variant_info
        .iter()
        .map(|(variant_name, _, _)| quote! { #enum_name::#variant_name(inner) => inner.offset() })
        .collect();

    let unwrap_arms: Vec<_> = variant_info
        .iter()
        .map(|(variant_name, _, _)| quote! { #enum_name::#variant_name(inner) => inner.unwrap() })
        .collect();

    let expanded = quote! {
        impl #enum_name {
            pub(crate) fn cast(node: SyntaxNode) -> Option<#enum_name> {
                match node.syntax_kind() {
                    #(#cast_arms,)*
                    _ => None,
                }
            }

            #(#is_methods)*
            #(#to_methods)*
            #(#as_methods)*
        }

        impl SyntaxNodeBase for #enum_name {
            fn id(&self) -> GreenId {
                match self {
                    #(#id_arms),*
                }
            }

            fn cast(node: SyntaxNode) -> Option<Self> {
                Self::cast(node)
            }

            fn span(&self) -> Span {
                match self {
                    #(#span_arms),*
                }
            }

            fn full_span(&self) -> Span {
                match self {
                    #(#full_span_arms),*
                }
            }

            fn text_length(&self) -> u32 {
                match self {
                    #(#text_length_arms),*
                }
            }

            fn file(&self) -> &File {
                match self {
                    #(#file_arms),*
                }
            }

            fn children(&self) -> impl Iterator<Item = SyntaxNode> {
                let children: Vec<_> = match self {
                    #(#children_arms),*
                };
                children.into_iter()
            }

            fn children_with_tokens(&self) -> SyntaxElementIter<'_> {
                match self {
                    #(#children_with_tokens_arms),*
                }
            }

            fn syntax_kind(&self) -> TokenKind {
                match self {
                    #(#syntax_kind_arms),*
                }
            }

            fn as_ptr(&self) -> SyntaxNodePtr {
                match self {
                    #(#as_ptr_arms),*
                }
            }

            fn syntax_node(&self) -> &SyntaxNode {
                match self {
                    #(#syntax_node_arms),*
                }
            }

            fn parent(&self) -> Option<SyntaxNode> {
                match self {
                    #(#parent_arms),*
                }
            }

            fn offset(&self) -> TextOffset {
                match self {
                    #(#offset_arms),*
                }
            }

            fn unwrap(self) -> SyntaxNode {
                match self {
                    #(#unwrap_arms),*
                }
            }
        }

        #(#from_impls)*
    };

    TokenStream::from(expanded)
}

fn ast_union_token_kind_override(variant: &syn::Variant) -> syn::Ident {
    for attr in &variant.attrs {
        if attr.path().is_ident("ast_union_kind") {
            return attr
                .parse_args::<syn::Ident>()
                .expect("ast_union_kind expects a TokenKind identifier");
        }
    }

    syn::Ident::new(
        &to_upper_snake_case(&variant.ident.to_string()),
        variant.ident.span(),
    )
}

fn extract_single_unnamed_field_type(variant: &syn::Variant) -> syn::Type {
    match &variant.fields {
        Fields::Unnamed(fields) if fields.unnamed.len() == 1 => fields.unnamed[0].ty.clone(),
        _ => panic!("AstUnion variants must be tuple variants with a single field"),
    }
}
