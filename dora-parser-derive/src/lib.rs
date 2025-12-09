use proc_macro::TokenStream;
use quote::quote;
use syn::{
    Attribute, Data, DataEnum, DeriveInput, Fields, GenericArgument, Ident, Meta, Path,
    PathArguments, Token, Type,
    parse::{Parse, ParseStream},
    parse_macro_input, parse_quote,
    spanned::Spanned,
};

#[proc_macro_derive(AstNode, attributes(ast_node_ref))]
pub fn derive_ast_node(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = &input.ident;
    let name_str = name.to_string();

    // Create the Ast-prefixed version name
    let struct_ast_node_name = syn::Ident::new(&format!("Ast{}", name), name.span());

    // Create the uppercase snake_case variant name for TokenKind
    let token_kind_variant = syn::Ident::new(&to_upper_snake_case(&name_str), name.span());

    let (struct_def, children_impl, ast_impl) = match &input.data {
        Data::Struct(data) => {
            // Generate the Ast-prefixed struct definition as a newtype wrapper around SyntaxNode
            let struct_def = quote! {
                #[derive(Clone, Debug)]
                #[repr(transparent)]
                pub struct #struct_ast_node_name(SyntaxNode);
            };

            // Generate field accessor methods for Ast* struct
            let field_methods = match &data.fields {
                Fields::Named(fields) => {
                    let methods = fields.named.iter().map(|field| {
                        let field_name = field.ident.as_ref().unwrap();
                        let field_type = &field.ty;
                        let to_method = syn::Ident::new(
                            &format!("to_{}", to_snake_case(&name.to_string())),
                            name.span(),
                        );
                        let field_name_at =
                            syn::Ident::new(&format!("{}_at", field_name), field_name.span());
                        let field_name_len =
                            syn::Ident::new(&format!("{}_len", field_name), field_name.span());

                        let raw_accessor_name =
                            syn::Ident::new(&format!("raw_{}", field_name), field_name.span());

                        let raw_accessor = quote! {
                            pub fn #raw_accessor_name(&self) -> &#field_type {
                                &self.syntax_node().file().node(self.syntax_node().id()).#to_method().unwrap().#field_name
                            }
                        };

                        if field_name == "span" || field_name == "full_span" || field_name == "green_elements" || field_name == "text_length" {
                            return quote! {};
                        }

                        // Get return type from attribute (defaults to SyntaxNode)
                        let return_type = get_return_type_from_attrs(&field.attrs);

                        let wrapper_accessor = if is_ast_id(field_type) {
                            quote! {
                                pub fn #field_name(&self) -> #return_type {
                                    let ast_id = *self.#raw_accessor_name();
                                    let file = self.syntax_node().file().clone();
                                    let child_ast = file.node(ast_id);
                                    let offset = TextOffset(child_ast.full_span().start());
                                    let syntax_node = SyntaxNode::new(file, ast_id, offset, Some(self.syntax_node().clone()));
                                    #return_type::cast(syntax_node).unwrap()
                                }
                            }
                        } else if is_option_ast_id(field_type) {
                            quote! {
                                pub fn #field_name(&self) -> Option<#return_type> {
                                    let ast_id = *self.#raw_accessor_name();
                                    ast_id.map(|ast_id| {
                                        let file = self.syntax_node().file().clone();
                                        let child_ast = file.node(ast_id);
                                        let offset = TextOffset(child_ast.full_span().start());
                                        let syntax_node = SyntaxNode::new(file, ast_id, offset, Some(self.syntax_node().clone()));
                                        #return_type::cast(syntax_node).unwrap()
                                    })
                                }
                            }
                        } else if is_vec_ast_id(field_type) {
                            quote! {
                                pub fn #field_name_at(&self, idx: usize) -> #return_type {
                                    let vec = self.#raw_accessor_name();
                                    let ast_id = vec[idx];
                                    let file = self.syntax_node().file().clone();
                                    let child_ast = file.node(ast_id);
                                    let offset = TextOffset(child_ast.full_span().start());
                                    let syntax_node = SyntaxNode::new(file, ast_id, offset, Some(self.syntax_node().clone()));
                                    #return_type::cast(syntax_node).unwrap()
                                }

                                pub fn #field_name_len(&self) -> usize {
                                    self.#raw_accessor_name().len()
                                }

                                pub fn #field_name(&self) -> AstIdIter<'_, #return_type> {
                                    let vec = self.#raw_accessor_name();
                                    AstIdIter::new(self.syntax_node().file().clone(), vec.as_slice())
                                }
                            }
                        } else if is_likely_copy_type(field_type) {
                            quote! {
                                pub fn #field_name(&self) -> #field_type {
                                    self.syntax_node().file().node(self.syntax_node().id()).#to_method().unwrap().#field_name
                                }
                            }
                        } else {
                            quote! {
                                pub fn #field_name(&self) -> &#field_type {
                                    self.#raw_accessor_name()
                                }
                            }
                        };

                        quote! {
                            #wrapper_accessor
                            #raw_accessor
                        }
                    });

                    quote! {
                        #(#methods)*
                    }
                }
                _ => quote! {},
            };

            let impl_block = quote! {
                impl #name {
                    pub fn node_name(&self) -> &'static str {
                        #name_str
                    }
                }
            };

            let is_method_name = syn::Ident::new(
                &format!("is_{}", to_snake_case(&name.to_string())),
                name.span(),
            );

            let ast_impl_block = quote! {
                impl SyntaxNodeBase for #struct_ast_node_name {
                    fn id(&self) -> AstId {
                        self.syntax_node().id()
                    }

                    fn cast(node: SyntaxNode) -> Option<Self> {
                        if node.syntax_kind() == TokenKind::#token_kind_variant {
                            Some(Self(node))
                        } else {
                            None
                        }
                    }

                    fn span(&self) -> Span {
                        self.syntax_node().span()
                    }

                    fn full_span(&self) -> Span {
                        self.syntax_node().full_span()
                    }

                    fn text_length(&self) -> u32 {
                        self.syntax_node().text_length()
                    }

                    fn file(&self) -> &File {
                        self.syntax_node().file()
                    }

                    fn children(&self) -> impl Iterator<Item = SyntaxNode> {
                        self.syntax_node().children()
                    }

                    fn children_with_tokens(&self) -> GreenElementIter<'_> {
                        self.syntax_node().children_with_tokens()
                    }

                    fn syntax_kind(&self) -> TokenKind {
                        self.syntax_node().syntax_kind()
                    }

                    fn as_ptr(&self) -> SyntaxNodePtr {
                        self.syntax_node().as_ptr()
                    }

                    fn syntax_node(&self) -> &SyntaxNode {
                        &self.0
                    }

                    fn parent(&self) -> Option<SyntaxNode> {
                        self.syntax_node().parent()
                    }

                    fn offset(&self) -> TextOffset {
                        self.syntax_node().offset()
                    }

                    fn unwrap(self) -> SyntaxNode {
                        self.0
                    }
                }

                impl #struct_ast_node_name {
                    pub fn new(file: File, id: AstId, offset: TextOffset, parent: Option<SyntaxNode>) -> Self {
                        assert!(file.node(id).#is_method_name());
                        #struct_ast_node_name(SyntaxNode::new(file, id, offset, parent))
                    }

                    pub fn syntax_node(&self) -> &SyntaxNode {
                        &self.0
                    }

                    pub fn offset(&self) -> TextOffset {
                        self.syntax_node().offset()
                    }

                    pub fn parent(&self) -> Option<SyntaxNode> {
                        self.syntax_node().parent()
                    }

                    pub fn syntax_kind() -> TokenKind {
                        TokenKind::#token_kind_variant
                    }

                    #field_methods
                }
            };

            (struct_def, impl_block, ast_impl_block)
        }
        _ => panic!("SyntaxNode can only be derived for structs"),
    };

    let expanded = quote! {
        #struct_def

        #ast_impl

        #children_impl
    };

    TokenStream::from(expanded)
}

fn is_likely_copy_type(ty: &Type) -> bool {
    if let Type::Path(type_path) = ty {
        if let Some(segment) = type_path.path.segments.last() {
            let type_name = segment.ident.to_string();

            // Common Copy types
            match type_name.as_str() {
                // Primitive types
                "bool" | "char" | "i8" | "i16" | "i32" | "i64" | "i128" | "isize" | "u8"
                | "u16" | "u32" | "u64" | "u128" | "usize" | "f32" | "f64" => true,

                // Known Copy types from the codebase
                "AstId" | "Span" | "TokenKind" | "UnOp" | "BinOp" | "CmpOp" | "FieldNameStyle" => {
                    true
                }

                // Option<T> is Copy if T is Copy (we'll check the inner type)
                "Option" => {
                    if let PathArguments::AngleBracketed(args) = &segment.arguments {
                        if let Some(GenericArgument::Type(inner_ty)) = args.args.first() {
                            return is_likely_copy_type(inner_ty);
                        }
                    }
                    false
                }

                _ => false,
            }
        } else {
            false
        }
    } else {
        false
    }
}

fn is_ast_id(ty: &Type) -> bool {
    if let Type::Path(type_path) = ty {
        if let Some(segment) = type_path.path.segments.last() {
            return segment.ident == "AstId";
        }
    }
    false
}

fn is_option_ast_id(ty: &Type) -> bool {
    if let Type::Path(type_path) = ty {
        if let Some(segment) = type_path.path.segments.last() {
            if segment.ident == "Option" {
                if let PathArguments::AngleBracketed(args) = &segment.arguments {
                    if let Some(GenericArgument::Type(inner_ty)) = args.args.first() {
                        return is_ast_id(inner_ty);
                    }
                }
            }
        }
    }
    false
}

fn is_vec_ast_id(ty: &Type) -> bool {
    if let Type::Path(type_path) = ty {
        if let Some(segment) = type_path.path.segments.last() {
            if segment.ident == "Vec" {
                if let PathArguments::AngleBracketed(args) = &segment.arguments {
                    if let Some(GenericArgument::Type(inner_ty)) = args.args.first() {
                        return is_ast_id(inner_ty);
                    }
                }
            }
        }
    }
    false
}

#[proc_macro_derive(AstEnum, attributes(extra_ast_node))]
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
    is_extra: bool,
}

struct ExtraAstNodeAttrArgs {
    _kind_ident: Ident,
    _eq_token: Token![=],
    token_kind: Path,
}

impl Parse for ExtraAstNodeAttrArgs {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let kind_ident: Ident = input.parse()?;

        if kind_ident != "kind" {
            return Err(syn::Error::new(
                kind_ident.span(),
                "expected `kind = TokenKind::XYZ`",
            ));
        }

        Ok(ExtraAstNodeAttrArgs {
            _kind_ident: kind_ident,
            _eq_token: input.parse()?,
            token_kind: input.parse()?,
        })
    }
}

fn collect_variant_info(data_enum: &DataEnum) -> Vec<EnumVariantInfo> {
    data_enum
        .variants
        .iter()
        .map(|variant| {
            let name = variant.ident.clone();
            let extra_token_kind = parse_extra_ast_node_attr(variant);
            let is_extra = extra_token_kind.is_some();

            let token_kind = extra_token_kind.clone().unwrap_or_else(|| {
                let token_kind_variant =
                    Ident::new(&to_upper_snake_case(&name.to_string()), name.span());
                parse_quote!(TokenKind::#token_kind_variant)
            });

            EnumVariantInfo {
                name,
                token_kind,
                is_extra,
            }
        })
        .collect()
}

fn parse_extra_ast_node_attr(variant: &syn::Variant) -> Option<Path> {
    for attr in &variant.attrs {
        if attr.path().is_ident("extra_ast_node") {
            let args = attr
                .parse_args::<ExtraAstNodeAttrArgs>()
                .expect("invalid #[extra_ast_node] usage, expected `#[extra_ast_node(kind = TokenKind::XYZ)]`");
            return Some(args.token_kind);
        }
    }

    None
}

fn generate_from_node_kind(data_enum: &DataEnum) -> TokenStream {
    let variant_info = collect_variant_info(data_enum);

    // Generate SyntaxNode::is_XXX(), SyntaxNode::to_XXX() and SyntaxNode::as_XXX() methods.
    let syntax_node_methods = generate_syntax_node_methods_per_variant(&variant_info);

    // Generate visitor pattern code
    let visitor_code = generate_visitor_pattern(&variant_info);

    // Generate wrappers for extra AST nodes declared via attribute.
    let extra_wrappers = generate_extra_ast_wrappers(&variant_info);

    let expanded = quote! {
        #syntax_node_methods

        #visitor_code

        #extra_wrappers
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

fn get_return_type_from_attrs(attrs: &[Attribute]) -> syn::Ident {
    for attr in attrs {
        if attr.path().is_ident("ast_node_ref") {
            if let Meta::List(meta_list) = &attr.meta {
                // Parse the tokens inside the attribute: #[ast_node_ref(TypeName)]
                let tokens = &meta_list.tokens;
                let tokens_str = tokens.to_string();
                let type_name = tokens_str.trim();

                if !type_name.is_empty() {
                    let prefixed_name = format!("Ast{}", type_name);
                    return syn::Ident::new(&prefixed_name, attr.meta.span());
                }
            }
        }
    }
    // Default to SyntaxNode if no attribute is specified
    syn::Ident::new("SyntaxNode", proc_macro2::Span::call_site())
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

fn generate_extra_ast_wrappers(variant_info: &[EnumVariantInfo]) -> proc_macro2::TokenStream {
    let wrappers: Vec<_> = variant_info
        .iter()
        .filter(|info| info.is_extra)
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

            fn children_with_tokens(&self) -> GreenElementIter<'_> {
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

struct ExtraAstNodeArgs {
    name: Ident,
    _comma: Token![,],
    token_kind: Path,
}

impl Parse for ExtraAstNodeArgs {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(ExtraAstNodeArgs {
            name: input.parse()?,
            _comma: input.parse()?,
            token_kind: input.parse()?,
        })
    }
}

/// Generate an Ast*-style wrapper for an existing TokenKind without adding it to NodeKind.
/// Usage: extra_ast_node!(NameExprRef, TokenKind::NAME_EXPR)
#[proc_macro]
pub fn extra_ast_node(input: TokenStream) -> TokenStream {
    let ExtraAstNodeArgs {
        name, token_kind, ..
    } = parse_macro_input!(input as ExtraAstNodeArgs);

    let ast_name = Ident::new(&format!("Ast{}", name), name.span());
    let method_suffix = to_snake_case(&name.to_string());
    let is_method = Ident::new(&format!("is_{}", method_suffix), name.span());
    let to_method = Ident::new(&format!("to_{}", method_suffix), name.span());
    let as_method = Ident::new(&format!("as_{}", method_suffix), name.span());

    let expanded = quote! {
        #[derive(Clone, Debug)]
        #[repr(transparent)]
        pub struct #ast_name(SyntaxNode);

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

        impl SyntaxNode {
            pub fn #is_method(&self) -> bool {
                self.syntax_kind() == #token_kind
            }

            pub fn #to_method(&self) -> Option<#ast_name> {
                if self.syntax_kind() == #token_kind {
                    Some(#ast_name(self.clone()))
                } else {
                    None
                }
            }

            pub fn #as_method(&self) -> #ast_name {
                self.#to_method().expect("wrong node kind")
            }
        }
    };

    TokenStream::from(expanded)
}
