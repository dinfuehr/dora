use proc_macro::TokenStream;
use quote::quote;
use syn::{
    Attribute, Data, DataEnum, DeriveInput, Fields, GenericArgument, Meta, PathArguments, Type,
    parse_macro_input, spanned::Spanned,
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
            let field_collection = match &data.fields {
                Fields::Named(fields) => {
                    let field_handlers = fields.named.iter().filter_map(|field| {
                        let field_name = field.ident.as_ref().unwrap();
                        let field_type = &field.ty;

                        // Only generate handler if the type is AstId-related
                        if is_ast_id_related(field_type) {
                            Some(generate_field_handler(field_name, field_type))
                        } else {
                            None
                        }
                    });

                    quote! {
                        let mut children = Vec::new();
                        #(#field_handlers)*
                        children
                    }
                }
                _ => quote! { Vec::new() },
            };

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

                        // Get return type from attribute (defaults to SyntaxNode)
                        let return_type = get_return_type_from_attrs(&field.attrs);

                        let wrapper_accessor = if is_ast_id(field_type) {
                            quote! {
                                pub fn #field_name(&self) -> #return_type {
                                    let ast_id = *self.#raw_accessor_name();
                                    let file = self.syntax_node().file().clone();
                                    let child_ast = file.node(ast_id);
                                    let offset = TextOffset(child_ast.span().start());
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
                                        let offset = TextOffset(child_ast.span().start());
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
                                    let offset = TextOffset(child_ast.span().start());
                                    let syntax_node = SyntaxNode::new(file, ast_id, offset, Some(self.syntax_node().clone()));
                                    #return_type::cast(syntax_node).unwrap()
                                }

                                pub fn #field_name_len(&self) -> usize {
                                    self.#raw_accessor_name().len()
                                }

                                pub fn #field_name(&self) -> AstIdIterator<'_, #return_type> {
                                    let vec = self.#raw_accessor_name();
                                    AstIdIterator::new(self.syntax_node().file().clone(), vec.as_slice())
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
                    pub fn children(&self) -> Vec<AstId> {
                        #field_collection
                    }

                    pub fn name(&self) -> &'static str {
                        #name_str
                    }
                }
            };

            let is_method_name = syn::Ident::new(
                &format!("is_{}", to_snake_case(&name.to_string())),
                name.span(),
            );

            let to_method_name = syn::Ident::new(
                &format!("to_{}", to_snake_case(&name.to_string())),
                name.span(),
            );

            let ast_impl_block = quote! {
                impl SyntaxNodeBase for #struct_ast_node_name {
                    type RawType = #name;

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

                    fn raw_node(&self) -> &#name {
                        self.syntax_node().file().node(self.syntax_node().id())
                            .#to_method_name()
                            .expect(concat!("expected ", stringify!(#name)))
                    }

                    fn span(&self) -> Span {
                        self.syntax_node().span()
                    }

                    fn text_length(&self) -> u32 {
                        self.syntax_node().text_length()
                    }

                    fn file(&self) -> &File {
                        self.syntax_node().file()
                    }

                    fn node_children(&self) -> impl Iterator<Item = SyntaxNode> {
                        self.syntax_node().node_children()
                    }

                    fn node_kind(&self) -> NodeKind {
                        self.syntax_node().node_kind()
                    }

                    fn syntax_kind(&self) -> TokenKind {
                        self.syntax_node().syntax_kind()
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

                    pub fn children(&self) -> GreenElementIterator<'_> {
                        self.syntax_node().children()
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

fn is_ast_id_related(ty: &Type) -> bool {
    is_ast_id(ty) || is_option_ast_id(ty) || is_vec_ast_id(ty) || is_option_vec_ast_id(ty)
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

fn generate_field_handler(field_name: &syn::Ident, field_type: &Type) -> proc_macro2::TokenStream {
    if is_option_vec_ast_id(field_type) {
        quote! {
            if let Some(ref elements) = self.#field_name {
                children.extend(elements);
            }
        }
    } else if is_option_ast_id(field_type) {
        quote! {
            if let Some(id) = self.#field_name {
                children.push(id);
            }
        }
    } else if is_vec_ast_id(field_type) {
        quote! {
            children.extend(&self.#field_name);
        }
    } else if is_ast_id(field_type) {
        quote! {
            children.push(self.#field_name);
        }
    } else {
        quote! {}
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

fn is_option_vec_ast_id(ty: &Type) -> bool {
    if let Type::Path(type_path) = ty {
        if let Some(segment) = type_path.path.segments.last() {
            if segment.ident == "Option" {
                if let PathArguments::AngleBracketed(args) = &segment.arguments {
                    if let Some(GenericArgument::Type(inner_ty)) = args.args.first() {
                        return is_vec_ast_id(inner_ty);
                    }
                }
            }
        }
    }
    false
}

#[proc_macro_derive(AstEnum)]
pub fn derive_ast_enum(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let enum_name = &input.ident;

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
    generate_from_node_kind(enum_name, data_enum)
}

fn generate_from_node_kind(enum_name: &syn::Ident, data_enum: &DataEnum) -> TokenStream {
    // Generate Ast enum variants
    let ast_variants: Vec<_> = data_enum
        .variants
        .iter()
        .map(|variant| {
            let variant_name = &variant.ident;
            quote! {
                #variant_name(#variant_name)
            }
        })
        .collect();

    // Generate Ast::is_XXX(), Ast::to_XXX() and Ast::as_XXX() methods.
    let variant_methods = generate_ast_methods_per_variant(data_enum);
    // Generate Ast::span().
    let span_method = generate_ast_span_method(data_enum);
    // Generate Ast::green_children().
    let green_children_method = generate_ast_green_children_method(data_enum);
    // Generate Ast::text_length().
    let text_length_method = generate_ast_text_length_method(data_enum);
    // Generate Ast::name().
    let name_method = generate_ast_name_method(data_enum);
    // Generate Ast::children().
    let children_method = generate_ast_children_method(data_enum);
    // Generate Ast::kind().
    let kind_method = generate_ast_kind_method(data_enum, enum_name);
    // Generate Ast::syntax_kind().
    let syntax_kind_method = generate_ast_syntax_kind_method(data_enum);

    // Generate SyntaxNode::is_XXX(), SyntaxNode::to_XXX() and SyntaxNode::as_XXX() methods.
    let syntax_node_methods = generate_syntax_node_methods_per_variant(data_enum);

    // Generate visitor pattern code
    let visitor_code = generate_visitor_pattern(data_enum);

    let expanded = quote! {
        #[derive(Clone, Debug)]
        pub enum Ast {
            #(#ast_variants),*
        }

        impl Ast {
            #span_method
            #green_children_method
            #text_length_method
            #name_method
            #children_method
            #kind_method
            #syntax_kind_method
            #variant_methods
        }

        #syntax_node_methods

        #visitor_code
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

fn generate_ast_methods_per_variant(data_enum: &DataEnum) -> proc_macro2::TokenStream {
    let mut all_methods = Vec::new();

    for variant in &data_enum.variants {
        let variant_name = &variant.ident;
        let method_name_str = to_snake_case(&variant_name.to_string());
        let is_method_name =
            syn::Ident::new(&format!("is_{}", method_name_str), variant_name.span());
        let to_method_name =
            syn::Ident::new(&format!("to_{}", method_name_str), variant_name.span());
        let as_method_name =
            syn::Ident::new(&format!("as_{}", method_name_str), variant_name.span());

        let per_variant_methods = quote! {
            pub fn #is_method_name(&self) -> bool {
                matches!(self, Self::#variant_name(..))
            }

            pub fn #to_method_name(&self) -> Option<&#variant_name> {
                match self {
                    Self::#variant_name(inner) => Some(inner),
                    _ => None,
                }
            }

            pub fn #as_method_name(&self) -> &#variant_name {
                self.#to_method_name().expect("wrong node kind")
            }
        };

        all_methods.push(per_variant_methods);
    }

    quote! {
        #(#all_methods)*
    }
}

fn generate_ast_span_method(data_enum: &DataEnum) -> proc_macro2::TokenStream {
    let match_arms: Vec<_> = data_enum
        .variants
        .iter()
        .map(|variant| {
            let variant_name = &variant.ident;
            quote! {
                Self::#variant_name(node) => node.span
            }
        })
        .collect();

    quote! {
        pub fn span(&self) -> Span {
            match self {
                #(#match_arms),*
            }
        }
    }
}

fn generate_ast_green_children_method(data_enum: &DataEnum) -> proc_macro2::TokenStream {
    let match_arms: Vec<_> = data_enum
        .variants
        .iter()
        .map(|variant| {
            let variant_name = &variant.ident;
            quote! {
                Self::#variant_name(node) => &node.green_elements
            }
        })
        .collect();

    quote! {
        pub fn green_children(&self) -> &[GreenElement] {
            match self {
                #(#match_arms),*
            }
        }
    }
}

fn generate_ast_text_length_method(data_enum: &DataEnum) -> proc_macro2::TokenStream {
    let match_arms: Vec<_> = data_enum
        .variants
        .iter()
        .map(|variant| {
            let variant_name = &variant.ident;
            quote! {
                Self::#variant_name(node) => node.text_length
            }
        })
        .collect();

    quote! {
        pub fn text_length(&self) -> u32 {
            match self {
                #(#match_arms),*
            }
        }
    }
}

fn generate_ast_name_method(data_enum: &DataEnum) -> proc_macro2::TokenStream {
    let match_arms: Vec<_> = data_enum
        .variants
        .iter()
        .map(|variant| {
            let variant_name = &variant.ident;
            quote! {
                Self::#variant_name(node) => node.name()
            }
        })
        .collect();

    quote! {
        pub fn name(&self) -> &'static str {
            match self {
                #(#match_arms),*
            }
        }
    }
}

fn generate_ast_children_method(data_enum: &DataEnum) -> proc_macro2::TokenStream {
    let match_arms: Vec<_> = data_enum
        .variants
        .iter()
        .map(|variant| {
            let variant_name = &variant.ident;
            quote! {
                Self::#variant_name(node) => node.children()
            }
        })
        .collect();

    quote! {
        pub fn children(&self) -> Vec<AstId> {
            match self {
                #(#match_arms),*
            }
        }
    }
}

fn generate_ast_kind_method(
    data_enum: &DataEnum,
    node_kind_name: &syn::Ident,
) -> proc_macro2::TokenStream {
    let match_arms: Vec<_> = data_enum
        .variants
        .iter()
        .map(|variant| {
            let variant_name = &variant.ident;
            quote! {
                Self::#variant_name(_) => #node_kind_name::#variant_name
            }
        })
        .collect();

    quote! {
        pub fn kind(&self) -> #node_kind_name {
            match self {
                #(#match_arms),*
            }
        }
    }
}

fn generate_ast_syntax_kind_method(data_enum: &DataEnum) -> proc_macro2::TokenStream {
    let match_arms: Vec<_> = data_enum
        .variants
        .iter()
        .map(|variant| {
            let variant_name = &variant.ident;
            let ast_type_name =
                syn::Ident::new(&format!("Ast{}", variant_name), variant_name.span());
            quote! {
                Self::#variant_name(_) => #ast_type_name::syntax_kind()
            }
        })
        .collect();

    quote! {
        pub fn syntax_kind(&self) -> TokenKind {
            match self {
                #(#match_arms),*
            }
        }
    }
}

fn generate_syntax_node_methods_per_variant(data_enum: &DataEnum) -> proc_macro2::TokenStream {
    let as_methods: Vec<_> = data_enum
        .variants
        .iter()
        .map(|variant| {
            let variant_name = &variant.ident;
            let method_name_str = to_snake_case(&variant_name.to_string());
            let is_method_name =
                syn::Ident::new(&format!("is_{}", method_name_str), variant_name.span());
            let to_method_name =
                syn::Ident::new(&format!("to_{}", method_name_str), variant_name.span());
            let as_method_name =
                syn::Ident::new(&format!("as_{}", method_name_str), variant_name.span());
            let ast_type_name =
                syn::Ident::new(&format!("Ast{}", variant_name), variant_name.span());

            quote! {
                pub fn #is_method_name(&self) -> bool {
                    self.file().node(self.id()).#is_method_name()
                }

                pub fn #to_method_name(&self) -> Option<#ast_type_name> {
                    if self.file().node(self.id()).#is_method_name() {
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

fn generate_visitor_pattern(data_enum: &DataEnum) -> proc_macro2::TokenStream {
    let trait_methods: Vec<_> = data_enum
        .variants
        .iter()
        .map(|variant| {
            let variant_name = &variant.ident;
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

    let visit_match_arms: Vec<_> = data_enum
        .variants
        .iter()
        .map(|variant| {
            let variant_name = &variant.ident;
            let method_name_str = to_snake_case(&variant_name.to_string());
            let visit_method =
                syn::Ident::new(&format!("visit_{}", method_name_str), variant_name.span());
            let as_method =
                syn::Ident::new(&format!("as_{}", method_name_str), variant_name.span());

            quote! {
                Ast::#variant_name(_n) => v.#visit_method(node.clone().#as_method()),
            }
        })
        .collect();

    quote! {
        pub trait Visitor: Sized {
            #(#trait_methods)*
        }

        pub fn visit_node<V: Visitor>(v: &mut V, node: SyntaxNode) {
            match node.raw_node() {
                #(#visit_match_arms)*
            }
        }
    }
}
