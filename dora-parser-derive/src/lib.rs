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

            // Generate the Ast-prefixed struct definition with only id and file fields
            let struct_def = quote! {
                #[derive(Clone, Debug)]
                pub struct #struct_ast_node_name {
                    pub id: AstId,
                    pub file: File,
                }
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
                                &self.file.node(self.id).#to_method().unwrap().#field_name
                            }
                        };

                        // Get return type from attribute (defaults to AstNode)
                        let return_type = get_return_type_from_attrs(&field.attrs);

                        let wrapper_accessor = if is_ast_id(field_type) {
                            quote! {
                                pub fn #field_name(&self) -> #return_type {
                                    let ast_id = *self.#raw_accessor_name();
                                    #return_type::new(self.file.clone(), ast_id)
                                }
                            }
                        } else if is_option_ast_id(field_type) {
                            quote! {
                                pub fn #field_name(&self) -> Option<#return_type> {
                                    let ast_id = *self.#raw_accessor_name();
                                    ast_id.map(|ast_id| {
                                        #return_type::new(self.file.clone(), ast_id)
                                    })
                                }
                            }
                        } else if is_vec_ast_id(field_type) {
                            quote! {
                                pub fn #field_name_at(&self, idx: usize) -> #return_type {
                                    let vec = self.#raw_accessor_name();
                                    let ast_id = vec[idx];
                                    #return_type::new(self.file.clone(), ast_id)
                                }

                                pub fn #field_name_len(&self) -> usize {
                                    self.#raw_accessor_name().len()
                                }

                                pub fn #field_name(&self) -> AstIdIterator<'_, #return_type> {
                                    let vec = self.#raw_accessor_name();
                                    AstIdIterator::new(self.file.clone(), vec.as_slice())
                                }
                            }
                        } else if is_likely_copy_type(field_type) {
                            quote! {
                                pub fn #field_name(&self) -> #field_type {
                                    self.file.node(self.id).#to_method().unwrap().#field_name
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

            let ast_impl_block = quote! {
                impl AstNodeBase for #struct_ast_node_name {
                    fn new(file: File, id: AstId) -> Self {
                        assert!(file.node(id).#is_method_name());
                        #struct_ast_node_name { file, id }
                    }

                    fn id(&self) -> AstId {
                        self.id
                    }

                    fn raw_node(&self) -> &Ast {
                        self.file.node(self.id)
                    }

                    fn span(&self) -> Span {
                        self.raw_node().span()
                    }

                    fn file(&self) -> &File {
                        &self.file
                    }

                    fn children(&self) -> impl Iterator<Item = AstNode> {
                        let children_vec = self.raw_node().children();
                        let file = self.file.clone();
                        children_vec.into_iter().map(move |id| AstNode::new(file.clone(), id))
                    }
                }

                impl #struct_ast_node_name {
                    #field_methods
                }
            };

            (struct_def, impl_block, ast_impl_block)
        }
        _ => panic!("AstNode can only be derived for structs"),
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

    let variant_methods = generate_per_variant_methods(data_enum);
    let span_method = generate_span_method(data_enum);
    let name_method = generate_name_method(data_enum);
    let children_method = generate_children_method(data_enum);

    // Generate as_* methods for AstNode
    let ast_node_methods = generate_ast_node_methods(data_enum);

    // Generate visitor pattern code
    let visitor_code = generate_visitor_pattern(data_enum);

    let expanded = quote! {
        impl #enum_name {
            #span_method
            #name_method
            #children_method
            #variant_methods
        }

        #ast_node_methods

        #visitor_code
    };

    TokenStream::from(expanded)
}

fn generate_per_variant_methods(data_enum: &DataEnum) -> proc_macro2::TokenStream {
    let mut all_methods = Vec::new();

    for variant in &data_enum.variants {
        let variant_name = &variant.ident;

        // Get the inner type if it's a tuple variant with one field
        let inner_type = match &variant.fields {
            Fields::Unnamed(fields) if fields.unnamed.len() == 1 => {
                Some(&fields.unnamed.first().unwrap().ty)
            }
            _ => None,
        };

        if let Some(inner_ty) = inner_type {
            // Convert variant name to snake_case for method names
            let method_name_str = to_snake_case(&variant_name.to_string());
            let is_method_name =
                syn::Ident::new(&format!("is_{}", method_name_str), variant_name.span());
            let to_method_name =
                syn::Ident::new(&format!("to_{}", method_name_str), variant_name.span());
            let as_method_name =
                syn::Ident::new(&format!("as_{}", method_name_str), variant_name.span());

            // Generate is<variant>, to_<variant> and as_<variant> method on Ast.
            let per_variant_methods = quote! {
                pub fn #is_method_name(&self) -> bool {
                    match self {
                        Self::#variant_name(..) => true,
                        _ => false,
                    }
                }

                pub fn #to_method_name(&self) -> Option<&#inner_ty> {
                    match self {
                        Self::#variant_name(inner) => Some(inner),
                        _ => None,
                    }
                }

                pub fn #as_method_name(&self) -> &#inner_ty {
                    self.#to_method_name().expect("wrong node kind")
                }
            };

            all_methods.push(per_variant_methods);
        }
    }

    quote! {
        #(#all_methods)*
    }
}

fn generate_span_method(data_enum: &DataEnum) -> proc_macro2::TokenStream {
    let mut match_arms = Vec::new();

    for variant in &data_enum.variants {
        let variant_name = &variant.ident;

        // Only handle tuple variants with one field
        if matches!(&variant.fields, Fields::Unnamed(fields) if fields.unnamed.len() == 1) {
            let arm = quote! {
                Self::#variant_name(node) => node.span
            };
            match_arms.push(arm);
        }
    }

    quote! {
        pub fn span(&self) -> Span {
            match self {
                #(#match_arms),*
            }
        }
    }
}

fn generate_name_method(data_enum: &DataEnum) -> proc_macro2::TokenStream {
    let mut match_arms = Vec::new();

    for variant in &data_enum.variants {
        let variant_name = &variant.ident;

        // Only handle tuple variants with one field
        if matches!(&variant.fields, Fields::Unnamed(fields) if fields.unnamed.len() == 1) {
            let arm = quote! {
                Self::#variant_name(node) => node.name()
            };
            match_arms.push(arm);
        }
    }

    quote! {
        pub fn name(&self) -> &'static str {
            match self {
                #(#match_arms),*
            }
        }
    }
}

fn generate_children_method(data_enum: &DataEnum) -> proc_macro2::TokenStream {
    let mut match_arms = Vec::new();

    for variant in &data_enum.variants {
        let variant_name = &variant.ident;

        // Only handle tuple variants with one field
        if matches!(&variant.fields, Fields::Unnamed(fields) if fields.unnamed.len() == 1) {
            let arm = quote! {
                Self::#variant_name(node) => node.children()
            };
            match_arms.push(arm);
        }
    }

    quote! {
        pub fn children(&self) -> Vec<AstId> {
            match self {
                #(#match_arms),*
            }
        }
    }
}

fn generate_ast_node_methods(data_enum: &DataEnum) -> proc_macro2::TokenStream {
    // Generate as_* methods for each variant
    let as_methods = data_enum.variants.iter().filter_map(|variant| {
        let variant_name = &variant.ident;

        // Only handle tuple variants with one field
        if !matches!(&variant.fields, Fields::Unnamed(fields) if fields.unnamed.len() == 1) {
            return None;
        }

        let method_name_str = to_snake_case(&variant_name.to_string());
        let is_method_name =
            syn::Ident::new(&format!("is_{}", method_name_str), variant_name.span());
        let to_method_name =
            syn::Ident::new(&format!("to_{}", method_name_str), variant_name.span());
        let as_method_name =
            syn::Ident::new(&format!("as_{}", method_name_str), variant_name.span());
        let ast_type_name = syn::Ident::new(&format!("Ast{}", variant_name), variant_name.span());

        // Generate is<variant>, to_<variant> and as_<variant> method on AstNode.
        Some(quote! {
            pub fn #is_method_name(&self) -> bool {
                self.file.node(self.id).#is_method_name()
            }

            pub fn #to_method_name(self) -> Option<#ast_type_name> {
                if self.file.node(self.id).#is_method_name() {
                    Some(#ast_type_name {
                        file: self.file,
                       id: self.id
                    })
                } else {
                    None
                }
            }

            pub fn #as_method_name(self) -> #ast_type_name {
                self.#to_method_name().expect("wrong node kind")
            }
        })
    });

    quote! {
        impl AstNode {
            #(#as_methods)*
        }
    }
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
    // Default to AstNode if no attribute is specified
    syn::Ident::new("AstNode", proc_macro2::Span::call_site())
}

fn generate_visitor_pattern(data_enum: &DataEnum) -> proc_macro2::TokenStream {
    // Generate the trait methods directly
    let trait_methods = data_enum.variants.iter().filter_map(|variant| {
        let variant_name = &variant.ident;

        // Only handle tuple variants with one field
        if !matches!(&variant.fields, Fields::Unnamed(fields) if fields.unnamed.len() == 1) {
            return None;
        }

        let method_name_str = to_snake_case(&variant_name.to_string());
        let visit_method =
            syn::Ident::new(&format!("visit_{}", method_name_str), variant_name.span());
        let ast_type_name = syn::Ident::new(&format!("Ast{}", variant_name), variant_name.span());

        Some(quote! {
            fn #visit_method(&mut self, _ast_node: #ast_type_name) {
                walk_children(self, _ast_node);
            }
        })
    });

    // Generate the visit_node function match arms
    let visit_match_arms = data_enum.variants.iter().filter_map(|variant| {
        let variant_name = &variant.ident;

        if !matches!(&variant.fields, Fields::Unnamed(fields) if fields.unnamed.len() == 1) {
            return None;
        }

        let method_name_str = to_snake_case(&variant_name.to_string());
        let visit_method =
            syn::Ident::new(&format!("visit_{}", method_name_str), variant_name.span());
        let as_method = syn::Ident::new(&format!("as_{}", method_name_str), variant_name.span());

        Some(quote! {
            Ast::#variant_name(_n) => v.#visit_method(node.clone().#as_method()),
        })
    });

    quote! {
        pub trait Visitor: Sized {
            #(#trait_methods)*
        }

        pub fn visit_node<V: Visitor>(v: &mut V, node: AstNode) {
            match node.raw_node() {
                #(#visit_match_arms)*
            }
        }
    }
}
