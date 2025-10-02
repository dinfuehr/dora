use proc_macro::TokenStream;
use quote::quote;
use syn::{
    Data, DataEnum, DeriveInput, Fields, GenericArgument, PathArguments, Type, parse_macro_input,
};

#[proc_macro_derive(AstNode)]
pub fn derive_ast_node(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = &input.ident;
    let name_str = name.to_string();

    let children_impl = match &input.data {
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

            quote! {
                impl #name {
                    pub fn children(&self) -> Vec<AstId> {
                        #field_collection
                    }

                    pub fn name(&self) -> &'static str {
                        #name_str
                    }
                }
            }
        }
        _ => panic!("AstNode can only be derived for structs"),
    };

    TokenStream::from(children_impl)
}

fn is_ast_id_related(ty: &Type) -> bool {
    is_ast_id(ty) || is_option_ast_id(ty) || is_vec_ast_id(ty) || is_option_vec_ast_id(ty)
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

    let variant_methods = generate_variant_methods(data_enum);
    let span_method = generate_span_method(data_enum);
    let name_method = generate_name_method(data_enum);
    let children_method = generate_children_method(data_enum);

    let expanded = quote! {
        impl #enum_name {
            #span_method
            #name_method
            #children_method
            #variant_methods
        }
    };

    TokenStream::from(expanded)
}

fn generate_variant_methods(data_enum: &DataEnum) -> proc_macro2::TokenStream {
    let mut methods = Vec::new();

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

            // Generate is_<variant> method
            let is_method = quote! {
                pub fn #is_method_name(&self) -> bool {
                    match self {
                        Self::#variant_name(..) => true,
                        _ => false,
                    }
                }
            };

            // Generate to_<variant> method
            let to_method = quote! {
                pub fn #to_method_name(&self) -> Option<&#inner_ty> {
                    match self {
                        Self::#variant_name(inner) => Some(inner),
                        _ => None,
                    }
                }
            };

            methods.push(is_method);
            methods.push(to_method);
        }
    }

    quote! {
        #(#methods)*
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
