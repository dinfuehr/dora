use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, Data, DeriveInput, Fields, GenericArgument, PathArguments, Type};

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
