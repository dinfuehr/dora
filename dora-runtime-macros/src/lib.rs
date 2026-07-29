use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{Error, Fields, ItemStruct, LitStr, parse_macro_input, parse_quote};

#[proc_macro_attribute]
pub fn dora_native(attr: TokenStream, item: TokenStream) -> TokenStream {
    let path = parse_macro_input!(attr as LitStr);
    let symbol = dora_symbol::mangle_name(&path.value());
    let symbol = LitStr::new(&symbol, path.span());
    let item = proc_macro2::TokenStream::from(item);

    quote! {
        #[unsafe(export_name = #symbol)]
        #item
    }
    .into()
}

#[proc_macro_attribute]
/// Defines a Rust mirror for a garbage-collected Dora object.
///
/// The macro inserts a `Header` as the first field, applies `repr(C)`, and
/// generates common object helpers and value getters/setters for fields.
/// Fields marked with `#[dora_ref]` receive a write-barrier-aware setter.
/// Getters return fields by value, so non-`Copy` fields or fields requiring
/// specialized access should use `#[dora_raw]` to suppress accessors.
pub fn dora_object(attr: TokenStream, item: TokenStream) -> TokenStream {
    if !attr.is_empty() {
        return Error::new(
            proc_macro2::Span::call_site(),
            "dora_object does not accept arguments",
        )
        .into_compile_error()
        .into();
    }

    let mut object = parse_macro_input!(item as ItemStruct);
    let mut object_fields = Vec::new();

    let fields = match &mut object.fields {
        Fields::Named(fields) => fields,
        _ => {
            return Error::new_spanned(&object, "dora_object requires a struct with named fields")
                .into_compile_error()
                .into();
        }
    };

    for field in &mut fields.named {
        if field.ident.as_ref().is_some_and(|ident| ident == "header") {
            return Error::new_spanned(field, "dora_object inserts the header field automatically")
                .into_compile_error()
                .into();
        }

        if !matches!(field.vis, syn::Visibility::Inherited) {
            return Error::new_spanned(
                &field.vis,
                "dora_object fields must be private; use the generated getter and setter",
            )
            .into_compile_error()
            .into();
        }

        let mut is_reference = false;
        let mut is_raw = false;
        let mut attributes = Vec::with_capacity(field.attrs.len());

        for attribute in std::mem::take(&mut field.attrs) {
            if attribute.path().is_ident("dora_ref") {
                if is_reference {
                    return Error::new_spanned(attribute, "duplicate dora_ref attribute")
                        .into_compile_error()
                        .into();
                }

                if !matches!(attribute.meta, syn::Meta::Path(_)) {
                    return Error::new_spanned(attribute, "dora_ref does not accept arguments")
                        .into_compile_error()
                        .into();
                }

                is_reference = true;
            } else if attribute.path().is_ident("dora_raw") {
                if is_raw {
                    return Error::new_spanned(attribute, "duplicate dora_raw attribute")
                        .into_compile_error()
                        .into();
                }

                if !matches!(attribute.meta, syn::Meta::Path(_)) {
                    return Error::new_spanned(attribute, "dora_raw does not accept arguments")
                        .into_compile_error()
                        .into();
                }

                is_raw = true;
            } else {
                attributes.push(attribute);
            }
        }

        field.attrs = attributes;

        if is_reference && is_raw {
            return Error::new_spanned(field, "dora_ref and dora_raw cannot be combined")
                .into_compile_error()
                .into();
        }

        if !is_raw {
            let field_name = field.ident.clone().expect("named field");
            object_fields.push((field_name, field.ty.clone(), is_reference));
        }
    }

    fields
        .named
        .insert(0, parse_quote!(header: ::dora_runtime::Header));
    object.attrs.push(parse_quote!(#[repr(C)]));

    let object_name = &object.ident;
    let (impl_generics, type_generics, where_clause) = object.generics.split_for_impl();
    let field_methods = object_fields
        .iter()
        .map(|(field_name, field_type, is_reference)| {
            let setter_name = format_ident!("set_{field_name}");
            let write_barrier = if *is_reference {
                quote!(self.write_barrier(value);)
            } else {
                quote!()
            };

            quote! {
                #[inline]
                pub fn #field_name(&self) -> #field_type {
                    self.#field_name
                }

                #[inline]
                pub fn #setter_name(&mut self, value: #field_type) {
                    self.#field_name = value;
                    #write_barrier
                }
            }
        });

    quote! {
        #object

        impl #impl_generics #object_name #type_generics #where_clause {
            #[inline(always)]
            pub fn header(&self) -> &::dora_runtime::Header {
                &self.header
            }

            #[inline(always)]
            pub fn header_mut(&mut self) -> &mut ::dora_runtime::Header {
                &mut self.header
            }

            #[inline(always)]
            pub fn address(&self) -> ::dora_runtime::Address {
                self.header.address()
            }

            #[inline(always)]
            pub fn object_end_address(&self) -> ::dora_runtime::Address {
                self.address().offset(::std::mem::size_of::<Self>())
            }

            #[inline(always)]
            pub fn object_end_ptr<__DoraValue>(&self) -> *const __DoraValue {
                self.object_end_address().to_ptr()
            }

            #[inline(always)]
            pub fn object_end_ptr_mut<__DoraValue>(&mut self) -> *mut __DoraValue {
                self.object_end_address().to_mut_ptr()
            }

            #[inline]
            pub fn write_barrier<__DoraValue>(
                &self,
                value: ::dora_runtime::Ref<__DoraValue>,
            ) {
                ::dora_runtime::gc::write_barrier(&self.header, value);
            }

            #(#field_methods)*

            #[allow(dead_code)]
            const DORA_OBJECT_HEADER_OFFSET_IS_ZERO: () =
                assert!(::std::mem::offset_of!(Self, header) == 0);
        }
    }
    .into()
}
