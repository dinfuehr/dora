use proc_macro::TokenStream;
use quote::quote;
use syn::{LitStr, parse_macro_input};

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
