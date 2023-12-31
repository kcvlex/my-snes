extern crate proc_macro;
use proc_macro::TokenStream;
use syn::{parse_macro_input, Data, DeriveInput, Fields};
use quote::{format_ident, quote};

#[proc_macro_derive(SNES_Events)]
pub fn derive_events(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let name = input.ident;
    let vis = input.vis;

    let fields = match input.data {
        Data::Struct(ds) => match ds.fields {
            Fields::Named(f) => f.named.into_iter(),
            _ => unimplemented!(),
        }
        _ => unimplemented!(),
    };

    let gen_funcs = |ident| {
        let enable = format_ident!("enable_{}", ident);
        let consume = format_ident!("consume_{}", ident);
        let get = format_ident!("get_{}", ident);
        quote! {
            #vis fn #enable(&mut self) {
                self.#ident = true;
            }

            #vis fn #consume(&mut self) -> bool {
                let res = self.#ident;
                self.#ident = false;
                res
            }

            #vis fn #get(&self) -> bool {
                self.#ident
            }
        }
    };

    let funcs = fields.map(|x| gen_funcs(x.ident.unwrap()));

    let expand = quote! {
        impl #name {
            #(#funcs)*
        }
    };

    TokenStream::from(expand)
}
