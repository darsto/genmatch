/* SPDX-License-Identifier: MIT
 * Copyright(c) 2023 Darek Stojaczyk
 */

extern crate proc_macro;
extern crate syn;
#[macro_use]
extern crate quote;

use proc_macro::TokenStream;
use proc_macro2::Ident;
use syn::Field;

struct EnumVariant {
    name: Ident,
    fields: Vec<Field>,
}

#[proc_macro_derive(ZerocopyEnum)]
pub fn zerocopy_enum_derive(input: TokenStream) -> TokenStream {
    let ast = syn::parse_macro_input!(input as syn::DeriveInput);
    let enum_vis = ast.vis;

    // Extract the enum variants
    let variants: Vec<&syn::Variant> = match &ast.data {
        syn::Data::Enum(ref data_enum) => data_enum.variants.iter().collect(),
        _ => panic!("#[derive(ZerocopyEnum)] expects enum"),
    };

    // For each variant create a struct
    let structs: Vec<EnumVariant> = variants
        .iter()
        .map(|variant| {
            let name = variant.ident.clone();

            // set visibility to each field
            let fields: Vec<Field> = variant
                .fields
                .iter()
                .cloned()
                .map(|mut field| {
                    field.vis = enum_vis.clone();
                    field
                })
                .collect();

            EnumVariant { name, fields }
        })
        .collect();

    let mut structs_stream = proc_macro2::TokenStream::default();
    for s in structs {
        let EnumVariant { name, fields } = &s;
        structs_stream.extend::<proc_macro2::TokenStream>(quote! {
            #enum_vis struct #name {
                #(#fields,)*
            }
        });
    }

    structs_stream.into()
}
