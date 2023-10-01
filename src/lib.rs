/* SPDX-License-Identifier: MIT
 * Copyright(c) 2023 Darek Stojaczyk
 */

extern crate proc_macro;
extern crate syn;
#[macro_use]
extern crate quote;

use proc_macro::TokenStream;
use proc_macro2::{Group, Ident, TokenTree};
use syn::{parse_quote, Attribute, Field};

#[derive(Debug)]
struct EnumVariant {
    id: usize,
    name: Ident,
    fields: Vec<Field>,
}

#[derive(Debug)]
struct EnumAttribute {
    ident: Ident,
    group: Option<Group>,
}

#[derive(Debug, Default)]
struct EnumInternalAttributes {
    parse_fn: Option<String>,
}

impl TryFrom<proc_macro2::TokenStream> for EnumInternalAttributes {
    type Error = ();

    fn try_from(tokens: proc_macro2::TokenStream) -> Result<Self, Self::Error> {
        let mut tokens_iter = tokens.into_iter();
        let mut ret = EnumInternalAttributes::default();

        loop {
            let Some(TokenTree::Ident(ident)) = tokens_iter.next() else {
                break;
            };

            if ident.to_string() == "parse_fn" {
                match tokens_iter.next() {
                    Some(TokenTree::Punct(punct)) => {
                        if punct.as_char() != '=' {
                            panic!("Unknown parse_fn syntax. Expected `parse_fn = my_fn`");
                        }
                    }
                    _ => panic!(
                        "parse_fn param should be followed by `= my_fn`. E.g. `parse_fn = my_fn`"
                    ),
                };
                let Some(TokenTree::Ident(value)) = tokens_iter.next() else {
                    panic!("Unknown parse_fn syntax. Expected `parse_fn = my_fn`");
                };
                ret.parse_fn = Some(value.to_string());
            }
        }

        Ok(ret)
    }
}

#[derive(Debug)]
struct EnumParseArgs {
    struct_attrs: Vec<EnumAttribute>,
    internal_attrs: EnumInternalAttributes,
}

impl TryFrom<proc_macro2::TokenStream> for EnumParseArgs {
    type Error = ();

    fn try_from(tokens: proc_macro2::TokenStream) -> Result<Self, Self::Error> {
        let mut tokens_iter = tokens.into_iter();
        let mut attrs: Vec<EnumAttribute> = Vec::new();

        loop {
            let Some(ident) = tokens_iter.next() else {
                break;
            };
            let TokenTree::Ident(ident) = ident else {
                panic!("Malformed syntax. Expected Ident");
            };

            let group = match tokens_iter.next() {
                Some(TokenTree::Group(group)) => {
                    let group = group.clone();
                    // skip the following comma
                    tokens_iter.next();
                    Some(group)
                }
                _ => {
                    // we consumed a comma
                    None
                }
            };

            attrs.push(EnumAttribute { ident, group });
        }

        let internal_attrs = attrs
            .iter()
            .position(|a| a.ident.to_string() == "attr")
            .map(|idx| {
                let attr = attrs.remove(idx);
                attr.group
                    .map(|g| EnumInternalAttributes::try_from(g.stream()))
            })
            .flatten()
            .unwrap_or(Ok(EnumInternalAttributes::default()))?;

        Ok(EnumParseArgs {
            struct_attrs: attrs,
            internal_attrs,
        })
    }
}

#[proc_macro_attribute]
pub fn enum_parse(attr: TokenStream, input: TokenStream) -> TokenStream {
    let attr: proc_macro2::TokenStream = attr.into();
    let args: EnumParseArgs = attr.try_into().unwrap();

    let ast = syn::parse_macro_input!(input as syn::DeriveInput);
    let enum_vis = ast.vis;
    let enum_attrs = ast.attrs;
    let enum_ident = ast.ident;

    // Extract the enum variants
    let variants: Vec<syn::Variant> = match ast.data {
        syn::Data::Enum(data_enum) => data_enum.variants.into_iter().collect(),
        _ => panic!("#[derive(ZerocopyEnum)] expects enum"),
    };

    // Organize info about variants
    let variants: Vec<EnumVariant> = variants
        .into_iter()
        .map(|variant| {
            let name = variant.ident.clone();

            // set visibility to each field
            let fields: Vec<Field> = variant
                .fields
                .into_iter()
                .map(|mut field| {
                    field.vis = enum_vis.clone();
                    field
                })
                .collect();

            EnumVariant {
                id: 0,
                name,
                fields,
            }
        })
        .collect();

    // Re-create the original enum, now referencing soon-to-be-created structs
    let variant_names = variants.iter().map(|v| &v.name);
    let mut ret_stream = quote! {
        #(#enum_attrs)*
        #enum_vis enum #enum_ident {
            #(#variant_names(#variant_names)),*
        }
    };

    // For each EnumVariant generate a struct and its impl
    let attributes: Vec<Attribute> = args
        .struct_attrs
        .into_iter()
        .map(|t| {
            let ident = t.ident;
            match t.group {
                None => parse_quote!(
                    #[#ident]
                ),
                Some(group) => parse_quote!(
                    #[#ident #group]
                ),
            }
        })
        .collect();

    for v in variants {
        let EnumVariant { id, name, fields } = &v;
        ret_stream.extend::<proc_macro2::TokenStream>(quote! {
            #(#attributes)*
            #enum_vis struct #name {
                #(#fields,)*
            }

            impl #name {
                pub const ID: usize = #id;
            }
        });
    }

    ret_stream.into()
}
