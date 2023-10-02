/* SPDX-License-Identifier: MIT
 * Copyright(c) 2023 Darek Stojaczyk
 */

extern crate proc_macro;
extern crate syn;
#[macro_use]
extern crate quote;

use proc_macro::TokenStream;
use proc_macro2::{Group, Ident, Span, TokenTree};
use syn::{parse_quote, Attribute, Field};

/// Enum variant extracted from the original enum
#[derive(Debug)]
struct EnumVariant {
    id: usize,
    name: Ident,
    fields: Vec<Field>,
}

/// Argument to #[enum_parse(...)] macro that will be passed 1:1
/// to generated structs. Can be derive(Debug) or just e.g. no_mangle,
/// so the group is optional.
#[derive(Debug)]
struct EnumAttribute {
    ident: Ident,
    group: Option<Group>,
}

/// Parsed attr(...) argument from #[enum_parse(..., attr(...)] macro
#[derive(Debug)]
struct EnumInternalAttributes {
    parse_fn: String,
}

impl Default for EnumInternalAttributes {
    fn default() -> Self {
        Self {
            parse_fn: "parse".into(),
        }
    }
}

fn expect_punct_token(token: Option<TokenTree>) {
    match token {
        Some(TokenTree::Punct(punct)) => {
            if punct.as_char() != '=' {
                panic!("Unknown parse_fn syntax. Expected `parse_fn = my_fn`");
            }
        }
        _ => panic!("parse_fn param should be followed by `= my_fn`. E.g. `parse_fn = my_fn`"),
    }
}

/// Parse attr() argument in #[enum_parse(..., attr(parse_fn = my_fn))]
/// We could technically use syn to consume the input and parse it for us,
/// but since we already parsed raw tokens in EnumParseArgs, do it here
/// as well.
impl TryFrom<proc_macro2::TokenStream> for EnumInternalAttributes {
    type Error = ();

    fn try_from(tokens: proc_macro2::TokenStream) -> Result<Self, Self::Error> {
        let mut tokens_iter = tokens.into_iter();
        let mut ret = EnumInternalAttributes::default();

        loop {
            let Some(token) = tokens_iter.next() else {
                break;
            };

            let TokenTree::Ident(ident) = token else {
                continue;
            };

            match ident.to_string().as_str() {
                "parse_fn" => {
                    expect_punct_token(tokens_iter.next());
                    let Some(TokenTree::Ident(value)) = tokens_iter.next() else {
                        panic!("Unknown parse_fn syntax. Expected `parse_fn = my_fn`");
                    };
                    ret.parse_fn = value.to_string();
                }
                name => {
                    panic!("Unknown attribute `{name}`")
                }
            }
        }

        Ok(ret)
    }
}

/// All arguments passed to #[enum_parse(...)] macro
#[derive(Debug)]
struct EnumParseArgs {
    struct_attrs: Vec<EnumAttribute>,
    internal_attrs: EnumInternalAttributes,
}

/// Organize enum_parse macro arguments into a struct. Note that only a small
/// part of arguments are getting parsed, the rest is technically invalid syntax
/// until it's wrapped in #[] and used to decorate a struct.
/// For that reason, we don't try to parse it yet.
impl TryFrom<proc_macro2::TokenStream> for EnumParseArgs {
    type Error = ();

    fn try_from(tokens: proc_macro2::TokenStream) -> Result<Self, Self::Error> {
        let mut tokens_iter = tokens.into_iter();
        let mut attrs: Vec<EnumAttribute> = Vec::new();

        loop {
            // The macro argument can be derive(Debug) - with brackets,
            // or without them - e.g. no_mangle
            let Some(ident) = tokens_iter.next() else {
                break;
            };
            let TokenTree::Ident(ident) = ident else {
                panic!("Malformed #[enum_parse(...)] syntax. Expected Ident. Example: \n\
                        \t#[enum_parse(derive(Debug, Default), \n\
                        \t\trepr(C, packed), \n\
                        \t\tattr(parse_fn = my_fn))]");
            };

            let group = match tokens_iter.next() {
                Some(TokenTree::Group(group)) => {
                    let group = group.clone();
                    // skip the following comma (or nothing)
                    tokens_iter.next();
                    Some(group)
                }
                _ => {
                    // we consumed a comma (or nothing)
                    None
                }
            };

            attrs.push(EnumAttribute { ident, group });
        }

        // Extract attr() argument and parse it as internal argument.
        // It's removed from the original arguments vector.
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
    // Also define the parsing method
    let parse_fn = Ident::new(args.internal_attrs.parse_fn.as_str(), Span::call_site());
    let variant_names: Vec<&Ident> = variants.iter().map(|v| &v.name).collect();
    let mut ret_stream = quote! {
        #(#enum_attrs)*
        #enum_vis enum #enum_ident {
            #(#variant_names(#variant_names)),*
        }

        impl #enum_ident {
            fn parse(data: &[u8], id: usize) -> Option<Self> {
                match id {
                    #(#variant_names ::ID => {
                        #variant_names :: #parse_fn (data).map(|s| Self:: #variant_names (s))
                    }),*
                    ,
                    _ => panic!()
                }
            }
        }
    };

    // Generate Attribute-s (this is the first time their syntax is checked)
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

    // For each EnumVariant generate a struct and its impl
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
