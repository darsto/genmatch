/* SPDX-License-Identifier: MIT
 * Copyright(c) 2023 Darek Stojaczyk
 */

extern crate proc_macro;
extern crate syn;
#[macro_use]
extern crate quote;

use std::str::FromStr;

use proc_macro2::{Group, Ident, Span, TokenTree, TokenStream};
use syn::{parse_quote, Attribute, Field, Variant, Meta};

/// Enum variant extracted from the original enum.
/// id == None means the default case
#[derive(Debug)]
struct EnumVariant {
    id: Option<TokenTree>,
    name: Ident,
    fields: Vec<Field>,
}

impl TryFrom<Variant> for EnumVariant {
    type Error = ();

    fn try_from(variant: Variant) -> Result<Self, Self::Error> {
        let name = variant.ident.clone();
        let mut attrs = variant.attrs;
        let fields = variant.fields.into_iter().collect();

        // Parse variant's attributes
        let internal_attrs_idx = attrs.iter().position(|a| match &a.meta {
            Meta::List(list) => {
                if let Some(ident) = list.path.get_ident() {
                    ident.to_string() == "attr"
                } else {
                    false
                }
            },
            _ => false
        }).expect("Each enum variant needs to be have an attr attribute. #[attr(ID = 0x42)]");
        let internal_attrs = attrs.remove(internal_attrs_idx);
        let Meta::List(internal_attrs) = internal_attrs.meta else {
            panic!("`attr` attribute needs to describe a list. E.g: #[attr(ID = 0x42)]");
        };

        let mut tokens_iter = internal_attrs.tokens.into_iter();
        let mut id: Option<Option<TokenTree>> = None;

        loop {
            let Some(token) = tokens_iter.next() else {
                break;
            };

            let TokenTree::Ident(ident) = token else {
                continue;
            };

            match ident.to_string().as_str() {
                "ID" => {
                    expect_punct_token(tokens_iter.next());
                    let value = tokens_iter.next().expect("Unknown attr syntax. Expected `#[attr(ID = 0x42)]`");

                    id = Some(match &value {
                        TokenTree::Ident(ident) => {
                            if ident.to_string() == "_" {
                                // The `default` case
                                None
                            } else {
                                Some(value)
                            }
                        }
                        _ => Some(value)
                    });
                }
                name => {
                    panic!("Unknown attribute `{name}`")
                }
            }
        }

        if attrs.len() > 1 {
            panic!("Currently additional variant attributes are not supported");
        }

        let id = id.expect("Missing ID identifier.Each enum variant needs to be assigned an ID. #[attr(ID = 0x42)]");
        Ok(EnumVariant {
            id,
            name,
            fields,
        })
    }
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
    parse_input: TokenStream,
    parse_fn: String,
}

impl Default for EnumInternalAttributes {
    fn default() -> Self {
        Self {
            parse_input: TokenStream::from_str("&[u8]").unwrap(),
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

fn consume_tokens_until_comma<I: Iterator<Item = TokenTree>>(tokens_iter: &mut I) -> TokenStream {
    let mut consumed: Vec<TokenTree> = Vec::new();
    let mut depth: usize = 0;

    loop {
        let Some(token) = tokens_iter.next() else {
            // end of input
            break;
        };

        if let TokenTree::Punct(ref punct) = token {
            match punct.as_char() {
                '<' => {
                    depth = depth.checked_add(1).unwrap();
                },
                '>' => {
                    depth = depth.checked_sub(1).unwrap();
                },
                ',' => {
                    if depth == 0 {
                        break;
                    }
                }
                _ => {}
            }
        }
        consumed.push(token);
    }

    quote!(#(#consumed)*)
}

/// Parse attr() argument in #[enum_parse(..., attr(parse_fn = my_fn))]
/// We could technically use syn to consume the input and parse it for us,
/// but since we already parsed raw tokens in EnumParseArgs, do it here
/// as well.
impl TryFrom<TokenStream> for EnumInternalAttributes {
    type Error = ();

    fn try_from(tokens: TokenStream) -> Result<Self, Self::Error> {
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
                "parse_input" => {
                    expect_punct_token(tokens_iter.next());
                    ret.parse_input = consume_tokens_until_comma(&mut tokens_iter);
                    // unfortunately we can't provide any type-checking at this stage
                }
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
impl TryFrom<TokenStream> for EnumParseArgs {
    type Error = ();

    fn try_from(tokens: TokenStream) -> Result<Self, Self::Error> {
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
            .and_then(|idx| {
                let attr = attrs.remove(idx);
                attr.group
                    .map(|g| EnumInternalAttributes::try_from(g.stream()))
            })
            .unwrap_or(Ok(EnumInternalAttributes::default()))?;

        Ok(EnumParseArgs {
            struct_attrs: attrs,
            internal_attrs,
        })
    }
}

#[proc_macro_attribute]
pub fn enum_parse(attr: proc_macro::TokenStream, input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let attr: TokenStream = attr.into();
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
        .map(|mut variant| {
            // set visibility to each field
            for f in &mut variant.fields {
                f.vis = enum_vis.clone();
            }
            EnumVariant::try_from(variant)
        })
        .collect::<Result<Vec<EnumVariant>, _>>().unwrap();

    // Re-create the original enum, now referencing soon-to-be-created structs
    // Also define the parsing method
    let parse_fn = Ident::new(args.internal_attrs.parse_fn.as_str(), Span::call_site());
    let mut default_variants  = variants.iter().filter(|v| v.id.is_none());

    // Print some pretty messages for otherwise hard-to-debug problems
    let default_variant = default_variants.next().expect(
        "Default variant must be defined. E.g:\n\
                \t#[attr(ID = _)]\n\
                Unknown");
    if let Some(..) = default_variants.next() {
        panic!("Only one variant with default ID (_) can be defined.");
    }

    // Build tokens for the default variant, differentiating between
    // variant with fields (which is parsed), or variant without fields
    // (which is simply returned from the parse function)
    let default_variant_name = &default_variant.name;
    let default_variant_has_fields = default_variant.fields.len() > 0;
    let default_variant_match_case = if default_variant_has_fields {
        quote!(
            #default_variant_name :: #parse_fn (data).map(|s| Self :: #default_variant_name (s))
        )
    } else {
        quote!(
            Some(Self :: #default_variant_name)
        )
    };
    let default_variant_enum: TokenStream = if default_variant_has_fields {
        quote!(
            #default_variant_name (#default_variant_name)
        )
    } else {
        quote!(
            #default_variant_name
        )
    };

    let parse_input_type = args.internal_attrs.parse_input;

    // Gather non-default variant names
    let variant_names: Vec<&Ident> = variants.iter().filter_map(|v| {
        if v.id.is_some() {
            Some(&v.name)
        } else {
            None
        }
    }).collect();

    let mut ret_stream = quote! {
        #(#enum_attrs)*
        #enum_vis enum #enum_ident {
            #(#variant_names (#variant_names)),*
            ,
            #default_variant_enum
        }

        impl #enum_ident {
            fn parse(data: #parse_input_type, id: usize) -> Option<Self> {
                match id {
                    #(#variant_names :: ID =>
                        #variant_names :: #parse_fn (data).map(|s| Self :: #variant_names (s))
                    ),*
                    ,
                    _ => #default_variant_match_case
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

        ret_stream.extend(quote! {
            #(#attributes)*
            #enum_vis struct #name {
                #(#fields,)*
            }
        });

        if let Some(id) = id {
            ret_stream.extend(quote! {
                impl #name {
                    pub const ID: usize = #id;
                }
            });
        }
    }

    ret_stream.into()
}
