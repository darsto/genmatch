/* SPDX-License-Identifier: MIT
 * Copyright(c) 2023 Darek Stojaczyk
 */

#![doc = include_str!("../README.md")]

extern crate proc_macro;
extern crate syn;
#[macro_use]
extern crate quote;

use lazy_static::lazy_static;
use proc_macro2::{Ident, Literal, Span, TokenStream, TokenTree};
use quote::{ToTokens, TokenStreamExt};
use std::{collections::HashMap, str::FromStr, sync::Mutex};
use syn::{Attribute, Meta, Type, Variant, Path};

#[allow(clippy::from_str_radix_10)]
fn parse_int(str: &str) -> Result<usize, std::num::ParseIntError> {
    if let Some(str) = str.strip_prefix("0x") {
        usize::from_str_radix(str, 16)
    } else {
        usize::from_str_radix(str, 10)
    }
}

// State shared between #[genmatch] and #[genmatch_<id|self>] calls
struct GlobalState {
    enums: HashMap<String, EnumRef>,
    pending_match_fns: HashMap<String, Vec<EnumMatchFn>>,
}

impl GlobalState {
    pub fn new() -> Self {
        GlobalState {
            enums: HashMap::new(),
            pending_match_fns: HashMap::new(),
        }
    }
}

lazy_static! {
    static ref CACHE: Mutex<GlobalState> = Mutex::new(GlobalState::new());
}

/// Saved data about the generated (final) enum
#[derive(Debug, Clone)]
struct EnumRef {
    name: String,
    variants: Vec<EnumVariantRef>,
}

/// Enum variant in the generated (final) enum
#[derive(Debug, Clone)]
struct EnumVariantRef {
    id: EnumVariantId,
    name: String,
    struct_path: String,
}

/// Enum variant extracted from the original enum.
struct EnumVariant {
    id: EnumVariantId,
    name: Ident,
    struct_path: Path,
    attrs: Vec<Attribute>,
}

/// ToTokens into the final (generated) enum.
impl ToTokens for EnumVariant {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let name = &self.name;
        let struct_path = &self.struct_path;
        let attrs = &self.attrs;
        tokens.extend(quote! {
            #(#attrs)*
            #name(#struct_path)
        })
    }
}

#[derive(Debug, Clone)]
enum EnumVariantId {
    /// Regular match case
    Explicit(usize),
    /// No ID provided - use <struct_name>::ID
    NotSpecified { struct_name: String },
    /// Default match case
    Default,
}

impl ToTokens for EnumVariantId {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            EnumVariantId::Explicit(id) => tokens.append(Literal::usize_unsuffixed(*id)),
            EnumVariantId::NotSpecified { struct_name } => {
                let struct_name = TokenStream::from_str(struct_name).unwrap();
                tokens.extend(quote!(#struct_name::ID));
            }
            EnumVariantId::Default => tokens.append(Ident::new("_", Span::call_site())),
        }
    }
}

#[derive(Clone, Copy)]
enum EnumMatchType {
    /// Match by ID
    Id,
    /// Match by &self
    Variant,
}

impl ToTokens for EnumMatchType {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match &self {
            EnumMatchType::Id => {
                tokens.append(Ident::new("id", Span::call_site()));
            }
            EnumMatchType::Variant => {
                tokens.append(Ident::new("self", Span::call_site()));
            }
        };
    }
}

struct EnumVariantMatch<'a> {
    match_by: EnumMatchType,
    enum_name: &'a Ident,
    variant: &'a EnumVariantRef,
    case: &'a TokenStream,
}

impl<'a> ToTokens for EnumVariantMatch<'a> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let enum_name = self.enum_name;
        let variant_name = Ident::new(&self.variant.name, Span::call_site());
        let struct_path = TokenStream::from_str(&self.variant.struct_path).unwrap();
        let id = &self.variant.id;
        let case = &self.case;

        tokens.extend({
            match self.match_by {
                EnumMatchType::Id => quote! {
                    #id => {
                        use #struct_path as EnumStructType;
                        use #enum_name::#variant_name as EnumVariantType;
                        #case
                    },
                },
                EnumMatchType::Variant => quote! {
                    #enum_name::#variant_name(inner) => {
                        use #struct_path as EnumStructType;
                        use #enum_name::#variant_name as EnumVariantType;
                        #case
                    },
                },
            }
        });
    }
}

struct EnumVariantMatcher<'a> {
    match_by: EnumMatchType,
    enum_name: &'a Ident,
    variants: &'a Vec<EnumVariantRef>,
    case: TokenStream,
}

impl<'a> ToTokens for EnumVariantMatcher<'a> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let mut default_variants = self
            .variants
            .iter()
            .filter(|v| matches!(v.id, EnumVariantId::Default));

        // Print some pretty messages for otherwise hard-to-debug problems
        let default_variant = default_variants.next();
        if default_variants.next().is_some() {
            panic!("Only one variant with default ID (_) can be defined.");
        }

        for variant in self.variants {
            if let EnumVariantId::Default = variant.id {
                continue;
            }

            let m = EnumVariantMatch {
                match_by: self.match_by,
                enum_name: self.enum_name,
                variant,
                case: &self.case,
            };
            m.to_tokens(tokens);
        }

        if matches!(self.match_by, EnumMatchType::Id) || default_variant.is_some() {
            let default_variant = default_variant.expect(
                "Default variant must be defined. E.g:\n\
                        \t#[attr(ID = _)]\n\
                        Unknown",
            );
            let m = EnumVariantMatch {
                match_by: self.match_by,
                enum_name: self.enum_name,
                variant: default_variant,
                case: &self.case,
            };
            m.to_tokens(tokens);
        }
    }
}

impl TryFrom<Variant> for EnumVariant {
    type Error = ();

    fn try_from(variant: Variant) -> Result<Self, Self::Error> {
        let name = variant.ident.clone();
        let struct_path = match variant.fields {
            syn::Fields::Named(..) => None,
            syn::Fields::Unnamed(struct_name) => struct_name
                .unnamed
                .into_iter()
                .next()
                .and_then(|f| match f.ty {
                    Type::Path(p) => Some(p.path),
                    _ => None,
                }),
            syn::Fields::Unit => None,
        }
        .unwrap_or_else(|| {
            panic!(
                "Invalid variant syntax. Expected {}(optional_path::to::StructName)",
                name
            )
        });

        // Parse variant's attributes
        let mut attrs = variant.attrs;
        let internal_attrs_idx = attrs.iter().position(|a| match &a.meta {
            Meta::List(list) => {
                if let Some(ident) = list.path.get_ident() {
                    *ident == "attr"
                } else {
                    false
                }
            }
            _ => false,
        });


        let id = match internal_attrs_idx {
            Some(internal_attrs_idx) => {
                let internal_attrs = attrs.remove(internal_attrs_idx);
                let Meta::List(internal_attrs) = internal_attrs.meta else {
                    panic!("`attr` attribute needs to describe a list. E.g: #[attr(ID = 0x42)]");
                };

                let mut tokens_iter = internal_attrs.tokens.into_iter();
                let mut id: Option<EnumVariantId> = None;

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
                            let value = tokens_iter
                                .next()
                                .expect("Unknown attr syntax. Expected `#[attr(ID = 0x42)]`");

                            id = Some(match &value {
                                TokenTree::Ident(ident) => {
                                    if *ident == "_" {
                                        EnumVariantId::Default
                                    } else {
                                        let str = value.to_string();
                                        EnumVariantId::Explicit(
                                            parse_int(&str)
                                                .expect("Invalid ID attribute. Expected a number"),
                                        )
                                    }
                                }
                                _ => {
                                    let str = value.to_string();
                                    EnumVariantId::Explicit(
                                        parse_int(&str)
                                            .expect("Invalid ID attribute. Expected a number"),
                                    )
                                }
                            });
                        }
                        name => {
                            panic!("Unknown attribute `{name}`")
                        }
                    }
                }

                id.expect("Incomplete `attr` attribute. Expected e.g. #[attr(ID = 0x42)]")
            }
            None => EnumVariantId::NotSpecified { struct_name: struct_path.to_token_stream().to_string() }
        };

        Ok(EnumVariant { id, name, struct_path, attrs })
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

/// Mark the structure for further use with `#[genmatch_id]` or
/// `#[genmatch_self]`. The `#[genmatch]` macro itself doesn't have any
/// effect, but must be specified in order to use `#[genmatch_<id|self>]`.
/// All variants in the attributed enum must contain a single unnamed field
/// - a struct name.
///
/// The variants can be given a custom attribute to be used with
/// `#[genmatch_id]`. Either:
///  - `#[attr(ID = 0x42)]` (numerical ID)
///  - `#[attr(ID = _)]` (default match case)
/// See `#[genmatch_id]` for details.
///
/// # Examples
///
/// ```rust
/// use genmatch::*;
///
/// #[genmatch]
/// enum Payload {
///     Hello(Hello),
///     Goodbye(Goodbye),
/// }
///
/// struct Hello {
///     pub a: u8,
///     pub b: u64,
/// }
///
/// struct Goodbye {
///     pub e: u8,
/// }
///
/// impl Payload {
///     #[genmatch_self(Payload)]
///     fn size(&self) -> usize {
///         std::mem::size_of_val(inner)
///     }
/// }
/// ```
///
/// For `#[genmatch_id]`, each enum variant must be given an ID. For each
/// element, this can be done in two ways:
///  - with `#[attr(ID = ...)]` attribute inside the enum
///  - by specifying `const ID: usize = 0x42` for the given structure
///
/// Both methods can be used at the same time, with `#[attr(ID = ...)]` having
/// higher priority. #[genmatch_id(...)]` requires each variant to have a
/// unique ID, and exactly one variant must be given the default ID with
/// `#[attr(ID = ...)]`. See #[genmatch_id] proc macro for details.
#[proc_macro_attribute]
pub fn genmatch(
    attr: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let attr: TokenStream = attr.into();
    if !attr.is_empty() {
        panic!("#[genmatch] macro doesn't accept any parameters");
    }

    let ast = syn::parse_macro_input!(input as syn::DeriveInput);
    let enum_vis = ast.vis;
    let enum_attrs = ast.attrs;
    let enum_ident = ast.ident;

    // Extract the enum variants
    let variants: Vec<syn::Variant> = match ast.data {
        syn::Data::Enum(data_enum) => data_enum.variants.into_iter().collect(),
        _ => panic!("#[genmatch] expects enum"),
    };

    // Organize info about variants
    let variants = variants
        .into_iter()
        .map(EnumVariant::try_from)
        .collect::<Result<Vec<EnumVariant>, _>>()
        .unwrap();

    // Re-create the original enum, minus the internal attributes
    let ret_stream = quote! {
        #(#enum_attrs)*
        #enum_vis enum #enum_ident {
            #(#variants),*
        }
    };

    // Lastly, save a global ref to this enum
    if let Ok(mut cache) = CACHE.lock() {
        let prev_val = cache.enums.insert(
            enum_ident.to_string(),
            EnumRef {
                name: enum_ident.to_string(),
                variants: variants
                    .iter()
                    .map(|v| EnumVariantRef {
                        id: v.id.clone(),
                        name: v.name.to_string(),
                        struct_path: v.struct_path.to_token_stream().to_string(),
                    })
                    .collect(),
            },
        );

        if prev_val.is_some() {
            // TODO Lift this limitation after Span::source_file() is implemented
            // https://github.com/rust-lang/rust/issues/54725
            // We would put source file into the hashmap id, although ideally we would
            // like caller's module instead.
            drop(cache);
            panic!("Enum name conflict! Consider using a different unique name, then create an alias to desired name");
        } else if let Some(pending_match_fns) =
            cache.pending_match_fns.remove(&enum_ident.to_string())
        {
            let enumref = cache.enums.get(&enum_ident.to_string()).unwrap();

            for pending in pending_match_fns {
                match_with_enum(enumref, &pending);
            }
        }
    } else {
        panic!("Internal chache is corrupted. Fix other problems and restart the compilation")
    }

    ret_stream.into()
}

/// Parsed #[genmatch_<id|self>](...)]. In case the enum definition is not available,
/// and the impl needs to be stored in the global state.
struct EnumMatchFn {
    match_by: EnumMatchType,
    fn_str: String,
}

fn match_with_enum(enumref: &EnumRef, enum_match_fn: &EnumMatchFn) -> proc_macro2::TokenStream {
    let enum_name = Ident::new(&enumref.name, Span::call_site());
    let mut tokens: Vec<TokenTree> = proc_macro2::TokenStream::from_str(&enum_match_fn.fn_str)
        .unwrap()
        .into_iter()
        .collect();

    // We're expecting a function, so last Group should be the function body
    let body = tokens
        .pop()
        .and_then(|t| {
            if let TokenTree::Group(g) = t {
                Some(g.stream())
            } else {
                None
            }
        })
        .expect("#[genmatch_<id|self>(...)] has to be used on function definition");

    let variant_matcher = EnumVariantMatcher {
        match_by: enum_match_fn.match_by,
        enum_name: &enum_name,
        variants: &enumref.variants,
        case: body,
    };

    let match_by = &variant_matcher.match_by;
    quote!(
        #(#tokens)* {
            match #match_by {
                #variant_matcher
            }
        }
    )
}

fn process_match_fn(enum_name: String, enum_match_fn: EnumMatchFn) -> proc_macro::TokenStream {
    if enum_name.is_empty() {
        panic!("Argument is missing. Expected `#[genmatch_<id|self>(MyEnumName)]`");
    }

    let mut cache = CACHE.lock().unwrap();
    if let Some(enumref) = cache.enums.get(&enum_name) {
        match_with_enum(enumref, &enum_match_fn).into()
    } else {
        // We may be called before #[genmatch], so handle it by storing
        // this (stringified) function into cache. Unfortunately we don't
        // know if the enum exists at all. If it doesn't, this function
        // won't be ever instantiated, and won't generate any warning.
        let pending_vec = cache.pending_match_fns.entry(enum_name).or_default();
        pending_vec.push(enum_match_fn);
        proc_macro::TokenStream::new()
    }
}

/// Provide EnumStructType and EnumVariantType aliases to the function body,
/// which correspond to enum variant with provided `id`. The `id` is expected
/// to be one of the function parameters.
///
/// This works by replacing the function body with an `id` match expression,
/// where every match arm is filled with the original body, but preceeded with
/// different `use X as EnumStructType`. For this reason it's recommended to
/// keep the function body minimal, potentially putting the common logic
/// elsewhere.
///
/// # Examples
/// ```rust
/// use genmatch::*;
///
/// #[genmatch]
/// enum Payload {
///     #[attr(ID = 0x2b)]
///     Hello(Hello),
///     Goodbye(Goodbye),
///     #[attr(ID = _)]
///     Invalid(Invalid)
/// }
///
/// #[derive(Default)]
/// struct Hello {
///     pub a: u32,
///     pub b: u32,
/// }
///
/// #[derive(Default)]
/// struct Goodbye {
///     pub e: u32,
/// }
///
/// impl Goodbye {
///     const ID: usize = 0x42;
/// }
///
/// #[derive(Default)]
/// struct Invalid {}
///
/// impl Payload {
///     #[genmatch_id(Payload)]
///     pub fn default(id: usize) -> Payload {
///         EnumVariantType(EnumStructType::default())
///     }
/// }
/// ```
///
/// The `default` function expands to:
///
/// ```ignore
/// pub fn default(id: usize) -> Payload {
///     match id {
///         43 => {
///             use Hello as EnumStructType;
///             use Payload::Hello as EnumVariantType;
///             EnumVariantType(EnumStructType::default())
///         }
///         Goodbye::ID => {
///             use Goodbye as EnumStructType;
///             use Payload::Goodbye as EnumVariantType;
///             EnumVariantType(EnumStructType::default())
///         }
///         _ => {
///             use Invalid as EnumStructType;
///             use Payload::Invalid as EnumVariantType;
///             EnumVariantType(EnumStructType::default())
///         }
///     }
/// }
/// ```
#[proc_macro_attribute]
pub fn genmatch_id(
    attr: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let attr: TokenStream = attr.into();
    let enum_name = attr.to_string();

    let enum_match_fn = EnumMatchFn {
        match_by: EnumMatchType::Id,
        fn_str: input.to_string(),
    };

    process_match_fn(enum_name, enum_match_fn)
}

/// Provide `inner` alias to the function body, which corresponds to the
/// object stored inside the `self`'s variant.
///
/// This works by replacing the function body with a `self` match expression,
/// where every match arm is filled with the original body. For this reason
/// it's recommended to keep the function body minimal, potentially putting
/// the common logic elsewhere.
///
/// This macro can be used on function with either `self`, `&self` or
/// `&mut self` parameter.
///
/// # Examples
/// ```rust
/// use genmatch::*;
///
/// #[genmatch]
/// pub enum Payload {
///     Hello(Hello),
///     Goodbye(Goodbye),
/// }
///
/// pub struct Hello {
///     pub a: u32,
///     pub b: u32,
/// }
///
/// pub struct Goodbye {
///     pub e: u32,
/// }
///
/// impl Payload {
///     #[genmatch_self(Payload)]
///     pub fn size(&self) -> usize {
///         std::mem::size_of_val(inner)
///     }
/// }
/// ```
///
/// The `size` function expands to:
///
/// ```ignore
/// impl Payload {
///     pub fn size(&self) -> usize {
///         match &self {
///             Payload::Hello(inner) => {
///                 std::mem::size_of_val(inner)
///             }
///             Payload::Goodbye(inner) => {
///                 std::mem::size_of_val(inner)
///             }
///         }
///     }
/// }
/// ```
#[proc_macro_attribute]
pub fn genmatch_self(
    attr: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let attr: TokenStream = attr.into();
    let enum_name = attr.to_string();

    let enum_match_fn = EnumMatchFn {
        match_by: EnumMatchType::Variant,
        fn_str: input.to_string(),
    };

    process_match_fn(enum_name, enum_match_fn)
}
