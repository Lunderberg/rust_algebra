#[allow(unused_imports)]
use std::collections::HashSet;
use std::fmt::Display;
use std::iter::Peekable;

#[allow(unused_imports)]
use proc_macro2::{Delimiter, Group, Ident, Punct, Spacing, Span, TokenStream, TokenTree};

use quote::{format_ident, quote, ToTokens};
use syn::parse::Parse;
use syn::parse_macro_input;

use itertools::Itertools;

#[derive(Debug, Clone)]
struct EnumDef {
    enum_type: Type,
    variants: Vec<EnumVariant>,
    attributes: Vec<Attribute>,
}

#[derive(Debug, Clone)]
struct Attribute {
    attr: Group,
}

#[derive(Debug, Clone)]
struct Type {
    name: Ident,
    generics: Vec<Generic>,
}

#[derive(Debug, Clone)]
enum Generic {
    Type(Type),
    Lifetime(Lifetime),
}

#[derive(Debug, Clone)]
struct Lifetime {
    name: Ident,
}

#[derive(Debug, Clone)]
struct EnumVariant {
    name: Ident,
    params: EnumVariantParams,
}

#[derive(Debug, Clone)]
enum EnumVariantParams {
    None,
    Tuple(Vec<Type>),
    Struct(Vec<(Ident, Type)>),
}

trait Parser {
    fn next_enum(&mut self) -> Option<EnumDef>;
    fn next_ident(&mut self) -> Option<Ident>;
    fn next_generic(&mut self) -> Option<Generic>;
    fn next_lifetime(&mut self) -> Option<Lifetime>;
    fn next_type(&mut self) -> Option<Type>;
    fn next_enum_variant(&mut self) -> Option<EnumVariant>;
    fn next_enum_variant_params(&mut self) -> EnumVariantParams;
    fn next_attribute(&mut self) -> Option<Attribute>;
    fn next_keyword(&mut self, keyword: &str) -> Option<()>;
    fn expect_punct(&mut self, c: char);
    fn expect_comma_or_end(&mut self);
    fn peek_punct(&mut self, c: char) -> bool;
}

impl<I: Iterator<Item = TokenTree>> Parser for Peekable<I> {
    fn next_enum(&mut self) -> Option<EnumDef> {
        let attributes = std::iter::from_fn(|| self.next_attribute()).collect();

        self.next_keyword("enum")?;
        let enum_type = self
            .next_type()
            .expect("Expected type definition after 'enum' keyword");

        let tt = self.next().expect("Expected ; or { after enum type name");
        let variants = match tt {
            TokenTree::Punct(p) if p.as_char() == ';' => Vec::new(),
            TokenTree::Group(g) if g.delimiter() == Delimiter::Brace => {
                let mut tokens = g.stream().into_iter().peekable();
                std::iter::from_fn(|| tokens.next_enum_variant()).collect()
            }
            tt => {
                panic!("Expected ; or {{ after enum type name, but found {tt}");
            }
        };

        Some(EnumDef {
            enum_type,
            variants,
            attributes,
        })
    }

    fn next_attribute(&mut self) -> Option<Attribute> {
        self.next_if(|tt| match tt {
            TokenTree::Punct(p) if p.as_char() == '#' => true,
            _ => false,
        })
        .map(|_| {
            let tt = self.next().expect("Unexpected end.  After #, expected a bracket-delimited group defining the attribute.");
            match tt {
                TokenTree::Group(attr) if attr.delimiter() == Delimiter::Bracket => Attribute { attr },
                tt => panic!("After #, expected a bracket-delimited group defining the attribute, but found {tt}"),
            }
        })
    }

    fn next_keyword(&mut self, keyword: &str) -> Option<()> {
        let token = self.next()?;
        if token.to_string() != keyword {
            panic!("Expected keyword '{keyword}', but received '{token}'");
        }
        Some(())
    }

    fn peek_punct(&mut self, c: char) -> bool {
        self.peek().map_or(false, |tt| match tt {
            TokenTree::Punct(p) if p.as_char() == c => true,
            _ => false,
        })
    }

    fn expect_punct(&mut self, c: char) {
        let token = self.next().expect("Expected {c}, but hit eof");
        match token {
            TokenTree::Punct(p) if p.as_char() == c => {}
            _ => panic!("Expected {c}, but received {token}"),
        }
    }

    fn next_ident(&mut self) -> Option<Ident> {
        match self.next()? {
            TokenTree::Ident(ident) => Some(ident),
            tt => panic!("Expected ident, but found {tt}"),
        }
    }

    fn next_generic(&mut self) -> Option<Generic> {
        if self.peek_punct('\'') {
            self.next_lifetime().map(|lifetime| lifetime.into())
        } else {
            self.next_type().map(|kind| kind.into())
        }
    }

    fn next_lifetime(&mut self) -> Option<Lifetime> {
        match self.next()? {
            TokenTree::Punct(p) if p.as_char() == '\'' && p.spacing() == Spacing::Joint => {}
            tt => {
                panic!("Expected lifetime to have ' prefix, but found {tt}")
            }
        }

        let ident = self.next_ident().expect("Lifetime missing identifier");
        Some(Lifetime { name: ident })
    }

    fn next_type(&mut self) -> Option<Type> {
        let name = self.next_ident()?;
        let generics = if self.peek_punct('<') {
            self.expect_punct('<');
            std::iter::from_fn(|| -> Option<Generic> {
                let tt = self.next().expect("No matching > found in type generic");
                match tt {
                    TokenTree::Punct(p) if p.as_char() == '>' => None,
                    TokenTree::Punct(p) if p.as_char() == ',' => Some(
                        self.next_generic()
                            .expect("Expected lifetime or type argument"),
                    ),
                    _ => {
                        panic!("Expected type, but found {tt}")
                    }
                }
            })
            .collect()
        } else {
            Vec::new()
        };
        Some(Type { name, generics })
    }

    fn next_enum_variant(&mut self) -> Option<EnumVariant> {
        let name = self.next_ident()?;
        let params = self.next_enum_variant_params();
        Some(EnumVariant { name, params })
    }

    fn next_enum_variant_params(&mut self) -> EnumVariantParams {
        let output = self
            .next_if(|tt| match tt {
                TokenTree::Group(_) => true,
                _ => false,
            })
            .map(|tt| match tt {
                TokenTree::Group(g) => g,
                _ => panic!("Internal error"),
            })
            .map(|group| -> EnumVariantParams {
                match group.delimiter() {
                    Delimiter::Parenthesis => {
                        let mut tokens = group.stream().into_iter().peekable();
                        let types = std::iter::from_fn(|| -> Option<Type> {
                            let out = tokens.next_type();
                            tokens.expect_comma_or_end();
                            out
                        }).collect();
                        EnumVariantParams::Tuple(types)
                    }
                    Delimiter::Brace => {
                        let mut tokens = group.stream().into_iter().peekable();
                        let types = std::iter::from_fn(|| -> Option<(Ident,Type)> {
                            let ident = tokens.next_ident()?;
                            self.expect_punct(':');
                            let ptype = tokens.next_type().expect(&format!(
                                "Struct-style enum variant ended without a type for {ident}"
                            ));

                            tokens.expect_comma_or_end();

                            Some((ident, ptype))

                        }).collect();
                        EnumVariantParams::Struct(types)
                    }
                    _ => {panic!("Expected enum variant parameters to be delimited by braces {{}} or parentheses ().")}
                }
            })
            .unwrap_or(EnumVariantParams::None);

        self.next_if(|tt| match tt {
            TokenTree::Punct(p) if p.as_char() == ',' => true,
            _ => false,
        });

        output
    }

    fn expect_comma_or_end(&mut self) {
        match self.next() {
            None => {}
            Some(TokenTree::Punct(p)) if p.as_char() == ',' => {}
            Some(tt) => {
                panic!("Expected list to be comma-delimited, but found {tt}")
            }
        }
    }
}

impl EnumDef {
    fn map_inner_types<F>(&self, mut update_type: F) -> Self
    where
        F: FnMut(&Type) -> Type,
    {
        self.with_variants(
            self.variants
                .iter()
                .map(|variant| variant.map_types(|t| update_type(t)))
                .collect(),
        )
    }

    fn with_variants(&self, variants: Vec<EnumVariant>) -> Self {
        Self {
            enum_type: self.enum_type.clone(),
            variants,
            attributes: self.attributes.clone(),
        }
    }
}

impl EnumVariant {
    fn map_types<F>(&self, mut update_type: F) -> Self
    where
        F: FnMut(&Type) -> Type,
    {
        let params = match &self.params {
            EnumVariantParams::None => EnumVariantParams::None,
            EnumVariantParams::Tuple(types) => {
                EnumVariantParams::Tuple(types.iter().map(|t| update_type(t)).collect())
            }
            EnumVariantParams::Struct(items) => EnumVariantParams::Struct(
                items
                    .iter()
                    .map(|(ident, t)| (ident.clone(), update_type(t)))
                    .collect(),
            ),
        };
        Self {
            name: self.name.clone(),
            params,
        }
    }
}

impl From<Type> for Generic {
    fn from(kind: Type) -> Self {
        Generic::Type(kind)
    }
}

impl From<Lifetime> for Generic {
    fn from(lifetime: Lifetime) -> Self {
        Generic::Lifetime(lifetime)
    }
}

impl Display for EnumDef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let attr = self
            .attributes
            .iter()
            .map(|attr| format!("{attr}\n"))
            .join("");
        if self.variants.is_empty() {
            write!(f, "{attr}enum {};", self.enum_type)
        } else {
            write!(
                f,
                "{attr}enum {} {{\n{}}}",
                self.enum_type,
                self.variants
                    .iter()
                    .map(|var| format!("    {var},\n"))
                    .join("")
            )
        }
    }
}

impl Display for Attribute {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "#{}", self.attr)
    }
}

impl Display for Generic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Generic::Type(kind) => write!(f, "{}", kind),
            Generic::Lifetime(lifetime) => write!(f, "{}", lifetime),
        }
    }
}

impl Display for Lifetime {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "'{}", self.name)
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)?;
        if !self.generics.is_empty() {
            write!(
                f,
                "<{}>",
                self.generics.iter().map(|t| format!("{}", t)).join(", ")
            )?;
        }
        Ok(())
    }
}

impl Display for EnumVariant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}", self.name, self.params)
    }
}

impl Display for EnumVariantParams {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            EnumVariantParams::None => Ok(()),
            EnumVariantParams::Tuple(vec) => {
                write!(f, "({})", vec.iter().map(|t| format!("{}", t)).join(", "))
            }
            EnumVariantParams::Struct(vec) => write!(
                f,
                "{{{}}}",
                vec.iter()
                    .map(|(name, t)| format!("{name}: {t}"))
                    .join(", ")
            ),
        }
    }
}

impl Into<TokenStream> for EnumDef {
    fn into(self) -> TokenStream {
        let attr: TokenStream = self
            .attributes
            .into_iter()
            .map(|attr| -> TokenStream {
                quote! { #attr }
            })
            .collect();
        let body: TokenTree = Group::new(
            Delimiter::Brace,
            self.variants
                .into_iter()
                .map(|variant| -> TokenStream { variant.into() })
                .collect(),
        )
        .into();
        let keyword: TokenTree = Ident::new("enum", Span::call_site()).into();
        let streams: Vec<TokenStream> =
            vec![attr, keyword.into(), self.enum_type.into(), body.into()];
        streams.into_iter().collect()
    }
}

impl ToTokens for EnumDef {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let attr = &self.attributes;
        let variants = &self.variants;
        let name = &self.enum_type;
        tokens.extend(quote! {
            #(#attr)*
            enum #name {
                #(#variants)*
            }
        });
    }
}

impl Into<TokenStream> for Attribute {
    fn into(self) -> TokenStream {
        let octothorpe = Punct::new('#', Spacing::Alone);
        let tokens: Vec<TokenTree> = vec![octothorpe.into(), self.attr.into()];
        tokens.into_iter().collect()
    }
}

impl ToTokens for Attribute {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let attr = &self.attr;
        tokens.extend(quote! { ##attr });
    }
}

impl Into<TokenStream> for Generic {
    fn into(self) -> TokenStream {
        match self {
            Generic::Type(kind) => kind.into(),
            Generic::Lifetime(lifetime) => quote! {#lifetime},
        }
    }
}

impl ToTokens for Generic {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.extend(match self {
            Generic::Type(kind) => quote! {#kind},
            Generic::Lifetime(lifetime) => quote! {#lifetime},
        });
    }
}

impl Into<TokenStream> for Lifetime {
    fn into(self) -> TokenStream {
        let tokens: Vec<TokenTree> =
            vec![Punct::new('\'', Spacing::Joint).into(), self.name.into()];
        tokens.into_iter().collect()
    }
}

impl ToTokens for Lifetime {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let lifetime = syn::Lifetime::new(&format!("'{}", self.name), self.name.span());
        tokens.extend(quote! {
             #lifetime
        });
    }
}

impl Into<TokenStream> for Type {
    fn into(self) -> TokenStream {
        let name: TokenTree = self.name.into();
        if self.generics.is_empty() {
            name.into()
        } else {
            let left: TokenTree = Punct::new('<', Spacing::Alone).into();
            let right: TokenTree = Punct::new('>', Spacing::Alone).into();
            let comma: TokenTree = Punct::new(',', Spacing::Alone).into();
            let comma: TokenStream = comma.into();

            // Silencing the warning about
            // itertools::Itertools::intersperse.  Can look into it more
            // later.
            #[allow(unstable_name_collisions)]
            let streams: Vec<TokenStream> = vec![
                name.into(),
                left.into(),
                self.generics
                    .into_iter()
                    .map(|t| -> TokenStream { t.into() })
                    .intersperse(comma)
                    .collect(),
                right.into(),
            ];
            streams.into_iter().collect()
        }
    }
}

impl ToTokens for Type {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let name = &self.name;
        let generics = &self.generics;
        tokens.extend(quote! {
            #name < #(#generics),* >
        });
    }
}

impl Into<TokenStream> for EnumVariant {
    fn into(self) -> TokenStream {
        let name = self.name;
        let params = self.params;
        quote! { #name #params, }
    }
}

impl ToTokens for EnumVariant {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let name = &self.name;
        let params = &self.params;
        tokens.extend(quote! { #name #params, });
    }
}

impl Into<TokenStream> for EnumVariantParams {
    fn into(self) -> TokenStream {
        // Silencing the warning about
        // itertools::Itertools::intersperse.  Can look into it more
        // later.
        #[allow(unstable_name_collisions)]
        match self {
            EnumVariantParams::None => TokenStream::new(),
            EnumVariantParams::Tuple(types) => quote! { ( #(#types),* ) },
            EnumVariantParams::Struct(items) => {
                let body: TokenStream = items
                    .into_iter()
                    .map(|(name, class)| quote! { #name: #class, })
                    .collect();
                quote! { { #body } }
            }
        }
    }
}

impl ToTokens for EnumVariantParams {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            EnumVariantParams::None => {}
            EnumVariantParams::Tuple(types) => tokens.extend(quote! { ( #(#types),* ) }),
            EnumVariantParams::Struct(items) => {
                let body: TokenStream = items
                    .into_iter()
                    .map(|(name, class)| quote! { #name: #class, })
                    .collect();
                tokens.extend(quote! { { #body } });
            }
        }
    }
}

#[allow(dead_code)]
fn with_graph_ref(enum_def: EnumDef, recursive_enums: &HashSet<String>) -> EnumDef {
    let update_type = |t: &Type| -> Type {
        if recursive_enums.contains(&format!("{}", t.name)) {
            let name = format_ident!("GraphRef");
            Type {
                name,
                generics: vec![t.clone().into()],
            }
        } else {
            t.clone()
        }
    };

    enum_def.map_inner_types(update_type)
}

#[allow(dead_code)]
fn with_live_graph_ref(enum_def: EnumDef, recursive_enums: &HashSet<String>) -> EnumDef {
    let lifetime: Generic = Lifetime {
        name: Ident::new("a", Span::call_site()),
    }
    .into();

    let update_type = |t: &Type| -> Type {
        if recursive_enums.contains(&format!("{}", t.name)) {
            let ref_name = format_ident!("LiveGraphRef");
            let inner_name = format_ident!("Live{}", t.name);
            let inner_type = Type {
                name: inner_name,
                generics: vec![lifetime.clone()],
            };
            Type {
                name: ref_name,
                generics: vec![lifetime.clone(), inner_type.into()],
            }
        } else {
            t.clone()
        }
    };

    let variants = enum_def
        .variants
        .iter()
        .map(|variant| variant.map_types(|t| update_type(t)))
        .collect();
    let enum_type = Type {
        name: Ident::new(
            &format!("Live{}", enum_def.enum_type.name),
            Span::call_site(),
        ),
        generics: std::iter::once(lifetime)
            .chain(enum_def.enum_type.generics.into_iter())
            .collect(),
    };
    EnumDef {
        enum_type,
        variants,
        attributes: enum_def.attributes,
    }
}

struct MyMacroInput {
    _enums: Vec<syn::ItemEnum>,
}

impl Parse for MyMacroInput {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let enums = std::iter::from_fn(|| {
            if input.is_empty() {
                None
            } else {
                Some(input.parse())
            }
        })
        .collect::<syn::Result<Vec<_>>>()?;
        Ok(Self { _enums: enums })
    }
}

#[proc_macro]
pub fn make_graph(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let _input = parse_macro_input!(input as MyMacroInput);

    "".parse().unwrap()
}
