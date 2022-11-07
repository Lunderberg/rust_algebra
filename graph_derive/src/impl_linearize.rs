use std::collections::{HashMap, HashSet};

use quote::{format_ident, quote, ToTokens};
use syn::{fold::Fold, parse_macro_input};

use itertools::Itertools;

fn is_attr(path: &syn::Path, name: &str) -> bool {
    path.segments.len() == 1 && path.segments[0].ident == name
}

fn collect_annotated_enums(
    mut item_mod: syn::ItemMod,
) -> (syn::ItemMod, Vec<(syn::ItemEnum, Vec<syn::ItemEnum>)>) {
    if let Some((brace, content)) = item_mod.content {
        let (enums, other): (Vec<_>, Vec<_>) = content.into_iter().partition(|item| match item {
            syn::Item::Enum(item) => item
                .attrs
                .iter()
                .any(|attr| is_attr(&attr.path, "requires_graph_storage_type")),
            _ => false,
        });

        let enums: Vec<syn::ItemEnum> = enums
            .into_iter()
            .map(|item: syn::Item| -> syn::ItemEnum {
                if let syn::Item::Enum(item_enum) = item {
                    item_enum
                } else {
                    panic!("List should only contain enums at this point")
                }
            })
            .collect();

        let enums: Vec<(syn::ItemEnum, Vec<syn::Ident>)> = enums
            .into_iter()
            .map(|mut item| {
                let referred: Vec<syn::Ident> = item
                    .attrs
                    .iter()
                    .filter(|attr| is_attr(&attr.path, "requires_graph_storage_type"))
                    .map(|attr| {
                        match attr.parse_meta() {
                            Ok(syn::Meta::List(meta_list)) => meta_list,
                            _ => panic!("Incorrect graph storage format"),
                        }
                        .nested
                        .into_iter()
                        .map(|meta| -> syn::Path {
                            match meta {
                                syn::NestedMeta::Meta(syn::Meta::Path(path)) => path,
                                _ => panic!("Incorrect graph storage format"),
                            }
                        })
                        .map(|path| -> syn::Ident { path.segments.last().unwrap().ident.clone() })
                    })
                    .flatten()
                    .collect();

                item.attrs = item
                    .attrs
                    .into_iter()
                    .filter(|attr| !is_attr(&attr.path, "requires_graph_storage_type"))
                    .collect();
                (item, referred)
            })
            .collect();

        let lookup: HashMap<syn::Ident, syn::ItemEnum> = enums
            .iter()
            .map(|(item_enum, _)| (item_enum.ident.clone(), item_enum.clone()))
            .collect();

        let enums: Vec<(syn::ItemEnum, Vec<syn::ItemEnum>)> = enums
            .into_iter()
            .map(|(item_enum, idents)| {
                let referenced = idents
                    .into_iter()
                    .map(|ident| lookup.get(&ident).expect("Ident not in map").clone())
                    .collect();
                (item_enum, referenced)
            })
            .collect();

        item_mod.content = Some((brace, other));

        (item_mod, enums)
    } else {
        (item_mod, Vec::new())
    }
}

fn generate_storage_enum(
    item_enum: &syn::ItemEnum,
    referenced: &Vec<syn::ItemEnum>,
) -> impl Iterator<Item = syn::Item> {
    struct Mutator {
        referenced: HashSet<syn::Ident>,
    }

    impl Fold for Mutator {
        fn fold_type(&mut self, ty: syn::Type) -> syn::Type {
            if let syn::Type::Path(syn::TypePath { path, .. }) = &ty {
                let ident = &path.segments.last().unwrap().ident;
                if self.referenced.contains(&ident) {
                    return syn::parse2(quote! { GraphRef<#path> })
                        .expect("Error parsing generated storage enum");
                }
            }
            ty
        }
    }

    let item_enum = item_enum.clone();
    std::iter::once(
        Mutator {
            referenced: referenced
                .iter()
                .map(|item_enum| item_enum.ident.clone())
                .collect(),
        }
        .fold_item_enum(item_enum)
        .into(),
    )
}

fn generate_storage_trait_impl<'a, 'b, 'c>(
    item_enum: &'b syn::ItemEnum,
    referenced_enums: &'a Vec<syn::ItemEnum>,
) -> impl Iterator<Item = syn::Item> + 'c
where
    'a: 'c,
    'b: 'c,
{
    let selector = &item_enum.ident;

    let referenced_names: HashSet<syn::Ident> = referenced_enums
        .iter()
        .map(|item_enum| item_enum.ident.clone())
        .collect();
    let is_recursive_type = move |ty: &syn::Type| -> bool {
        match ty {
            syn::Type::Path(path) => {
                let ident = &path.path.segments.last().unwrap().ident;
                referenced_names.contains(ident)
            }
            _ => false,
        }
    };

    referenced_enums.iter().map(move |referenced_enum| {
        let referenced_name = &referenced_enum.ident;
        let live_type_arms: Vec<syn::Arm> = referenced_enum.variants
            .iter()
            .map(|var: &syn::Variant| -> syn::Arm {
                let ident = &var.ident;
                match &var.fields {
                    syn::Fields::Named(_) => todo!("Named enum fields"),
                    syn::Fields::Unnamed(fields) => {
                        let (field_names,live_field_exprs): (Vec<_>, Vec<_>) = fields.unnamed.iter()
                            .enumerate()
                            .map(|(i,field)| {
                                let ident = format_ident!("field_{i}");
                                let stream = if is_recursive_type(&field.ty) {
                                    quote!{ LiveGraphRef::new(*#ident, subgraph.clone()) }
                                } else {
                                    quote!{ *#ident }
                                };
                                let expr: syn::Expr = syn::parse2(stream).expect("Error parsing generated live field");
                                (ident,expr)
                            })
                            .unzip();
                        syn::parse2(quote!{
                            Self::#ident(#(#field_names),*) =>
                                Self::LiveType::#ident(#(#live_field_exprs),*),
                        }).expect("Error parsing generated arm in match statement")
                    }
                    syn::Fields::Unit => {
                        syn::parse2(quote!{
                            Self::#ident => Self::LiveType::#ident,
                        }).unwrap()
                    }
                }
            })
            .collect();

        let stream = quote! {
            impl NodeType<super::selector::#selector> for #referenced_name {
                type LiveType<'a> = super::live::#referenced_name<'a, super::selector::#selector>;

                const NAME: &'static str = "#referenced_name";

                fn from_base(base: &super::selector::#selector) -> Option<&Self> {
                    match base {
                        super::selector::#selector::#referenced_name(e) => Some(e),
                        _ => None,
                    }
                }

                fn to_live_type<'a, 'b: 'a>(&self, subgraph: Subgraph<'b, super::selector::#selector>) -> Self::LiveType<'a> {
                    match self {
                        #(#live_type_arms)*
                    }
                }
            }
        };
        syn::parse2(stream)
            .expect("Error parsing generated storage trait")
    })
}

fn generate_live_enum(
    item_enum: &syn::ItemEnum,
    referenced: &Vec<syn::ItemEnum>,
) -> impl Iterator<Item = syn::Item> {
    struct Mutator {
        referenced: HashSet<syn::Ident>,
    }

    impl Fold for Mutator {
        fn fold_generics(&mut self, generics: syn::Generics) -> syn::Generics {
            let params = generics.params;
            syn::parse2(quote! {
                <'a, Selector: NodeTypeSelector, #params>
            })
            .expect("Error parsing generated generics for live enum")
        }

        fn fold_type(&mut self, ty: syn::Type) -> syn::Type {
            if let syn::Type::Path(syn::TypePath { path, .. }) = &ty {
                let ident = &path.segments.last().unwrap().ident;
                if self.referenced.contains(&ident) {
                    return syn::parse2(
                        quote! { LiveGraphRef<'a, Selector, super::storage::#path> },
                    )
                    .expect("Error parsing generated type for live enum");
                }
            }
            ty
        }
    }

    let item_enum = item_enum.clone();
    std::iter::once(
        Mutator {
            referenced: referenced
                .iter()
                .map(|item_enum| item_enum.ident.clone())
                .collect(),
        }
        .fold_item_enum(item_enum)
        .into(),
    )
}

fn generate_selector_enum(
    item: &syn::ItemEnum,
    referenced_enums: &Vec<syn::ItemEnum>,
) -> impl Iterator<Item = syn::Item> {
    let mut item = item.clone();
    item.variants = referenced_enums
        .iter()
        .map(|referred_enum: &syn::ItemEnum| -> syn::Variant {
            let ident = &referred_enum.ident;
            syn::parse2::<syn::Variant>(quote! {
                #ident(super::storage::#ident)
            })
            .expect("Error parsing generated selector enum")
        })
        .collect();
    std::iter::once(item.into())
}

fn generate_selector_trait_impl(
    item: &syn::ItemEnum,
    referenced_enums: &Vec<syn::ItemEnum>,
) -> impl Iterator<Item = syn::Item> {
    let name = &item.ident;

    let referenced_names: Vec<String> = referenced_enums
        .iter()
        .map(|item_enum| {
            let p = item_enum.ident.to_token_stream();
            format!("{p}")
        })
        .collect();

    let referenced_idents: Vec<&syn::Ident> = referenced_enums
        .iter()
        .map(|item_enum| &item_enum.ident)
        .collect();

    let stream = quote! {
            impl NodeTypeSelector for #name {
                type LiveType<'a> = super::live_selector::#name<'a, Self>;

                fn type_name(&self) -> &'static str {
                    match self {
                        #(Self::#referenced_idents(_) => #referenced_names,)*
                    }
                }

                fn to_live_type<'a, 'b: 'a>(&self, subgraph: Subgraph<'b, Self>) -> Self::LiveType<'a> {
                    match self {
                        #(Self::#referenced_idents(e) => Self::LiveType::#referenced_idents(e.to_live_type(subgraph)),)*
                    }
                }
            }
    };

    std::iter::once(syn::parse2(stream).expect("Error parsing generated selector trait"))
}

fn generate_selector_from_storage_impl<'a>(
    item: &'a syn::ItemEnum,
    referenced_enums: &'a Vec<syn::ItemEnum>,
) -> impl Iterator<Item = syn::Item> + 'a {
    let name = &item.ident;

    referenced_enums.iter().map(move |referenced_enum| {
        let ref_ident = &referenced_enum.ident;

        let stream = quote! {
            impl From<super::storage::#ref_ident> for #name {
                fn from(val: super::storage::#ref_ident) -> Self {
                    Self::#ref_ident(val)
                }
            }
        };

        syn::parse2(stream).unwrap()
    })
}

fn generate_live_selector_enum(
    item: &syn::ItemEnum,
    recursive: &Vec<syn::ItemEnum>,
) -> impl Iterator<Item = syn::Item> {
    let mut item = item.clone();
    item.variants = recursive
        .iter()
        .map(|item_enum: &syn::ItemEnum| -> syn::Variant {
            let ident = &item_enum.ident;
            syn::parse2::<syn::Variant>(quote! {
                #ident(super::live::#ident<'a, Selector>)
            })
            .expect("Error in generated live selector enum")
        })
        .collect();

    let prev_generics = item.generics.params;
    item.generics = syn::parse2(quote! {
        <'a, Selector: NodeTypeSelector, #prev_generics>
    })
    .expect("Error parsing generics of live selector");

    std::iter::once(item.into())
}

fn apply_generator<'a, G, I>(
    enums: &'a Vec<(syn::ItemEnum, Vec<syn::ItemEnum>)>,
    dest_module: &'a str,
    mut generator: G,
) -> impl Iterator<Item = (&'a str, syn::Item)> + 'a
where
    G: FnMut(&'a syn::ItemEnum, &'a Vec<syn::ItemEnum>) -> I + 'a,
    I: Iterator<Item = syn::Item>,
{
    enums
        .iter()
        .map(move |(e, p)| generator(e, p))
        .flatten()
        .map(move |item| (dest_module, item))
}

pub fn linearize_recursive_enums(
    _attr: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let orig: syn::ItemMod = parse_macro_input!(item as syn::ItemMod);

    let (orig_mod, enums) = collect_annotated_enums(orig);
    let enums: Vec<_> = enums
        .into_iter()
        .map(|(mut item_enum, paths)| {
            item_enum.vis = syn::parse2(quote! {pub}).unwrap();
            (item_enum, paths)
        })
        .collect();

    let modules: Vec<_> = std::iter::empty::<(&str, syn::Item)>()
        .chain(apply_generator(&enums, "storage", generate_storage_enum))
        .chain(apply_generator(
            &enums,
            "storage",
            generate_storage_trait_impl,
        ))
        .chain(apply_generator(&enums, "live", generate_live_enum))
        .chain(apply_generator(&enums, "selector", generate_selector_enum))
        .chain(apply_generator(
            &enums,
            "live_selector",
            generate_live_selector_enum,
        ))
        .chain(apply_generator(
            &enums,
            "selector",
            generate_selector_trait_impl,
        ))
        .chain(apply_generator(
            &enums,
            "selector",
            generate_selector_from_storage_impl,
        ))
        .into_group_map()
        .into_iter()
        .map(|(name, items)| {
            let mut item_mod = orig_mod.clone();
            item_mod.ident = format_ident!("{}", name);
            if !items.is_empty() {
                let content = item_mod
                    .content
                    .map(|(_brace, vec)| vec.into_iter())
                    .into_iter()
                    .flatten()
                    .chain(items.into_iter())
                    .collect();

                item_mod.content = Some((syn::token::Brace::default(), content));
            }
            item_mod.vis = syn::parse2(quote! {pub}).unwrap();
            item_mod
        })
        .collect();

    let mut out_mod = orig_mod;
    let content = out_mod
        .content
        .map(|tuple| tuple.1.into_iter())
        .into_iter()
        .flatten()
        .chain(modules.into_iter().map(|item_mod| item_mod.into()))
        .collect();
    out_mod.content = Some((syn::token::Brace::default(), content));
    out_mod.to_token_stream().into()
}
