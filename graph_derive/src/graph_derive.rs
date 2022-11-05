use std::collections::{HashMap, HashSet};

use proc_macro2::{Span, TokenStream};

use quote::{format_ident, quote, ToTokens};
use syn::{fold::Fold, parse_macro_input, visit::Visit};

use itertools::Itertools;

struct CollectDetails {
    ident_lookup: HashMap<syn::Ident, syn::ItemEnum>,
    direct_references: HashMap<syn::ItemEnum, HashSet<syn::ItemEnum>>,
    current: Option<HashSet<syn::ItemEnum>>,
}

impl<'ast> Visit<'ast> for CollectDetails {
    fn visit_item_enum(&mut self, i: &'ast syn::ItemEnum) {
        assert!(self.current.is_none(), "Nested enum definition");
        self.current = Some(HashSet::new());
        syn::visit::visit_item_enum(self, i);
        self.direct_references
            .insert(i.clone(), self.current.take().unwrap());
    }

    fn visit_field(&mut self, i: &'ast syn::Field) {
        if let Some(item_enum) = self.get_enum(&i.ty) {
            self.current
                .as_mut()
                .expect("Field occurs outside of enum")
                .insert(item_enum);
        }
    }
}

impl CollectDetails {
    fn get_enum(&self, ty: &syn::Type) -> Option<syn::ItemEnum> {
        match ty {
            syn::Type::Path(p) if p.path.segments.len() == 1 => Some(&p.path.segments[0].ident),
            _ => None,
        }
        .and_then(|ident| self.ident_lookup.get(ident))
        .cloned()
    }

    fn collect_direct_references(
        item_mod: &syn::ItemMod,
    ) -> HashMap<syn::ItemEnum, HashSet<syn::ItemEnum>> {
        let item_enums: Vec<syn::ItemEnum> = item_mod
            .content
            .as_ref()
            .expect("#[recursive_graph] module may not be empty")
            .1
            .iter()
            .filter_map(|item| match item {
                syn::Item::Enum(item_enum) => Some(item_enum),
                _ => None,
            })
            .cloned()
            .collect();

        let ident_lookup: HashMap<syn::Ident, syn::ItemEnum> = item_enums
            .iter()
            .map(|item_enum| (item_enum.ident.clone(), item_enum.clone()))
            .collect();

        let mut visitor = Self {
            ident_lookup,
            current: None,
            direct_references: HashMap::new(),
        };
        visitor.visit_item_mod(item_mod);

        visitor.direct_references
    }
    fn collect_indirect_references(
        item_mod: &syn::ItemMod,
    ) -> HashMap<syn::ItemEnum, HashSet<syn::ItemEnum>> {
        let direct_references = Self::collect_direct_references(item_mod);

        let indirect_references: HashMap<syn::ItemEnum, HashSet<syn::ItemEnum>> = direct_references
            .iter()
            .map(|(from, _)| {
                let mut reachable = HashSet::new();
                let mut to_visit = vec![from];
                while !to_visit.is_empty() {
                    let visiting = to_visit.pop().unwrap();
                    direct_references
                        .get(&visiting)
                        .unwrap()
                        .iter()
                        .filter(|indirect| !reachable.contains(*indirect))
                        .collect::<Vec<_>>()
                        .into_iter()
                        .for_each(|indirect| {
                            reachable.insert(indirect.clone());
                            to_visit.push(indirect);
                        });
                }
                (from.clone(), reachable)
            })
            .collect();

        indirect_references
    }
}

fn make_attr(name: &str, tokens: TokenStream) -> syn::Attribute {
    let ident: syn::Ident = proc_macro2::Ident::new(name, Span::call_site());
    let seg: syn::PathSegment = ident.into();
    let path: syn::Path = seg.into();
    syn::Attribute {
        pound_token: syn::Token![#](Span::call_site()),
        style: syn::AttrStyle::Outer,
        bracket_token: syn::token::Bracket(Span::call_site()),
        path,
        tokens,
    }
}

fn is_attr(path: &syn::Path, name: &str) -> bool {
    path.segments.len() == 1 && path.segments[0].ident == name
}

struct AnnotateEnums {
    indirect_references: HashMap<syn::ItemEnum, HashSet<syn::ItemEnum>>,
}

impl AnnotateEnums {
    fn new(indirect_references: HashMap<syn::ItemEnum, HashSet<syn::ItemEnum>>) -> Self {
        Self {
            indirect_references,
        }
    }
}

impl Fold for AnnotateEnums {
    fn fold_item_enum(&mut self, mut item_enum: syn::ItemEnum) -> syn::ItemEnum {
        let indirect_references: Vec<&syn::Ident> = self
            .indirect_references
            .get(&item_enum)
            .as_mut()
            .expect("Enum appeared in annotation step without appearing in analysis step")
            .iter()
            .map(|item_enum| &item_enum.ident)
            .collect();
        let tokens = quote! { ( #(#indirect_references, )* ) }.into();
        item_enum
            .attrs
            .push(make_attr("requires_graph_storage_type", tokens));
        syn::fold::fold_item_enum(self, item_enum)
    }
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
                    return syn::parse2(quote! { GraphRef<#path> }).unwrap();
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
            .unwrap()
        }

        fn fold_type(&mut self, ty: syn::Type) -> syn::Type {
            if let syn::Type::Path(syn::TypePath { path, .. }) = &ty {
                let ident = &path.segments.last().unwrap().ident;
                if self.referenced.contains(&ident) {
                    return syn::parse2(
                        quote! { LiveGraphRef<'a, Selector, super::storage::#path> },
                    )
                    .unwrap();
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
            .unwrap()
        })
        .collect();
    std::iter::once(item.into())
}

#[proc_macro_attribute]
pub fn derive_enum_types(
    _attr: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let orig: syn::ItemMod = parse_macro_input!(item as syn::ItemMod);

    let indirect_references = CollectDetails::collect_indirect_references(&orig);

    let annotated = AnnotateEnums::new(indirect_references).fold_item_mod(orig);

    annotated.to_token_stream().into()
}

fn apply_generator<'a, G, I>(
    enums: &'a Vec<(syn::ItemEnum, Vec<syn::ItemEnum>)>,
    dest_module: &str,
    mut generator: G,
) -> impl Iterator<Item = (String, syn::Item)> + 'a
where
    G: FnMut(&syn::ItemEnum, &Vec<syn::ItemEnum>) -> I,
    G: 'static,
    I: Iterator<Item = syn::Item>,
{
    let dest_module: String = dest_module.into();
    enums
        .iter()
        .map(move |(e, p)| generator(e, p))
        .flatten()
        .map(move |item| (dest_module.clone(), item))
}

#[proc_macro_attribute]
pub fn apply_enum_types(
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

    let modules: Vec<_> = std::iter::empty::<(String, syn::Item)>()
        .chain(apply_generator(&enums, "storage", generate_storage_enum))
        .chain(apply_generator(&enums, "live", generate_live_enum))
        .chain(apply_generator(&enums, "selector", generate_selector_enum))
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
            item_mod
        })
        .collect();

    quote! {
        mod temp{
            #(#modules)*
        }
    }
    .into()
}

#[proc_macro_attribute]
pub fn recursive_graph(
    _attr: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let item = derive_enum_types(proc_macro::TokenStream::new(), item);
    let item = apply_enum_types(proc_macro::TokenStream::new(), item);
    item
}
