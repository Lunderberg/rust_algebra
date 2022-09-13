use std::collections::HashSet;

use proc_macro2::Span;

use quote::{format_ident, quote};
use syn::{fold::Fold, parse_macro_input};

use itertools::{Either, Itertools};

struct ToStorageEnum<'a> {
    enum_idents: &'a HashSet<syn::Ident>,
}

impl<'a> ToStorageEnum<'a> {
    fn new(enum_idents: &'a HashSet<syn::Ident>) -> Self {
        Self { enum_idents }
    }

    fn is_recursive_enum(&self, ty: &syn::Type) -> bool {
        match ty {
            syn::Type::Path(path) => {
                let segments = &path.path.segments;
                segments.len() == 1 && self.enum_idents.contains(&segments.last().unwrap().ident)
            }
            _ => false,
        }
    }
}

impl<'a> Fold for ToStorageEnum<'a> {
    fn fold_item_enum(&mut self, mut item_enum: syn::ItemEnum) -> syn::ItemEnum {
        let ident = format_ident!("super");
        let segment: syn::PathSegment = ident.into();
        let path: syn::Path = segment.into();

        let vis = syn::VisRestricted {
            pub_token: syn::Token![pub](Span::call_site()),
            paren_token: syn::token::Paren(Span::call_site()),
            in_token: None,
            path: path.into(),
        };
        item_enum.vis = syn::Visibility::Restricted(vis);
        syn::fold::fold_item_enum(self, item_enum)
    }

    fn fold_field(&mut self, field: syn::Field) -> syn::Field {
        if self.is_recursive_enum(&field.ty) {
            let old_type = &field.ty;
            let type_ident = quote! { GraphRef<#old_type> };
            let mut field = field;
            field.ty = syn::Type::Verbatim(type_ident);
            field
        } else {
            field
        }
    }
}

struct ToLiveEnum<'a> {
    enum_idents: &'a HashSet<syn::Ident>,
}

impl<'a> ToLiveEnum<'a> {
    fn new(enum_idents: &'a HashSet<syn::Ident>) -> Self {
        Self { enum_idents }
    }

    fn is_recursive_enum(&self, ty: &syn::Type) -> Option<syn::PathSegment> {
        match ty {
            syn::Type::Path(path) => {
                let segments = &path.path.segments;
                (segments.len() == 1)
                    .then(|| segments.last().unwrap())
                    .filter(|segment| self.enum_idents.contains(&segment.ident))
                    .cloned()
            }
            _ => None,
        }
    }

    fn new_lifetime(&self) -> syn::Lifetime {
        syn::Lifetime::new("'a", Span::call_site())
    }

    fn new_base_name(&self) -> syn::Ident {
        format_ident!("NodeBase")
    }
}

impl<'a> Fold for ToLiveEnum<'a> {
    fn fold_generics(&mut self, mut generics: syn::Generics) -> syn::Generics {
        let base_name: syn::Ident = self.new_base_name().into();
        let base_name: syn::GenericParam = syn::GenericParam::Type(base_name.into());
        let lifetime: syn::LifetimeDef = syn::LifetimeDef::new(self.new_lifetime());
        let lifetime: syn::GenericParam = syn::GenericParam::Lifetime(lifetime);

        generics.params = vec![base_name, lifetime]
            .into_iter()
            .chain(generics.params.into_iter())
            .collect();
        generics
    }

    fn fold_field(&mut self, mut field: syn::Field) -> syn::Field {
        if let Some(recursive_segment) = self.is_recursive_enum(&field.ty) {
            let lifetime = self.new_lifetime();
            let base_name = self.new_base_name();

            field.ty = syn::Type::Verbatim(quote! {
                LiveGraphRef<#lifetime, #base_name, super::storage::#recursive_segment>
            });
        }

        field
    }
}

#[proc_macro_attribute]
pub fn recursive_graph(
    _attr: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let orig: syn::ItemMod = parse_macro_input!(item as syn::ItemMod);

    let mod_name = orig.ident.clone();
    let (enums, other_items): (Vec<syn::ItemEnum>, Vec<syn::Item>) = orig
        .clone()
        .content
        .expect("Recursive graph module may not be empty")
        .1
        .into_iter()
        .partition_map(|item| match item {
            syn::Item::Enum(item_enum) => Either::Left(item_enum),
            other => Either::Right(other),
        });

    let enum_idents: HashSet<syn::Ident> = enums.iter().map(|e| e.ident.clone()).collect();
    let is_recursive_enum = |ty: &syn::Type| -> bool {
        match ty {
            syn::Type::Path(path) => {
                let segments = &path.path.segments;
                segments.len() == 1 && enum_idents.contains(&segments.last().unwrap().ident)
            }
            _ => false,
        }
    };

    let storage_mod = {
        let mut item_mod = orig.clone();
        item_mod.ident = format_ident!("storage");
        ToStorageEnum::new(&enum_idents).fold_item_mod(item_mod)
    };
    let live_mod = {
        let mut item_mod = orig.clone();
        item_mod.ident = format_ident!("live");
        ToLiveEnum::new(&enum_idents).fold_item_mod(item_mod)
    };

    let _storage_enums: Vec<_> = enums
        .iter()
        .cloned()
        .map(|mut e| {
            e.variants.iter_mut().for_each(|var| {
                var.fields
                    .iter_mut()
                    .filter(|field| is_recursive_enum(&field.ty))
                    .for_each(|field| {
                        let old_type = &field.ty;
                        let type_ident = quote! { GraphRef<#old_type> };
                        field.ty = syn::Type::Verbatim(type_ident);
                    });
            });
            e
        })
        .collect();

    let _live_enums: Vec<_> = enums
        .iter()
        .cloned()
        .map(|mut e| {
            let to_live_ident =
                |ident: &syn::Ident| -> syn::Ident { format_ident!("Live{}", ident) };

            let base_name: syn::Ident = format_ident!("NodeBase");
            let lifetime: syn::Lifetime = syn::Lifetime::new("'a", Span::call_site());

            e.ident = to_live_ident(&e.ident);
            e.generics
                .params
                .insert(0, syn::LifetimeDef::new(lifetime.clone()).into());
            e.generics.params.insert(0, {
                let param: syn::TypeParam = base_name.clone().into();
                param.into()
            });
            e.variants.iter_mut().for_each(|var| {
                var.fields
                    .iter_mut()
                    .filter(|field| is_recursive_enum(&field.ty))
                    .for_each(|field| {
                        let old_type = &field.ty;
                        let type_ident = quote! { LiveGraphRef<#lifetime, #base_name, #old_type> };
                        field.ty = syn::Type::Verbatim(type_ident);
                    });
            });
            e
        })
        .collect();

    quote! {
        mod #mod_name {
            #(#other_items)*

            #storage_mod
            #live_mod
            // #(#storage_enums)*
            // #(#live_enums)*
        }
    }
    .into()
}
