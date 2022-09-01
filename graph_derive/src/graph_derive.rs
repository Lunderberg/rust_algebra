use std::collections::HashSet;

use proc_macro2::Span;

use quote::{format_ident, quote};
use syn::parse::Parse;
use syn::parse_macro_input;

struct MyMacroInput {
    enums: Vec<syn::ItemEnum>,
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
        Ok(Self { enums })
    }
}

#[proc_macro]
pub fn make_graph(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let enums: Vec<syn::ItemEnum> = parse_macro_input!(input as MyMacroInput).enums;

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

    let storage_enums: Vec<_> = enums
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

    let live_enums: Vec<_> = enums
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
            e.generics.params.push({
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
        //#(#enums)*
        #(#storage_enums)*
        #(#live_enums)*
    }
    .into()
}
