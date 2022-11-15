use std::collections::{HashMap, HashSet};

use quote::{format_ident, quote, ToTokens};
use syn::{fold::Fold, parse_macro_input};

use itertools::Itertools;

fn is_attr(path: &syn::Path, name: &str) -> bool {
    path.segments.len() == 1 && path.segments[0].ident == name
}

fn recursive_type_checker(referenced_enums: &Vec<syn::ItemEnum>) -> impl Fn(&syn::Type) -> bool {
    let referenced_names: HashSet<syn::Ident> = referenced_enums
        .iter()
        .map(|item_enum| item_enum.ident.clone())
        .collect();
    move |ty: &syn::Type| -> bool {
        match ty {
            syn::Type::Path(path) => {
                let ident = &path.path.segments.last().unwrap().ident;
                referenced_names.contains(ident)
            }
            _ => false,
        }
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
    struct Mutator<'a> {
        referenced: HashSet<&'a syn::Ident>,
    }

    impl<'a> Fold for Mutator<'a> {
        fn fold_type(&mut self, ty: syn::Type) -> syn::Type {
            if let syn::Type::Path(syn::TypePath { path, .. }) = &ty {
                let ident = &path.segments.last().unwrap().ident;
                if self.referenced.contains(&ident) {
                    return syn::parse2(quote! {
                        ::algebra::graph::GraphRef<#path>
                    })
                    .expect("Error parsing generated storage enum");
                }
            }
            ty
        }
    }

    std::iter::once(
        Mutator {
            referenced: referenced
                .iter()
                .map(|item_enum| &item_enum.ident)
                .collect(),
        }
        .fold_item_enum(item_enum.clone())
        .into(),
    )
}

fn generate_storage_trait_impl<'a>(
    item_enum: &'a syn::ItemEnum,
    referenced_enums: &'a Vec<syn::ItemEnum>,
) -> impl Iterator<Item = syn::Item> + 'a {
    let selector = &item_enum.ident;

    let is_recursive_type = recursive_type_checker(referenced_enums);

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
                                    quote!{
                                        ::algebra::graph::LiveGraphRef::new(*#ident, subgraph.clone())
                                    }
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
            impl ::algebra::graph::GraphNode<super::selector::#selector>
                for #referenced_name
            where
                for<'c> &'c Self : TryFrom<&'c super::selector::#selector>
            {
                type LiveType<'a> =
                    super::live::#referenced_name<'a, super::selector::#selector>;

                fn to_live_type<'a>(
                    &self, subgraph: ::algebra::graph::Subgraph<'a, super::selector::#selector>
                ) -> Self::LiveType<'a> {
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

fn generate_storage_try_from_selector_impl<'a>(
    item: &'a syn::ItemEnum,
    referenced_enums: &'a Vec<syn::ItemEnum>,
) -> impl Iterator<Item = syn::Item> + 'a {
    let selector = &item.ident;
    referenced_enums.iter().map(move |item_enum| {
        let storage = &item_enum.ident;
        let str_storage = format!("{storage}");

        let stream = quote! {
            impl<'a,'b:'a> TryFrom<&'b super::selector::#selector> for &'a #storage {
                type Error = crate::Error;

                fn try_from(val: &'b super::selector::#selector)
                            -> Result<Self,Self::Error> {
                    use ::algebra::graph::GraphNodeSelector;
                    match val {
                        super::selector::#selector::#storage(e) => Ok(e),
                        _ => Err(Self::Error::IncorrectType{
                            expected: #str_storage,
                            actual: val.type_name(),
                        })
                    }
                }
            }
        };

        syn::parse2(stream).unwrap()
    })
}

fn generate_live_enum(
    item_enum: &syn::ItemEnum,
    referenced: &Vec<syn::ItemEnum>,
) -> impl Iterator<Item = syn::Item> {
    struct Mutator<'a> {
        referenced: HashSet<&'a syn::Ident>,
    }

    impl<'a> Fold for Mutator<'a> {
        fn fold_generics(&mut self, generics: syn::Generics) -> syn::Generics {
            let params = generics.params;
            syn::parse2(quote! {
                <'a, Selector, #params>
            })
            .expect("Error parsing generated generics for live enum")
        }

        fn fold_type(&mut self, ty: syn::Type) -> syn::Type {
            if let syn::Type::Path(syn::TypePath { path, .. }) = &ty {
                let ident = &path.segments.last().unwrap().ident;
                if self.referenced.contains(&ident) {
                    return syn::parse2(quote! {
                        ::algebra::graph::LiveGraphRef<'a, Selector, super::storage::#path>
                    })
                    .expect("Error parsing generated type for live enum");
                }
            }
            ty
        }
    }

    std::iter::once(
        Mutator {
            referenced: referenced
                .iter()
                .map(|item_enum| &item_enum.ident)
                .collect(),
        }
        .fold_item_enum(item_enum.clone())
        .into(),
    )
}

fn generate_live_enum_trait_impl(
    item_enum: &syn::ItemEnum,
    _referenced: &Vec<syn::ItemEnum>,
) -> impl Iterator<Item = syn::Item> {
    let ident = &item_enum.ident;

    let stream = quote! {
        impl<'a, Selector: 'a> ::algebra::graph::LiveGraphNode<'a, Selector> for #ident<'a, Selector>
        where
            super::storage::#ident: ::algebra::graph::GraphNode<Selector, LiveType<'a> = Self>,
        {
            type StorageType = super::storage::#ident;
        }
    };

    std::iter::once(syn::parse2(stream).unwrap())
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
            impl ::algebra::graph::GraphNodeSelector for #name {
                fn type_name(&self) -> &'static str {
                    match self {
                        #(Self::#referenced_idents(_) => #referenced_names,)*
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

fn generate_builder_trait<'a>(
    item_enum: &'a syn::ItemEnum,
    referenced_enums: &'a Vec<syn::ItemEnum>,
) -> impl Iterator<Item = syn::Item> + 'a {
    let ident = &item_enum.ident;
    let builder = format_ident!("{ident}Builder");

    let is_recursive_type = recursive_type_checker(referenced_enums);

    struct MethodInfo {
        method_name: syn::Ident,
        variant_name: syn::Ident,
        arg_names: Vec<syn::Ident>,
        arg_types: Vec<syn::Type>,
        param_exprs: Vec<syn::Expr>,
    }

    let info: Vec<MethodInfo> = item_enum
        .variants
        .iter()
        .map(|var: &syn::Variant| -> MethodInfo {
            let variant_name = var.ident.clone();
            let method_name = format_ident!("{ident}_{variant_name}");

            let (arg_names, arg_types, param_exprs) = var
                .fields
                .iter()
                .enumerate()
                .map(|(i, field)| -> (syn::Ident, syn::Type, syn::Expr) {
                    let arg_name = format_ident!("param{i}");
                    if is_recursive_type(&field.ty) {
                        let ty = &field.ty;
                        let ty = syn::parse2(quote! {
                            ::algebra::graph::GraphBuilderRef<super::storage::#ty>
                        })
                        .unwrap();
                        let param_expr = syn::parse2(quote! {
                            self.backref(#arg_name)
                        })
                        .unwrap();
                        (arg_name, ty, param_expr)
                    } else {
                        let param_expr = syn::parse2(quote! {
                            #arg_name
                        })
                        .unwrap();
                        (arg_name, field.ty.clone(), param_expr)
                    }
                })
                .multiunzip();

            MethodInfo {
                method_name,
                variant_name,
                arg_names,
                arg_types,
                param_exprs,
            }
        })
        .collect();

    let trait_methods: Vec<syn::TraitItemMethod> = info
        .iter()
        .map(|info| {
            let method_name = &info.method_name;
            let arg_names = &info.arg_names;
            let arg_types = &info.arg_types;
            let stream = quote! {
                fn #method_name(&mut self, #( #arg_names: #arg_types ),* )
                                -> ::algebra::graph::GraphBuilderRef<super::storage::#ident>;
            };
            syn::parse2(stream).expect("Error parsing generated trait method for builder")
        })
        .collect();

    let trait_method_impls: Vec<syn::ImplItemMethod> = info
        .iter()
        .map(|info| {
            let method_name = &info.method_name;
            let variant_name = &info.variant_name;
            let arg_names = &info.arg_names;
            let arg_types = &info.arg_types;
            let param_exprs = &info.param_exprs;
            let stream = quote! {
                fn #method_name(&mut self, #( #arg_names: #arg_types ),* )
                                -> ::algebra::graph::GraphBuilderRef<super::storage::#ident> {
                    self.push_top(super::storage::#ident::#variant_name(
                        #( #param_exprs ),*
                    ))
                }
            };
            syn::parse2(stream).expect("Error parsing generated trait impl for builder")
        })
        .collect();

    let builder_trait = quote! {
        pub trait #builder {
            #![allow(non_snake_case)]

            #( #trait_methods )*
        }
    };

    let builder_trait_impl = quote! {
        impl<Selector> #builder for ::algebra::graph::Graph<Selector>
        where Selector: From<super::storage::#ident>,
        {
            #![allow(non_snake_case)]

            #( #trait_method_impls )*
        }
    };

    vec![builder_trait, builder_trait_impl]
        .into_iter()
        .map(|stream| syn::parse2(stream).expect("Failed to parse generated builder trait/impl"))
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
        .chain(apply_generator(
            &enums,
            "storage",
            generate_storage_try_from_selector_impl,
        ))
        .chain(apply_generator(&enums, "live", generate_live_enum))
        .chain(apply_generator(
            &enums,
            "live",
            generate_live_enum_trait_impl,
        ))
        .chain(apply_generator(&enums, "selector", generate_selector_enum))
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
        .chain(apply_generator(&enums, "builder", generate_builder_trait))
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
