use std::collections::HashMap;

use quote::{format_ident, quote, ToTokens};
use syn::{fold::Fold, parse_macro_input};

use itertools::Itertools;

struct EnumInfo {
    item_enum: syn::ItemEnum,
    referenced_enums: Vec<syn::ItemEnum>,
    view_lifetime: syn::Lifetime,
    user_defined_generics: (Vec<syn::GenericParam>, Vec<syn::GenericArgument>),
}

impl EnumInfo {
    fn is_recursive_type(&self, ty: &syn::Type) -> bool {
        if let syn::Type::Path(ty) = &ty {
            let ty_ident = &ty.path.segments.last().unwrap().ident;
            self.referenced_enums
                .iter()
                .any(|ref_enum| &ref_enum.ident == ty_ident)
        } else {
            false
        }
    }

    fn is_self_reference(&self, ty: &syn::Type) -> bool {
        if let syn::Type::Path(ty) = &ty {
            let ty_ident = &ty.path.segments.last().unwrap().ident;
            &self.item_enum.ident == ty_ident
        } else {
            false
        }
    }
}

fn collect_annotated_enums(mut item_mod: syn::ItemMod) -> (syn::ItemMod, Vec<EnumInfo>) {
    fn is_attr(path: &syn::Path, name: &str) -> bool {
        path.segments.len() == 1 && path.segments[0].ident == name
    }

    if let Some((brace, content)) = item_mod.content {
        let (enums, other): (Vec<_>, Vec<_>) = content.into_iter().partition(|item| match item {
            syn::Item::Enum(item) => item
                .attrs
                .iter()
                .any(|attr| is_attr(&attr.path, "requires_graph_storage_type")),
            _ => false,
        });

        let stripped_enums: Vec<(syn::ItemEnum, Vec<syn::Ident>)> = enums
            .into_iter()
            .map(|item: syn::Item| -> syn::ItemEnum {
                if let syn::Item::Enum(item_enum) = item {
                    item_enum
                } else {
                    panic!("List should only contain enums at this point")
                }
            })
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

        let lookup: HashMap<syn::Ident, syn::ItemEnum> = stripped_enums
            .iter()
            .map(|(item_enum, _)| (item_enum.ident.clone(), item_enum.clone()))
            .collect();

        let enum_info: Vec<EnumInfo> = stripped_enums
            .into_iter()
            .map(|(item_enum, idents)| {
                let referenced_enums = idents
                    .into_iter()
                    .map(|ident| lookup.get(&ident).expect("Ident not in map").clone())
                    .collect();

                let current_lifetimes = crate::utils::collect_lifetimes(&item_enum.clone().into());

                let view_lifetime: syn::Lifetime = (0..)
                    .map(|i| -> syn::Lifetime {
                        let name = if i == 0 {
                            "'view".to_string()
                        } else {
                            format!("'view_{i}")
                        };
                        syn::Lifetime::new(&name, proc_macro2::Span::call_site())
                    })
                    .find(|lifetime| !current_lifetimes.contains(lifetime))
                    .unwrap();

                let user_defined_generic_params: Vec<_> = item_enum
                    .generics
                    .params
                    .iter()
                    .cloned()
                    .map(|param| match param {
                        syn::GenericParam::Lifetime(mut lifetime_def) => {
                            lifetime_def.bounds = std::iter::once(view_lifetime.clone())
                                .chain(lifetime_def.bounds.into_iter())
                                .collect();
                            lifetime_def.into()
                        }
                        _ => param,
                    })
                    .collect();

                let user_defined_generic_args = user_defined_generic_params
                    .iter()
                    .map(|param| match param {
                        syn::GenericParam::Type(type_def) => {
                            let ident = &type_def.ident;
                            let type_arg: syn::Type = syn::parse2(quote! { #ident }).unwrap();
                            syn::GenericArgument::Type(type_arg)
                        }
                        syn::GenericParam::Lifetime(lifetime_def) => {
                            syn::GenericArgument::Lifetime(lifetime_def.lifetime.clone())
                        }
                        syn::GenericParam::Const(const_def) => {
                            let ident = &const_def.ident;
                            let const_arg: syn::Expr = syn::parse2(quote! { #ident }).unwrap();
                            syn::GenericArgument::Const(const_arg)
                        }
                    })
                    .collect();

                let user_defined_generics =
                    (user_defined_generic_params, user_defined_generic_args);

                EnumInfo {
                    item_enum,
                    referenced_enums,
                    view_lifetime,
                    user_defined_generics,
                }
            })
            .collect();

        item_mod.content = Some((brace, other));

        (item_mod, enum_info)
    } else {
        (item_mod, Vec::new())
    }
}

fn generate_import_generic_recursive_enum<'a>(
    info: &'a EnumInfo,
) -> impl Iterator<Item = syn::Item> + 'a {
    let ident = &info.item_enum.ident;
    let stream = quote! {
        pub use generic_enum::#ident;
    };
    std::iter::once(syn::parse2(stream).expect("Error parsing generated 'use' statement"))
}

fn generate_generic_recursive_enum<'a>(info: &'a EnumInfo) -> impl Iterator<Item = syn::Item> + 'a {
    struct Mutator<'a> {
        info: &'a EnumInfo,
    }

    impl Fold for Mutator<'_> {
        fn fold_type(&mut self, ty: syn::Type) -> syn::Type {
            let lifetime = &self.info.view_lifetime;
            if self.info.is_self_reference(&ty) {
                let (_, generic_args) = &self.info.user_defined_generics;
                syn::parse2(quote! {
                    Ref::Ref<#ty<
                            #lifetime
                            #( , #generic_args )*
                        >>
                })
                .expect("Error parsing re-written recursive type for generic enum")
            } else if self.info.is_recursive_type(&ty) {
                syn::parse2(quote! {
                    Ref::Ref<#ty<#lifetime, ::graph::Storage>>
                })
                .expect("Error parsing re-written recursive type for generic enum")
            } else {
                syn::parse2(quote! {
                    Ref::Value<#ty>
                })
                .expect("Error parsing re-written recursive type for generic enum")
            }
        }
    }

    let (generic_params, _) = &info.user_defined_generics;
    let lifetime = &info.view_lifetime;

    let mut item_enum: syn::ItemEnum = Mutator { info }.fold_item_enum(info.item_enum.clone());

    item_enum.generics = syn::parse2(quote! {
        <#lifetime
         , Ref: ::graph::RecursiveRefType<#lifetime> = ::graph::Storage
         #( , #generic_params )*
         >
    })
    .expect("Error parsing generics for enum definition");

    item_enum.vis = syn::parse2(quote! {pub}).unwrap();

    std::iter::once(item_enum.into())
}

fn generate_recursive_obj_impl<'a>(info: &'a EnumInfo) -> impl Iterator<Item = syn::Item> + 'a {
    let lifetime = &info.view_lifetime;
    let ident = &info.item_enum.ident;

    let stream = quote! {
        impl<#lifetime, RefType: ::graph::RecursiveRefType<#lifetime>>
            ::graph::RecursiveObj<#lifetime> for
            #ident<#lifetime, RefType> {
                type Family = super::family::#ident;
                type RefType = RefType;
            }
    };

    std::iter::once(syn::parse2(stream).expect("Error parsing generated RecursiveObj impl"))
}

fn generate_recursive_family<'a>(info: &'a EnumInfo) -> impl Iterator<Item = syn::Item> + 'a {
    let ident = &info.item_enum.ident;
    let lifetime = &info.view_lifetime;

    let struct_def = syn::parse2(quote! {
        pub struct #ident;
    })
    .expect("Error parsing generated family struct");

    let builder_to_storage_arms: Vec<_> = info
        .item_enum
        .variants
        .iter()
        .map(|variant: &syn::Variant| -> syn::Arm {
            let arm_ident = &variant.ident;
            match &variant.fields {
                syn::Fields::Named(_) => todo!("Named enum fields"),

                syn::Fields::Unnamed(fields) => {
                    let (lhs_names, rhs_exprs): (Vec<_>, Vec<syn::Expr>) = fields
                        .unnamed
                        .iter()
                        .enumerate()
                        .map(|(i, field)| {
                            let field_ident = format_ident!("field_{i}");
                            let stream = if info.is_recursive_type(&field.ty) {
                                quote! { #field_ident.to_storage(new_pos) }
                            } else {
                                quote! { #field_ident }
                            };
                            let rhs_expr = syn::parse2(stream)
                                .expect("Error parsing generated builder_to_storage field");

                            (field_ident, rhs_expr)
                        })
                        .unzip();

                    let stream = quote! {
                        super::generic_enum::#ident::#arm_ident(#(#lhs_names),*) =>
                            super::generic_enum::#ident::#arm_ident(#(#rhs_exprs),*),
                    };

                    syn::parse2(stream).expect("Error parsing builder_to_storage match arm")
                }

                syn::Fields::Unit => syn::parse2(quote! {
                super::generic_enum::#ident::#arm_ident => super::generic_enum::#ident::#arm_ident,
                })
                .expect("Error parsing generated arm for builder_to_storage"),
            }
        })
        .collect();

    let storage_to_visiting_arms: Vec<_> = info
        .item_enum
        .variants
        .iter()
        .map(|variant: &syn::Variant| -> syn::Arm {
            let arm_ident = &variant.ident;
            match &variant.fields {
                syn::Fields::Named(_) => todo!("Named enum fields"),

                syn::Fields::Unnamed(fields) => {
                    let (lhs_names, rhs_exprs): (Vec<_>, Vec<syn::Expr>) = fields
                        .unnamed
                        .iter()
                        .enumerate()
                        .map(|(i, field)| {
                            let field_ident = format_ident!("field_{i}");
                            let stream = if info.is_recursive_type(&field.ty) {
                                quote! { #field_ident.to_visiting(view) }
                            } else {
                                quote! { #field_ident }
                            };
                            let rhs_expr = syn::parse2(stream)
                                .expect("Error parsing generated storage_to_visiting field");

                            (field_ident, rhs_expr)
                        })
                        .unzip();

                    let stream = quote! {
                        super::generic_enum::#ident::#arm_ident(#(#lhs_names),*) =>
                            super::generic_enum::#ident::#arm_ident(#(#rhs_exprs),*),
                    };

                    syn::parse2(stream).expect("Error parsing storage_to_visiting match arm")
                }

                syn::Fields::Unit => syn::parse2(quote! {
                super::generic_enum::#ident::#arm_ident => super::generic_enum::#ident::#arm_ident,
                })
                .expect("Error parsing generated arm for storage_to_visiting"),
            }
        })
        .collect();

    let struct_impl = syn::parse2(quote! {
        impl ::graph::RecursiveFamily for #ident {
            type Obj<#lifetime, Ref: ::graph::RecursiveRefType<#lifetime>>
                = super::generic_enum::#ident<#lifetime, Ref>;

            type DefaultContainer<#lifetime>
                = super::container::#ident<#lifetime>;

            fn builder_to_storage<#lifetime>(
                builder_obj: Self::Obj<#lifetime, ::graph::Builder>,
                new_pos: usize,
            ) -> Self::Obj<#lifetime, ::graph::Storage> {
                match builder_obj {
                    #( #builder_to_storage_arms )*
                }
            }

            fn storage_to_visiting<#lifetime, Container>(
                storage_obj: & #lifetime Self::Obj<#lifetime, ::graph::Storage>,
                view: & #lifetime [Container]
            ) -> Self::Obj<#lifetime, ::graph::Visiting<#lifetime, Container>> {
                match storage_obj {
                    #( #storage_to_visiting_arms )*
                }
            }
        }

    })
    .expect("Error parsing generated family impl");

    vec![struct_def, struct_impl].into_iter()
}

fn generate_container_enum(info: &EnumInfo) -> impl Iterator<Item = syn::Item> {
    let lifetime = &info.view_lifetime;
    let ident = &info.item_enum.ident;
    let referenced_idents: Vec<_> = info
        .referenced_enums
        .iter()
        .map(|ref_enum| &ref_enum.ident)
        .collect();

    let stream = quote! {
        pub enum #ident<#lifetime> {
            #(
                #referenced_idents (
                    super::generic_enum::#referenced_idents<#lifetime, ::graph::Storage>
                ),
            )*
        }
    };

    std::iter::once(syn::parse2(stream).expect("Error parsing generated container enum"))
}

fn generate_container_impl(info: &EnumInfo) -> impl Iterator<Item = syn::Item> + '_ {
    let lifetime = &info.view_lifetime;
    let ident = &info.item_enum.ident;
    info.referenced_enums.iter().map(move |ref_enum| {
        let ref_ident = &ref_enum.ident;
        let ref_ident_str = format!("{ref_ident}");

        let (other_node_idents, other_node_str): (Vec<_>, Vec<_>) = info
            .referenced_enums.iter()
            .map(|other_enum| &other_enum.ident)
            .filter(|other_ident| other_ident != &ref_ident)
            .map(|other_ident| (other_ident.clone(), format!("{other_ident}")))
            .unzip();

        let stream = quote! {
            impl<#lifetime>
                ::graph::ContainerOf<
                    super::generic_enum::#ref_ident<#lifetime, ::graph::Storage>>
                for #ident<#lifetime> {
                    fn to_container(val: super::generic_enum::#ref_ident<#lifetime, ::graph::Storage>) -> Self {
                        Self::#ref_ident(val)
                    }

                    fn from_container(&self) -> Result<
                            & super::generic_enum::#ref_ident<
                                    #lifetime, ::graph::Storage
                                >, ::graph::Error> {
                        match self {
                            Self::#ref_ident(val) => Ok(val),
                            #(
                                Self::#other_node_idents(_) => Err(::graph::Error::IncorrectType {
                                    expected: #ref_ident_str,
                                    actual: #other_node_str,
                                }),
                            )*
                        }
                    }
                }
        };

        syn::parse2(stream).expect("Error parsing generated container impl")
    })
}

fn apply_generator<'a, G, I>(
    enums: &'a Vec<EnumInfo>,
    dest_module: Option<&'a str>,
    mut generator: G,
) -> impl Iterator<Item = (Option<&'a str>, syn::Item)> + 'a
where
    G: FnMut(&'a EnumInfo) -> I + 'a,
    I: Iterator<Item = syn::Item>,
{
    enums
        .iter()
        .map(move |info| generator(info))
        .flatten()
        .map(move |item| (dest_module, item))
}

pub fn linearize_recursive_enums(
    _attr: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let orig: syn::ItemMod = parse_macro_input!(item as syn::ItemMod);

    let (orig_mod, enums) = collect_annotated_enums(orig);

    let orig_mod_other_content: Vec<syn::Item> = orig_mod
        .content
        .as_ref()
        .iter()
        .map(|(_brace, vec)| vec.into_iter())
        .flatten()
        .cloned()
        .collect();

    let modules: Vec<_> = std::iter::empty::<(Option<&str>, syn::Item)>()
        .chain(apply_generator(
            &enums,
            None,
            generate_import_generic_recursive_enum,
        ))
        .chain(apply_generator(
            &enums,
            Some("generic_enum"),
            generate_generic_recursive_enum,
        ))
        .chain(apply_generator(
            &enums,
            Some("generic_enum"),
            generate_recursive_obj_impl,
        ))
        .chain(apply_generator(
            &enums,
            Some("family"),
            generate_recursive_family,
        ))
        .chain(apply_generator(
            &enums,
            Some("container"),
            generate_container_enum,
        ))
        .chain(apply_generator(
            &enums,
            Some("container"),
            generate_container_impl,
        ))
        .into_group_map()
        .into_iter()
        .map(|(opt_submodule_name, items)| {
            if let Some(name) = opt_submodule_name {
                let mut item_mod = orig_mod.clone();
                item_mod.ident = format_ident!("{}", name);
                item_mod.attrs.push({
                    let mut dummy: syn::ItemStruct = syn::parse2(quote! {
                        #[allow(non_camel_case_types)]
                        struct Dummy;
                    })
                    .expect("Error parsing generated dummy struct");
                    dummy.attrs.pop().unwrap()
                });
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
                vec![item_mod.into()].into_iter()
            } else {
                items.into_iter()
            }
        })
        .flatten()
        .collect();

    let mut out_mod = orig_mod;
    let content = orig_mod_other_content
        .into_iter()
        .chain(modules.into_iter().map(|item_mod| item_mod.into()))
        .collect();
    out_mod.content = Some((syn::token::Brace::default(), content));
    out_mod.to_token_stream().into()
}
