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
                    Ref::RefType<#ty<
                            #lifetime, ::graph::Storage<#lifetime>
                            #( , #generic_args )*
                        >>
                })
                .expect("Error parsing re-written recursive type for generic enum")
            } else if self.info.is_recursive_type(&ty) {
                syn::parse2(quote! {
                    Ref::RefType<#ty<#lifetime, ::graph::Storage<#lifetime>>>
                })
                .expect("Error parsing re-written recursive type for generic enum")
            } else {
                syn::parse2(quote! {
                    Ref::ValueType<#ty>
                })
                .expect("Error parsing re-written recursive type for generic enum")
            }
        }
    }

    let (generic_params, _) = &info.user_defined_generics;
    let lifetime = &info.view_lifetime;

    let mut item_enum: syn::ItemEnum = Mutator { info }.fold_item_enum(info.item_enum.clone());

    item_enum.generics = syn::parse2(quote! {
        <#lifetime, Ref: ::graph::NodeUsage<#lifetime>
         #( , #generic_params )*
         >
    })
    .expect("Error parsing generics for enum definition");

    item_enum.vis = syn::parse2(quote! {pub}).unwrap();

    std::iter::once(item_enum.into())
}

fn generate_generic_node_trait_impl<'a>(
    info: &'a EnumInfo,
) -> impl Iterator<Item = syn::Item> + 'a {
    let ident = &info.item_enum.ident;

    let live_type_arms: Vec<syn::Arm> = info
        .item_enum
        .variants
        .iter()
        .map(|var: &syn::Variant| -> syn::Arm {
            let ident = &var.ident;
            match &var.fields {
                syn::Fields::Named(_) => todo!("Named enum fields"),
                syn::Fields::Unnamed(fields) => {
                    let (field_names, live_field_exprs): (Vec<_>, Vec<_>) = fields
                        .unnamed
                        .iter()
                        .enumerate()
                        .map(|(i, field)| {
                            let ident = format_ident!("field_{i}");
                            let stream = // if info.is_self_reference(&field.ty) {
                            //     let generic_args = &info.user_defined_generic_args;
                            //     quote! {
                            //         converter.convert_reference( #ident< #( #generic_args ),* > )
                            //     }
                            // } else
                                if info.is_recursive_type(&field.ty) {
                                quote! {
                                    converter.convert_reference( #ident )
                                }
                            } else {
                                quote! { converter.convert_value( #ident ) }
                            };
                            let expr: syn::Expr =
                                syn::parse2(stream).expect("Error parsing generated live field");
                            (ident, expr)
                        })
                        .unzip();
                    syn::parse2(quote! {
                        Self::#ident(#(#field_names),*) =>
                            Self::WithRef::<NewRef>::#ident(
                                #(#live_field_exprs),*
                            ),
                    })
                    .expect("Error parsing generated arm in match statement")
                }
                syn::Fields::Unit => syn::parse2(quote! {
                    Self::#ident => Self::WithRef::<NewRef>::#ident,
                })
                .expect("Error parsing arm in generated match statement"),
            }
        })
        .collect();

    let lifetime = &info.view_lifetime;
    let (generic_params, generic_args) = &info.user_defined_generics;
    let stream = quote! {
        impl<#lifetime, Ref: ::graph::NodeUsage<#lifetime>, #( #generic_params ),* >
            ::graph::GenericGraphNode<#lifetime, Ref>
            for #ident<#lifetime, Ref, #( #generic_args ),*>
        {
            type DefaultSelector = super::selector::#ident<#lifetime #( , #generic_args )* >;

            type WithRef<NewRef: ::graph::NodeUsage<#lifetime>> =
                #ident<#lifetime, NewRef #( , #generic_args )*>;

            fn convert_references<
                    NewRef: ::graph::NodeUsage<#lifetime>,
                    Converter: ::graph::NodeUsageConverter<#lifetime, Ref, NewRef>,
                >(
                &#lifetime self,
                converter: Converter,
            ) -> Self::WithRef<NewRef> {
                match self {
                    #(#live_type_arms)*
                }
            }
        }
    };

    std::iter::once(syn::parse2(stream).expect("Error parsing generated storage trait"))
}

fn generate_generic_enum_try_from_selector_impl<'a>(
    info: &'a EnumInfo,
) -> impl Iterator<Item = syn::Item> + 'a {
    let selector = &info.item_enum.ident;

    info.referenced_enums
        .iter()
        .map(|item_enum| &item_enum.ident)
        .map(move |node_type| {
            let node_name = format!("{node_type}");

            let (other_node_types, other_node_names): (Vec<_>, Vec<_>) = info
                .referenced_enums
                .iter()
                .map(|item_enum| &item_enum.ident)
                .filter(|other_type| other_type != &node_type)
                .map(|other_type| (other_type.clone(), format!("{other_type}")))
                .unzip();

            let lifetime = &info.view_lifetime;
            let (generic_params, generic_args) = &info.user_defined_generics;
            let selector_type = quote! {
                super::selector::#selector::<#lifetime, #( #generic_args ),* >
            };
            let stream = quote! {
                impl<#lifetime, #( #generic_params ),* >
                    TryFrom<&#lifetime #selector_type>
                    for &#lifetime #node_type<#lifetime,::graph::Storage<#lifetime>, #( #generic_args ),*>
                {
                    type Error = ::graph::Error;

                    fn try_from(val: &#lifetime #selector_type)
                                -> Result<Self,Self::Error> {
                        match val {
                            #selector_type::#node_type(e) => Ok(e),
                            #(
                                #selector_type::#other_node_types(_) => {
                                    Err(Self::Error::IncorrectType {
                                        expected: #node_name,
                                        actual: #other_node_names,
                                    })
                                }
                            )*
                        }
                    }
                }
            };

            syn::parse2(stream).expect("Error parsing generated TryFrom trait")
        })
}

fn generate_selector_enum(info: &EnumInfo) -> impl Iterator<Item = syn::Item> {
    let mut selector = info.item_enum.clone();
    let lifetime = &info.view_lifetime;
    let lifetime_def: syn::LifetimeDef = { syn::parse2(quote! { #lifetime }).unwrap() };
    let (_, generic_args) = &info.user_defined_generics;
    selector.generics.params = std::iter::once(lifetime_def.into())
        .chain(selector.generics.params.into_iter())
        .collect();
    selector.variants = info
        .referenced_enums
        .iter()
        .map(|referred_enum: &syn::ItemEnum| -> syn::Variant {
            let ident = &referred_enum.ident;
            syn::parse2::<syn::Variant>(quote! {
                #ident(super::generic_enum::#ident<
                    #lifetime, ::graph::Storage<#lifetime>
                    #( , #generic_args )*
                    >)
            })
            .expect("Error parsing generated selector enum")
        })
        .collect();

    selector.vis = syn::parse2(quote! {pub}).unwrap();

    std::iter::once(selector.into())
}

fn generate_selector_from_storage_impl<'a>(
    info: &'a EnumInfo,
) -> impl Iterator<Item = syn::Item> + 'a {
    let selector = &info.item_enum.ident;
    let lifetime = &info.view_lifetime;
    let (generic_params, generic_args) = &info.user_defined_generics;

    info.referenced_enums.iter().map(move |referenced_enum| {
        let node = &referenced_enum.ident;

        let storage = quote! {
            super::generic_enum::#node::<
                    #lifetime, ::graph::Storage<#lifetime>
                    #( , #generic_args )*
                >
        };

        let stream = quote! {
            impl<#lifetime #( , #generic_params )* >
                From<#storage >
                for #selector<#lifetime #( , #generic_args )* > {
                fn from(val: #storage) -> Self {
                    Self::#node(val)
                }
            }
        };

        syn::parse2(stream).expect("Error parsing generated From<storage> type")
    })
}

fn generate_typed_builder_trait<'a>(info: &'a EnumInfo) -> impl Iterator<Item = syn::Item> + 'a {
    let ident = &info.item_enum.ident;
    let builder = format_ident!("{ident}Builder");

    struct MethodInfo {
        variant_name: syn::Ident,
        param_exprs: Vec<syn::Expr>,
        signature: proc_macro2::TokenStream,
    }

    let lifetime = &info.view_lifetime;
    let (generic_params, generic_args) = &info.user_defined_generics;

    let method_info: Vec<MethodInfo> = info
        .item_enum
        .variants
        .iter()
        .map(|var: &syn::Variant| -> MethodInfo {
            let variant_name = var.ident.clone();
            let method_name = format_ident!("{ident}_{variant_name}");

            let (arg_names, arg_types, param_exprs): (Vec<_>, Vec<_>, Vec<_>) = var
                .fields
                .iter()
                .enumerate()
                .map(|(i, field)| -> (syn::Ident, syn::Type, syn::Expr) {
                    let arg_name = format_ident!("param{i}");
                    if info.is_recursive_type(&field.ty) {
                        let ty = &field.ty;
                        let ty = syn::parse2(quote! {
                            ::graph::GraphBuilderRef<
                                    super::generic_enum::#ty
                                    <#lifetime, ::graph::Storage<#lifetime>
                                     #( , #generic_args )*
                                     >>
                        })
                        .expect("Error generating GraphBuilderRef wrapper");
                        let param_expr = syn::parse2(quote! {
                            self.backref(#arg_name)
                        })
                        .expect("Error generating self.backref() wrapper");
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

            let signature = quote! {
                fn #method_name(&mut self, #( #arg_names: #arg_types ),* )
                                -> ::graph::GraphBuilderRef<
                        super::generic_enum::#ident<
                                #lifetime, ::graph::Storage<#lifetime>
                                #( , #generic_args )* >
                        >
            };

            MethodInfo {
                variant_name,
                param_exprs,
                signature,
            }
        })
        .collect();

    let trait_methods: Vec<syn::TraitItemMethod> = method_info
        .iter()
        .map(|info| {
            let signature = &info.signature;
            let stream = quote! { # signature; };
            syn::parse2(stream).expect("Error parsing generated trait method for builder")
        })
        .collect();

    let trait_method_impls: Vec<syn::ImplItemMethod> = method_info
        .iter()
        .map(|info| {
            let variant_name = &info.variant_name;
            let param_exprs = &info.param_exprs;

            let variant = if param_exprs.is_empty() {
                quote! { super::generic_enum::#ident::#variant_name }
            } else {
                quote! { super::generic_enum::#ident::#variant_name
                    ( #( #param_exprs ),* )
                }
            };

            let signature = &info.signature;

            let stream = quote! {
                #signature {
                    self.push_top( #variant )
                }
            };
            syn::parse2(stream).expect("Error parsing generated trait impl for builder")
        })
        .collect();

    let builder_trait = quote! {
        pub trait #builder<#lifetime, #( #generic_params ),* > {
            #![allow(non_snake_case)]

            #( #trait_methods )*
        }
    };

    let builder_trait_impl = quote! {
        impl<#lifetime, BaseType: ::graph::GenericGraphNode<#lifetime,::graph::Storage<#lifetime>>, #( #generic_params ),* >
            #builder<#lifetime, #( #generic_args ),* >
            for ::graph::Graph<#lifetime, BaseType>
        where BaseType::DefaultSelector: From<
                super::generic_enum::#ident<
                        #lifetime, ::graph::Storage<#lifetime>
                        #( , #generic_args )*
                >>,
        {
            #![allow(non_snake_case)]

            #( #trait_method_impls )*
        }
    };

    vec![builder_trait, builder_trait_impl]
        .into_iter()
        .map(|stream| syn::parse2(stream).expect("Failed to parse generated builder trait/impl"))
}

fn generate_overload_dummy_struct<'a>(
    module_name: Option<&'a str>,
) -> impl Iterator<Item = (Option<&'a str>, syn::Item)> + 'a {
    let stream = quote! {
        pub struct OverloadDummy;
    };
    let obj = syn::parse2(stream).expect("Error parsing dummy overload struct");
    std::iter::once((module_name, obj))
}

fn generate_overloaded_builder_trait<'a>(
    info: &'a EnumInfo,
) -> impl Iterator<Item = syn::Item> + 'a {
    let base_type = &info.item_enum.ident;

    #[derive(Debug)]
    struct MethodInfo {
        storage_type: syn::Ident,
        method_name: syn::Ident,
        field_types: Vec<syn::Type>,
    }

    struct OverloadSet {
        method_name: syn::Ident,
        dummy_trait_name: syn::Ident,
        method_trait_name: syn::Ident,
        param_names: Vec<syn::Ident>,
        generic_type_params: Vec<syn::Ident>,
        overloads: Vec<MethodInfo>,
    }

    let lifetime = &info.view_lifetime;
    let (generic_params, generic_args) = &info.user_defined_generics;

    let method_info: Vec<MethodInfo> = info
        .referenced_enums
        .iter()
        .map(|ref_enum| {
            let storage_type = ref_enum.ident.clone();
            ref_enum
                .variants
                .iter()
                .map(|var: &syn::Variant| {
                    let field_types = var
                        .fields
                        .iter()
                        .map(|field| {
                            let ty = &field.ty;
                            if info.is_recursive_type(ty) {
                                syn::parse2(quote! {
                                    ::graph::GraphBuilderRef<
                                            super::generic_enum::#ty<
                                                #lifetime, ::graph::Storage<#lifetime>
                                                #( , #generic_args )*
                                                >>
                                })
                                .expect("Error parsing generated GraphBuilderRef")
                            } else {
                                ty.clone()
                            }
                        })
                        .collect();
                    MethodInfo {
                        storage_type: storage_type.clone(),
                        method_name: var.ident.clone(),
                        field_types,
                    }
                })
                .collect::<Vec<_>>()
                .into_iter()
        })
        .flatten()
        .collect();

    let overload_sets: Vec<OverloadSet> = method_info
        .into_iter()
        .map(|overloads| (overloads.method_name.clone(), overloads))
        .into_group_map()
        .into_values()
        .filter(|overloads| {
            overloads
                .iter()
                .map(|info| info.field_types.len())
                .unique()
                .count()
                == 1
        })
        .filter(|overloads| {
            overloads
                .iter()
                .map(|info| &info.field_types)
                .duplicates()
                .next()
                .is_none()
        })
        .map(|overloads| {
            let method_name = overloads[0].method_name.clone();
            let dummy_trait_name = format_ident!("{base_type}_{method_name}_OverloadDummy");
            let method_trait_name = format_ident!("{base_type}_{method_name}_OverloadMethod");
            let param_names = overloads[0]
                .field_types
                .iter()
                .enumerate()
                .map(|(i, _ty)| format_ident!("param{i}"))
                .collect();
            let generic_type_params = overloads[0]
                .field_types
                .iter()
                .enumerate()
                .map(|(i, _ty)| format_ident!("T{i}"))
                .collect();
            OverloadSet {
                method_name,
                dummy_trait_name,
                method_trait_name,
                param_names,
                generic_type_params,
                overloads,
            }
        })
        .collect();

    let overload_dummy_traits = overload_sets
        .iter()
        .map(|overload_set| {
            let method_name = &overload_set.method_name;
            let dummy_trait_name = &overload_set.dummy_trait_name;
            let param_names = &overload_set.param_names;
            let generic_type_params = &overload_set.generic_type_params;
            let stream = quote! {
                pub trait #dummy_trait_name<#lifetime #( , #generic_type_params )*  #( , #generic_params )* > {
                    #![allow(non_snake_case)]

                    type OutNode;
                    fn #method_name(
                        graph: &mut ::graph::Graph<
                                #lifetime, super::generic_enum::#base_type<
                                    #lifetime, ::graph::Storage<#lifetime>
                                    #( , #generic_args )*
                                    >>,
                        #( #param_names: #generic_type_params, )*
                    ) -> ::graph::GraphBuilderRef<Self::OutNode>;
                }
            };
            syn::parse2(stream).expect("Error parsing overloaded dummy trait")
        })
        .collect::<Vec<syn::Item>>()
        .into_iter();

    let overload_dummy_trait_impls = overload_sets
        .iter()
        .map(|overload_set| {
            let method_name = &overload_set.method_name;
            let dummy_trait_name = &overload_set.dummy_trait_name;
            let param_names = &overload_set.param_names;
            overload_set
                .overloads
                .iter()
                .map(|overload| -> syn::Item {
                    let field_types = &overload.field_types;
                    let storage_type = &overload.storage_type;
                    let delegate = format_ident!("{storage_type}_{method_name}");
                    let (generic_params, _) = &info.user_defined_generics;
                    let stream = quote! {
                        impl<#lifetime, #( #generic_params ),* >
                            #dummy_trait_name<#lifetime #( , #field_types )* #( , #generic_args )* >
                        for OverloadDummy {
                            type OutNode = super::generic_enum::#storage_type<
                                #lifetime, ::graph::Storage<#lifetime>
                                #( , #generic_args )*
                             >;
                            fn #method_name (
                                graph: &mut ::graph::Graph<
                                    #lifetime,
                                    super::generic_enum::#base_type<
                                        #lifetime, ::graph::Storage<#lifetime>
                                        #( , #generic_args )*
                                    >>,
                                #( #param_names: #field_types, )*
                            ) -> ::graph::GraphBuilderRef<Self::OutNode> {
                                graph.#delegate( #(#param_names),* )
                            }

                        }
                    };
                    syn::parse2(stream).expect("Error parsing overloaded dummy trait impl")
                })
                .collect::<Vec<_>>()
                .into_iter()
        })
        .flatten()
        .collect::<Vec<syn::Item>>()
        .into_iter();

    let overload_method_trait = overload_sets
        .iter()
        .map(|overload_set| {
            let method_name = &overload_set.method_name;
            let dummy_trait_name = &overload_set.dummy_trait_name;
            let method_trait_name = &overload_set.method_trait_name;
            let param_names = &overload_set.param_names;
            let generic_type_params = &overload_set.generic_type_params;

            let stream = quote! {
                pub trait #method_trait_name<#lifetime, #( #generic_params ),* > {
                    #![allow(non_snake_case)]

                    fn #method_name< #( #generic_type_params ),* >(
                        &mut self,
                        #( #param_names: #generic_type_params, )*
                    ) -> ::graph::GraphBuilderRef<<
                        OverloadDummy as #dummy_trait_name
                             <#lifetime #( , #generic_type_params )* #( , #generic_args )* >
                        >::OutNode>
                    where
                        OverloadDummy: #dummy_trait_name
                    <#lifetime  #( , #generic_type_params )* #( , #generic_args )* >;
                }
            };
            syn::parse2(stream).expect("Error parsing overloaded method trait")
        })
        .collect::<Vec<syn::Item>>()
        .into_iter();

    let overload_method_trait_impls = overload_sets
        .iter()
        .map(|overload_set| {
            let method_name = &overload_set.method_name;
            let dummy_trait_name = &overload_set.dummy_trait_name;
            let method_trait_name = &overload_set.method_trait_name;
            let param_names = &overload_set.param_names;
            let generic_type_params = &overload_set.generic_type_params;

            let stream = quote! {
                impl<#lifetime, #( #generic_params ),*>
                    #method_trait_name<#lifetime, #( #generic_args ),* >
                for ::graph::Graph<
                    #lifetime,
                    super::generic_enum::#base_type<
                        #lifetime, ::graph::Storage<#lifetime>
                        #( , #generic_args )*
                    >> {
                    fn #method_name< #( #generic_type_params ),* >(
                        &mut self,
                        #( #param_names: #generic_type_params, )*
                    ) -> ::graph::GraphBuilderRef<<
                            OverloadDummy as #dummy_trait_name
                                <#lifetime  #( , #generic_type_params )* #( , #generic_args)* >
                            >::OutNode>
                    where
                        OverloadDummy: #dummy_trait_name
                        <#lifetime #( , #generic_type_params )* #( , #generic_args )* > {
                        <OverloadDummy as #dummy_trait_name
                         <#lifetime #( , #generic_type_params )* #( , #generic_args )* >
                        >::#method_name(self, #( #param_names ),* )
                    }
                }
            };
            syn::parse2(stream).expect("Error parsing overloaded method trait")
        })
        .collect::<Vec<syn::Item>>()
        .into_iter();

    std::iter::empty()
        .chain(overload_dummy_traits)
        .chain(overload_dummy_trait_impls)
        .chain(overload_method_trait)
        .chain(overload_method_trait_impls)
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
            generate_generic_node_trait_impl,
        ))
        .chain(apply_generator(
            &enums,
            Some("generic_enum"),
            generate_generic_enum_try_from_selector_impl,
        ))
        .chain(apply_generator(
            &enums,
            Some("selector"),
            generate_selector_enum,
        ))
        .chain(apply_generator(
            &enums,
            Some("selector"),
            generate_selector_from_storage_impl,
        ))
        .chain(apply_generator(
            &enums,
            Some("builder"),
            generate_typed_builder_trait,
        ))
        .chain(generate_overload_dummy_struct(Some("builder")))
        .chain(apply_generator(
            &enums,
            Some("builder"),
            generate_overloaded_builder_trait,
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
