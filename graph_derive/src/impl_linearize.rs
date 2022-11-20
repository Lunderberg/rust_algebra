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

        let enum_referents: Vec<(syn::ItemEnum, Vec<syn::ItemEnum>)> = stripped_enums
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

        (item_mod, enum_referents)
    } else {
        (item_mod, Vec::new())
    }
}

fn generate_import_generic_recursive_enum<'a>(
    item_enum: &'a syn::ItemEnum,
    _referenced_enums: &'a Vec<syn::ItemEnum>,
) -> impl Iterator<Item = syn::Item> + 'a {
    let ident = &item_enum.ident;
    let stream = quote! {
        pub use generic_enum::#ident;
    };
    std::iter::once(syn::parse2(stream).expect("Error parsing generated 'use' statement"))
}

fn generate_generic_recursive_enum<'a>(
    item_enum: &'a syn::ItemEnum,
    referenced_enums: &'a Vec<syn::ItemEnum>,
) -> impl Iterator<Item = syn::Item> + 'a {
    struct Mutator<F: Fn(&syn::Type) -> bool> {
        is_recursive_type: F,
    }

    impl<F: Fn(&syn::Type) -> bool> Fold for Mutator<F> {
        fn fold_generics(&mut self, generics: syn::Generics) -> syn::Generics {
            let params = generics.params;
            syn::parse2(quote! {
                <Ref: ::graph::Reference, #params>
            })
            .expect("Error parsing generated generics for generated enum")
        }

        fn fold_type(&mut self, ty: syn::Type) -> syn::Type {
            if (self.is_recursive_type)(&ty) {
                syn::parse2(quote! {
                    Ref::TypedRef<#ty<::graph::StorageReference>>
                })
                .expect("Error parsing re-written recursive type for generic enum")
            } else {
                ty
            }
        }
    }

    let enum_def: syn::ItemEnum = Mutator {
        is_recursive_type: recursive_type_checker(referenced_enums),
    }
    // .fold_item_enum({
    //     let mut item_enum = item_enum.clone();
    //     item_enum.attrs.clear();
    //     item_enum
    // });
    .fold_item_enum(item_enum.clone());

    std::iter::once(enum_def.into())
}

fn generate_storage_enum(
    item_enum: &syn::ItemEnum,
    _referenced: &Vec<syn::ItemEnum>,
) -> impl Iterator<Item = syn::Item> {
    let ident = &item_enum.ident;
    let stream = quote! {
        pub type #ident =
            super::generic_enum::#ident<::graph::StorageReference>;
    };
    std::iter::once(syn::parse2(stream).unwrap())
}

fn generate_storage_trait_impl<'a>(
    item_enum: &'a syn::ItemEnum,
    referenced_enums: &'a Vec<syn::ItemEnum>,
) -> impl Iterator<Item = syn::Item> + 'a {
    let is_recursive_type = recursive_type_checker(referenced_enums);

    let ident = &item_enum.ident;

    let live_type_arms: Vec<syn::Arm> = item_enum
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
                            let stream = if is_recursive_type(&field.ty) {
                                quote! {
                                    ::graph::LiveGraphRef::new(*#ident, subgraph.clone())
                                }
                            } else {
                                quote! { *#ident }
                            };
                            let expr: syn::Expr =
                                syn::parse2(stream).expect("Error parsing generated live field");
                            (ident, expr)
                        })
                        .unzip();
                    syn::parse2(quote! {
                        Self::#ident(#(#field_names),*) =>
                            Self::LiveType::<'a, BaseType>::#ident(
                                #(#live_field_exprs),*
                            ),
                    })
                    .expect("Error parsing generated arm in match statement")
                }
                syn::Fields::Unit => syn::parse2(quote! {
                    Self::#ident => Self::LiveType::<'a, BaseType>::#ident,
                })
                .unwrap(),
            }
        })
        .collect();

    let stream = quote! {
        impl ::graph::GraphNode for #ident {
            type DefaultSelector = super::selector::#ident;

            type LiveType<'a, BaseType: ::graph::GraphNode+ 'a> =
                super::live::#ident<'a, BaseType>;

            fn to_live_type<'a, BaseType: ::graph::GraphNode+ 'a>(
                &self, subgraph: ::graph::Subgraph<'a, BaseType>
            ) -> Self::LiveType<'a, BaseType>
            where
                for<'c> &'c Self: TryFrom<&'c BaseType::DefaultSelector> {
                match self {
                    #(#live_type_arms)*
                }
            }
        }
    };

    std::iter::once(syn::parse2(stream).expect("Error parsing generated storage trait"))
}

fn generate_storage_try_from_selector_impl<'a>(
    item: &'a syn::ItemEnum,
    referenced_enums: &'a Vec<syn::ItemEnum>,
) -> impl Iterator<Item = syn::Item> + 'a {
    let selector = &item.ident;

    let node_types: Vec<_> = referenced_enums
        .iter()
        .map(|item_enum| &item_enum.ident)
        .cloned()
        .collect();

    referenced_enums
        .iter()
        .map(|item_enum| &item_enum.ident)
        .map(move |node_type| {
            let node_name = format!("{node_type}");

            let (other_node_types, other_node_names): (Vec<_>, Vec<_>) = node_types
                .iter()
                .filter(|other_type| other_type != &node_type)
                .map(|other_type| (other_type.clone(), format!("{other_type}")))
                .unzip();

            let stream = quote! {
                impl<'a,'b:'a> TryFrom<&'b super::selector::#selector> for &'a #node_type {
                    type Error = ::graph::Error;

                    fn try_from(val: &'b super::selector::#selector)
                                -> Result<Self,Self::Error> {
                        match val {
                            super::selector::#selector::#node_type(e) => Ok(e),
                            #(
                                super::selector::#selector::#other_node_types(_) => {
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

            syn::parse2(stream).unwrap()
        })
}

fn generate_live_enum(
    item_enum: &syn::ItemEnum,
    _referenced: &Vec<syn::ItemEnum>,
) -> impl Iterator<Item = syn::Item> {
    let ident = &item_enum.ident;
    let stream = quote! {
        pub type #ident<'a, BaseType> =
            super::generic_enum::#ident<
                    ::graph::LiveReference<'a,BaseType>
                    >;
    };
    std::iter::once(syn::parse2(stream).unwrap())
}

fn generate_live_enum_trait_impl(
    item_enum: &syn::ItemEnum,
    _referenced: &Vec<syn::ItemEnum>,
) -> impl Iterator<Item = syn::Item> {
    let ident = &item_enum.ident;

    let stream = quote! {
        impl<'a, BaseType: ::graph::GraphNode+ 'a>
            ::graph::LiveGraphNode<'a, BaseType>
            for #ident<'a, BaseType>
        where
            super::storage::#ident: ::graph::GraphNode<LiveType<'a, BaseType> = Self>,
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

fn generate_typed_builder_trait<'a>(
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
                            ::graph::GraphBuilderRef<super::storage::#ty>
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
                                -> ::graph::GraphBuilderRef<super::storage::#ident>;
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
                                -> ::graph::GraphBuilderRef<super::storage::#ident> {
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
        impl<BaseType: ::graph::GraphNode> #builder
            for ::graph::Graph<BaseType>
        where BaseType::DefaultSelector: From<super::storage::#ident>,
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
    item_enum: &'a syn::ItemEnum,
    referenced_enums: &'a Vec<syn::ItemEnum>,
) -> impl Iterator<Item = syn::Item> + 'a {
    let base_type = &item_enum.ident;

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

    let is_recursive_type = recursive_type_checker(referenced_enums);

    let info: Vec<MethodInfo> = referenced_enums
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
                            if is_recursive_type(ty) {
                                syn::parse2(quote! {
                                    ::graph::GraphBuilderRef<super::storage::#ty>
                                })
                                .unwrap()
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

    let overload_sets: Vec<OverloadSet> = info
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
                pub trait #dummy_trait_name< #( #generic_type_params ),* > {
                    #![allow(non_snake_case)]

                    type OutNode;
                    fn #method_name(
                        graph: &mut ::graph::Graph<super::storage::#base_type>,
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
                    let stream = quote! {
                        impl #dummy_trait_name< #( #field_types ),* > for OverloadDummy {
                            type OutNode = super::storage::#storage_type;
                            fn #method_name (
                                graph: &mut ::graph::Graph<super::storage::#base_type>,
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
                pub trait #method_trait_name {
                    #![allow(non_snake_case)]

                    fn #method_name< #( #generic_type_params ),* >(
                        &mut self,
                        #( #param_names: #generic_type_params, )*
                    ) -> ::graph::GraphBuilderRef<<
                        OverloadDummy as #dummy_trait_name
                             < #( #generic_type_params ),* >
                        >::OutNode>
                    where
                        OverloadDummy: #dummy_trait_name
                    < #( #generic_type_params ),* >;
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
                impl #method_trait_name for ::graph::Graph<super::storage::#base_type> {
                    fn #method_name< #( #generic_type_params ),* >(
                        &mut self,
                        #( #param_names: #generic_type_params, )*
                    ) -> ::graph::GraphBuilderRef<<
                            OverloadDummy as #dummy_trait_name
                                < #( #generic_type_params ),* >
                            >::OutNode>
                    where
                        OverloadDummy: #dummy_trait_name
                        < #( #generic_type_params ),* > {
                        <OverloadDummy as #dummy_trait_name
                         < #( #generic_type_params ),* >
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
    enums: &'a Vec<(syn::ItemEnum, Vec<syn::ItemEnum>)>,
    dest_module: Option<&'a str>,
    mut generator: G,
) -> impl Iterator<Item = (Option<&'a str>, syn::Item)> + 'a
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
            Some("storage"),
            generate_storage_enum,
        ))
        .chain(apply_generator(
            &enums,
            Some("storage"),
            generate_storage_trait_impl,
        ))
        .chain(apply_generator(
            &enums,
            Some("storage"),
            generate_storage_try_from_selector_impl,
        ))
        .chain(apply_generator(&enums, Some("live"), generate_live_enum))
        .chain(apply_generator(
            &enums,
            Some("live"),
            generate_live_enum_trait_impl,
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
