use std::collections::HashMap;

use quote::{format_ident, quote, ToTokens};
use syn::{fold::Fold, parse_macro_input, parse_quote};

use itertools::Itertools;

struct EnumInfo {
    item_enum: syn::ItemEnum,
    referenced_enums: Vec<syn::ItemEnum>,
    ext_lifetime: syn::Lifetime,
    ref_type_param: syn::Type,
    generic_params: Vec<syn::GenericParam>,
    generic_args: Vec<syn::GenericArgument>,
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

    fn _is_self_reference(&self, ty: &syn::Type) -> bool {
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

                let lifetime_params: Vec<_> = item_enum
                    .generics
                    .params
                    .iter()
                    .cloned()
                    .filter_map(|param| match param {
                        syn::GenericParam::Lifetime(lifetime_def) => Some(lifetime_def.lifetime),
                        _ => None,
                    })
                    .collect();
                let type_params: Vec<syn::Type> = item_enum
                    .generics
                    .params
                    .iter()
                    .cloned()
                    .filter_map(|param| -> Option<syn::Type> {
                        match param {
                            syn::GenericParam::Type(type_param) => {
                                let ident = type_param.ident;
                                Some(parse_quote! {#ident})
                            }
                            _ => None,
                        }
                    })
                    .collect();

                let ext_lifetime: syn::Lifetime = match lifetime_params.len() {
                    0 => parse_quote! { 'static },
                    1 => lifetime_params[0].clone(),
                    _ => std::iter::once(parse_quote! { 'ext })
                        .chain(
                            (1..)
                                .map(|i| format_ident!("'ext_{i}"))
                                .map(|name| parse_quote! { #name }),
                        )
                        .find(|lifetime| !lifetime_params.contains(lifetime))
                        .unwrap(),
                };

                let ext_lifetime_param: Option<syn::LifetimeDef> = (lifetime_params.len() == 2)
                    .then(|| {
                        parse_quote! {
                            #ext_lifetime: #(#lifetime_params)+*
                        }
                    });

                let ref_type_param: syn::Type = std::iter::once(parse_quote! { Ref })
                    .chain(
                        (1..)
                            .map(|i| format_ident!("Ref_{i}"))
                            .map(|name| parse_quote! { #name }),
                    )
                    .find(|ty| !type_params.contains(ty))
                    .unwrap();

                let generic_params: Vec<_> = std::iter::empty()
                    .chain(ext_lifetime_param.into_iter().map_into())
                    .chain(item_enum.generics.params.iter().cloned())
                    .collect();

                let generic_args = generic_params
                    .iter()
                    .map(|param| match param {
                        syn::GenericParam::Type(type_def) => {
                            let ident = &type_def.ident;
                            let type_arg: syn::Type = parse_quote! { #ident };
                            syn::GenericArgument::Type(type_arg)
                        }
                        syn::GenericParam::Lifetime(lifetime_def) => {
                            syn::GenericArgument::Lifetime(lifetime_def.lifetime.clone())
                        }
                        syn::GenericParam::Const(const_def) => {
                            let ident = &const_def.ident;
                            let const_arg: syn::Expr = parse_quote! { #ident };
                            syn::GenericArgument::Const(const_arg)
                        }
                    })
                    .collect();

                EnumInfo {
                    item_enum,
                    referenced_enums,
                    ext_lifetime,
                    ref_type_param,
                    generic_params,
                    generic_args,
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
            let ext_lifetime = &self.info.ext_lifetime;
            if self.info.is_recursive_type(&ty) {
                parse_quote! {
                    Ref::Node<#ty>
                }
            } else {
                parse_quote! {
                    <Ref::ValueRef as ::graph::ValueRefType<#ext_lifetime>>
                        ::Value<#ty>
                }
            }
        }
    }

    let generic_params = &info.generic_params;
    let ext_lifetime = &info.ext_lifetime;
    let ref_type_param = &info.ref_type_param;

    let mut item_enum: syn::ItemEnum = Mutator { info }.fold_item_enum(info.item_enum.clone());

    item_enum.generics = parse_quote! {
        <#(#generic_params,)*
         #ref_type_param: ::graph::RefType<#ext_lifetime> = ::graph::StorageRef
         >
    };

    item_enum.vis = syn::parse2(quote! {pub}).unwrap();

    std::iter::once(item_enum.into())
}

fn generate_recursive_obj_impl<'a>(info: &'a EnumInfo) -> impl Iterator<Item = syn::Item> + 'a {
    let ext_lifetime = &info.ext_lifetime;
    let ident = &info.item_enum.ident;
    let ref_type_param = &info.ref_type_param;
    let generic_params = &info.generic_params;
    let generic_args = &info.generic_args;

    let item = parse_quote! {
        impl<#(#generic_params,)*
             #ref_type_param: ::graph::RefType<#ext_lifetime>
             >
            ::graph::RecursiveObj<#ext_lifetime> for
            #ident<#(#generic_args,)* #ref_type_param> {
                type Family = #ident<#(#generic_args),*>;
                type Ref = #ref_type_param;
            }
    };

    std::iter::once(item)
}

fn generate_recursive_family<'a>(info: &'a EnumInfo) -> impl Iterator<Item = syn::Item> + 'a {
    let ident = &info.item_enum.ident;
    let ext_lifetime = &info.ext_lifetime;
    let ref_type_param = &info.ref_type_param;
    let generic_args = &info.generic_args;
    let generic_params = &info.generic_params;

    let (convert_arms, view_arms) = {
        let arm_builder = |update_rhs: fn(&syn::Ident) -> syn::Expr| -> Vec<syn::Arm> {
            info.item_enum
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
                                    let rhs_expr = if info.is_recursive_type(&field.ty) {
                                        let rhs = update_rhs(&field_ident);
                                        parse_quote! { converter.convert_ref(#rhs) }
                                    } else {
                                        parse_quote! { #field_ident }
                                    };

                                    (field_ident, rhs_expr)
                                })
                                .unzip();

                            parse_quote! {
                                #ident::#arm_ident(#(#lhs_names),*) =>
                                    #ident::#arm_ident(#(#rhs_exprs),*),
                            }
                        }

                        syn::Fields::Unit => parse_quote! {
                            #ident::#arm_ident => #ident::#arm_ident,
                        },
                    }
                })
                .collect()
        };
        (
            arm_builder(|x| parse_quote! {#x}),
            arm_builder(|x| parse_quote! { *#x }),
        )
    };

    let recursive_family_impl = parse_quote! {
        impl<#(#generic_params,)*> ::graph::RecursiveFamily<#ext_lifetime> for #ident {
            type Sibling<#ref_type_param: ::graph::RefType<#ext_lifetime>>
                = #ident<#(#generic_args,)* #ref_type_param>;

            fn convert<FromRef, ToRef, Converter>(
                from_obj: Self::Sibling<FromRef>,
                converter: Converter,
            ) -> Self::Sibling<ToRef>
            where Self: Sized,
                  Converter: ::graph::RefConverter<#ext_lifetime,
                                                   FromRef = FromRef,
                                                   ToRef = ToRef>,

                  FromRef: ::graph::RefType<#ext_lifetime,
                                            ValueRef = ::graph::ValueOwner>,

                  ToRef: ::graph::RefType<#ext_lifetime,
                                          ValueRef = ::graph::ValueOwner>,
            {
                match from_obj {
                    #( #convert_arms )*
                }
            }

            fn view<'view, FromRef, ToRef, Converter>(
                from_obj: &'view Self::Sibling<FromRef>,
                converter: Converter,
            ) -> Self::Sibling<ToRef>
            where
                #ext_lifetime: 'view,
                Self: Sized,

                Converter: ::graph::RefConverter<#ext_lifetime,
                                                 FromRef = FromRef,
                                                 ToRef = ToRef>,

                FromRef: ::graph::RefType<#ext_lifetime,
                                          ValueRef = ::graph::ValueOwner>,

                ToRef: ::graph::RefType<#ext_lifetime,
                                        ValueRef = ::graph::ValueVisitor<'view>>,
            {
                match from_obj {
                    #( #view_arms )*
                }
            }
        }

    };

    std::iter::once(recursive_family_impl)
}

fn generate_container_enum(info: &EnumInfo) -> impl Iterator<Item = syn::Item> {
    let ident = &info.item_enum.ident;
    let referenced_idents: Vec<_> = info
        .referenced_enums
        .iter()
        .map(|ref_enum| &ref_enum.ident)
        .collect();
    let generic_params = &info.generic_params;

    let item = parse_quote! {
        pub enum #ident<#(#generic_params),*> {
            #(
                #referenced_idents (
                    super::generic_enum::#referenced_idents
                ),
            )*
        }
    };

    std::iter::once(item)
}

fn generate_container_impl(info: &EnumInfo) -> impl Iterator<Item = syn::Item> + '_ {
    let ident = &info.item_enum.ident;
    let generic_params = &info.generic_params;

    info.referenced_enums.iter().map(move |ref_enum| {
        let ref_ident = &ref_enum.ident;
        let ref_ident_str = format!("{ref_ident}");

        let (other_node_idents, other_node_str): (Vec<_>, Vec<_>) = info
            .referenced_enums
            .iter()
            .map(|other_enum| &other_enum.ident)
            .filter(|other_ident| other_ident != &ref_ident)
            .map(|other_ident| (other_ident.clone(), format!("{other_ident}")))
            .unzip();

        let try_as_ref_impl = parse_quote! {
            impl<#(#generic_params),*>
                ::graph::TryAsRef<super::generic_enum::#ref_ident>
                for #ident {
                    type Error = ::graph::Error;
                    fn try_as_ref(&self) -> Result<&super::generic_enum::#ref_ident, Self::Error> {
                        match self {
                            Self::#ref_ident(val) => Ok(val),
                            #(
                                Self::#other_node_idents =>
                                    Err(Self::Error::IncorrectType{
                                        expected: #ref_ident_str,
                                        actual: #other_node_str,
                                    }),
                            )*
                        }
                    }
                }
        };

        let from_impl = parse_quote! {
            impl<#(#generic_params),*>
                From<super::generic_enum::#ref_ident>
                for #ident {
                    fn from(obj: super::generic_enum::#ref_ident) -> Self {
                        Self::#ref_ident(obj)
                    }
                }


        };

        vec![try_as_ref_impl, from_impl].into_iter()
    }).flatten()
}

fn generate_default_container_impl(info: &EnumInfo) -> impl Iterator<Item = syn::Item> + '_ {
    let ident = &info.item_enum.ident;
    let generic_params = &info.generic_params;
    let generic_args = &info.generic_args;

    let item = parse_quote! {
        impl<#(#generic_params),*> ::graph::HasDefaultContainer
            for super::generic_enum::#ident<#(#generic_args),*> {
            type Container = #ident<#(#generic_args),*>;
        }
    };

    std::iter::once(item)
}

fn generate_visitor_trait(info: &EnumInfo) -> impl Iterator<Item = syn::Item> + '_ {
    let ident = &info.item_enum.ident;
    let ext_lifetime = &info.ext_lifetime;
    let generic_params = &info.generic_params;
    let generic_args = &info.generic_args;
    let referenced_idents: Vec<_> = info
        .referenced_enums
        .iter()
        .map(|ref_enum| &ref_enum.ident)
        .collect();
    let ref_type_param = &info.ref_type_param;

    let trait_def = parse_quote! {
        pub trait #ident<#(#generic_params,)* 'view>
        where
            #(
            Self: ::graph::VisitorOf<#ext_lifetime,
                                     super::generic_enum::#referenced_idents,
                                     ValueRef = ::graph::ValueVisitor<'view>>,
        )*
        { }
    };
    let trait_impl = parse_quote! {
        impl<#(#generic_params,)* 'view, #ref_type_param>
            #ident<#(#generic_args,)* 'view> for #ref_type_param
        where
            #(
            Self: ::graph::VisitorOf<#ext_lifetime,
                                     super::generic_enum::#referenced_idents,
                                     ValueRef = ::graph::ValueVisitor<'view>>,
        )*
        {}
    };

    vec![trait_def, trait_impl].into_iter()
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
            generate_recursive_family,
        ))
        .chain(apply_generator(
            &enums,
            Some("generic_enum"),
            generate_recursive_obj_impl,
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
        .chain(apply_generator(
            &enums,
            Some("container"),
            generate_default_container_impl,
        ))
        .chain(apply_generator(
            &enums,
            Some("visitor"),
            generate_visitor_trait,
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
