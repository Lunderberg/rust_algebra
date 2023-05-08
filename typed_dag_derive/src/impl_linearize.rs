use std::collections::{HashMap, HashSet};

use proc_macro2::Span;
use quote::{format_ident, quote, ToTokens};
use syn::{fold::Fold, parse_macro_input, parse_quote};

use itertools::Itertools;

struct EnumInfo {
    item_enum: syn::ItemEnum,
    referenced_enums: Vec<syn::ItemEnum>,
    num_lifetimes: usize,
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

    fn type_params(&self) -> impl Iterator<Item = syn::TypeParam> + '_ {
        self.item_enum
            .generics
            .params
            .iter()
            .filter_map(|param| match param {
                syn::GenericParam::Type(type_param) => Some(type_param),
                _ => None,
            })
            .cloned()
    }

    fn nonconflicting_type(&self, name: &str) -> syn::Ident {
        let type_params: HashSet<syn::Ident> = self.type_params().map(|p| p.ident).collect();
        std::iter::empty()
            .chain(std::iter::once(syn::Ident::new(name, Span::call_site())))
            .chain((1..).map(|i| format_ident!("{name}_{i}")))
            .find(|ident| !type_params.contains(ident))
            .unwrap()
    }

    fn ref_type_param(&self) -> syn::Ident {
        self.nonconflicting_type("Ref")
    }

    fn lifetime_params(&self) -> impl Iterator<Item = syn::Lifetime> + '_ {
        self.item_enum
            .generics
            .params
            .iter()
            .filter_map(|param| match param {
                syn::GenericParam::Lifetime(lifetime_def) => Some(lifetime_def.lifetime.clone()),
                _ => None,
            })
    }

    fn nonconflicting_lifetime(&self, name: &str) -> syn::Lifetime {
        let lifetime_params: HashSet<_> = self.lifetime_params().collect();
        std::iter::empty()
            .chain(std::iter::once(format!("'{name}")))
            .chain((1..).map(|i| format!("'{name}_{i}")))
            .map(|name| syn::Lifetime::new(&name, Span::call_site()))
            .find(|lifetime| !lifetime_params.contains(lifetime))
            .unwrap()
    }

    fn _new_lifetime(&self, name: syn::Lifetime) -> Option<syn::LifetimeParam> {
        let lifetime_params: HashSet<_> = self.lifetime_params().collect();
        match lifetime_params.len() {
            0 | 1 => None,
            _ => std::iter::empty::<syn::LifetimeParam>()
                .chain(std::iter::once(parse_quote! {#name}))
                .chain(
                    (1..)
                        .map(|i| format_ident!("{name}_{i}"))
                        .map(|full_name| parse_quote! { #full_name }),
                )
                .find(|lifetime_def| !lifetime_params.contains(&lifetime_def.lifetime)),
        }
    }

    fn new_ext_lifetime(&self) -> Option<syn::Lifetime> {
        match self.num_lifetimes {
            0 | 1 => None,
            _ => Some(self.nonconflicting_lifetime("ext")),
        }
    }

    fn ext_lifetime(&self) -> syn::Lifetime {
        match self.num_lifetimes {
            0 => parse_quote! {'static},
            1 => self.lifetime_params().next().unwrap(),
            _ => self.new_ext_lifetime().unwrap(),
        }
    }

    fn _generic_params(&self) -> impl Iterator<Item = syn::GenericParam> + '_ {
        std::iter::empty()
            .chain(
                self.new_ext_lifetime()
                    .into_iter()
                    .map(|lifetime| syn::LifetimeParam::new(lifetime).into()),
            )
            .chain(self.item_enum.generics.params.iter().cloned())
    }

    fn generic_params(&self) -> Vec<syn::GenericParam> {
        self._generic_params().collect()
    }

    fn _param_to_argument(param: syn::GenericParam) -> syn::GenericArgument {
        match param {
            syn::GenericParam::Type(ty) => {
                let ident = ty.ident;
                parse_quote! {#ident}
            }
            syn::GenericParam::Lifetime(def) => syn::GenericArgument::Lifetime(def.lifetime),
            syn::GenericParam::Const(val) => {
                let ident = val.ident;
                parse_quote! {#ident}
            }
        }
    }

    fn generic_args(&self) -> Vec<syn::GenericArgument> {
        self._generic_params()
            .map(Self::_param_to_argument)
            .collect()
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
                .any(|attr| is_attr(&attr.meta.path(), "requires_graph_storage_type")),
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
                    .filter(|attr| is_attr(&attr.meta.path(), "requires_graph_storage_type"))
                    .flat_map(|attr| {
                        attr.parse_args_with(
                            syn::punctuated::Punctuated::<syn::Ident, syn::Token![,]>::parse_terminated,
                        )
                            .into_iter()
                            .flatten()
                    })
                    .collect();

                item.attrs = item
                    .attrs
                    .into_iter()
                    .filter(|attr| !is_attr(&attr.meta.path(), "requires_graph_storage_type"))
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

                let num_lifetimes = item_enum
                    .generics
                    .params
                    .iter()
                    .filter(|param| match param {
                        syn::GenericParam::Lifetime(_) => true,
                        _ => false,
                    })
                    .count();

                EnumInfo {
                    item_enum,
                    referenced_enums,
                    num_lifetimes,
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
        node_ref_type: &'a syn::Ident,
        value_ref_type: &'a syn::Ident,
    }

    impl Fold for Mutator<'_> {
        fn fold_type(&mut self, ty: syn::Type) -> syn::Type {
            let ext_lifetime = &self.info.ext_lifetime();
            let node_ref_type = &self.node_ref_type;
            let value_ref_type = &self.value_ref_type;
            if self.info.is_recursive_type(&ty) {
                parse_quote! {
                    <
                        #node_ref_type as ::typed_dag::NodeRefType<#ext_lifetime>
                    >::Node<
                        <#value_ref_type as ::typed_dag::TypeLaunder>::Identity<#ty>
                    >
                }
            } else {
                parse_quote! {
                    <#value_ref_type as ::typed_dag::ValueRefType<#ext_lifetime>>
                        ::Value<#ty>
                }
            }
        }
    }

    let generic_params = &info.generic_params();
    let ext_lifetime = info.ext_lifetime();
    let node_ref_type = info.nonconflicting_type("NodeRef");
    let value_ref_type = info.nonconflicting_type("ValueRef");

    let mut item_enum: syn::ItemEnum = Mutator {
        info,
        node_ref_type: &node_ref_type,
        value_ref_type: &value_ref_type,
    }
    .fold_item_enum(info.item_enum.clone());

    item_enum.generics = parse_quote! {
        <#(#generic_params,)*
         #node_ref_type: ::typed_dag::NodeRefType<#ext_lifetime>
             = ::typed_dag::RelativePos,
         #value_ref_type: ::typed_dag::ValueRefType<#ext_lifetime>
             = <#node_ref_type as ::typed_dag::NodeRefType<#ext_lifetime>>::DefaultValueRef,
         >
    };

    item_enum.vis = syn::parse2(quote! {pub}).unwrap();

    std::iter::once(item_enum.into())
}

fn generate_recursive_obj_impl<'a>(info: &'a EnumInfo) -> impl Iterator<Item = syn::Item> + 'a {
    let ext_lifetime = info.ext_lifetime();
    let ident = &info.item_enum.ident;
    let node_ref_type = info.nonconflicting_type("NodeRef");
    let value_ref_type = info.nonconflicting_type("ValueRef");
    let generic_params = info.generic_params();
    let generic_args = info.generic_args();

    let item = parse_quote! {
        impl<#(#generic_params,)*
             #node_ref_type: ::typed_dag::NodeRefType<#ext_lifetime>,
             #value_ref_type: ::typed_dag::ValueRefType<#ext_lifetime>,
             >
            ::typed_dag::RecursiveObj<#ext_lifetime> for
            #ident<#(#generic_args,)* #node_ref_type, #value_ref_type> {
                type Family = #ident<#(#generic_args),*>;
                type NodeRef = #node_ref_type;
                type ValueRef = #value_ref_type;
            }
    };

    std::iter::once(item)
}

fn generate_recursive_family<'a>(info: &'a EnumInfo) -> impl Iterator<Item = syn::Item> + 'a {
    let ident = &info.item_enum.ident;
    let ext_lifetime = info.ext_lifetime();
    let node_ref_type = info.nonconflicting_type("NodeRef");
    let value_ref_type = info.nonconflicting_type("ValueRef");
    let generic_args = info.generic_args();
    let generic_params = info.generic_params();

    let view_lifetime = info.nonconflicting_lifetime("view");
    let subview_lifetime = info.nonconflicting_lifetime("subview");

    let (convert_arms, view_arms, subview_arms) = {
        // TODO: De-dup with copy in generate_try_expand
        let arm_builder = |update_value: fn(&syn::Ident) -> syn::Expr,
                           update_node_ref: fn(&syn::Ident) -> syn::Expr|
         -> Vec<syn::Arm> {
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
                                        let rhs = update_node_ref(&field_ident);
                                        parse_quote! { converter.convert_ref(#rhs) }
                                    } else {
                                        update_value(&field_ident)
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
            arm_builder(|x| parse_quote! { #x }, |x| parse_quote! {&#x}),
            arm_builder(|x| parse_quote! { #x }, |x| parse_quote! { #x }),
            arm_builder(|x| parse_quote! { *#x }, |x| parse_quote! { #x }),
        )
    };

    let recursive_family_impl = parse_quote! {
        impl<#(#generic_params,)*>
            ::typed_dag::RecursiveFamily<#ext_lifetime>
            for #ident<#(#generic_args,)*>
        {
            type Sibling<
                #node_ref_type: ::typed_dag::NodeRefType<#ext_lifetime>,
                #value_ref_type: ::typed_dag::ValueRefType<#ext_lifetime>,
             >
                = #ident<#(#generic_args,)* #node_ref_type, #value_ref_type>;

            fn convert<FromRef, ToRef, Converter>(
                from_obj: Self::Sibling<FromRef, ::typed_dag::ValueOwner>,
                converter: Converter,
            ) -> Self::Sibling<ToRef, ::typed_dag::ValueOwner>
            where Self: Sized,
                  Converter: ::typed_dag::RefConverter<#ext_lifetime,
                                                   FromRef = FromRef,
                                                   ToRef = ToRef>,

                  FromRef: ::typed_dag::NodeRefType<#ext_lifetime>,

                  ToRef: ::typed_dag::NodeRefType<#ext_lifetime>,
            {
                match from_obj {
                    #( #convert_arms )*
                }
            }

            fn view<#view_lifetime, FromRef, ToRef, Converter>(
                from_obj: &#view_lifetime Self::Sibling<FromRef, ::typed_dag::ValueOwner>,
                converter: Converter,
            ) -> Self::Sibling<ToRef, ::typed_dag::ValueVisitor<#view_lifetime>>
            where
                #ext_lifetime: #view_lifetime,
                Self: Sized,

                Converter: ::typed_dag::RefConverter<#ext_lifetime,
                                                 FromRef = FromRef,
                                                 ToRef = ToRef>,

                FromRef: ::typed_dag::NodeRefType<#ext_lifetime>,

                ToRef: ::typed_dag::NodeRefType<#ext_lifetime>,
            {
                match from_obj {
                    #( #view_arms )*
                }
            }

            fn subview<#view_lifetime, #subview_lifetime, FromRef, ToRef, Converter>(
                from_obj: &#subview_lifetime Self::Sibling<FromRef, ::typed_dag::ValueVisitor<#view_lifetime>>,
                converter: Converter,
            ) -> Self::Sibling<ToRef, ::typed_dag::ValueVisitor<#subview_lifetime>>
            where
                #ext_lifetime: #view_lifetime,
                #view_lifetime: #subview_lifetime,
                Self: Sized,

                Converter: ::typed_dag::RefConverter<#ext_lifetime,
                                                 FromRef = FromRef,
                                                 ToRef = ToRef>,

                FromRef: ::typed_dag::NodeRefType<#ext_lifetime>,

                ToRef: ::typed_dag::NodeRefType<#ext_lifetime>,
            {
                match from_obj {
                    #( #subview_arms )*
                }
            }
        }

    };

    std::iter::once(recursive_family_impl)
}

fn generate_container_enum(info: &EnumInfo) -> impl Iterator<Item = syn::Item> {
    let ident = &info.item_enum.ident;
    let generic_params = info.generic_params();
    let generic_args = info.generic_args();

    // TODO: Use the generic arguments appropriate for each
    // referred-to node, rather than passing all the generic
    // arguments.
    let container_variants: Vec<syn::Variant> = info
        .referenced_enums
        .iter()
        .map(|ref_enum| {
            let ref_ident = &ref_enum.ident;

            parse_quote! {
                #ref_ident ( super::generic_enum::#ref_ident<#(#generic_args),*> )
            }
        })
        .collect();

    let item = parse_quote! {
        pub enum #ident<#(#generic_params),*> {
            #(#container_variants,)*
        }
    };

    std::iter::once(item)
}

fn generate_container_from(info: &EnumInfo) -> impl Iterator<Item = syn::Item> + '_ {
    let ident = &info.item_enum.ident;
    let generic_params = info.generic_params();
    let generic_args = info.generic_args();

    info.referenced_enums.iter().map(move |ref_enum| {
        let ref_ident = &ref_enum.ident;
        let ref_enum = quote! {
            super::generic_enum::#ref_ident<#(#generic_args),*>
        };

        let from_impl = parse_quote! {
            impl<#(#generic_params),*>
                From<#ref_enum>
                for #ident<#(#generic_args),*> {
                    fn from(obj: #ref_enum) -> Self {
                        Self::#ref_ident(obj)
                    }
                }


        };

        from_impl
    })
}

fn generate_container_try_as_ref(info: &EnumInfo) -> impl Iterator<Item = syn::Item> + '_ {
    let ident = &info.item_enum.ident;
    let generic_params = info.generic_params();
    let generic_args = info.generic_args();

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

        let ref_enum = quote! {
            super::generic_enum::#ref_ident<#(#generic_args),*>
        };

        let try_as_ref_impl = parse_quote! {
            impl<#(#generic_params),*>
                ::typed_dag::TryAsRef<#ref_enum>
                for #ident<#(#generic_args),*> {
                    type Error = ::typed_dag::Error;
                    fn try_as_ref(&self) -> Result<&#ref_enum, Self::Error> {
                        match self {
                            Self::#ref_ident(val) => Ok(val),
                            #(
                                Self::#other_node_idents(_) =>
                                    Err(Self::Error::IncorrectType{
                                        expected: #ref_ident_str,
                                        actual: #other_node_str,
                                    }),
                            )*
                        }
                    }
                }
        };

        try_as_ref_impl
    })
}

fn generate_default_container_impl(info: &EnumInfo) -> impl Iterator<Item = syn::Item> + '_ {
    let ident = &info.item_enum.ident;
    let generic_params = info.generic_params();
    let generic_args = info.generic_args();

    let item = parse_quote! {
        impl<#(#generic_params),*> ::typed_dag::HasDefaultContainer
            for super::generic_enum::#ident<#(#generic_args),*> {
            type Container = #ident<#(#generic_args),*>;
        }
    };

    std::iter::once(item)
}

fn generate_container_trait(info: &EnumInfo) -> impl Iterator<Item = syn::Item> + '_ {
    let ident = &info.item_enum.ident;
    let ref_type_param = info.ref_type_param();

    let obj_args = info.generic_args();

    let trait_params: Vec<_> = info.generic_params();

    let trait_args: Vec<_> = info.generic_args();

    let from_bounds = info.referenced_enums.iter().map(|ref_enum| {
        let ref_ident = &ref_enum.ident;
        parse_quote! {
            Self: From<super::generic_enum::#ref_ident<#(#obj_args),*>>
        }
    });

    let try_as_ref_bounds = info.referenced_enums.iter().map(|ref_enum| {
        let ref_ident = &ref_enum.ident;
        parse_quote! {
            Self: ::typed_dag::TryAsRef<
                    super::generic_enum::#ref_ident<#(#obj_args),*>,
                    Error = ::typed_dag::Error
                >
        }
    });
    let trait_bounds: Vec<syn::WherePredicate> = std::iter::empty()
        .chain(from_bounds)
        .chain(try_as_ref_bounds)
        .collect();

    let trait_def = parse_quote! {
        pub trait #ident<#(#trait_params),*>
        where
            #( #trait_bounds, )*
        { }
    };
    let trait_impl = parse_quote! {
        impl<#(#trait_params,)* #ref_type_param>
            #ident<#(#trait_args,)*> for #ref_type_param
        where
            #( #trait_bounds, )*
        {}
    };
    vec![trait_def, trait_impl].into_iter()
}

fn generate_visitor_trait(info: &EnumInfo) -> impl Iterator<Item = syn::Item> + '_ {
    let ident = &info.item_enum.ident;
    let ext_lifetime = info.ext_lifetime();
    let ref_type_param = info.ref_type_param();
    let error_type_param = info.nonconflicting_type("Error");

    let new_view_lifetime = match info.num_lifetimes {
        1 => None,
        _ => Some(info.nonconflicting_lifetime("view")),
    };
    let view_lifetime = new_view_lifetime
        .clone()
        .or_else(|| info.lifetime_params().next())
        .unwrap();

    let obj_args = info.generic_args();

    let trait_params: Vec<_> = info
        .generic_params()
        .into_iter()
        .chain(
            new_view_lifetime
                .iter()
                .map(|lifetime| syn::LifetimeParam::new(lifetime.clone()).into()),
        )
        .collect();

    let trait_args: Vec<_> = info
        .generic_args()
        .into_iter()
        .chain(
            new_view_lifetime
                .iter()
                .map(|lifetime| syn::GenericArgument::Lifetime(lifetime.clone())),
        )
        .collect();

    let make_trait_bounds = |error: proc_macro2::TokenStream| -> Vec<syn::WherePredicate> {
        let visitor_of_bounds = info.referenced_enums.iter().map(|ref_enum| {
            let ref_ident = &ref_enum.ident;
            parse_quote! {
                Self: ::typed_dag::VisitorOf<
                    #ext_lifetime,
                    super::generic_enum::#ref_ident<#(#obj_args),*>,
                    ValueRef = ::typed_dag::ValueVisitor<#view_lifetime>,
                    DefaultValueRef = ::typed_dag::ValueVisitor<#view_lifetime>,
                    Error = #error,
                >
            }
        });
        let lifetime_bounds = new_view_lifetime.iter().flat_map(|view| {
            info.lifetime_params()
                .map(move |lifetime| parse_quote! { #lifetime : #view })
        });

        let trait_bounds: Vec<syn::WherePredicate> = {
            std::iter::empty()
                .chain(visitor_of_bounds)
                .chain(lifetime_bounds)
                .collect()
        };
        trait_bounds
    };

    let trait_def = {
        let trait_bounds =
            make_trait_bounds(quote! { <Self as #ident<#(#trait_params),*>>::Error });
        parse_quote! {
            pub trait #ident<#(#trait_params),*>
            where
                #( #trait_bounds, )*
            {
                type Error: ::std::fmt::Debug;
            }
        }
    };
    let trait_impl = {
        let trait_bounds = make_trait_bounds(quote! { #error_type_param });
        parse_quote! {
            impl<#(#trait_params,)* #ref_type_param, #error_type_param>
                #ident<#(#trait_args,)*> for #ref_type_param
            where
                #( #trait_bounds,)*
                #error_type_param: ::std::fmt::Debug,
            {
                type Error = #error_type_param;
            }
        }
    };

    vec![trait_def, trait_impl].into_iter()
}

fn generate_try_expand(info: &EnumInfo) -> impl Iterator<Item = syn::Item> + '_ {
    let ident = &info.item_enum.ident;
    let this_enum = quote! { super::generic_enum::#ident };
    let ext_lifetime = info.ext_lifetime();
    let ref_type_param = info.ref_type_param();
    let error_type_param = info.nonconflicting_type("Error");

    let new_view_lifetime = match info.num_lifetimes {
        1 => None,
        _ => Some(info.nonconflicting_lifetime("view")),
    };
    let view_lifetime = new_view_lifetime
        .clone()
        .or_else(|| info.lifetime_params().next())
        .unwrap();

    let trait_params: Vec<_> = info
        .generic_params()
        .into_iter()
        .chain(
            new_view_lifetime
                .iter()
                .map(|lifetime| syn::LifetimeParam::new(lifetime.clone()).into()),
        )
        .chain(std::iter::once(parse_quote! { #error_type_param }))
        .collect();

    let trait_args: Vec<_> = info
        .generic_args()
        .into_iter()
        .chain(
            new_view_lifetime
                .iter()
                .map(|lifetime| syn::GenericArgument::Lifetime(lifetime.clone())),
        )
        .collect();

    let generic_args = info.generic_args();

    let visitor_of_bounds = info.referenced_enums.iter().map(|ref_enum| {
        let ref_ident = &ref_enum.ident;
        parse_quote! {
            Ref: ::typed_dag::VisitorOf<
                    #ext_lifetime,
                super::generic_enum::#ref_ident<#(#generic_args),*>,
                ValueRef = ::typed_dag::ValueVisitor<#view_lifetime>,
                DefaultValueRef = ::typed_dag::ValueVisitor<#view_lifetime>,
                Error = #error_type_param,
                    >
        }
    });
    let lifetime_bounds = new_view_lifetime.iter().flat_map(|view| {
        info.lifetime_params()
            .map(move |lifetime| parse_quote! { #lifetime : #view })
    });
    let debug_bound = parse_quote! { #error_type_param: ::std::fmt::Debug };

    let trait_bounds: Vec<syn::WherePredicate> = {
        std::iter::empty()
            .chain(visitor_of_bounds)
            .chain(lifetime_bounds)
            .chain(std::iter::once(debug_bound))
            .collect()
    };

    // TODO: De-dup with copy in generate_recursive_family
    let arm_builder = |update_value: fn(&syn::Ident) -> syn::Expr,
                       update_node_ref: fn(&syn::Ident, &syn::Ident) -> syn::Expr|
     -> Vec<syn::Arm> {
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
                                    update_node_ref(&ref_type_param, &field_ident)
                                } else {
                                    update_value(&field_ident)
                                };

                                (field_ident, rhs_expr)
                            })
                            .unzip();

                        parse_quote! {
                            #this_enum::#arm_ident(#(#lhs_names),*) =>
                                #this_enum::#arm_ident(#(#rhs_exprs),*),
                        }
                    }

                    syn::Fields::Unit => parse_quote! {
                        #this_enum::#arm_ident => #this_enum::#arm_ident,
                    },
                }
            })
            .collect()
    };

    let match_arms = arm_builder(
        |x| parse_quote! { *#x },
        |ref_type_param, x| parse_quote! { #ref_type_param::try_expand_impl(#x)? },
    );

    let target = quote! { super::generic_enum::#ident<#(#generic_args,)*> };

    let trait_impl = parse_quote! {
        impl<#(#trait_params,)* #ref_type_param>
            ::typed_dag::VisitorOf<#ext_lifetime, #target>
            for ::typed_dag::NestedVisitor<#ref_type_param, ::typed_dag::ValueVisitor<#view_lifetime>>
        where
            //#ref_type_param: super::visitor::#ident<#(#trait_args,)*>,
            #( #trait_bounds, )*
        {
            type Error = <#ref_type_param as super::visitor::#ident<#(#trait_args,)*>>::Error;
            type ValueRef = ::typed_dag::ValueVisitor<#view_lifetime>;

            fn try_expand_impl(
                obj: &<Self as ::typed_dag::NodeRefType<#ext_lifetime>>::Node<#target>
            ) -> Result<
                    <
                        #target as ::typed_dag::RecursiveFamily<#ext_lifetime>
                    >::Sibling<Self, ::typed_dag::ValueVisitor<#view_lifetime>>,
                Self::Error> {
                let nested_obj = match obj {
                    #(
                        #match_arms
                    )*
                };
                Ok(nested_obj)
            }
        }
    };

    std::iter::once(trait_impl)
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
            Some("default_container"),
            generate_container_enum,
        ))
        .chain(apply_generator(
            &enums,
            Some("default_container"),
            generate_container_from,
        ))
        .chain(apply_generator(
            &enums,
            Some("default_container"),
            generate_container_try_as_ref,
        ))
        .chain(apply_generator(
            &enums,
            Some("default_container"),
            generate_default_container_impl,
        ))
        .chain(apply_generator(
            &enums,
            Some("container"),
            generate_container_trait,
        ))
        .chain(apply_generator(
            &enums,
            Some("visitor"),
            generate_visitor_trait,
        ))
        .chain(apply_generator(
            &enums,
            Some("nested_expand"),
            generate_try_expand,
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
