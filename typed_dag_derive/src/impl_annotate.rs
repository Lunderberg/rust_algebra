use std::collections::{HashMap, HashSet};
use std::hash::Hash;

use quote::ToTokens;
use syn::{fold::Fold, parse_macro_input, parse_quote, visit::Visit};

pub fn annotate_recursive_enums(
    _attr: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let orig: syn::ItemMod = parse_macro_input!(item as syn::ItemMod);

    let indirect_references = CollectDetails::collect_indirect_references(&orig);

    let annotated = AnnotateEnums::new(indirect_references).fold_item_mod(orig);

    annotated.to_token_stream().into()
}

struct OrderedHash<T: Hash + Eq> {
    items: Vec<T>,
    lookup: HashSet<T>,
}

impl<T: Hash + Eq> OrderedHash<T> {
    fn new() -> Self {
        Self {
            items: Vec::new(),
            lookup: HashSet::new(),
        }
    }

    fn insert(&mut self, elem: T)
    where
        T: Clone,
    {
        if !self.lookup.contains(&elem) {
            self.items.push(elem.clone());
            self.lookup.insert(elem);
        }
    }

    fn contains(&self, elem: &T) -> bool {
        self.lookup.contains(elem)
    }
}

impl<T: Hash + Eq + Clone> std::iter::FromIterator<T> for OrderedHash<T> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        let mut output = Self::new();
        iter.into_iter().for_each(|elem| output.insert(elem));
        output
    }
}

struct CollectDetails {
    ident_lookup: HashMap<syn::Ident, syn::ItemEnum>,
    direct_references: HashMap<syn::ItemEnum, OrderedHash<syn::ItemEnum>>,
    current: Option<OrderedHash<syn::ItemEnum>>,
}

impl<'ast> Visit<'ast> for CollectDetails {
    fn visit_item_enum(&mut self, i: &'ast syn::ItemEnum) {
        assert!(self.current.is_none(), "Nested enum definition");
        self.current = Some(std::iter::once(i.clone()).collect());
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
    ) -> HashMap<syn::ItemEnum, Vec<syn::ItemEnum>> {
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

        visitor
            .direct_references
            .into_iter()
            .map(|(item_enum, ordered_hash)| (item_enum, ordered_hash.items))
            .collect()
    }
    fn collect_indirect_references(
        item_mod: &syn::ItemMod,
    ) -> HashMap<syn::ItemEnum, Vec<syn::ItemEnum>> {
        let direct_references = Self::collect_direct_references(item_mod);

        let indirect_references: HashMap<syn::ItemEnum, OrderedHash<syn::ItemEnum>> =
            direct_references
                .iter()
                .map(|(from, _)| {
                    let mut reachable = OrderedHash::new();
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
            .into_iter()
            .map(|(item_enum, ordered_hash)| (item_enum, ordered_hash.items))
            .collect()
    }
}

struct AnnotateEnums {
    indirect_references: HashMap<syn::ItemEnum, Vec<syn::ItemEnum>>,
}

impl AnnotateEnums {
    fn new(indirect_references: HashMap<syn::ItemEnum, Vec<syn::ItemEnum>>) -> Self {
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
        item_enum
            .attrs
            .push(parse_quote! { #[requires_graph_storage_type(
                #(#indirect_references),*
            )]});
        syn::fold::fold_item_enum(self, item_enum)
    }
}
