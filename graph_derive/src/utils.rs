use std::collections::HashSet;
use syn::visit::Visit;

pub(crate) fn collect_lifetimes(item: &syn::Item) -> Vec<syn::Lifetime> {
    struct Visitor {
        lifetimes: Vec<syn::Lifetime>,
        lookup: HashSet<syn::Lifetime>,
    }

    impl<'ast> Visit<'ast> for Visitor {
        fn visit_lifetime(&mut self, lifetime: &'ast syn::Lifetime) {
            if self.lookup.contains(lifetime) {
                self.lookup.insert(lifetime.clone());
                self.lifetimes.push(lifetime.clone());
            }
        }
    }

    let mut visitor = Visitor {
        lifetimes: Vec::new(),
        lookup: HashSet::new(),
    };
    visitor.visit_item(&item);
    visitor.lifetimes
}
