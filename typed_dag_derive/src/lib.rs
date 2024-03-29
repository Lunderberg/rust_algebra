mod impl_annotate;
mod impl_linearize;
mod impl_tree;

#[proc_macro_attribute]
pub fn annotate_recursive_enum_types(
    attr: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    impl_annotate::annotate_recursive_enums(attr, item)
}

#[proc_macro_attribute]
pub fn linearize_recursive_enum_types(
    attr: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    impl_linearize::linearize_recursive_enums(attr, item)
}

#[proc_macro_attribute]
pub fn typed_dag(
    _attr: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let item = annotate_recursive_enum_types(proc_macro::TokenStream::new(), item);
    let item = linearize_recursive_enum_types(proc_macro::TokenStream::new(), item);
    item
}

#[proc_macro]
pub fn tree(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    impl_tree::tree(input)
}
