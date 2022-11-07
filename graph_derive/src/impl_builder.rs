use quote::quote;
use syn::parse_macro_input;

pub fn graph_build(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let orig: syn::ExprCall = parse_macro_input!(item as syn::ExprCall);

    let mut elements = Vec::new();
    process_call(orig, &mut elements);

    // TODO: Use backing type as determined by the top-level node,
    // maybe with a user-specifiable override.
    quote! { crate::Graph::new(vec![
        #(#elements,)*
    ])}
    .into()
}

enum ArgPrecursor {
    Expr(syn::Expr),
    ElementIndex(usize),
}

fn process_call(mut call: syn::ExprCall, elements: &mut Vec<syn::Expr>) {
    let precursors: Vec<ArgPrecursor> = call
        .args
        .iter()
        .cloned()
        .map(|arg: syn::Expr| -> ArgPrecursor {
            match arg {
                syn::Expr::Call(call_arg) => {
                    process_call(call_arg, elements);
                    ArgPrecursor::ElementIndex(elements.len() - 1)
                }
                _ => ArgPrecursor::Expr(arg),
            }
        })
        .collect();

    let new_element_index = elements.len();

    call.args = precursors
        .into_iter()
        .map(|precursor: ArgPrecursor| -> syn::Expr {
            match precursor {
                ArgPrecursor::Expr(expr) => expr,
                ArgPrecursor::ElementIndex(arg_index) => {
                    let rel_pos = new_element_index - arg_index;
                    syn::parse2(quote! {
                        #rel_pos.into()
                    })
                    .unwrap()
                }
            }
        })
        .collect();

    // TODO: Use the appropriate selector for the storage type.
    elements.push(
        syn::parse2(quote! {
            IntSelector::IntExpr(#call)
        })
        .unwrap(),
    );
}
