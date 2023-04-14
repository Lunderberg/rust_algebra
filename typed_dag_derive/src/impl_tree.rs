use quote::{format_ident, quote};
use syn::parse_macro_input;

pub fn tree(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let orig: syn::ExprCall = parse_macro_input!(input as syn::ExprCall);
    let call = tree_arg(orig);

    let stream = quote! {
        ::typed_dag::Arena::build(|arena| {
            #call
        })
    };

    stream.into()
}

fn tree_arg(call: syn::ExprCall) -> syn::Expr {
    let func = call.func;
    let (statements, args): (Vec<_>, Vec<_>) = call
        .args
        .into_iter()
        .enumerate()
        .map(|(i, expr)| -> (Option<syn::Stmt>, syn::Expr) {
            match expr {
                syn::Expr::Call(call) => {
                    let ident = format_ident!("arg{i}");
                    let call = tree_arg(call);
                    let stmt = syn::parse2(quote! {
                        let #ident = #call;
                    })
                    .expect("Failed to parse generated let stmt");
                    let arg = syn::parse2(quote! { #ident })
                        .expect("Failed to parse ident as expression");
                    (Some(stmt), arg)
                }
                _ => (None, expr),
            }
        })
        .unzip();

    let statements: Vec<_> = statements.into_iter().flatten().collect();

    let builder_push = quote! {
        arena.push( #func ( #(#args),* ) )
    };

    let stream = if statements.is_empty() {
        builder_push
    } else {
        quote! {
            {
                #(#statements)*
                #builder_push
            }
        }
    };

    syn::parse2(stream).expect("Failed to parse generated Builder calls")
}
