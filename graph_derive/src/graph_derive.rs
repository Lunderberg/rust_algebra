use std::collections::HashSet;

use proc_macro::{Delimiter, Group, Ident, Punct, Spacing, TokenStream, TokenTree};

use itertools::Itertools;

#[proc_macro]
pub fn make_graph(tokens: TokenStream) -> TokenStream {
    let is_enum_ident = |tt: TokenTree| match tt {
        TokenTree::Ident(ident) if ident.to_string() == "enum" => true,
        _ => false,
    };

    let enum_idents: HashSet<String> = tokens
        .clone()
        .into_iter()
        .tuple_windows()
        .filter_map(|(a, b)| is_enum_ident(a).then_some(b))
        .filter_map(|tt| match tt {
            TokenTree::Ident(ident) => Some(ident),
            _ => None,
        })
        .map(|id: Ident| format!("{}", id))
        .collect();

    let replace_enum_class = |ident: Ident| -> TokenStream {
        if enum_idents.contains(&format!("{}", ident)) {
            let tokens: Vec<TokenTree> = vec![
                Ident::new("GraphRef", ident.span()).into(),
                Punct::new('<', Spacing::Alone).into(),
                ident.into(),
                Punct::new('>', Spacing::Alone).into(),
            ];
            tokens.into_iter().collect()
        } else {
            std::iter::once::<TokenTree>(ident.into()).collect()
        }
    };

    let update_enum_variant = |variant: Group| -> TokenTree {
        let stream: TokenStream = match variant.delimiter() {
            Delimiter::Parenthesis => variant
                .stream()
                .into_iter()
                .map(|tt| match tt {
                    TokenTree::Ident(ident) => replace_enum_class(ident),
                    _ => std::iter::once(tt).collect(),
                })
                .collect(),

            Delimiter::Brace => variant
                .stream()
                .into_iter()
                .map(|tt| -> TokenStream {
                    match tt {
                        _ => std::iter::once(tt).collect(),
                    }
                })
                .collect(),
            _ => {
                panic!(
                    "Expected () or {{}} in enum variant, but found {:?}",
                    variant.delimiter()
                )
            }
        };

        Group::new(variant.delimiter(), stream).into()
    };

    let update_enum_body = |body: Group| {
        Group::new(
            body.delimiter(),
            body.stream()
                .into_iter()
                .map(|tt| match tt {
                    TokenTree::Group(variant) => update_enum_variant(variant),
                    _ => tt,
                })
                .collect(),
        )
    };

    let look_behind = std::iter::repeat(None)
        .take(2)
        .chain(tokens.clone().into_iter().map(|tt| Some(tt)));

    look_behind
        .zip(tokens.into_iter())
        .map(|(prev, tt): (Option<TokenTree>, TokenTree)| -> TokenTree {
            match (prev.map_or(false, |p| is_enum_ident(p)), tt) {
                (true, TokenTree::Group(group)) => update_enum_body(group).into(),
                (_, tt) => tt,
            }
        })
        .collect()
}
