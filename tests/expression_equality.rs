// use algebra::expr::*;
// use graph::{Builder, TypedTree};

// #[test]
// fn int_equal_separate_tree() {
//     let expr1: TypedTree<IntExpr> = {
//         let mut builder = Builder::new();
//         builder.push(IntExpr::Int(5));
//         builder.into()
//     };
//     let expr2: TypedTree<IntExpr> = {
//         let mut builder = Builder::new();
//         builder.push(IntExpr::Int(5));
//         builder.into()
//     };
//     assert!(expr1.root().equivalent_structure(expr2.root()));
// }
