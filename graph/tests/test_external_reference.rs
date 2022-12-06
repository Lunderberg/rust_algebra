// use graph::{Builder, ContainerOf, Visiting};
// use graph_derive::recursive_graph;

// #[recursive_graph]
// mod expr {
//     enum IntExpr<'a> {
//         Int(&'a i64),
//         Add(IntExpr, IntExpr),
//     }
// }

// use expr::IntExpr;

// impl<'a, Container: ContainerOf<'a, IntExpr<'a>>> IntExpr<'a, Visiting<'a, Container>> {
//     fn eval(&self) -> i64 {
//         match self {
//             IntExpr::Int(val) => ***val,
//             IntExpr::Add(a, b) => a.borrow().unwrap().eval() + b.borrow().unwrap().eval(),
//         }
//     }
// }

// #[test]
// fn eval() {
//     let data: Vec<i64> = vec![5, 10];
//     let expression = {
//         use expr::builder::*;
//         let mut builder = Builder::new();
//         let a = builder.push(IntExpr::Int(&data[0]));
//         let b = builder.push(IntExpr::Int(&data[1]));
//         let _c = builder.push(IntExpr::Add(a, b));
//         builder
//     };
//     let root_node: expr::IntExpr<_> = expression.borrow_root().unwrap();
//     assert_eq!(root_node.eval(), 15);
// }
