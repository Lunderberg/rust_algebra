use std::fmt::Debug;
use std::marker::PhantomData;

use crate::{Error, Result};

#[allow(dead_code)]
#[derive(Debug)]
struct GraphRef<T> {
    rel_pos: usize,
    _phantom: PhantomData<*const T>,
}

impl<T> From<usize> for GraphRef<T> {
    fn from(rel_pos: usize) -> Self {
        Self {
            rel_pos,
            _phantom: PhantomData,
        }
    }
}

use graph_derive::make_graph;

make_graph! {
    // First enum must have no items, becomes the template for the
    // storage enum.
    #[derive(Debug)]
    enum Expr;

    #[derive(Debug)]
    enum IntExpr {
        Int(i64),
        Add(IntExpr, IntExpr),
        Sub(IntExpr, IntExpr),
    }

    #[derive(Debug)]
    enum FloatExpr {
        Float(f64),
        Add(FloatExpr, FloatExpr),
        Sub(FloatExpr, FloatExpr),
    }

    #[derive(Debug)]
    enum BoolExpr {
        Bool(bool),
        IntEqual(IntExpr, IntExpr),
        FloatEqual(FloatExpr, FloatExpr),
        And(BoolExpr, BoolExpr),
        Or(BoolExpr, BoolExpr),
    }
}

// These Into implementations should be replaceable with Provider,
// feature(provide_any).
// https://github.com/rust-lang/rust/issues/96024
impl<'a, 'b> Into<Option<LiveIntExpr<'b>>> for LiveExpr<'a>
where
    'a: 'b,
{
    fn into(self) -> Option<LiveIntExpr<'b>> {
        match self {
            LiveExpr::IntExpr(e) => Some(e),
            _ => None,
        }
    }
}

impl<'a, 'b> Into<Option<LiveFloatExpr<'b>>> for LiveExpr<'a>
where
    'a: 'b,
{
    fn into(self) -> Option<LiveFloatExpr<'b>> {
        match self {
            LiveExpr::FloatExpr(e) => Some(e),
            _ => None,
        }
    }
}

impl<'a, 'b> Into<Option<LiveBoolExpr<'b>>> for LiveExpr<'a>
where
    'a: 'b,
{
    fn into(self) -> Option<LiveBoolExpr<'b>> {
        match self {
            LiveExpr::BoolExpr(e) => Some(e),
            _ => None,
        }
    }
}

impl Expr {
    fn to_live_inner<'a, 'b>(&self, subgraph: Subgraph<'a>) -> LiveExpr<'b>
    where
        'a: 'b,
    {
        match self {
            Expr::IntExpr(e) => LiveExpr::IntExpr(e.to_live(subgraph)),
            Expr::FloatExpr(e) => LiveExpr::FloatExpr(e.to_live(subgraph)),
            Expr::BoolExpr(e) => LiveExpr::BoolExpr(e.to_live(subgraph)),
        }
    }
}

impl IntExpr {
    fn to_live<'a, 'b>(&self, subgraph: Subgraph<'a>) -> LiveIntExpr<'b>
    where
        'a: 'b,
    {
        match self {
            IntExpr::Int(i) => LiveIntExpr::Int(*i),
            IntExpr::Add(a, b) => LiveIntExpr::Add(
                LiveGraphRef::new(a.rel_pos, subgraph.clone()),
                LiveGraphRef::new(b.rel_pos, subgraph),
            ),
            IntExpr::Sub(a, b) => LiveIntExpr::Sub(
                LiveGraphRef::new(a.rel_pos, subgraph.clone()),
                LiveGraphRef::new(b.rel_pos, subgraph),
            ),
        }
    }
}

impl FloatExpr {
    fn to_live<'a, 'b>(&self, subgraph: Subgraph<'a>) -> LiveFloatExpr<'b>
    where
        'a: 'b,
    {
        match self {
            FloatExpr::Float(f) => LiveFloatExpr::Float(*f),
            FloatExpr::Add(a, b) => LiveFloatExpr::Add(
                LiveGraphRef::new(a.rel_pos, subgraph.clone()),
                LiveGraphRef::new(b.rel_pos, subgraph),
            ),
            FloatExpr::Sub(a, b) => LiveFloatExpr::Sub(
                LiveGraphRef::new(a.rel_pos, subgraph.clone()),
                LiveGraphRef::new(b.rel_pos, subgraph),
            ),
        }
    }
}

impl BoolExpr {
    fn to_live<'a, 'b>(&self, subgraph: Subgraph<'a>) -> LiveBoolExpr<'b>
    where
        'a: 'b,
    {
        match self {
            BoolExpr::Bool(b) => LiveBoolExpr::Bool(*b),
            BoolExpr::IntEqual(a, b) => LiveBoolExpr::IntEqual(
                LiveGraphRef::new(a.rel_pos, subgraph.clone()),
                LiveGraphRef::new(b.rel_pos, subgraph),
            ),
            BoolExpr::FloatEqual(a, b) => LiveBoolExpr::FloatEqual(
                LiveGraphRef::new(a.rel_pos, subgraph.clone()),
                LiveGraphRef::new(b.rel_pos, subgraph),
            ),
            BoolExpr::And(a, b) => LiveBoolExpr::And(
                LiveGraphRef::new(a.rel_pos, subgraph.clone()),
                LiveGraphRef::new(b.rel_pos, subgraph),
            ),
            BoolExpr::Or(a, b) => LiveBoolExpr::Or(
                LiveGraphRef::new(a.rel_pos, subgraph.clone()),
                LiveGraphRef::new(b.rel_pos, subgraph),
            ),
        }
    }
}

#[allow(dead_code)]
struct Graph {
    items: Vec<Expr>,
}

impl Graph {
    #[allow(dead_code)]
    fn new(items: Vec<Expr>) -> Result<Self> {
        (!items.is_empty())
            .then(|| Self { items })
            .ok_or(Error::EmptyExpression)
    }
}

#[allow(dead_code)]
#[derive(Clone)]
struct Subgraph<'a> {
    items: &'a [Expr],
}

impl<'a, T> LiveGraphRef<'a, T> {}

impl<'a, 'b> From<&'a Graph> for Subgraph<'b>
where
    'a: 'b,
{
    fn from(g: &'a Graph) -> Self {
        Self { items: &g.items }
    }
}

impl Graph {
    #[allow(dead_code)]
    fn root<'a, 'b>(&'a self) -> LiveGraphRef<'b, LiveExpr>
    where
        'a: 'b,
    {
        LiveGraphRef {
            rel_pos: 0,
            _phantom: PhantomData,
            subgraph: self.into(),
        }
    }
}

#[allow(dead_code)]
struct LiveGraphRef<'a, T> {
    rel_pos: usize,
    _phantom: PhantomData<*const T>,
    subgraph: Subgraph<'a>,
}

impl<'a, T> Debug for LiveGraphRef<'a, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("LiveGraphRef")
            .field("rel_pos", &self.rel_pos)
            .finish()
    }
}

impl<'a, T> LiveGraphRef<'a, T> {
    fn new<'b>(rel_pos: usize, subgraph: Subgraph<'b>) -> Self
    where
        'b: 'a,
    {
        Self {
            rel_pos,
            subgraph,
            _phantom: PhantomData,
        }
    }

    // The borrow method can't be replaced with Deref because we need
    // to result a LiveGraphRef value, which doesn't already exist.
    //
    // May be able to replace with std::ops::Try implementation?
    // https://github.com/rust-lang/rfcs/pull/3058
    #[allow(dead_code)]
    fn borrow<'b>(&self) -> Result<T>
    where
        'a: 'b,
        LiveExpr<'a>: Into<Option<T>>,
    {
        let subgraph_size = self.subgraph.items.len();
        if self.rel_pos >= subgraph_size {
            return Err(Error::InvalidReference {
                rel_pos: self.rel_pos,
                subgraph_size,
            });
        }

        let index = (subgraph_size - 1) - self.rel_pos;

        let item: &Expr = &self.subgraph.items[index];
        let live_expr: LiveExpr = item.to_live_inner(self.subgraph.clone());

        let output: Option<T> = live_expr.into();

        output.ok_or_else(|| Error::IncorrectType {
            expected: std::any::type_name::<T>().to_string(),
            actual: "???".to_string(),
        })
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::Result;

    #[test]
    fn test_basic() -> Result<()> {
        // let expr: Graph<Expr> = Graph::new(Expr::Sub(
        //     Expr::Add(Expr::Int(5), Expr::Int(15)),
        //     Expr::Int(10),
        // ));
        let expr = Graph::new(vec![
            Expr::IntExpr(IntExpr::Int(5)),
            Expr::IntExpr(IntExpr::Int(15)),
            Expr::IntExpr(IntExpr::Add(2.into(), 1.into())),
            Expr::IntExpr(IntExpr::Int(10)),
            Expr::IntExpr(IntExpr::Sub(2.into(), 1.into())),
        ])?;

        let root: LiveGraphRef<LiveExpr> = expr.root();
        let borrowed: Result<LiveExpr> = root.borrow();

        match borrowed? {
            LiveExpr::IntExpr(i) => {
                println!("Found int expression, {i:?}");
                match i {
                    LiveIntExpr::Int(a) => {
                        println!("IntLiteral {a:?}")
                    }
                    LiveIntExpr::Add(a, b) => {
                        println!("Addition of {a:?} and {b:?}")
                    }
                    LiveIntExpr::Sub(a, b) => {
                        println!(
                            "Subtraction of {a:?} and {b:?}, which are {:?} and {:?}",
                            a.borrow()?,
                            b.borrow()?
                        )
                    }
                }
            }
            LiveExpr::FloatExpr(_f) => {
                println!("Found float expression");
            }
            LiveExpr::BoolExpr(_b) => {
                println!("Found bool expression");
            }
        }

        Ok(())
    }
}
