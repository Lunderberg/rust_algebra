use std::fmt::Debug;
use std::marker::PhantomData;

use crate::{Error, Result};

pub trait NodeTypeSelector: Sized {
    fn type_name(&self) -> &'static str;
}

pub struct Graph<Selector: NodeTypeSelector, T: NodeType<Selector>>
where
    for<'c> &'c T: TryFrom<&'c Selector, Error = Error>,
{
    items: Vec<Selector>,
    _top_level: PhantomData<T>,
}

pub struct Subgraph<'a, Selector: NodeTypeSelector> {
    items: &'a [Selector],
}

#[derive(Debug)]
pub struct GraphRef<NodeType> {
    rel_pos: usize,
    _node: PhantomData<*const NodeType>,
}

pub struct LiveGraphRef<'a, Selector: NodeTypeSelector, NodeType> {
    subgraph: Subgraph<'a, Selector>,
    graph_ref: GraphRef<NodeType>,
}

pub trait NodeType<Selector: NodeTypeSelector>
where
    for<'c> &'c Self: TryFrom<&'c Selector>,
{
    type LiveType<'a>
    where
        Selector: 'a;
    const NAME: &'static str;
    fn to_live_type<'a>(&self, subgraph: Subgraph<'a, Selector>) -> Self::LiveType<'a>;
}

impl<'a, Selector: NodeTypeSelector> Subgraph<'a, Selector> {
    pub fn node(&self) -> &Selector {
        self.items.last().unwrap()
    }
}

impl<Selector: NodeTypeSelector, T: NodeType<Selector>> Graph<Selector, T>
where
    for<'c> &'c T: TryFrom<&'c Selector, Error = Error>,
{
    pub fn new(items: Vec<Selector>) -> Result<Self> {
        (!items.is_empty())
            .then(|| Self {
                items,
                _top_level: PhantomData,
            })
            .ok_or(Error::EmptyExpression)
    }

    pub fn borrow<'a, 'b: 'a>(&'b self) -> Result<T::LiveType<'a>> {
        let selector: &Selector = self.items.last().unwrap();
        let node: &T = selector.try_into()?;
        Ok(node.to_live_type(self.into()))
    }
}

impl<'a, Selector: NodeTypeSelector, NodeType> LiveGraphRef<'a, Selector, NodeType> {
    pub fn new<'b: 'a>(graph_ref: GraphRef<NodeType>, subgraph: Subgraph<'b, Selector>) -> Self {
        Self {
            graph_ref,
            subgraph,
        }
    }

    pub fn get_subgraph<'b>(&self) -> Result<Subgraph<'b, Selector>>
    where
        'a: 'b,
    {
        let index = self
            .subgraph
            .items
            .len()
            .checked_sub(self.graph_ref.rel_pos)
            .ok_or(Error::InvalidReference {
                rel_pos: self.graph_ref.rel_pos,
                subgraph_size: self.subgraph.items.len(),
            })?;
        let items = &self.subgraph.items[0..index];
        Ok(Subgraph { items })
    }
}

impl<'a, Selector: NodeTypeSelector, T: NodeType<Selector>> LiveGraphRef<'a, Selector, T>
where
    for<'c> &'c T: TryFrom<&'c Selector, Error = Error>,
{
    pub fn borrow<'b>(&self) -> Result<T::LiveType<'b>>
    where
        'a: 'b,
    {
        let subgraph = self.get_subgraph()?;
        let selector: &Selector = subgraph.node();
        let node: &T = selector.try_into()?;
        Ok((*node).to_live_type(subgraph.clone()))
    }
}

impl<'a, Selector: NodeTypeSelector, LiveItem> Debug for LiveGraphRef<'a, Selector, LiveItem> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("LiveGraphRef")
            .field("rel_pos", &self.graph_ref.rel_pos)
            .finish()
    }
}

impl<NodeType> Clone for GraphRef<NodeType> {
    fn clone(&self) -> Self {
        Self {
            rel_pos: self.rel_pos,
            _node: PhantomData,
        }
    }
}

impl<NodeType> Copy for GraphRef<NodeType> {}

impl<'a, Selector: NodeTypeSelector> Clone for Subgraph<'a, Selector> {
    fn clone(&self) -> Self {
        Self {
            items: self.items.clone(),
        }
    }
}

impl<NodeType> From<usize> for GraphRef<NodeType> {
    fn from(rel_pos: usize) -> Self {
        Self {
            rel_pos,
            _node: PhantomData,
        }
    }
}

impl<'a, 'b, Selector: NodeTypeSelector, T: NodeType<Selector>> From<&'a Graph<Selector, T>>
    for Subgraph<'b, Selector>
where
    'a: 'b,
    for<'c> &'c T: TryFrom<&'c Selector, Error = Error>,
{
    fn from(g: &'a Graph<Selector, T>) -> Self {
        Self { items: &g.items }
    }
}
