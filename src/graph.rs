use std::fmt::Debug;
use std::marker::PhantomData;

use crate::{Error, Result};

pub trait GraphNodeSelector: Sized {
    fn type_name(&self) -> &'static str;
}

pub struct Graph<Selector, Node> {
    items: Vec<Selector>,
    _top_level: PhantomData<Node>,
}

pub struct Subgraph<'a, Selector> {
    items: &'a [Selector],
}

#[derive(Debug)]
pub struct GraphRef<NodeType> {
    rel_pos: usize,
    _node: PhantomData<*const NodeType>,
}

pub struct LiveGraphRef<'a, Selector, NodeType> {
    subgraph: Subgraph<'a, Selector>,
    graph_ref: GraphRef<NodeType>,
}

pub trait GraphNode<Selector> {
    type LiveType<'a>
    where
        Selector: 'a;
    fn to_live_type<'a>(&self, subgraph: Subgraph<'a, Selector>) -> Self::LiveType<'a>;
}

impl<'a, Selector> Subgraph<'a, Selector> {
    pub fn node(&self) -> &Selector {
        self.items.last().unwrap()
    }
}

impl<Selector, Node> Graph<Selector, Node> {
    pub fn new(items: Vec<Selector>) -> Result<Self> {
        if items.is_empty() {
            Err(Error::EmptyExpression)
        } else {
            Ok(Self {
                items,
                _top_level: PhantomData,
            })
        }
    }
}

impl<Selector: GraphNodeSelector, Node: GraphNode<Selector>> Graph<Selector, Node>
where
    for<'c> &'c Node: TryFrom<&'c Selector, Error = Error>,
{
    pub fn borrow<'a, 'b: 'a>(&'b self) -> Result<Node::LiveType<'a>> {
        LiveGraphRef::new(0.into(), self.into()).borrow()
    }
}

impl<'a, Selector, NodeType> LiveGraphRef<'a, Selector, NodeType> {
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

impl<'a, Selector, Node: GraphNode<Selector>> LiveGraphRef<'a, Selector, Node>
where
    for<'c> &'c Node: TryFrom<&'c Selector, Error = Error>,
{
    pub fn borrow<'b>(&self) -> Result<Node::LiveType<'b>>
    where
        'a: 'b,
    {
        let subgraph = self.get_subgraph()?;
        let selector: &Selector = subgraph.node();
        let node: &Node = selector.try_into()?;
        Ok((*node).to_live_type(subgraph.clone()))
    }
}

impl<'a, Selector: GraphNodeSelector, LiveItem> Debug for LiveGraphRef<'a, Selector, LiveItem> {
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

impl<'a, Selector> Clone for Subgraph<'a, Selector> {
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

impl<'a: 'b, 'b, Selector, Node> From<&'a Graph<Selector, Node>> for Subgraph<'b, Selector> {
    fn from(g: &'a Graph<Selector, Node>) -> Self {
        Self { items: &g.items }
    }
}
