use std::fmt::Debug;
use std::marker::PhantomData;

use crate::{Error, Result};

pub trait GraphNodeSelector: Sized {
    fn type_name(&self) -> &'static str;
}

#[derive(Debug)]
pub struct Graph<Selector> {
    items: Vec<Selector>,
}

pub struct Subgraph<'a, Selector> {
    items: &'a [Selector],
}

// Relative backreference to earlier node.  Used while traversing the
// constructed graph.
#[derive(Debug)]
pub struct GraphRef<NodeType> {
    rel_pos: usize,
    _node: PhantomData<*const NodeType>,
}

// Absolute reference to node.  Used while constructing the graph.
#[derive(Debug)]
pub struct GraphBuilderRef<NodeType> {
    pos: usize,
    _node: PhantomData<*const NodeType>,
}

pub struct LiveGraphRef<'a, Selector, NodeType> {
    subgraph: Subgraph<'a, Selector>,
    graph_ref: GraphRef<NodeType>,
}

pub trait LiveGraphNode<'a, Selector: 'a> {
    type StorageType: GraphNode<Selector, LiveType<'a> = Self>;
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

impl<Selector> Graph<Selector> {
    pub fn new() -> Self {
        Self { items: Vec::new() }
    }

    pub fn push_top<NodeType, Item: Into<Selector>>(
        &mut self,
        item: Item,
    ) -> GraphBuilderRef<NodeType> {
        self.items.push(item.into());
        (self.items.len() - 1).into()
    }

    pub fn backref<NodeType>(&self, abs_ref: GraphBuilderRef<NodeType>) -> GraphRef<NodeType> {
        let rel_pos = self.items.len() - abs_ref.pos;
        rel_pos.into()
    }
}

impl<Selector> From<Vec<Selector>> for Graph<Selector> {
    fn from(items: Vec<Selector>) -> Self {
        Self { items }
    }
}

impl<'a, Selector, NodeType> TryFrom<&'a Graph<Selector>> for LiveGraphRef<'a, Selector, NodeType>
where
    NodeType: GraphNode<Selector>,
    for<'b> &'b NodeType: TryFrom<&'b Selector, Error = Error>,
{
    type Error = Error;

    fn try_from(value: &'a Graph<Selector>) -> Result<Self> {
        let selector: &Selector = value.items.last().unwrap();
        let _node: &NodeType = selector.try_into()?;
        let subgraph: Subgraph<Selector> = value.into();
        Ok(Self {
            subgraph,
            graph_ref: 0.into(),
        })
    }
}

impl<Selector: GraphNodeSelector> Graph<Selector> {
    // Intentionally introduce OutLiveType as a deducible parameter,
    // rather than specifying the return type as
    // Result<Node::LiveType<'a>>.  Otherwise, the usage of the output
    // value cannot be used to deduce the return type.
    pub fn borrow<'a, 'b: 'a, OutLiveType: LiveGraphNode<'a, Selector>>(
        &'b self,
    ) -> Result<OutLiveType>
    where
        for<'c> &'c OutLiveType::StorageType: TryFrom<&'c Selector, Error = Error>,
    {
        let graph_ref: LiveGraphRef<'a, Selector, OutLiveType::StorageType> =
            LiveGraphRef::new(0.into(), self.into());
        graph_ref.borrow()
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

impl<'a, Selector, LiveItem> Debug for LiveGraphRef<'a, Selector, LiveItem> {
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

impl<NodeType> From<usize> for GraphBuilderRef<NodeType> {
    fn from(pos: usize) -> Self {
        Self {
            pos,
            _node: PhantomData,
        }
    }
}

impl<'a: 'b, 'b, Selector> From<&'a Graph<Selector>> for Subgraph<'b, Selector> {
    fn from(g: &'a Graph<Selector>) -> Self {
        Self { items: &g.items }
    }
}
