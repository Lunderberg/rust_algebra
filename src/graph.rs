use std::fmt::Debug;
use std::marker::PhantomData;

use crate::{Error, Result};

#[allow(dead_code)]
pub struct Graph<NodeBase> {
    items: Vec<NodeBase>,
}

#[allow(dead_code)]
pub struct Subgraph<'a, NodeBase> {
    items: &'a [NodeBase],
}

#[allow(dead_code)]
#[derive(Debug)]
pub struct GraphRef<NodeType> {
    rel_pos: usize,
    _node: PhantomData<*const NodeType>,
}

#[allow(dead_code)]
pub struct LiveGraphRef<'a, NodeBase, NodeType> {
    subgraph: Subgraph<'a, NodeBase>,
    graph_ref: GraphRef<NodeType>,
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

impl<'a, Base> Clone for Subgraph<'a, Base> {
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

impl<'a, 'b, NodeBase> From<&'a Graph<NodeBase>> for Subgraph<'b, NodeBase>
where
    'a: 'b,
{
    fn from(g: &'a Graph<NodeBase>) -> Self {
        Self { items: &g.items }
    }
}

impl<'a, Base> Subgraph<'a, Base> {
    pub fn node(&self) -> &Base {
        self.items.last().unwrap()
    }
}

impl<NodeBase> Graph<NodeBase> {
    #[allow(dead_code)]
    pub fn new(items: Vec<NodeBase>) -> Result<Self> {
        (!items.is_empty())
            .then(|| Self { items })
            .ok_or(Error::EmptyExpression)
    }

    #[allow(dead_code)]
    pub fn root<'a, 'b>(&'a self) -> LiveGraphRef<'b, NodeBase, NodeBase>
    where
        'a: 'b,
    {
        LiveGraphRef {
            subgraph: self.into(),
            graph_ref: 0.into(),
        }
    }
}

impl<'a, NodeBase, LiveItem> Debug for LiveGraphRef<'a, NodeBase, LiveItem> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("LiveGraphRef")
            .field("rel_pos", &self.graph_ref.rel_pos)
            .finish()
    }
}

impl<'a, NodeBase, NodeType> LiveGraphRef<'a, NodeBase, NodeType> {
    pub fn new<'b>(graph_ref: GraphRef<NodeType>, subgraph: Subgraph<'b, NodeBase>) -> Self
    where
        'b: 'a,
    {
        Self {
            graph_ref,
            subgraph,
        }
    }

    pub fn get_subgraph<'b>(&self) -> Result<Subgraph<'b, NodeBase>>
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
