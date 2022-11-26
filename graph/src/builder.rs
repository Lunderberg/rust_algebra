use std::marker::PhantomData;

use crate::{GenericGraphNode, Graph, GraphRef, Storage};

/// Absolute reference to node.  Used while constructing the graph.
pub struct GraphBuilderRef<NodeType> {
    /// The location of the referred-to node, relative to start of the
    /// subgraph.
    pub(crate) pos: usize,
    _node: PhantomData<*const NodeType>,
}

impl<'a, BaseType: GenericGraphNode<'a, Storage<'a>>> Graph<'a, BaseType> {
    // TODO: See if I can pass in a Enum<GraphBuilderRef> instead of
    // needing all the extra mechanisms for the builder traits
    pub fn push_top<NodeType, Item: Into<BaseType::DefaultSelector>>(
        &mut self,
        item: Item,
    ) -> GraphBuilderRef<NodeType> {
        self.items.push(item.into());
        GraphBuilderRef {
            pos: self.items.len() - 1,
            _node: PhantomData,
        }
    }

    pub fn backref<NodeType>(&self, abs_ref: GraphBuilderRef<NodeType>) -> GraphRef<NodeType> {
        let rel_pos = self.items.len() - abs_ref.pos;
        GraphRef {
            rel_pos,
            _node: PhantomData,
        }
    }
}
