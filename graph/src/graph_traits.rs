use std::fmt::Debug;
use std::marker::PhantomData;

use crate::Error;

/// An element of a graph, whose types may recursively include each
/// other.
pub trait GraphNode {
    /// A type which can be used to identify any of the recursively
    /// defined types that may appear within a tree of this top-level
    /// type.
    ///
    /// # Examples
    ///
    /// Consider the following recursively defined types.
    ///
    /// ```rust
    /// #[recursive_graph]
    /// mod example {
    ///   enum BoolExpr {
    ///       Bool(bool),
    ///       And(BoolExpr, BoolExpr),
    ///       IntEqual(IntExpr, IntExpr),
    ///   }
    ///
    ///   enum IntExpr {
    ///       Int(i64),
    ///       Add(IntExpr, IntExpr),
    ///   }
    /// }
    /// ```
    ///
    /// Both `BoolExpr` and `IntExpr` may recursively contain
    /// themselves.  However, a `BoolExpr` may also internally contain
    /// `IntExpr`.  To store both in a contiguous data structure, an
    /// element of that structure must be able to store either type.
    /// In this example, the following enum could be used as the
    /// `DefaultSelector`.
    ///
    /// ```rust
    /// enum ExampleSelector {
    ///     BoolExpr(BoolExpr),
    ///     IntExpr(IntExpr),
    /// }
    /// ```
    type DefaultSelector;

    /// The corresponding node type when visiting the graph.
    type LiveType<'a, BaseType: GraphNode + 'a>;

    /// Given a subgraph, convert from a storage type to an live type.
    /// That is, this method should convert all instances of
    /// `GraphRef<T>` into `LiveGraphRef<'a, BaseType, T>`.
    fn to_live_type<'a, BaseType: GraphNode + 'a>(
        &self,
        subgraph: Subgraph<'a, BaseType>,
    ) -> Self::LiveType<'a, BaseType>
    where
        for<'c> &'c Self: TryFrom<&'c BaseType::DefaultSelector>;
}

#[derive(Debug)]
pub struct Graph<BaseType: GraphNode> {
    items: Vec<BaseType::DefaultSelector>,
}

pub struct Subgraph<'a, BaseType: GraphNode> {
    items: &'a [BaseType::DefaultSelector],
}

// Relative backreference to earlier node.  Used while traversing the
// constructed graph.
pub struct GraphRef<NodeType: ?Sized> {
    rel_pos: usize,
    _node: PhantomData<*const NodeType>,
}

// Absolute reference to node.  Used while constructing the graph.
pub struct GraphBuilderRef<NodeType> {
    pos: usize,
    _node: PhantomData<*const NodeType>,
}

pub struct LiveGraphRef<'a, BaseType: GraphNode, NodeType: ?Sized> {
    subgraph: Subgraph<'a, BaseType>,
    graph_ref: GraphRef<NodeType>,
}

pub trait Reference {
    type TypedRef<Ptr: std::ops::Deref>;
}

#[derive(Debug)]
pub struct StorageReference;

impl Reference for StorageReference {
    type TypedRef<Ptr: std::ops::Deref> = GraphRef<Ptr::Target>;
}

#[derive(Debug)]
pub struct LiveReference<'a, BaseType: GraphNode> {
    _node: PhantomData<&'a BaseType>,
}

impl<'a, BaseType: GraphNode> Reference for LiveReference<'a, BaseType> {
    type TypedRef<Ptr: std::ops::Deref> = LiveGraphRef<'a, BaseType, Ptr::Target>;
}

pub trait LiveGraphNode<'a, BaseType: GraphNode + 'a> {
    type StorageType: GraphNode<LiveType<'a, BaseType> = Self>;
}

impl<BaseType: GraphNode> Graph<BaseType> {
    pub fn new() -> Self {
        Self { items: Vec::new() }
    }

    pub fn push_top<NodeType, Item: Into<BaseType::DefaultSelector>>(
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

impl<BaseType: GraphNode> From<Vec<BaseType::DefaultSelector>> for Graph<BaseType> {
    fn from(items: Vec<BaseType::DefaultSelector>) -> Self {
        Self { items }
    }
}

impl<'a, BaseType: GraphNode, NodeType: GraphNode> TryFrom<&'a Graph<BaseType>>
    for LiveGraphRef<'a, BaseType, NodeType>
where
    for<'b> &'b NodeType: TryFrom<&'b BaseType::DefaultSelector, Error = Error>,
{
    type Error = Error;

    fn try_from(value: &'a Graph<BaseType>) -> Result<Self, Self::Error> {
        let selector: &BaseType::DefaultSelector = value.items.last().unwrap();
        let _node: &NodeType = selector.try_into()?;
        let subgraph: Subgraph<BaseType> = value.into();
        Ok(Self {
            subgraph,
            graph_ref: 0.into(),
        })
    }
}

impl<BaseType: GraphNode> Graph<BaseType> {
    // Intentionally introduce OutLiveType as a deducible parameter,
    // rather than specifying the return type as
    // Result<Node::LiveType<'a>>.  Otherwise, the usage of the output
    // value cannot be used to deduce the return type.
    pub fn borrow<'a, 'b: 'a, OutLiveType: LiveGraphNode<'a, BaseType>>(
        &'b self,
    ) -> Result<OutLiveType, Error>
    where
        for<'c> &'c OutLiveType::StorageType: TryFrom<&'c BaseType::DefaultSelector, Error = Error>,
    {
        let graph_ref: LiveGraphRef<'a, BaseType, OutLiveType::StorageType> =
            LiveGraphRef::new(0.into(), self.into());
        graph_ref.borrow()
    }
}

impl<'a, BaseType: GraphNode, NodeType: GraphNode> LiveGraphRef<'a, BaseType, NodeType> {
    pub fn new<'b: 'a>(graph_ref: GraphRef<NodeType>, subgraph: Subgraph<'b, BaseType>) -> Self {
        Self {
            graph_ref,
            subgraph,
        }
    }

    pub fn get_subgraph<'b>(&self) -> Result<Subgraph<'b, BaseType>, Error>
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

impl<'a, BaseType: GraphNode, Node: GraphNode> LiveGraphRef<'a, BaseType, Node>
where
    for<'c> &'c Node: TryFrom<&'c BaseType::DefaultSelector, Error = Error>,
{
    pub fn borrow(&self) -> Result<Node::LiveType<'a, BaseType>, Error> {
        let subgraph = self.get_subgraph()?;
        let selector: &BaseType::DefaultSelector =
            subgraph.items.last().ok_or(Error::EmptyExpression)?;
        let node: &Node = selector.try_into()?;
        Ok((*node).to_live_type(subgraph.clone()))
    }
}

impl<'a, BaseType: GraphNode, NodeType: GraphNode> Debug for LiveGraphRef<'a, BaseType, NodeType> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("LiveGraphRef")
            .field("rel_pos", &self.graph_ref.rel_pos)
            .finish()
    }
}

impl<NodeType> Debug for GraphRef<NodeType> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct(&format!(
            "GraphRef<{}>",
            std::any::type_name::<NodeType>()
                .rsplit("::")
                .next()
                .unwrap()
        ))
        .field("rel_pos", &self.rel_pos)
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

impl<'a, BaseType: GraphNode> Clone for Subgraph<'a, BaseType> {
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

impl<'a: 'b, 'b, BaseType: GraphNode> From<&'a Graph<BaseType>> for Subgraph<'b, BaseType> {
    fn from(g: &'a Graph<BaseType>) -> Self {
        Self { items: &g.items }
    }
}
