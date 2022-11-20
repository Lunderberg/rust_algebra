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
    ///
    /// # Arguments
    ///
    /// * `BaseType` - The expression type in which this graph node
    /// occurs.
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

/// Owning container for a graph capable of containing `BaseType` or
/// any type that `BaseType` refers to.
#[derive(Debug)]
pub struct Graph<BaseType: GraphNode> {
    pub(crate) items: Vec<BaseType::DefaultSelector>,
}

/// Non-owning view into a graph capable of containing `BaseNode`
pub struct Subgraph<'a, BaseType: GraphNode> {
    items: &'a [BaseType::DefaultSelector],
}

/// Relative backreference to earlier node.  Used to represent
/// references into recursively-defined structures in the linearized
/// data storage.
pub struct GraphRef<NodeType: ?Sized> {
    /// The location of the referred-to node, relative to the node
    /// holding the reference, in the direction of the start of the
    /// `Graph`.
    ///
    /// `rel_pos` is unsigned, in order to avoid needing
    /// loop-detection when walking through a graph.  In the
    /// linearized structure, the index of a referent is strictly less
    /// than the index of the node holding the reference, making
    /// reference loops unrepresentable.
    rel_pos: usize,
    _node: PhantomData<*const NodeType>,
}

/// Relative backreference to earlier node.  Used to represent
/// references into recursively-defined structures while traversing
/// the graph.
pub struct LiveGraphRef<'a, BaseType: GraphNode, NodeType: ?Sized> {
    /// The subgraph in which the reference points.  The reference is
    /// relative to the last item in the subgraph.
    subgraph: Subgraph<'a, BaseType>,

    /// The reference to be followed, relative to the last element in
    /// `subgraph`.
    graph_ref: GraphRef<NodeType>,
}

/// Abstract across a type of references
///
/// Allows the same generic enum definition to be used both as a value
/// type using `GraphRef<T>` instances for recursive references and as
/// a lifetimed type using `LiveGraphRef<'a, BaseType, T>` for
/// recursive references.
pub trait Reference {
    /// The representation to use for recursive references
    ///
    /// # Arguments
    ///
    /// `Ptr` - The reference to represent
    ///
    /// # Returns
    ///
    /// The representation of `Ptr::Target` for this reference type.
    type TypedRef<Ptr: std::ops::Deref>;
}

/// Reference suitable for storing
///
/// A reference category that uses `GraphRef<NodeType>` to represent
/// recursive references.
#[derive(Debug)]
pub struct StorageReference;

impl Reference for StorageReference {
    type TypedRef<Ptr: std::ops::Deref> = GraphRef<Ptr::Target>;
}

/// Reference suitable for visiting subgraphs
///
/// A reference category that uses `LiveGraphRef<'a, BaseType,
/// NodeType>` to represent recursive references.
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
    /// Borrow the top-level node, starting a recursive visit of the graph.
    ///
    /// Intentionally introduce OutLiveType as a deducible parameter,
    /// rather than specifying the return type as
    /// `Result<Node::LiveType<'a>>`.  This way, the usage of the
    /// output value can be used to deduce the return type.
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
    /// Recurse down a level of the graph
    ///
    /// When visiting a recursive graph, recursive references are
    /// represented as `LiveGraphRef` instances.  Borrowing the
    /// reference constructs the live type for the referenced type.
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

impl<'a: 'b, 'b, BaseType: GraphNode> From<&'a Graph<BaseType>> for Subgraph<'b, BaseType> {
    fn from(g: &'a Graph<BaseType>) -> Self {
        Self { items: &g.items }
    }
}
