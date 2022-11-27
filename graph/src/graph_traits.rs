use std::fmt::Debug;
use std::marker::PhantomData;

use crate::{Error, Live, NodeUsage, NodeUsageConverter, Storage, StorageToLive};

pub trait GenericGraphNode<'a, Ref: NodeUsage<'a> = Storage<'a>>: 'a {
    type DefaultSelector;

    type WithRef<NewRef: NodeUsage<'a>>: GenericGraphNode<'a, NewRef, WithRef<Ref> = Self>;

    fn convert_references<NewRef: NodeUsage<'a>, Converter: NodeUsageConverter<'a, Ref, NewRef>>(
        &'a self,
        converter: Converter,
    ) -> Self::WithRef<NewRef>;
}

pub trait ContainerOf<'a, NodeType> {
    fn to_container(node: NodeType) -> Self;
    fn from_container(&'a self) -> Result<&'a NodeType, Error>;
}

/// Owning container for a graph capable of containing `BaseType` or
/// any type that `BaseType` refers to.
#[derive(Debug)]
pub struct Graph<'a, BaseType: GenericGraphNode<'a>> {
    pub(crate) items: Vec<BaseType::DefaultSelector>,
}

/// Non-owning view into a graph capable of containing `BaseNode`
pub struct Subgraph<'a, BaseType: GenericGraphNode<'a>> {
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
    pub(crate) rel_pos: usize,
    pub(crate) _node: PhantomData<*const NodeType>,
}

/// Relative backreference to earlier node.  Used to represent
/// references into recursively-defined structures while traversing
/// the graph.
pub struct LiveGraphRef<'a, BaseType: GenericGraphNode<'a>, NodeType: ?Sized> {
    /// The subgraph in which the reference points.  The reference is
    /// relative to the last item in the subgraph.
    pub(crate) subgraph: Subgraph<'a, BaseType>,

    /// The reference to be followed, relative to the last element in
    /// `subgraph`.
    pub(crate) graph_ref: GraphRef<NodeType>,
}

impl<'a, BaseType: GenericGraphNode<'a>> Graph<'a, BaseType> {
    pub fn new() -> Self {
        Self { items: Vec::new() }
    }
}

impl<'a, BaseType: GenericGraphNode<'a>> From<Vec<BaseType::DefaultSelector>>
    for Graph<'a, BaseType>
{
    fn from(items: Vec<BaseType::DefaultSelector>) -> Self {
        Self { items }
    }
}

impl<'a, BaseType: GenericGraphNode<'a>> Graph<'a, BaseType> {
    /// Borrow the top-level node, starting a recursive visit of the graph.
    ///
    /// Intentionally introduce OutLiveType as a deducible parameter,
    /// rather than specifying the return type as
    /// `Result<Node::LiveType<'a>>`.  This way, the usage of the
    /// output value can be used to deduce the return type.
    pub fn borrow_root<OutLiveType>(&'a self) -> Result<OutLiveType, Error>
    where
        OutLiveType: GenericGraphNode<'a, Live<'a, BaseType>>,

        OutLiveType::WithRef<Storage<'a>>:
            GenericGraphNode<'a, WithRef<Live<'a, BaseType>> = OutLiveType>,

        BaseType::DefaultSelector: ContainerOf<'a, OutLiveType::WithRef<Storage<'a>>>,
    {
        let subgraph: Subgraph<'a, BaseType> = self.into();
        let graph_ref: GraphRef<OutLiveType::WithRef<Storage<'a>>> = GraphRef {
            rel_pos: 0,
            _node: PhantomData,
        };
        let live_graph_ref: LiveGraphRef<BaseType, OutLiveType::WithRef<Storage<'a>>> =
            LiveGraphRef {
                graph_ref,
                subgraph,
            };
        let res_borrowed: Result<OutLiveType, Error> = live_graph_ref.borrow();
        let borrowed: OutLiveType = res_borrowed?;
        Ok(borrowed)
    }
}

impl<'a, BaseType: GenericGraphNode<'a>, NodeType: GenericGraphNode<'a>>
    LiveGraphRef<'a, BaseType, NodeType>
{
    pub fn new(graph_ref: GraphRef<NodeType>, subgraph: Subgraph<'a, BaseType>) -> Self {
        Self {
            graph_ref,
            subgraph,
        }
    }
}

impl<'a, BaseType: GenericGraphNode<'a>, NodeType: GenericGraphNode<'a>>
    LiveGraphRef<'a, BaseType, NodeType>
{
    /// Recurse down a level of the graph
    ///
    /// When visiting a recursive graph, recursive references are
    /// represented as `LiveGraphRef` instances.  Borrowing the
    /// reference constructs the live type for the referenced type.
    pub fn borrow(&self) -> Result<NodeType::WithRef<Live<'a, BaseType>>, Error>
    where
        BaseType::DefaultSelector: ContainerOf<'a, NodeType>,
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
        let selector: &BaseType::DefaultSelector = items.last().ok_or(Error::EmptyExpression)?;
        let node: &NodeType = selector.from_container()?;
        let subgraph = Subgraph { items };
        let converter = StorageToLive::new(subgraph);
        let live_node = node.convert_references(converter);
        Ok(live_node)
    }
}

impl<'a, BaseType: GenericGraphNode<'a>, NodeType: GenericGraphNode<'a>> Debug
    for LiveGraphRef<'a, BaseType, NodeType>
{
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

impl<'a, BaseType: GenericGraphNode<'a>> Clone for Subgraph<'a, BaseType> {
    fn clone(&self) -> Self {
        Self {
            items: self.items.clone(),
        }
    }
}

impl<
        'a: 'b,
        'b,
        InBaseType: GenericGraphNode<'a>,
        OutBaseType: GenericGraphNode<'b, DefaultSelector = InBaseType::DefaultSelector>,
    > From<&'a Graph<'a, InBaseType>> for Subgraph<'b, OutBaseType>
{
    fn from(g: &'a Graph<'a, InBaseType>) -> Self {
        Self { items: &g.items }
    }
}
