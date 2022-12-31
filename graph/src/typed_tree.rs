use std::marker::PhantomData;

use crate::{RecursiveFamily, RecursiveObj, RecursiveRefType, StorageToVisiting};

/// A container for an entire tree structure.  The container must
/// be able to represent any individual node type that may occur
/// within the tree.  These should be expected by implementing
/// `ContainerOf<Node>` for each type that may be contained.
pub struct TypedTree<
    'a,
    RootNodeType: RecursiveObj<'a>,
    Container = <<RootNodeType as RecursiveObj<'a>>::Family as RecursiveFamily>::DefaultContainer<
        'a,
    >,
> {
    pub(crate) nodes: Vec<Container>,
    pub(crate) _phantom: PhantomData<*const RootNodeType>,
    pub(crate) _a: PhantomData<&'a usize>,
}

/// A usage annotation for objects that may be stored in the
/// linearized structure.
pub struct Storage;

impl<'a> RecursiveRefType<'a> for Storage {
    type Ref<T: ?Sized> = StorageRef<T>;
    type Value<T: 'a> = T;
}

/// A reference in the linearized structure.
pub struct StorageRef<T: ?Sized> {
    /// The location of the referred-to node, relative to the node
    /// holding the reference, in the direction of the start of the
    /// `TypedTree`.
    ///
    /// `rel_pos` is unsigned, in order to avoid needing
    /// loop-detection when walking through a graph.  In the
    /// linearized structure, the index of a referent is strictly less
    /// than the index of the node holding the reference, making
    /// reference loops unrepresentable.
    pub(crate) rel_pos: usize,
    pub(crate) _node: PhantomData<*const T>,
}

/// A usage annotation for objects that contain a view into the full
/// tree structure that they represent.
pub struct Visiting<'a, Container: 'a> {
    _a: PhantomData<&'a usize>,
    _c: PhantomData<*const Container>,
}

/// Relative backreference to earlier node.  Used to represent
/// references into recursively-defined structures while traversing
/// the graph.
pub struct VisitingRef<'a, T: ?Sized, Container: 'a> {
    /// The reference to be followed, relative to the last element in
    /// `view`.
    pub(crate) rel_pos: usize,

    /// The subgraph in which the reference points.  The reference is
    /// relative to the last item in the view.
    pub(crate) view: &'a [Container],
    pub(crate) _phantom: PhantomData<*const T>,
}

impl<'a, Container> RecursiveRefType<'a> for Visiting<'a, Container> {
    type Ref<T: ?Sized> = VisitingRef<'a, T, Container>;
    type Value<T: 'a> = &'a T;
}

/// Exposes a type `T` as being potentially stored in a container
/// `Container`.
///
/// This is effectively the same as `Container: From<T> +
/// TryInto<T>`, and may be replaced in the future, if that works
/// with type inference and doesn't require extra bounds to be
/// specified on the user side.
///
/// - `trait_alias`: https://github.com/rust-lang/rust/issues/41517
/// - `implied_bounds`: https://github.com/rust-lang/rust/issues/44491
/// - `provide_any`: https://github.com/rust-lang/rust/issues/96024
pub trait ContainerOf<'a, R: RecursiveObj<'a>>: 'a {
    fn to_container(val: <R::Family as RecursiveFamily>::Obj<'a, Storage>) -> Self;
    fn from_container(
        &'a self,
    ) -> Result<&'a <R::Family as RecursiveFamily>::Obj<'a, Storage>, graph::Error>;
}

/// Inverse of `ContainerOf`, marks a node type as being stored
/// inside a specific `Container`.  Automatically implemented in
/// terms of `ContainerOf`.
pub trait ContainedBy<'a, Container>: 'a {
    fn to_container(self) -> Container;
    fn from_container(container: &'a Container) -> Result<&'a Self, graph::Error>;
}

impl<
        'a,
        F: RecursiveFamily,
        T: RecursiveObj<'a, Family = F, RefType = Storage>,
        Container: ContainerOf<'a, T>,
    > ContainedBy<'a, Container> for T
where
    F: RecursiveFamily<Obj<'a, Storage> = T>,
{
    fn to_container(self) -> Container {
        Container::to_container(self)
    }
    fn from_container(container: &'a Container) -> Result<&'a Self, graph::Error> {
        container.from_container()
    }
}

impl<'a, F: RecursiveFamily, RootNodeType: RecursiveObj<'a, Family = F>, Container>
    TypedTree<'a, RootNodeType, Container>
where
    Container: ContainerOf<'a, RootNodeType>,
{
    /// Borrow the top-level node, starting a recursive visit of the graph.
    ///
    /// Intentionally introduce OutLiveType as a deducible parameter,
    /// rather than specifying the return type as
    /// `Result<Node::LiveType<'a>>`.  This way, the usage of the
    /// output value can be used to deduce the return type.
    pub fn borrow(&'a self) -> Result<F::Obj<'a, Visiting<'a, Container>>, graph::Error> {
        let container: &Container = self.nodes.last().unwrap();
        let node: &F::Obj<'a, Storage> = container.from_container()?;
        let converter = StorageToVisiting { view: &self.nodes };
        let live_ref = F::view_ref(node, &converter);
        Ok(live_ref)
    }
}

impl<
        'a,
        Family: RecursiveFamily,
        T: RecursiveObj<'a, Family = Family>,
        Container: ContainerOf<'a, T>,
    > VisitingRef<'a, T, Container>
{
    /// Recurse down a level of the graph
    ///
    /// When visiting a recursive graph, recursive references are
    /// represented as `LiveGraphRef` instances.  Borrowing the
    /// reference constructs the live type for the referenced type.
    pub fn borrow(&self) -> Result<Family::Obj<'a, Visiting<'a, Container>>, graph::Error> {
        let self_index = self
            .view
            .len()
            .checked_sub(1)
            .ok_or(graph::Error::EmptyExpression)?;

        let index = self_index
            .checked_sub(self.rel_pos)
            .ok_or(graph::Error::InvalidReference {
                rel_pos: self.rel_pos,
                subgraph_size: self.view.len(),
            })?;

        let container: &Container = &self.view[index];
        let node: &Family::Obj<'a, Storage> = container.from_container()?;
        let converter = StorageToVisiting {
            view: &self.view[..=index],
        };
        let live_ref = Family::view_ref(node, &converter);
        Ok(live_ref)
    }
}
