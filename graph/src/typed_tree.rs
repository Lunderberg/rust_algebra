use std::marker::PhantomData;

use crate::{RecursiveFamily, RecursiveObj, Storage, Visiting, VisitingRef};

/// A container for an entire tree structure.  The container must
/// be able to represent any individual node type that may occur
/// within the tree.  These should be expected by implementing
/// `ContainerOf<Node>` for each type that may be contained.
pub struct TypedTree<
    'a,
    RootNodeType: RecursiveObj<'a, RefType = Storage>,
    Container = <<RootNodeType as RecursiveObj<'a>>::Family as RecursiveFamily>::DefaultContainer<
        'a,
    >,
> {
    pub(crate) nodes: Vec<Container>,
    pub(crate) _phantom: PhantomData<*const RootNodeType>,
    pub(crate) _a: PhantomData<&'a usize>,
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
pub trait ContainerOf<T> {
    fn to_container(val: T) -> Self;
    fn from_container(&self) -> Result<&T, graph::Error>;
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
        Container: ContainerOf<T>,
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

impl<
        'a,
        RootNodeType: RecursiveObj<'a, RefType = Storage>,
        Container: ContainerOf<RootNodeType>,
    > TypedTree<'a, RootNodeType, Container>
{
    /// Borrow the top-level node, starting a recursive visit of the graph.
    ///
    /// Intentionally introduce OutLiveType as a deducible parameter,
    /// rather than specifying the return type as
    /// `Result<Node::LiveType<'a>>`.  This way, the usage of the
    /// output value can be used to deduce the return type.
    pub fn borrow<'b: 'a>(
        &'b self,
    ) -> Result<
        <RootNodeType::Family as RecursiveFamily>::Obj<'a, Visiting<'a, Container>>,
        graph::Error,
    > {
        let container: &Container = self.nodes.last().unwrap();
        let node: &RootNodeType = container.from_container()?;
        let live_ref = RootNodeType::Family::storage_to_visiting(node, &self.nodes);
        Ok(live_ref)
    }
}

impl<'a, T: RecursiveObj<'a, RefType = Storage>, Container: ContainerOf<T>>
    VisitingRef<'a, T, Container>
{
    /// Recurse down a level of the graph
    ///
    /// When visiting a recursive graph, recursive references are
    /// represented as `LiveGraphRef` instances.  Borrowing the
    /// reference constructs the live type for the referenced type.
    pub fn borrow(
        &self,
    ) -> Result<<T::Family as RecursiveFamily>::Obj<'a, Visiting<'a, Container>>, graph::Error>
    {
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
        let node: &T = container.from_container()?;
        let subview = &self.view[..=index];
        let live_ref = T::Family::storage_to_visiting(node, subview);
        Ok(live_ref)
    }
}
