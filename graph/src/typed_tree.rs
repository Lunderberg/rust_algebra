use std::marker::PhantomData;

use crate::{RecursiveFamily, RecursiveObj, Storage, Visiting, VisitingRef};

/// A container for an entire tree structure.  The container must
/// be able to represent any individual node type that may occur
/// within the tree.  These should be expected by implementing
/// `ContainerOf<Node>` for each type that may be contained.
pub struct TypedTree<
    RootNodeType: HasDefaultContainer,
    Container = <RootNodeType as HasDefaultContainer>::DefaultContainer,
> {
    pub(crate) nodes: Vec<Container>,
    pub(crate) _phantom: PhantomData<*const RootNodeType>,
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

/// Utility trait for providing a default container.
///
/// Most of the time, a tree should be contained in the enum that
/// was generated to contain it, or any recursively referenced
/// type contained by it.  However, in some cases it may be
/// desirable to use a broader container type.  For example,
/// consider the following structure.
///
/// ```
/// #[recursive_graph]
/// mod my_graph {
///     enum BoolExpr {
///         And(BoolExpr, BoolExpr),
///         Equal(IntExpr, IntExpr),
///     }
///
///     enum IntExpr {
///         Int(i64),
///         Add(IntExpr, IntExpr),
///     }
/// }
/// ```
///
/// An `IntExpr` can only refer to itself, while a `BoolExpr` may
/// refer either to itself or to an `IntExpr`.  The auto-generated
/// type `my_graph::container::IntExpr` and
/// `my_graph::container::BoolExpr` may be used for their
/// respective types.  However, an `IntExpr` stored inside a
/// `Vec<container::IntExpr>` would need to be converted into a
/// `Vec<container::BoolExpr>` prior to use in a `BoolExpr`.  The
/// user may want to use the `IntExpr` inside a
/// `Vec<container::BoolExpr>` from the start, such that no
/// conversion is required.
///
/// Since this isn't the typical case, this trait is implemented
/// for all recursive types, allowing a default container to be
/// chosen for each recursive type.
pub trait HasDefaultContainer: Sized {
    type DefaultContainer: ContainerOf<Self>;
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

impl<RootNodeType: HasDefaultContainer, Container> TypedTree<RootNodeType, Container> {
    /// Borrow the top-level node, starting a recursive visit of the graph.
    pub fn root(&self) -> VisitingRef<RootNodeType, Container> {
        VisitingRef {
            rel_pos: 0,
            view: &self.nodes,
            _phantom: PhantomData,
        }
    }
}

impl<'a, NodeType, Container> VisitingRef<'a, NodeType, Container> {
    /// Recurse down a level of the graph
    ///
    /// When visiting a recursive graph, recursive references are
    /// represented as `VisitingRef` instances.  Borrowing the
    /// reference constructs an instance of the pointed-to enum, with
    /// further recursion represented by `VisitingRef`.
    pub fn borrow(
        &self,
    ) -> Result<<NodeType::Family as RecursiveFamily>::Obj<'a, Visiting<'a, Container>>, graph::Error>
    where
        NodeType: RecursiveObj<'a, RefType = Storage>,
        Container: ContainerOf<NodeType>,
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
        let node: &NodeType = container.from_container()?;
        let subview = &self.view[..=index];
        let live_ref = NodeType::Family::storage_to_visiting(node, subview);
        Ok(live_ref)
    }
}
