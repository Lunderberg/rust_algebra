use std::marker::PhantomData;

use crate::{
    Builder, BuilderRef, ContainedBy, ContainerOf, RecursiveFamily, RecursiveObj, Storage,
    TypedTree,
};

/// A constructor used to generate a `TypedTree<Container>`.
pub struct BuilderObj<Container> {
    pub(crate) nodes: Vec<Container>,
}

impl Builder {
    /// Constructs an empty `Builder`.
    pub fn new<Container>() -> BuilderObj<Container> {
        BuilderObj { nodes: Vec::new() }
    }
}

impl<Container> BuilderObj<Container> {
    /// Insert a new node to the builder
    pub fn push<'a, T: RecursiveObj<'a, RefType = Builder>>(
        &mut self,
        builder_obj: T,
    ) -> BuilderRef<<T::Family as RecursiveFamily>::Obj<'a, Storage>>
    where
        Container: ContainerOf<<T::Family as RecursiveFamily>::Obj<'a, Storage>>,
    {
        let abs_pos = self.nodes.len();
        let storage_obj = <T::Family as RecursiveFamily>::builder_to_storage(builder_obj, abs_pos);
        let container: Container = storage_obj.to_container();
        self.nodes.push(container);
        BuilderRef {
            abs_pos,
            _node: PhantomData,
        }
    }
}

impl<'a, RootNodeType: RecursiveObj<'a, RefType = Storage>, Container> From<BuilderObj<Container>>
    for TypedTree<RootNodeType, Container>
{
    fn from(builder: BuilderObj<Container>) -> Self {
        Self {
            nodes: builder.nodes,
            _phantom: PhantomData,
        }
    }
}
