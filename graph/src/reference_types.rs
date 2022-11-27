use std::marker::PhantomData;

use crate::{GenericGraphNode, GraphRef, LiveGraphRef, Subgraph};

/// Abstract across a type of references
///
/// Allows the same generic enum definition to be used both as a value
/// type using `GraphRef<T>` instances for recursive references and as
/// a lifetimed type using `LiveGraphRef<'a, BaseType, T>` for
/// recursive references.
pub trait NodeUsage<'a>: 'a {
    /// The representation to use for recursive references
    ///
    /// # Arguments
    ///
    /// `Ptr` - The reference to represent
    ///
    /// # Returns
    ///
    /// The representation of a recursive reference to `NodeType` for
    /// this reference type.
    type RefType<NodeType: ?Sized>;

    /// The representation to use for all other types
    ///
    /// # Arguments
    ///
    /// `T` - The value type to represent
    ///
    /// # Returns
    ///
    /// The representation of a value type `T` for this usage.
    /// (e.g. `T` for storage, `&T` for visiting)
    type ValueType<T: 'a>;
}

pub trait NodeUsageConverter<'a, T: NodeUsage<'a>, U: NodeUsage<'a>> {
    fn convert_reference<NodeType>(&self, t_ref: &'a T::RefType<NodeType>) -> U::RefType<NodeType>;

    fn convert_value<ValueType: 'a>(
        &self,
        t_value: &'a T::ValueType<ValueType>,
    ) -> U::ValueType<ValueType>;
}

/// Reference suitable for storing
///
/// A reference category that uses `GraphRef<NodeType>` to represent
/// recursive references.
#[derive(Debug)]
pub struct Storage<'a> {
    _node: PhantomData<&'a usize>,
}

impl<'a> NodeUsage<'a> for Storage<'a> {
    type RefType<NodeType: ?Sized> = GraphRef<NodeType>;
    type ValueType<T: 'a> = T;
}

/// Reference suitable for visiting subgraphs
///
/// A reference category that uses `LiveGraphRef<'a, BaseType,
/// NodeType>` to represent recursive references.
#[derive(Debug)]
pub struct Live<'a, BaseType: GenericGraphNode<'a>> {
    _node: PhantomData<&'a BaseType>,
}

impl<'a, BaseType: GenericGraphNode<'a>> NodeUsage<'a> for Live<'a, BaseType> {
    type RefType<NodeType: ?Sized> = LiveGraphRef<'a, BaseType, NodeType>;
    type ValueType<T: 'a> = &'a T;
}

pub struct StorageToLive<'a, BaseType: GenericGraphNode<'a>> {
    subgraph: Subgraph<'a, BaseType>,
}

impl<'a, BaseType: GenericGraphNode<'a>> StorageToLive<'a, BaseType> {
    pub(crate) fn new(subgraph: Subgraph<'a, BaseType>) -> Self {
        Self { subgraph }
    }
}

impl<'a, BaseType: GenericGraphNode<'a>> NodeUsageConverter<'a, Storage<'a>, Live<'a, BaseType>>
    for StorageToLive<'a, BaseType>
{
    fn convert_reference<NodeType>(
        &self,
        graph_ref: &GraphRef<NodeType>,
    ) -> LiveGraphRef<'a, BaseType, NodeType> {
        LiveGraphRef {
            subgraph: self.subgraph.clone(),
            graph_ref: *graph_ref,
        }
    }

    fn convert_value<ValueType: 'a>(&self, t_value: &'a ValueType) -> &'a ValueType {
        t_value
    }
}
