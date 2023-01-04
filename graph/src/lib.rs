//! Recursive data structures in contiguous memory.
//!
//! A utility to represent recursive data structures within a single
//! block of contiguous memory.  While these data structures can be
//! represented using `Box`, doing so requires additional memory
//! allocations to construct, and additional pointer dereferences to
//! traverse.
//!
//! The linearized structures and traits can be generated with macros
//! provided in `graph_derive`,

extern crate self as graph;

mod error;
pub use error::*;

mod recursive_types;
pub use recursive_types::*;

mod reference_types;
pub use reference_types::*;

mod typed_tree;
pub use typed_tree::*;

mod builder;
pub use builder::*;
