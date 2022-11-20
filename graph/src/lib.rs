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

mod graph_traits;
pub use graph_traits::*;

mod builder;
pub use builder::*;

mod error;
pub use error::*;
