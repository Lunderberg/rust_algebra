use std::fmt::Debug;

/// A fallible reference-to-reference conversion.
///
/// For example, a reference to an enum may be convertible into one of
/// its variants.
///
/// ```
///   # use typed_dag::TryAsRef;
///   enum Container {
///       Int(i64),
///       Float(f64),
///   }
///
///   impl TryAsRef<i64> for Container {
///       type Error = &'static str;
///       fn try_as_ref(&self) -> Result<&i64, Self::Error> {
///           match self {
///               Container::Int(value) => Ok(value),
///               Container::Float(_) => Err("Unexpected f64"),
///           }
///       }
///   }
///
///   let c = Container::Int(10);
///   assert_eq!(<Container as TryAsRef<i64>>::try_as_ref(&c).unwrap(), &10);
///
///   let c = Container::Float(15.0);
///   assert!(<Container as TryAsRef<i64>>::try_as_ref(&c).is_err());
/// ```
pub trait TryAsRef<Target> {
    type Error: Debug;
    fn try_as_ref(&self) -> Result<&Target, Self::Error>;
}

/// Blanket implementation for TryAsRef into itself.
impl<Target> TryAsRef<Target> for Target {
    type Error = std::convert::Infallible;

    fn try_as_ref(&self) -> Result<&Target, Self::Error> {
        Ok(self)
    }
}
