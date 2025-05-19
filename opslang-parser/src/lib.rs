#![feature(iterator_try_collect)]

pub mod version;
pub use version::*;

pub(crate) mod parol_macro;

/// A syntax element that can be parsed from a specific format.
///
/// # Parameters
///
/// - `Parser`: The type that distinguishes the parser. This is used to
///   differentiate between different parsing strategies or contexts.
pub trait ParseOps<Format, Parser>: Sized {
    type Context;
    type Error;

    fn parse(from: Format, context: Self::Context) -> Result<Self, Self::Error>;
}
