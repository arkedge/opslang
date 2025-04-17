//! Dependency Injection.

/// A syntax element that can be parsed from a specific format.
///
/// # Parameters
///
/// - `Parser`: The type that distinguishes the parser. This is used to
///   differentiate between different parsing strategies or contexts.
pub trait Parse<Parser>: Sized {
    type Format<'a>
    where
        Self: 'a;
    type Context;
    type Error;

    fn parse<'a>(from: Self::Format<'a>, context: Self::Context) -> Result<Self, Self::Error>
    where
        Self: 'a;
}
