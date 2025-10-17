pub mod version;
use thiserror::Error;
pub use version::*;

use std::{marker::PhantomData, path::PathBuf};

pub(crate) mod parol_macro;

/// A syntax element that can be parsed from a specific format.
///
/// # Parameters
///
/// - `Parser`: The type that distinguishes the parser. This is used to
///   differentiate between different parsing strategies or contexts.
pub trait ParseOps<Format: InputFormat, Parser>: Sized {
    type Context;
    type Error;

    fn parse(from: Format, context: Self::Context) -> Result<Self, Self::Error>;
}

/// A marker trait for input formats.
#[diagnostic::on_unimplemented(
    message = "the type `{Self}` is not a valid input format",
    label = "this type does not implement the `InputFormat` trait"
)]
pub trait InputFormat {}

/// An input structure for the parser.
pub struct ParserInput<'a> {
    /// The string content to be parsed.
    pub content: &'a str,

    /// The name of the file from which the content was read.
    ///
    /// FIXME: This is specific to V1 and not used for accessing the content.
    pub file_name: PathBuf,
}

impl<'a> ParserInput<'a> {
    /// Assumes that the input is in the format of the inferred grammar version.
    pub fn assume_inferred<Version: ParserVersion>(self) -> Versioned<Version, Self> {
        Versioned::assume_inferred(self)
    }
}

/// A wrapper type for a value that is assumed to be in the specific format.
pub struct Versioned<Version: ParserVersion, T>(pub T, PhantomData<Version>);
impl<Version: ParserVersion, T> InputFormat for Versioned<Version, T> {}

impl<Version: ParserVersion, T> Versioned<Version, T> {
    /// Creates a [`Versioned`] instance with the given value.
    ///
    /// This is useful when the type system already knows the version of the value.
    pub fn assume_inferred(value: T) -> Self {
        Self(value, PhantomData)
    }

    /// Creates a [`Versioned`] instance with an assumed version and the given value.
    ///
    /// This is useful when you want to explicitly specify the version at the point of parsing, not just at the type level.
    /// If the type system already knows the version, you can use [`Versioned::assume_inferred`].
    pub fn assume_version(version: Version, value: T) -> Self {
        let _ = version;
        Self(value, PhantomData)
    }
}

#[derive(Debug, Error)]
/// An error type that is thrown when the self-describing parser encounters an error.
pub enum SelfDescribingError<'a, E> {
    /// An error that occurred while parsing the shebang line.
    #[error(transparent)]
    ShebangSearchError(ShebangSearchError<'a>),
    #[error("version mismatch: expected '{expected}', found '{found}'")]
    VersionMismatch {
        /// The expected version of the parser.
        expected: &'static str,
        /// The found version in the input.
        found: &'a str,
    },
    #[error(transparent)]
    Other(#[from] E),
}

pub struct SelfDescribingParser<Version>(PhantomData<Version>);

impl<'a> InputFormat for ParserInput<'a> {}
impl<
    'a,
    Version: ParserVersion,
    T: ParseOps<Versioned<Version, ParserInput<'a>>, Version> + opslang_ast::version::Versioned,
> ParseOps<ParserInput<'a>, SelfDescribingParser<Version>> for T
{
    type Context = T::Context;

    type Error = SelfDescribingError<'a, T::Error>;

    fn parse(from: ParserInput<'a>, context: Self::Context) -> Result<Self, Self::Error> {
        // parse version from the input content
        let version_found = Shebang::parse(from.content)
            .map_err(SelfDescribingError::ShebangSearchError)?
            .lang;
        // check if the version matches the expected version
        if version_found != T::version() {
            return Err(SelfDescribingError::VersionMismatch {
                expected: T::version(),
                found: version_found,
            });
        }
        // parse the input using the versioned format
        T::parse(Versioned::assume_inferred(from), context).map_err(SelfDescribingError::Other)
    }
}

pub struct Shebang<'a> {
    pub lang: &'a str,
}

#[derive(Debug, Error)]
pub enum ShebangSearchError<'a> {
    /// The content is empty, so no shebang line can be found.
    #[error("the file is empty, no shebang line found")]
    FileIsEmpty,

    /// The shebang line was not found in the first line of the content.
    #[error("the first line is not a shebang line")]
    FirstLineIsNotShebang,

    /// An syntax error occurred while parsing the shebang line.
    #[error("syntax error in shebang line: expected {expected}, found '{found}'")]
    SyntaxError {
        expected: &'static str,
        found: &'a str,
    },
}

impl<'a> Shebang<'a> {
    pub fn parse(content: &'a str) -> Result<Self, ShebangSearchError<'a>> {
        // currently, we assume the version is at the start of the content
        let Some(first_line) = content.lines().next() else {
            return Err(ShebangSearchError::FileIsEmpty);
        };
        let trimmed = first_line.trim();
        let Some(shebang) = trimmed.strip_prefix("#!") else {
            return Err(ShebangSearchError::FirstLineIsNotShebang);
        };

        // handwritten parser for the shebang line
        // parses shebangs like: "#! lang=v1"
        let mut shebang_input = shebang.trim();

        if let Some(next) = shebang_input.strip_prefix("lang") {
            shebang_input = next.trim();
            let Some(next) = shebang_input.strip_prefix("=") else {
                return Err(ShebangSearchError::SyntaxError {
                    expected: "`=` after `lang`",
                    found: shebang_input,
                });
            };
            shebang_input = next.trim();

            let (value, next) = shebang_input
                .split_once(|c: char| !c.is_ascii_alphanumeric())
                .unwrap_or((shebang_input, ""));

            if value.is_empty() {
                return Err(ShebangSearchError::SyntaxError {
                    expected: "identifier after `=`",
                    found: shebang_input,
                });
            };
            shebang_input = next.trim();
            if !shebang_input.is_empty() {
                return Err(ShebangSearchError::SyntaxError {
                    expected: "end of line after identifier",
                    found: shebang_input,
                });
            }
            Ok(Shebang { lang: value })
        } else {
            Err(ShebangSearchError::SyntaxError {
                expected: "`lang` in the shebang line",
                found: shebang_input,
            })
        }
    }
}
