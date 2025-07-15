/// A trait for migrating AST from one version to another.
///
/// This trait defines the interface for converting AST nodes from version A to version B.
/// It is typically implemented on Zero-Sized Types (ZSTs) that represent the migration
/// from one specific version to the next.
///
/// # Type Parameters
///
/// - `A`: The source version (must implement [`VersionMarker`])
/// - `B`: The target version (must implement [`VersionMarker`])
///
/// # Examples
///
/// ```rust
/// # use opslang_migration::Migrate;
/// # use opslang_ast::{V0, V1};
/// struct V0ToV1;
///
/// impl Migrate<V0, V1> for V0ToV1 {
///     type Error = String;
///     
///     fn migrate(&self, input: &str) -> Result<String, Self::Error> {
///         // Parse v0, convert to v1, and print
///         todo!()
///     }
/// }
/// ```
pub trait Migrate<A: VersionMarker, B: VersionMarker> {
    /// The error type that can be returned during migration.
    type Error;

    /// Migrates the input string from version A to version B.
    ///
    /// This method takes a string representation of the AST in version A format,
    /// parses it, converts it to version B format, and returns the string
    /// representation of the converted AST.
    ///
    /// # Parameters
    ///
    /// - `input`: The source code string in version A format
    ///
    /// # Returns
    ///
    /// - `Ok(String)`: The converted code string in version B format
    /// - `Err(Self::Error)`: An error if the migration fails
    fn migrate(&self, input: &str) -> Result<String, Self::Error>;
}

/// Errors that can occur during migration.
#[derive(Debug, thiserror::Error)]
pub enum MigrationError {
    /// The migration is not yet implemented.
    #[error("Migration from {from} to {to} is not yet implemented")]
    NotImplemented {
        from: &'static str,
        to: &'static str,
    },

    /// An error occurred during parsing.
    #[error("Parse error: {0}")]
    ParseError(String),

    /// An error occurred during AST conversion.
    #[error("Conversion error: {0}")]
    ConversionError(#[from] ConversionError),

    /// An error occurred during printing.
    #[error("Print error: {0}")]
    PrintError(String),
}

/// Errors that can occur during AST conversion.
#[derive(Debug, thiserror::Error)]
pub enum ConversionError {
    /// A feature in v0 is not supported in v1.
    #[error("Unsupported feature: {feature}")]
    UnsupportedFeature { feature: String },

    /// A required field is missing.
    #[error("Missing field: {field}")]
    MissingField { field: String },

    /// An invalid value was encountered.
    #[error("Invalid value: {value}")]
    InvalidValue { value: String },
}

// Re-export v0 to v1 migration
pub mod v0_to_v1;
use opslang_ast::version::VersionMarker;
pub use v0_to_v1::V0ToV1;
