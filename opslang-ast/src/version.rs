pub(crate) mod sealed {
    pub trait Sealed {}
}

/// A marker trait for versioning the AST.
pub trait VersionMarker: sealed::Sealed {
    /// Shows the version of the AST in lowercase.
    fn version() -> &'static str;
}

/// A trait for types that is versioned.
pub trait Versioned {
    /// The version type.
    type Version: VersionMarker;

    /// Returns the version of the AST in lowercase.
    fn version() -> &'static str {
        // Re-export the function from the sealed trait.
        <Self::Version as VersionMarker>::version()
    }
}
