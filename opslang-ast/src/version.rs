pub(crate) mod sealed {
    //! This module is used to seal the `VersionMarker` trait, preventing external implementations.

    /// A marker trait for versioning.
    pub trait VersionMarker {}
}

pub struct Versioned<T, Version: sealed::VersionMarker = crate::Default> {
    value: T,
    _marker: std::marker::PhantomData<Version>,
}

impl<T, Version: sealed::VersionMarker> Versioned<T, Version> {
    pub fn new_version_checked(value: T, _version: Version) -> Self {
        Self {
            value,
            _marker: std::marker::PhantomData,
        }
    }

    /// Unwraps the value with the version check.
    pub fn unwrap_version_checked(self, _version: Version) -> T {
        self.value
    }
}
