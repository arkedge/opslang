pub(crate) mod sealed {
    //! This module is used to seal the `VersionMarker` trait, preventing external implementations.

    /// A marker trait for versioning.
    pub trait VersionMarker<DiParser> {}
}

pub struct Versioned<T, DiParser, Version: sealed::VersionMarker<DiParser> = crate::Default> {
    value: T,
    _marker: std::marker::PhantomData<(Version, DiParser)>,
}

impl<T, DiParser, Version: sealed::VersionMarker<DiParser>> Versioned<T, DiParser, Version> {
    fn new(value: T) -> Self {
        Self {
            value,
            _marker: std::marker::PhantomData,
        }
    }

    /// Unwraps the value with the version check.
    pub fn unwrap_version_checked(self, version: Version) -> T {
        drop(version);
        self.value
    }
}

impl<T: crate::di::Parse<DiParser>, DiParser, Version: sealed::VersionMarker<DiParser>>
    Versioned<T, DiParser, Version>
{
    pub fn parse<'a>(from: T::Format<'a>) -> Result<Self, T::Error>
    where
        Self: 'a,
    {
        let value = T::parse(from)?;
        Ok(Self::new(value))
    }
}
