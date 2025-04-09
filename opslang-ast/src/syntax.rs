//! type definition of AST.

macro_rules! declare_versions {
    () => {};
    ($([$current:vis])? $path:ident, $ty:ident; $($tt:tt)*) => {
        pub mod $path;
        pub struct $ty;
        impl<DiParser> $crate::version::sealed::VersionMarker<DiParser> for $ty {}

        $(
            $current type Current = $ty;
            $current use $path::*;
        )?

        declare_versions! { $($tt)* }
    }
}

/// Assertion that [`Current`] type alias exists at this scope.
///
/// This is a compile-time check that will fail if [`Current`] is not defined.
#[allow(dead_code)]
const _: () = {
    let _: Current;
};

declare_versions! {
    [pub] v0, V0;
}
