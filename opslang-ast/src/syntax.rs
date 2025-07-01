//! type definition of AST.

macro_rules! declare_versions {
    () => {};
    ($([$current:vis])? $path:ident, $ty:ident; $($tt:tt)*) => {
        pub mod $path;
        pub struct $ty;
        impl $crate::version::sealed::VersionMarker for $ty {
            fn version() -> &'static str {
                stringify!($path)
            }
        }

        $(
            /// Type alias for the default version of the ast.
            $current type Default = $ty;
            $current use $path::*;
        )?

        declare_versions! { $($tt)* }
    }
}

declare_versions! {
    v0, V0;
    [pub] v1, V1;
}

macro_rules! type_exists {
    () => {};
    ($name:path ; $($rest:tt)*) => {
        /// Assertion that the given type exists.
        ///
        /// This is a compile-time check that will fail if the type is not defined.
        const _: () = {
            let _: $name;
        };

        type_exists! { $($rest)* }
    };
}

type_exists! {
    V0; V1;
    Default;
}
