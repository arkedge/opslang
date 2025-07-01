/// A marker trait for parser versions.
pub trait ParserVersion {}

/// A macro to declare parser versions and their associated modules.
///
/// Note that this macro **re-exports** the types from the module.
macro_rules! declare_versions {
    (
        $(
            $([$current:vis])? $path:ident, $ty:ident;
        )*
    ) => {
        $(
            pub mod $path;
            pub struct $ty;
            impl ParserVersion for $ty {}

            $(
                /// Type alias for the default version of the parser.
                $current type Default = $ty;

                // Re-export the types from the module
                $current use $path::*;
            )?
        )*

        // assert that `[pub]` is used only once

        /// Assertion that [`Default`] type alias exists at this scope.
        ///
        /// This is a compile-time check that will fail if [`Default`] is not defined.
        #[allow(dead_code)]
        const _: () = {
            let _: Default;
        };
    }
}

declare_versions! {
    [pub] v0, V0;
    v1, V1;
}
