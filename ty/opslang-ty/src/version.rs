macro_rules! declare_versions {
    () => {};
    ($([$current:vis])? $path:ident, $ty:ident; $($tt:tt)*) => {
        pub mod $path;
        pub struct $ty;

        $(
            /// Type alias for the default version of the ast.
            $current type Default = $ty;
            $current use $path::*;
        )?

        declare_versions! { $($tt)* }
    }
}

/// Assertion that [`Default`] type alias exists at this scope.
///
/// This is a compile-time check that will fail if [`Default`] is not defined.
#[allow(dead_code)]
const _: () = {
    let _: Default;
};

declare_versions! {
    [pub] v1, V1;
}
