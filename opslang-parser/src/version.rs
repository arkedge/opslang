macro_rules! declare_versions {
    () => {};
    ($([$current:vis])? $path:ident, $ty:ident; $($tt:tt)*) => {
        pub mod $path;
        pub struct $ty;

        $(
            $current type Current = $ty;
            $current use $path::*;
        )?

        declare_versions! { $($tt)* }
    }
}

declare_versions! {
    [pub] v0, V0;
}
