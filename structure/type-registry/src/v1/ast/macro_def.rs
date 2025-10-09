#[macro_export]
/// Macro for defining AST node types using Rust-like syntax.
///
/// Supports both top-level types and module-grouped types.
macro_rules! define_v1_ast_node_types {
    (
        crate ast<'cx> {
            $(type $name:ident;)*
            $(mod $module:ident {
                $(type $mod_name:ident;)*
            })*
        }
    ) => {
        &[
            $(
                AstNodeTy {
                    name: stringify!($name),
                    child: None,
                },
            )*
            $(
                $(
                    AstNodeTy {
                        name: stringify!($mod_name),
                        child: Some(stringify!($module))
                    },
                )*
            )*
        ]
    };
}

#[macro_export]
/// Macro for defining intermediate AST types with optional lifetime parameters.
macro_rules! define_v1_ast_inter_types {
    (
        crate ast {
            $(type $name:ident $(<$cx:lifetime>)?;)*
        }
    ) => {
        &[
            $({
                #[allow(unused_mut)]
                let mut t = AstInterTy {
                    name: stringify!($name),
                    has_lifetime: false,
                };
                $(
                    stringify!($cx);
                    t.has_lifetime = true;
                )?
                t
            },)*
        ]
    };
}
