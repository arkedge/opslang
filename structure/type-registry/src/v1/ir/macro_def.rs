#[macro_export]
/// Macro for defining IR node types from multiple crates with type family support.
///
/// Supports AST types with both default and IR type families, plus native IR and Ty types.
macro_rules! define_ir_node_types {
    (
        crate ast<'cx, ir> {
            $(type $ast_name:ident;)*
            $(mod $ast_module:ident {
                $(type $ast_mod_name:ident;)*
            })*
        }
        crate ast<'cx, default> {
            $(type $ast_default_name:ident;)*
        }
        crate ir<'cx> {
            $(type $ir_name:ident;)*
        }
        crate ty<'cx> {
            $(type $ty_name:ident;)*
        }
        crate ty {
            $(type $ty_no_cx_name:ident;)*
        }
        crate module<'cx> {
            $(type $module_name:ident;)*
        }
    ) => {
        {
            let default = IrNodeTy {
                name: "!",
                child: None,
                ty: None,
            };
            &[
                $(
                    IrNodeTy {
                        ty: Some(IrNodeTyInstance {
                            ty: InstanceKind::ast_ir(),
                            has_lifetime: true,
                        }),
                        name: stringify!($ast_name),
                        ..default
                    },
                )*
                $(
                    $(
                        IrNodeTy {
                            ty: Some(IrNodeTyInstance {
                                ty: InstanceKind::ast_ir(),
                                has_lifetime: true,
                            }),
                            child: Some(stringify!($ast_module)),
                            name: stringify!($ast_mod_name),
                        },
                    )*
                )*
                $(
                    IrNodeTy {
                        ty: Some(IrNodeTyInstance {
                            ty: InstanceKind::ast_default(),
                            has_lifetime: true,
                        }),
                        name: stringify!($ast_default_name),
                        ..default
                    },
                )*
                $(
                    IrNodeTy {
                        ty: Some(IrNodeTyInstance {
                            ty: InstanceKind::ir(),
                            has_lifetime: true,
                        }),
                        name: stringify!($ir_name),
                        ..default
                    },
                )*
                $(
                    IrNodeTy {
                        ty: Some(IrNodeTyInstance {
                            ty: InstanceKind::Other("opslang_ty"),
                            has_lifetime: true,
                        }),
                        name: stringify!($ty_name),
                        ..default
                    },
                )*
                $(
                    IrNodeTy {
                        ty: Some(IrNodeTyInstance {
                            ty: InstanceKind::Other("opslang_ty"),
                            has_lifetime: false,
                        }),
                        name: stringify!($ty_no_cx_name),
                        ..default
                    },
                )*
                $(
                    IrNodeTy {
                        ty: Some(IrNodeTyInstance {
                            ty: InstanceKind::Other("opslang_module"),
                            has_lifetime: true,
                        }),
                        name: stringify!($module_name),
                        ..default
                    },
                )*
            ]
        }
    };
}

#[macro_export]
/// Macro for defining intermediate IR types with wrapper and external type support.
macro_rules! define_ir_inter_types {
    (
        crate ast {
            $(type $ast_name:ident $(<$ast_cx:lifetime, $ast_subst:ident>)? $(: $ast_wrapper:path)?;)*
        }
        crate ir {
            $(type $ir_name:ident $(<$ir_cx:lifetime>)? $(: $ir_wrapper:path)?;)*
        }
        crate ty {
            $(type $ty_name:ident $(<$ty_cx:lifetime>)? $(: $ty_wrapper:path)?;)*
        }
        crate module {
            $(type $module_name:ident $(<$module_cx:lifetime>)? $(: $module_wrapper:path)?;)*
        }
        extern {
            $(type $ext_name:path;)*
        }
    ) => {
        {
            let default = IrInterTyInstance {
                name: "!",
                child: None,
                ty: None,
                has_lifetime: false,
                wrapper: None,
            };
            &[
                $(
                    IrInterTy::Instance(IrInterTyInstance {
                        ty: Some(InstanceKind::Ast({
                            #[allow(unused_mut, unused_assignments)]
                            let mut kind = TypeSubstitution::Default;
                            $(
                                kind = TypeSubstitution::$ast_subst();
                            )?
                            kind
                        })),
                        name: stringify!($ast_name),
                        $(
                            has_lifetime: {stringify!($ast_cx); true},
                        )?
                        $(
                            wrapper: Some(stringify!($ast_wrapper)),
                        )?
                        ..default
                    }),
                )*
                $(
                    IrInterTy::Instance(IrInterTyInstance {
                        ty: Some(InstanceKind::ir()),
                        name: stringify!($ir_name),
                        $(
                            has_lifetime: {stringify!($ir_cx); true},
                        )?
                        $(
                            wrapper: Some(stringify!($ir_wrapper)),
                        )?
                        ..default
                    }),
                )*
                $(
                    IrInterTy::Instance(IrInterTyInstance {
                        ty: Some(InstanceKind::Other("opslang_ty")),
                        name: stringify!($ty_name),
                        $(
                            has_lifetime: {stringify!($ty_cx); true},
                        )?
                        $(
                            wrapper: Some(stringify!($ty_wrapper)),
                        )?
                        ..default
                    }),
                )*
                $(
                    IrInterTy::Instance(IrInterTyInstance {
                        ty: Some(InstanceKind::Other("opslang_module")),
                        name: stringify!($module_name),
                        $(
                            has_lifetime: {stringify!($module_cx); true},
                        )?
                        $(
                            wrapper: Some(stringify!($module_wrapper)),
                        )?
                        ..default
                    }),
                )*
                $(
                    IrInterTy::External(stringify!($ext_name)),
                )*
            ]
        }
    };
}
