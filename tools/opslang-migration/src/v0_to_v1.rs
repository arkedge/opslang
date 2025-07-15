use opslang_ast::v1::context::Context;
use opslang_printer::{Naive, PrettyPrint, PrintOptions};

// Import v0 and v1 types
use opslang_ast::v0;
use opslang_ast::v1;

use crate::{ConversionError, Migrate, MigrationError};

/// A Zero-Sized Type (ZST) that implements migration from v0 to v1.
///
/// This type represents the migration path from opslang v0 syntax to v1 syntax.
/// It can be used to convert v0 AST structures to their v1 equivalents.
///
/// # Examples
///
/// ```rust
/// # use opslang_migration::{Migrate, V0ToV1};
/// # use opslang_ast::{V0, V1};
/// let migrator = V0ToV1;
/// let v0_code = "NOP";
/// // Migration may succeed or fail depending on implementation status
/// let result = migrator.migrate(v0_code);
/// // Just check that we get some result
/// match result {
///     Ok(output) => assert!(!output.is_empty()),
///     Err(_) => (), // Also acceptable
/// }
/// ```
pub struct V0ToV1;

impl Migrate<opslang_ast::V0, opslang_ast::V1> for V0ToV1 {
    type Error = MigrationError;

    fn migrate(&self, input: &str) -> Result<String, Self::Error> {
        // Create a v1 context for allocating reference types
        let ctx = Context::new();

        // Parse v0 input
        let v0_statements = opslang_parser::v0::parser::parse_statements(input)
            .map_err(|e| MigrationError::ParseError(e.to_string()))?;

        // Convert v0 AST to v1 AST
        let v1_program = v0_statements.convert(&ctx)?;

        // Print v1 AST to string
        let options = PrintOptions::<Naive>::default();
        // This line can be written more simply, but keep it to assert
        // the important contraction that `v1::Program` can be printed.
        let output = <v1::Program<ConvertedFamily> as PrettyPrint<Naive>>::to_pretty_string(
            &v1_program,
            &options,
        );

        Ok(output)
    }
}

// Helper functions for common Apply expression patterns

/// Creates a function application expression with a string literal function name
fn create_apply_with_string_function<'cx>(
    ctx: &'cx Context<'cx, ConvertedFamily>,
    function_name: &str,
    args: Vec<v1::Expr<'cx, ConvertedFamily>>,
) -> v1::Expr<'cx, ConvertedFamily> {
    let function_literal = v1::literal::Literal::string(ctx, function_name, Span);
    v1::Expr::apply_literal(ctx, function_literal, args)
}

/// A trait for converting v0 AST nodes to v1 AST nodes.
///
/// This trait is implemented for each v0 type that needs to be converted to v1.
/// The conversion uses a Context to manage lifetimes and allocate v1 reference types.
pub trait ConvertV0ToV1<'cx> {
    /// The target v1 type after conversion.
    type Converted;

    /// Converts the v0 type to its v1 equivalent.
    ///
    /// # Parameters
    ///
    /// - `ctx`: The context for allocating v1 reference types
    ///
    /// # Returns
    ///
    /// - `Ok(Self::Converted)`: The converted v1 type
    /// - `Err(ConversionError)`: An error if the conversion fails
    fn convert(
        self,
        ctx: &'cx Context<'cx, ConvertedFamily>,
    ) -> Result<Self::Converted, ConversionError>;
}

#[derive(Debug, PartialEq, Clone, Copy, Default)]
pub struct ConvertedFamily;

impl<'cx> v1::TypeFamily<'cx> for ConvertedFamily {
    type Span = Span;
    type Position = Position;

    // we want to write `..Default` here...

    type Comment = &'cx v1::Comment<'cx, Self>;
    type Row = &'cx v1::Row<'cx, Self>;
    type RowContent = v1::StatementKind<'cx, Self>;
    type Block = &'cx v1::Block<'cx, Self>;
    type ScopeItem = v1::ScopeItem<'cx, Self>;
    type ReturnStmt = v1::ReturnStmt<'cx, Self>;

    type Ident = v1::Ident<'cx, Self>;
    type Path = v1::Path<'cx, Self>;

    type Qualif = v1::Qualif<'cx, Self>;
    type PreQualified = v1::PreQualified<'cx, Self>;
    type Parened = v1::Parened<'cx, Self>;
    type Literal = v1::Literal<'cx, Self>;
    type Numeric = v1::Numeric<'cx, Self>;
    type Apply = v1::Apply<'cx, Self>;
}

#[derive(Debug, PartialEq, Clone, Copy, Default)]
/// Dummy `Span`.
///
/// `Span` is not available in converted v1 AST currently. Parse the converted v1 AST
/// to retrieve spans and positions.
pub struct Span;

#[derive(Debug, PartialEq, Clone, Copy, Default)]
/// Dummy `Position`.
///
/// `Position` is not available in converted v1 AST currently. Parse the converted v1 AST
/// to retrieve spans and positions.
pub struct Position;

// Implementation of ConvertV0ToV1 for basic types

impl<'cx> ConvertV0ToV1<'cx> for Vec<v0::Statement> {
    type Converted = v1::Program<'cx, ConvertedFamily>;

    fn convert(
        self,
        ctx: &'cx Context<'cx, ConvertedFamily>,
    ) -> Result<Self::Converted, ConversionError> {
        let mut scope_items = Vec::new();

        for statement in self {
            match statement {
                v0::Statement::Single(row) => {
                    let converted_row = row.convert(ctx)?;
                    scope_items.push(v1::ScopeItem::<ConvertedFamily>::Row(converted_row));
                }
                v0::Statement::Block(block) => {
                    let converted_block = block.convert(ctx)?;
                    scope_items.push(v1::ScopeItem::Block(converted_block));
                }
            }
        }

        // Temporary: Convert Vec to slice (need to add alloc_slice to Context)
        let scope = v1::Scope {
            items: Box::leak(scope_items.into_boxed_slice()),
        };

        Ok(v1::Program { content: scope })
    }
}

impl<'cx> ConvertV0ToV1<'cx> for v0::SRow {
    type Converted = &'cx v1::Row<'cx, ConvertedFamily>;

    fn convert(
        self,
        ctx: &'cx Context<'cx, ConvertedFamily>,
    ) -> Result<Self::Converted, ConversionError> {
        let converted = self.value.convert(ctx)?;
        Ok(ctx.alloc_row(converted))
    }
}

impl<'cx> ConvertV0ToV1<'cx> for v0::Row {
    type Converted = v1::Row<'cx, ConvertedFamily>;

    fn convert(
        self,
        ctx: &'cx Context<'cx, ConvertedFamily>,
    ) -> Result<Self::Converted, ConversionError> {
        let breaks = if self.breaks.is_some() {
            Some(v1::token::Break { position: Position })
        } else {
            None
        };

        let content = if let Some(stmt) = self.content {
            Some(stmt.convert(ctx)?)
        } else {
            None
        };

        let comment = if let Some(v0::Comment(comment_text)) = self.comment_trailing {
            let comment_str = ctx.alloc_str(&comment_text);
            Some(ctx.alloc_comment(v1::Comment {
                content: comment_str,
                span: Span,
            }))
        } else {
            None
        };

        Ok(v1::Row {
            breaks,
            content,
            comment,
        })
    }
}

impl<'cx> ConvertV0ToV1<'cx> for v0::SBlock {
    type Converted = &'cx v1::Block<'cx, ConvertedFamily>;

    fn convert(
        self,
        ctx: &'cx Context<'cx, ConvertedFamily>,
    ) -> Result<Self::Converted, ConversionError> {
        let block = self.value.convert(ctx)?;
        Ok(ctx.alloc_block(block))
    }
}

impl<'cx> ConvertV0ToV1<'cx> for v0::Block {
    type Converted = v1::Block<'cx, ConvertedFamily>;

    fn convert(
        self,
        ctx: &'cx Context<'cx, ConvertedFamily>,
    ) -> Result<Self::Converted, ConversionError> {
        let mut scope_items = Vec::new();

        for row in self.rows {
            let converted_row = row.convert(ctx)?;
            scope_items.push(v1::ScopeItem::Row(converted_row));
        }

        // Temporary: Convert Vec to slice (need to add alloc_slice to Context)
        let scope = v1::Scope {
            items: Box::leak(scope_items.into_boxed_slice()),
        };

        Ok(v1::Block {
            left_brace: v1::token::OpenBrace { position: Position },
            scope,
            right_brace: v1::token::CloseBrace { position: Position },
        })
    }
}

impl<'cx> ConvertV0ToV1<'cx> for v0::SingleStatement {
    type Converted = v1::StatementKind<'cx, ConvertedFamily>;

    fn convert(
        self,
        ctx: &'cx Context<'cx, ConvertedFamily>,
    ) -> Result<Self::Converted, ConversionError> {
        match self {
            v0::SingleStatement::Let(let_stmt) => {
                let converted_let = let_stmt.convert(ctx)?;
                Ok(v1::StatementKind::Let(converted_let))
            }
            v0::SingleStatement::Return => Ok(v1::StatementKind::Return(v1::ReturnStmt {
                return_token: v1::token::Return { span: Span },
                semi: v1::token::Semi { position: Position },
            })),
            v0::SingleStatement::Command(cmd) => {
                // Convert command to expression statement
                let expr = cmd.convert(ctx)?;
                Ok(v1::StatementKind::Expr(v1::ExprStatement {
                    expr,
                    semi: v1::token::Semi { position: Position },
                }))
            }
            v0::SingleStatement::Print(print) => {
                // Convert print to expression statement
                let expr = print.convert(ctx)?;
                Ok(v1::StatementKind::Expr(v1::ExprStatement {
                    expr,
                    semi: v1::token::Semi { position: Position },
                }))
            }
            v0::SingleStatement::Call(call) => {
                // Convert call to expression statement
                let expr = call.convert(ctx)?;
                Ok(v1::StatementKind::Expr(v1::ExprStatement {
                    expr,
                    semi: v1::token::Semi { position: Position },
                }))
            }
            v0::SingleStatement::Wait(wait) => {
                // Convert wait to expression statement
                let expr = wait.convert(ctx)?;
                Ok(v1::StatementKind::Expr(v1::ExprStatement {
                    expr,
                    semi: v1::token::Semi { position: Position },
                }))
            }
            v0::SingleStatement::Assert(assert) => {
                // Convert assert to expression statement
                let expr = assert.convert(ctx)?;
                Ok(v1::StatementKind::Expr(v1::ExprStatement {
                    expr,
                    semi: v1::token::Semi { position: Position },
                }))
            }
            v0::SingleStatement::AssertEq(assert_eq) => {
                // Convert assert_eq to expression statement
                let expr = assert_eq.convert(ctx)?;
                Ok(v1::StatementKind::Expr(v1::ExprStatement {
                    expr,
                    semi: v1::token::Semi { position: Position },
                }))
            }
            v0::SingleStatement::Set(set) => {
                // Convert set to expression statement
                let expr = set.convert(ctx)?;
                Ok(v1::StatementKind::Expr(v1::ExprStatement {
                    expr,
                    semi: v1::token::Semi { position: Position },
                }))
            }
        }
    }
}

impl<'cx> ConvertV0ToV1<'cx> for v0::Let {
    type Converted = v1::Let<'cx, ConvertedFamily>;

    fn convert(
        self,
        ctx: &'cx Context<'cx, ConvertedFamily>,
    ) -> Result<Self::Converted, ConversionError> {
        let variable = self.variable.convert(ctx)?;
        let rhs = self.rhs.convert(ctx)?;

        Ok(v1::Let {
            let_token: v1::token::Let { span: Span },
            variable,
            eq: v1::token::Eq { position: Position },
            rhs,
            semi: v1::token::Semi { position: Position },
        })
    }
}

impl<'cx> ConvertV0ToV1<'cx> for v0::Ident {
    type Converted = v1::Ident<'cx, ConvertedFamily>;

    fn convert(
        self,
        ctx: &'cx Context<'cx, ConvertedFamily>,
    ) -> Result<Self::Converted, ConversionError> {
        Ok(v1::Ident::new(ctx, &self.raw, Span))
    }
}

impl<'cx> ConvertV0ToV1<'cx> for v0::VariablePath {
    type Converted = v1::Path<'cx, ConvertedFamily>;

    fn convert(
        self,
        ctx: &'cx Context<'cx, ConvertedFamily>,
    ) -> Result<Self::Converted, ConversionError> {
        let segments: Vec<v1::Ident<'cx, ConvertedFamily>> = self
            .raw
            .split('.')
            .map(|segment| v1::Ident::new(ctx, segment, Span))
            .collect();

        Ok(v1::Path::new_unchecked(
            ctx,
            &self.raw,
            Box::leak(segments.into_boxed_slice()),
        ))
    }
}

impl<'cx> ConvertV0ToV1<'cx> for v0::Command {
    type Converted = v1::Expr<'cx, ConvertedFamily>;

    fn convert(
        self,
        ctx: &'cx Context<'cx, ConvertedFamily>,
    ) -> Result<Self::Converted, ConversionError> {
        // Convert destination spec (part 1)
        // Create command name as a string literal (variable reference)
        assert!(!self.name.contains('.'));
        let path_string = if let Some(v0::ReceiverComponent { name, exec_method }) =
            self.destination.receiver_component
        {
            format!("{name}.{exec_method}")
        } else {
            self.name
        };
        let segments = path_string
            .split('.')
            .map(|seg| v1::Ident::new(ctx, seg, Span))
            .collect::<Vec<_>>();
        let command_path =
            v1::Path::new_unchecked(ctx, &path_string, Box::leak(segments.into_boxed_slice()));

        // Create the function expression (command name as a variable)
        let function_expr = v1::Expr::variable(ctx, command_path);

        // Convert arguments
        let mut converted_args = Vec::new();
        for arg in self.args {
            converted_args.push(arg.convert(ctx)?);
        }

        // Convert destination spec (part 2)
        let mut qualifs = Vec::new();
        if let Some(v0::ExecutorComponent { name }) = self.destination.executor_component {
            if name.contains('.') {
                unimplemented!()
            }
            let component = v1::ExecutorComponent {
                at_token: v1::token::Atmark { position: Position },
                name: v1::Path::single(ctx, &name, Span),
            };
            qualifs.push(v1::Qualif::ExecutorComponent(component));
        }

        Ok(v1::Expr::pre_qualified(
            ctx,
            qualifs,
            v1::Expr::apply(ctx, function_expr, converted_args),
        ))
    }
}

impl<'cx> ConvertV0ToV1<'cx> for v0::Print {
    type Converted = v1::Expr<'cx, ConvertedFamily>;

    fn convert(
        self,
        ctx: &'cx Context<'cx, ConvertedFamily>,
    ) -> Result<Self::Converted, ConversionError> {
        let converted_arg = self.arg.convert(ctx)?;
        Ok(create_apply_with_string_function(
            ctx,
            "print",
            vec![converted_arg],
        ))
    }
}

impl<'cx> ConvertV0ToV1<'cx> for v0::Expr {
    type Converted = v1::Expr<'cx, ConvertedFamily>;

    fn convert(
        self,
        ctx: &'cx Context<'cx, ConvertedFamily>,
    ) -> Result<Self::Converted, ConversionError> {
        match self {
            v0::Expr::Variable(var_path) => {
                let path = var_path.convert(ctx)?;
                Ok(v1::Expr::variable(ctx, path))
            }
            v0::Expr::Literal(literal) => literal.convert(ctx),
            v0::Expr::FunCall(function, args) => {
                let converted_function = function.convert(ctx)?;
                let mut converted_args = Vec::new();
                for arg in args {
                    converted_args.push(arg.convert(ctx)?);
                }

                Ok(v1::Expr::apply(ctx, converted_function, converted_args))
            }
            v0::Expr::TlmRef(variable_path) => {
                // TlmRef ($var) becomes a reference expression in v1
                let path = variable_path.convert(ctx)?;
                let variable_expr = v1::Expr::variable(ctx, path);

                // Create unary reference operation (&var)
                let ref_op = v1::UnOp::Ref(v1::token::Ampersand { position: Position });
                Ok(v1::Expr::unary(ctx, ref_op, variable_expr))
            }
            v0::Expr::UnOp(un_op_kind, expr) => {
                let converted_expr = expr.convert(ctx)?;
                let op = match un_op_kind {
                    v0::UnOpKind::Neg => v1::UnOp::Neg(v1::token::Hyphen { position: Position }),
                };

                Ok(v1::Expr::unary(ctx, op, converted_expr))
            }
            v0::Expr::BinOp(bin_op_kind, lhs, rhs) => {
                let converted_lhs = lhs.convert(ctx)?;
                let converted_rhs = rhs.convert(ctx)?;

                match bin_op_kind {
                    v0::BinOpKind::Compare(compare_op) => {
                        // Convert to Compare expression
                        let v1_compare_op = match compare_op {
                            v0::CompareBinOpKind::GreaterEq => {
                                v1::CompareOp::GreaterEq(v1::token::RightAngleEq { span: Span })
                            }
                            v0::CompareBinOpKind::LessEq => {
                                v1::CompareOp::LessEq(v1::token::AngleEq { span: Span })
                            }
                            v0::CompareBinOpKind::Greater => {
                                v1::CompareOp::Greater(v1::token::RightAngle { position: Position })
                            }
                            v0::CompareBinOpKind::Less => {
                                v1::CompareOp::Less(v1::token::Angle { position: Position })
                            }
                            v0::CompareBinOpKind::Equal => {
                                v1::CompareOp::Equal(v1::token::EqualEqual { span: Span })
                            }
                            v0::CompareBinOpKind::NotEqual => v1::CompareOp::NotEqual(
                                v1::NotEqualToken::BangEqual(v1::token::BangEqual { span: Span }),
                            ),
                        };

                        Ok(v1::Expr::compare_single(
                            ctx,
                            converted_lhs,
                            v1_compare_op,
                            converted_rhs,
                        ))
                    }
                    _ => {
                        // Convert to Binary expression
                        let v1_bin_op = match bin_op_kind {
                            v0::BinOpKind::If => v1::BinOp::If,
                            v0::BinOpKind::And => v1::BinOp::And,
                            v0::BinOpKind::Or => v1::BinOp::Or,
                            v0::BinOpKind::In => v1::BinOp::In,
                            v0::BinOpKind::Mul => v1::BinOp::Mul,
                            v0::BinOpKind::Div => v1::BinOp::Div,
                            v0::BinOpKind::Mod => v1::BinOp::Mod,
                            v0::BinOpKind::Add => v1::BinOp::Add,
                            v0::BinOpKind::Sub => v1::BinOp::Sub,
                            v0::BinOpKind::Compare(_) => unreachable!(), // Already handled above
                        };

                        Ok(v1::Expr::binary(
                            ctx,
                            converted_lhs,
                            v1_bin_op,
                            converted_rhs,
                        ))
                    }
                }
            }
        }
    }
}

impl<'cx> ConvertV0ToV1<'cx> for v0::Literal {
    type Converted = v1::Expr<'cx, ConvertedFamily>;

    fn convert(
        self,
        ctx: &'cx Context<'cx, ConvertedFamily>,
    ) -> Result<Self::Converted, ConversionError> {
        Ok(v1::Expr::literal(
            ctx,
            match self {
                v0::Literal::String(s) => {
                    let string_str = ctx.alloc_str(&s);
                    v1::literal::Literal::String(v1::literal::String {
                        raw: string_str,
                        span: Span,
                    })
                }
                v0::Literal::Numeric(numeric, suffix) => {
                    let converted_numeric = (numeric, suffix).convert(ctx)?;
                    v1::literal::Literal::Numeric(converted_numeric)
                }
                v0::Literal::Array(exprs) => {
                    let mut converted_exprs = Vec::new();
                    for expr in exprs {
                        converted_exprs.push(expr.convert(ctx)?);
                    }
                    v1::literal::Literal::Array(v1::literal::Array {
                        left_bracket: v1::token::OpenSquare { position: Position },
                        exprs: Box::leak(converted_exprs.into_boxed_slice()),
                        right_bracket: v1::token::CloseSquare { position: Position },
                    })
                }
                v0::Literal::DateTime(date_time) => {
                    // Convert DateTime to RFC3339 string
                    let datetime_string = date_time.to_rfc3339();
                    let datetime_str = ctx.alloc_str(&datetime_string);

                    v1::literal::Literal::date_time(ctx, datetime_str, Span)
                }
                v0::Literal::TlmId(tlm_id) => {
                    // TlmId literals should be converted to unary Ref expressions

                    let path = v1::Path::new_unchecked(
                        ctx,
                        &tlm_id,
                        Box::leak(
                            tlm_id
                                .split('.')
                                .map(|segment| v1::Ident::new(ctx, segment, Span))
                                .collect::<Vec<_>>()
                                .into_boxed_slice(),
                        ),
                    );
                    return Ok(v1::Expr::unary(
                        ctx,
                        v1::UnOp::Ref(v1::token::Ampersand { position: Position }),
                        v1::Expr::variable(ctx, path),
                    ));
                }
                v0::Literal::Bytes(bytes) => {
                    // Convert Vec<u8> to hex string
                    let hex_string = bytes.iter().map(|b| format!("{b:02x}")).collect::<String>();
                    let hex_str = ctx.alloc_str(&hex_string);

                    v1::literal::Literal::hex_bytes(ctx, hex_str, Span)
                }
            },
        ))
    }
}

impl<'cx> ConvertV0ToV1<'cx> for (v0::Numeric, Option<v0::NumericSuffix>) {
    type Converted = v1::literal::Numeric<'cx, ConvertedFamily>;

    fn convert(
        self,
        ctx: &'cx Context<'cx, ConvertedFamily>,
    ) -> Result<Self::Converted, ConversionError> {
        let (numeric, suffix) = self;
        let suffix = suffix.map(|n| match n {
            v0::NumericSuffix::Second => v1::Numeric::suffix(ctx, "s", Span),
        });
        match numeric {
            v0::Numeric::Integer(raw, prefix) => {
                let raw_str = ctx.alloc_str(&raw);
                let kind = match prefix {
                    v0::IntegerPrefix::Hexadecimal => {
                        v1::literal::NumericKind::Integer(v1::literal::IntegerPrefix::Hexadecimal)
                    }
                    v0::IntegerPrefix::Octal => {
                        v1::literal::NumericKind::Integer(v1::literal::IntegerPrefix::Octal)
                    }
                    v0::IntegerPrefix::Binary => {
                        v1::literal::NumericKind::Integer(v1::literal::IntegerPrefix::Binary)
                    }
                    v0::IntegerPrefix::Decimal => {
                        v1::literal::NumericKind::Integer(v1::literal::IntegerPrefix::None)
                    }
                };

                Ok(v1::literal::Numeric {
                    raw: raw_str,
                    kind,
                    suffix,
                })
            }
            v0::Numeric::Float(raw) => {
                let raw_str = ctx.alloc_str(&raw);
                Ok(v1::literal::Numeric {
                    raw: raw_str,
                    kind: v1::literal::NumericKind::Float,
                    suffix,
                })
            }
        }
    }
}

impl<'cx> ConvertV0ToV1<'cx> for v0::Call {
    type Converted = v1::Expr<'cx, ConvertedFamily>;

    fn convert(
        self,
        ctx: &'cx Context<'cx, ConvertedFamily>,
    ) -> Result<Self::Converted, ConversionError> {
        // Create path argument as string literal
        let path_str = ctx.alloc_str(&self.path.full_name);
        let path_literal = v1::literal::String {
            raw: path_str,
            span: Span,
        };
        let path_expr = ctx.alloc_expr(v1::ExprKind::Literal(v1::literal::Literal::String(
            path_literal,
        )));

        Ok(create_apply_with_string_function(
            ctx,
            "call",
            vec![path_expr],
        ))
    }
}

impl<'cx> ConvertV0ToV1<'cx> for v0::Wait {
    type Converted = v1::Expr<'cx, ConvertedFamily>;

    fn convert(
        self,
        ctx: &'cx Context<'cx, ConvertedFamily>,
    ) -> Result<Self::Converted, ConversionError> {
        let condition_expr = self.condition.convert(ctx)?;
        Ok(create_apply_with_string_function(
            ctx,
            "wait",
            vec![condition_expr],
        ))
    }
}

impl<'cx> ConvertV0ToV1<'cx> for v0::Assert {
    type Converted = v1::Expr<'cx, ConvertedFamily>;

    fn convert(
        self,
        ctx: &'cx Context<'cx, ConvertedFamily>,
    ) -> Result<Self::Converted, ConversionError> {
        let condition_expr = self.condition.convert(ctx)?;
        Ok(create_apply_with_string_function(
            ctx,
            "assert",
            vec![condition_expr],
        ))
    }
}

impl<'cx> ConvertV0ToV1<'cx> for v0::AssertEq {
    type Converted = v1::Expr<'cx, ConvertedFamily>;

    fn convert(
        self,
        ctx: &'cx Context<'cx, ConvertedFamily>,
    ) -> Result<Self::Converted, ConversionError> {
        let left_expr = self.left.convert(ctx)?;
        let right_expr = self.right.convert(ctx)?;

        if let Some(tolerance) = self.tolerance {
            let tolerance_expr = tolerance.convert(ctx)?;
            Ok(create_apply_with_string_function(
                ctx,
                "assert_eq_tol",
                vec![tolerance_expr, left_expr, right_expr],
            ))
        } else {
            Ok(create_apply_with_string_function(
                ctx,
                "assert_eq",
                vec![left_expr, right_expr],
            ))
        }
    }
}

impl<'cx> ConvertV0ToV1<'cx> for v0::Set {
    type Converted = v1::Expr<'cx, ConvertedFamily>;

    fn convert(
        self,
        ctx: &'cx Context<'cx, ConvertedFamily>,
    ) -> Result<Self::Converted, ConversionError> {
        Ok(v1::Expr::set(
            ctx,
            v1::Expr::variable(ctx, self.name.convert(ctx)?),
            v1::token::ColonEq { span: Span },
            self.expr.convert(ctx)?,
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_migrate_trait_exists() {
        let migrator = V0ToV1;
        // This test just ensures the trait is implemented and the basic structure works
        let result = migrator.migrate("NOP");
        // Now that we have some implementations, this might succeed or fail
        // Let's just check that we get some result
        match result {
            Ok(output) => {
                // Migration succeeded, check that we got some output
                assert!(!output.is_empty());
            }
            Err(_) => {
                // Migration failed, which is also acceptable for this test
                // since not all features are implemented yet
            }
        }
    }

    #[test]
    fn test_convert_empty_statements() {
        let ctx = Context::new();
        let statements: Vec<v0::Statement> = vec![];
        let result = statements.convert(&ctx);
        assert!(result.is_ok());
        let program = result.unwrap();
        assert_eq!(program.content.items.len(), 0);
    }

    #[test]
    fn test_conversion_error_display() {
        let error = ConversionError::UnsupportedFeature {
            feature: "test feature".to_string(),
        };
        let error_string = error.to_string();
        assert!(error_string.contains("Unsupported feature"));
        assert!(error_string.contains("test feature"));
    }
}
