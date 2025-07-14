use super::generated::grammar_trait::{self, ActionTrait, Program};
use opslang_ast::v1::{self as syn, context::Context};

#[allow(unused_imports)]
use parol_runtime::{Result, Token};

/// The main action for parsing the grammar.
///
/// This type is referenced in the `build.rs` file to generate the parser.
pub struct Action<'cx> {
    cx: &'cx Context<'cx>,
    parsed: Option<syn::Program<'cx>>,
}

impl<'cx> Action<'cx> {
    pub fn new(cx: &'cx Context<'cx>) -> Self {
        Self { cx, parsed: None }
    }

    pub fn finish(self) -> syn::Program<'cx> {
        self.parsed.expect("Action was not parsed")
    }
}

/// Linking the [`Action`] type to the [`ActionTrait`] trait.
impl<'cx> ActionTrait<'cx> for Action<'cx> {
    fn program(&mut self, arg: &Program<'cx>) -> Result<()> {
        let program = arg.process_token(self.cx);
        self.parsed = Some(program);
        Ok(())
    }
}

/// A trait for processing tokens in the grammar.
///
/// This trait is used to process tokens in the grammar and convert them into
/// the appropriate types of the AST.
/// The implementations of this trait, which is the rest of this file, should be synced with the `parol` grammar file.
trait ProcessToken<'cx> {
    type Output;

    /// Processes a token into an [`Output`] type.
    ///
    /// [`Output`]: ProcessToken::Output
    fn process_token(&self, cx: &'cx Context<'cx>) -> Self::Output;
}

impl<'cx, T: ProcessToken<'cx>> ProcessToken<'cx> for Option<T> {
    type Output = Option<T::Output>;

    fn process_token(&self, cx: &'cx Context<'cx>) -> Self::Output {
        self.as_ref().map(|this| this.process_token(cx))
    }
}

impl<'cx, T: ProcessToken<'cx>> ProcessToken<'cx> for Box<T> {
    type Output = <T as ProcessToken<'cx>>::Output;
    #[inline(always)]
    fn process_token(&self, cx: &'cx Context<'cx>) -> Self::Output {
        <T as ProcessToken<'cx>>::process_token(&**self, cx)
    }
}

trait TokenSpan {
    fn span(&self) -> syn::Span;
}

trait TokenLocation {
    fn location(&self) -> &'_ parol_runtime::Location;
}

impl TokenLocation for parol_runtime::Token<'_> {
    fn location(&self) -> &'_ parol_runtime::Location {
        &self.location
    }
}

impl TokenLocation for parol_runtime::Location {
    fn location(&self) -> &'_ parol_runtime::Location {
        self
    }
}

impl<T: TokenLocation> TokenSpan for T {
    fn span(&self) -> syn::Span {
        let parol_runtime::Location { start, end, .. } = self.location();
        syn::Span {
            start: syn::BytePos(*start),
            end: syn::BytePos(*end),
        }
    }
}

impl TokenLocation for grammar_trait::Break<'_> {
    fn location(&self) -> &'_ parol_runtime::Location {
        &self.r#break.location
    }
}

impl<'cx> ProcessToken<'cx> for grammar_trait::Program<'_> {
    type Output = syn::Program<'cx>;

    fn process_token(&self, cx: &'cx Context<'cx>) -> Self::Output {
        syn::Program {
            content: self.scope.process_token(cx),
        }
    }
}

impl<'cx> ProcessToken<'cx> for grammar_trait::Scope<'_> {
    type Output = syn::Scope<'cx>;

    fn process_token(&self, cx: &'cx Context<'cx>) -> Self::Output {
        let vec: Vec<_> = self
            .scope_list
            .iter()
            .map(|x| x.scope_content.process_token(cx))
            .collect();
        syn::Scope {
            items: Box::leak(vec.into_boxed_slice()),
        }
    }
}

impl<'cx> ProcessToken<'cx> for grammar_trait::ScopeContentOpt<'_> {
    type Output = syn::token::Break<'cx>;

    fn process_token(&self, _cx: &'cx Context<'cx>) -> Self::Output {
        syn::token::Break {
            position: syn::BytePos(self.r#break.r#break.location.start),
        }
    }
}

impl<'cx> ProcessToken<'cx> for grammar_trait::ScopeContent<'_> {
    type Output = syn::ScopeItem<'cx>;

    fn process_token(&self, cx: &'cx Context<'cx>) -> Self::Output {
        let Self {
            scope_content_opt,
            scope_content_opt0,
            scope_content_opt1,
            end_of_line: _end_of_line,
        } = self;
        let breaks = scope_content_opt.process_token(cx);
        let content = if let Some(scope_content_kind) = scope_content_opt0 {
            match &*scope_content_kind.scope_content_kind {
                grammar_trait::ScopeContentKind::Block(block) => {
                    let block = block.block.process_token(cx);
                    return syn::ScopeItem::Block(cx.alloc_block(block));
                }
                grammar_trait::ScopeContentKind::Statement(stmt) => {
                    Some(stmt.statement.process_token(cx))
                }
            }
        } else {
            None
        };
        let comment = scope_content_opt1
            .as_ref()
            .map(|comment| comment.comment.process_token(cx));
        syn::ScopeItem::Row(cx.alloc_row(syn::Row {
            breaks,
            content,
            comment,
        }))
    }
}

impl<'cx> ProcessToken<'cx> for grammar_trait::Block<'_> {
    type Output = syn::Block<'cx>;

    fn process_token(&self, cx: &'cx Context<'cx>) -> Self::Output {
        syn::Block {
            left_brace: syn::token::OpenBrace {
                position: syn::BytePos(self.l_brace.location.start),
            },
            scope: self.scope.process_token(cx),
            right_brace: syn::token::CloseBrace {
                position: syn::BytePos(self.r_brace.location.start),
            },
        }
    }
}

impl<'cx> ProcessToken<'cx> for grammar_trait::Statement<'_> {
    type Output = syn::StatementKind<'cx>;

    fn process_token(&self, cx: &'cx Context<'cx>) -> Self::Output {
        let semi = syn::token::Semi {
            position: syn::BytePos(self.semi.semi.location.start),
        };
        match &*self.statement_kind {
            grammar_trait::StatementKind::LetStmt(grammar_trait::StatementKindLetStmt {
                let_stmt,
            }) => {
                let grammar_trait::LetStmt {
                    r#let,
                    ident,
                    equ,
                    expr,
                } = &**let_stmt;
                syn::StatementKind::Let(syn::Let {
                    let_token: syn::token::Let { span: r#let.span() },
                    variable: ident.process_token(cx),
                    eq: syn::token::Eq {
                        position: syn::BytePos(equ.location.start),
                    },
                    rhs: expr.process_token(cx),
                    semi,
                })
            }
            grammar_trait::StatementKind::Expr(statement_kind_expr) => {
                syn::StatementKind::Expr(syn::ExprStatement {
                    expr: statement_kind_expr.expr.process_token(cx),
                    semi,
                })
            }
            grammar_trait::StatementKind::ReturnStmt(statement_kind_return_stmt) => {
                syn::StatementKind::Return(syn::ReturnStmt {
                    return_token: syn::token::Return {
                        span: statement_kind_return_stmt.return_stmt.return_stmt.span(),
                    },
                    semi,
                })
            }
        }
    }
}

impl<'cx> ProcessToken<'cx> for grammar_trait::Expr<'_> {
    type Output = syn::Expr<'cx>;

    fn process_token(&self, cx: &'cx Context<'cx>) -> Self::Output {
        if let Some(grammar_trait::SetExprOpt {
            colon_equ,
            infix_if_expr,
        }) = &self.set_expr.set_expr_opt
        {
            cx.alloc_expr(syn::ExprKind::Set(syn::Set {
                lhs: self.set_expr.infix_if_expr.process_token(cx),
                colon_eq: syn::token::ColonEq {
                    span: colon_equ.span(),
                },
                rhs: infix_if_expr.process_token(cx),
            }))
        } else {
            self.set_expr.infix_if_expr.process_token(cx)
        }
    }
}

impl<'cx> ProcessToken<'cx> for grammar_trait::InfixIfExpr<'_> {
    type Output = syn::Expr<'cx>;

    fn process_token(&self, cx: &'cx Context<'cx>) -> Self::Output {
        if let Some(grammar_trait::InfixIfExprOpt {
            r#if: _,
            logical_or_expr,
        }) = &self.infix_if_expr_opt
        {
            cx.alloc_expr(syn::ExprKind::Binary(syn::Binary {
                lhs: self.logical_or_expr.process_token(cx),
                op: syn::BinOp::If,
                rhs: logical_or_expr.process_token(cx),
            }))
        } else {
            self.logical_or_expr.process_token(cx)
        }
    }
}

impl<'cx> ProcessToken<'cx> for grammar_trait::LogicalOrExpr<'_> {
    type Output = syn::Expr<'cx>;

    fn process_token(&self, cx: &'cx Context<'cx>) -> Self::Output {
        if self.logical_or_expr_list.is_empty() {
            self.logical_and_expr.process_token(cx)
        } else {
            self.logical_or_expr_list.iter().rfold(
                self.logical_and_expr.process_token(cx),
                |acc, expr| {
                    cx.alloc_expr(syn::ExprKind::Binary(syn::Binary {
                        lhs: expr.logical_and_expr.process_token(cx),
                        op: syn::BinOp::Or,
                        rhs: acc,
                    }))
                },
            )
        }
    }
}

impl<'cx> ProcessToken<'cx> for grammar_trait::LogicalAndExpr<'_> {
    type Output = syn::Expr<'cx>;

    fn process_token(&self, cx: &'cx Context<'cx>) -> Self::Output {
        if self.logical_and_expr_list.is_empty() {
            self.infix_in_expr.process_token(cx)
        } else {
            self.logical_and_expr_list.iter().rfold(
                self.infix_in_expr.process_token(cx),
                |acc, expr| {
                    cx.alloc_expr(syn::ExprKind::Binary(syn::Binary {
                        lhs: expr.infix_in_expr.process_token(cx),
                        op: syn::BinOp::And,
                        rhs: acc,
                    }))
                },
            )
        }
    }
}

impl<'cx> ProcessToken<'cx> for grammar_trait::InfixInExpr<'_> {
    type Output = syn::Expr<'cx>;

    fn process_token(&self, cx: &'cx Context<'cx>) -> Self::Output {
        if let Some(grammar_trait::InfixInExprOpt {
            r#in: _,
            compare_expr,
        }) = &self.infix_in_expr_opt
        {
            cx.alloc_expr(syn::ExprKind::Binary(syn::Binary {
                lhs: self.compare_expr.process_token(cx),
                op: syn::BinOp::If,
                rhs: compare_expr.process_token(cx),
            }))
        } else {
            self.compare_expr.process_token(cx)
        }
    }
}

impl<'cx> ProcessToken<'cx> for grammar_trait::CompareExpr<'_> {
    type Output = syn::Expr<'cx>;

    fn process_token(&self, cx: &'cx Context<'cx>) -> Self::Output {
        if self.compare_expr_list.is_empty() {
            self.arithmetic_expr.process_token(cx)
        } else {
            cx.alloc_expr(syn::ExprKind::Compare(syn::Compare {
                head: self.arithmetic_expr.process_token(cx),
                tail_with_op: Box::leak(
                    self.compare_expr_list
                        .iter()
                        .map(|expr| {
                            (
                                expr.compare_op.process_token(cx),
                                expr.arithmetic_expr.process_token(cx),
                            )
                        })
                        .collect::<Vec<_>>()
                        .into_boxed_slice(),
                ),
            }))
        }
    }
}

impl<'cx> ProcessToken<'cx> for grammar_trait::CompareOp<'_> {
    type Output = syn::CompareOp<'cx>;

    fn process_token(&self, _cx: &'cx Context<'cx>) -> Self::Output {
        match self {
            grammar_trait::CompareOp::GTEqu(compare_op_gtequ) => {
                syn::CompareOp::GreaterEq(syn::token::RightAngleEq {
                    span: compare_op_gtequ.g_t_equ.span(),
                })
            }
            grammar_trait::CompareOp::LTEqu(compare_op_ltequ) => {
                syn::CompareOp::LessEq(syn::token::AngleEq {
                    span: compare_op_ltequ.l_t_equ.span(),
                })
            }
            grammar_trait::CompareOp::GT(compare_op_gt) => {
                syn::CompareOp::Greater(syn::token::RightAngle {
                    position: syn::BytePos(compare_op_gt.g_t.location.start),
                })
            }
            grammar_trait::CompareOp::LT(compare_op_lt) => {
                syn::CompareOp::Less(syn::token::Angle {
                    position: syn::BytePos(compare_op_lt.l_t.location.start),
                })
            }
            grammar_trait::CompareOp::BangEqu(compare_op_bang_equ) => {
                syn::CompareOp::NotEqual(syn::NotEqualToken::BangEqual(syn::token::BangEqual {
                    span: compare_op_bang_equ.bang_equ.span(),
                }))
            }
            grammar_trait::CompareOp::SlashEqu(compare_op_slash_equ) => {
                syn::CompareOp::NotEqual(syn::NotEqualToken::SlashEqual(syn::token::SlashEqual {
                    span: compare_op_slash_equ.slash_equ.span(),
                }))
            }
            grammar_trait::CompareOp::EquEqu(compare_op_equ_equ) => {
                syn::CompareOp::Equal(syn::token::EqualEqual {
                    span: compare_op_equ_equ.equ_equ.span(),
                })
            }
        }
    }
}

impl<'cx> ProcessToken<'cx> for grammar_trait::ArithmeticExpr<'_> {
    type Output = syn::Expr<'cx>;

    fn process_token(&self, cx: &'cx Context<'cx>) -> Self::Output {
        if self.arithmetic_expr_list.is_empty() {
            self.factor_expr.process_token(cx)
        } else {
            self.arithmetic_expr_list.iter().rfold(
                self.factor_expr.process_token(cx),
                |acc, expr| {
                    cx.alloc_expr(syn::ExprKind::Binary(syn::Binary {
                        lhs: expr.factor_expr.process_token(cx),
                        op: expr.arithmetic_op.process_token(cx),
                        rhs: acc,
                    }))
                },
            )
        }
    }
}

impl<'cx> ProcessToken<'cx> for grammar_trait::ArithmeticOp<'_> {
    type Output = syn::BinOp;

    fn process_token(&self, _: &'cx Context<'cx>) -> Self::Output {
        match self {
            grammar_trait::ArithmeticOp::Plus(..) => syn::BinOp::Add,
            grammar_trait::ArithmeticOp::Minus(..) => syn::BinOp::Sub,
        }
    }
}

impl<'cx> ProcessToken<'cx> for grammar_trait::FactorExpr<'_> {
    type Output = syn::Expr<'cx>;

    fn process_token(&self, cx: &'cx Context<'cx>) -> Self::Output {
        if self.factor_expr_list.is_empty() {
            self.prefix_expr.process_token(cx)
        } else {
            self.factor_expr_list
                .iter()
                .rfold(self.prefix_expr.process_token(cx), |acc, expr| {
                    cx.alloc_expr(syn::ExprKind::Binary(syn::Binary {
                        lhs: expr.prefix_expr.process_token(cx),
                        op: expr.factor_op.process_token(cx),
                        rhs: acc,
                    }))
                })
        }
    }
}

impl<'cx> ProcessToken<'cx> for grammar_trait::FactorOp<'_> {
    type Output = syn::BinOp;

    fn process_token(&self, _: &'cx Context<'cx>) -> Self::Output {
        match self {
            grammar_trait::FactorOp::Star(..) => syn::BinOp::Mul,
            grammar_trait::FactorOp::Slash(..) => syn::BinOp::Div,
            grammar_trait::FactorOp::Percent(..) => syn::BinOp::Mod,
        }
    }
}

impl<'cx> ProcessToken<'cx> for grammar_trait::PrefixExpr<'_> {
    type Output = syn::Expr<'cx>;

    fn process_token(&self, cx: &'cx Context<'cx>) -> Self::Output {
        match self {
            grammar_trait::PrefixExpr::MinusApplyExpr(prefix_expr_minus_apply_expr) => cx
                .alloc_expr(syn::ExprKind::Unary(syn::Unary {
                    op: syn::UnOp::Neg(syn::token::Hyphen {
                        position: syn::BytePos(prefix_expr_minus_apply_expr.minus.location.start),
                    }),
                    expr: prefix_expr_minus_apply_expr.apply_expr.process_token(cx),
                })),
            grammar_trait::PrefixExpr::PrefixExprListApplyExpr(
                prefix_expr_prefix_expr_list_apply_expr,
            ) => {
                if prefix_expr_prefix_expr_list_apply_expr
                    .prefix_expr_list
                    .is_empty()
                {
                    prefix_expr_prefix_expr_list_apply_expr
                        .apply_expr
                        .process_token(cx)
                } else {
                    cx.alloc_expr(syn::ExprKind::PreQualified(syn::PreQualified {
                        qualifs: Box::leak(
                            prefix_expr_prefix_expr_list_apply_expr
                                .prefix_expr_list
                                .iter()
                                .map(|qualif| qualif.qualif.process_token(cx))
                                .collect::<Vec<_>>()
                                .into_boxed_slice(),
                        ),
                        expr: prefix_expr_prefix_expr_list_apply_expr
                            .apply_expr
                            .process_token(cx),
                    }))
                }
            }
        }
    }
}

impl<'cx> ProcessToken<'cx> for grammar_trait::Qualif<'_> {
    type Output = syn::Qualif<'cx>;

    fn process_token(&self, cx: &'cx Context<'cx>) -> Self::Output {
        match self {
            grammar_trait::Qualif::ExecutorComponent(qualif_executor_component) => {
                let e = &*qualif_executor_component.executor_component;
                syn::Qualif::ExecutorComponent(syn::ExecutorComponent {
                    at_token: syn::token::Atmark {
                        position: syn::BytePos(e.at.location.start),
                    },
                    name: e.path.process_token(cx),
                })
            }
            grammar_trait::Qualif::TimeIndicator(qualif_time_indicator) => {
                syn::Qualif::TimeIndicator(
                    qualif_time_indicator
                        .time_indicator
                        .callable
                        .process_token(cx),
                )
            }
        }
    }
}

impl<'cx> ProcessToken<'cx> for grammar_trait::ApplyExpr<'_> {
    type Output = syn::Expr<'cx>;

    fn process_token(&self, cx: &'cx Context<'cx>) -> Self::Output {
        if self.apply_expr_list.is_empty() {
            self.lower_prefix_expr.process_token(cx)
        } else {
            cx.alloc_expr(syn::ExprKind::Apply(syn::Apply {
                function: self.lower_prefix_expr.process_token(cx),
                args: Box::leak(
                    self.apply_expr_list
                        .iter()
                        .map(|arg| arg.atomic_expr.process_token(cx))
                        .collect::<Vec<_>>()
                        .into_boxed_slice(),
                ),
            }))
        }
    }
}

impl<'cx> ProcessToken<'cx> for grammar_trait::LowerPrefixExpr<'_> {
    type Output = syn::Expr<'cx>;

    fn process_token(&self, cx: &'cx Context<'cx>) -> Self::Output {
        if let Some(lower_prefix) = self.lower_prefix_expr_opt.as_ref() {
            cx.alloc_expr(syn::ExprKind::Unary(syn::Unary {
                op: syn::UnOp::Ref(syn::token::Ampersand {
                    position: syn::BytePos(lower_prefix.amp.location.start),
                }),
                expr: self.callable.process_token(cx),
            }))
        } else {
            self.callable.process_token(cx)
        }
    }
}

impl<'cx> ProcessToken<'cx> for grammar_trait::Callable<'_> {
    type Output = syn::Expr<'cx>;

    fn process_token(&self, cx: &'cx Context<'cx>) -> Self::Output {
        match self {
            grammar_trait::Callable::Path(callable_path) => cx.alloc_expr(syn::ExprKind::Variable(
                callable_path.path.process_token(cx),
            )),
            grammar_trait::Callable::Literal(callable_literal) => cx.alloc_expr(
                syn::ExprKind::Literal(callable_literal.literal.process_token(cx)),
            ),
            grammar_trait::Callable::LParenExprRParen(callable_lparen_expr_rparen) => cx
                .alloc_expr(syn::ExprKind::Parened(syn::Parened {
                    left_paren: syn::token::OpenParen {
                        position: syn::BytePos(callable_lparen_expr_rparen.l_paren.location.start),
                    },
                    expr: callable_lparen_expr_rparen.expr.process_token(cx),
                    right_paren: syn::token::CloseParen {
                        position: syn::BytePos(callable_lparen_expr_rparen.r_paren.location.start),
                    },
                })),
        }
    }
}

impl<'cx> ProcessToken<'cx> for grammar_trait::AtomicExpr<'_> {
    type Output = syn::Expr<'cx>;

    fn process_token(&self, cx: &'cx Context<'cx>) -> Self::Output {
        match self {
            grammar_trait::AtomicExpr::Qualif(atomic_expr_qualif) => cx.alloc_expr(
                syn::ExprKind::Qualif(atomic_expr_qualif.qualif.process_token(cx)),
            ),
            grammar_trait::AtomicExpr::LowerPrefixExpr(atomic_expr_lower_prefix) => {
                atomic_expr_lower_prefix.lower_prefix_expr.process_token(cx)
            }
        }
    }
}

impl<'cx> ProcessToken<'cx> for grammar_trait::Literal<'_> {
    type Output = syn::Literal<'cx>;

    fn process_token(&self, cx: &'cx Context<'cx>) -> Self::Output {
        match self {
            grammar_trait::Literal::Array(literal_array) => syn::Literal::Array(syn::Array {
                left_bracket: syn::token::OpenSquare {
                    position: syn::BytePos(literal_array.array.l_bracket.location.start),
                },
                exprs: {
                    let mut exprs = Vec::new();
                    if let Some(comma_sep_elements) = &literal_array.array.array_opt {
                        let mut elements = &comma_sep_elements.comma_sep_elements;
                        loop {
                            exprs.push(elements.expr.process_token(cx));
                            let Some(comma_expr_list) = &elements.comma_sep_elements_opt else {
                                break;
                            };
                            // ignoring the comma...
                            let Some(comma_sep_elements) =
                                &comma_expr_list.comma_expr_list.comma_expr_list_opt
                            else {
                                break;
                            };
                            elements = &comma_sep_elements.comma_sep_elements;
                        }
                    }
                    Box::leak(exprs.into_boxed_slice())
                },
                right_bracket: syn::token::CloseSquare {
                    position: syn::BytePos(literal_array.array.r_bracket.location.start),
                },
            }),
            grammar_trait::Literal::String(literal_string) => syn::Literal::String(syn::String {
                raw: cx.alloc_str(literal_string.string.string.text().trim_matches('"')),
                span: literal_string.string.string.span(),
            }),
            grammar_trait::Literal::ByteLiteral(literal_byte_literal) => {
                syn::Literal::Bytes(syn::Bytes {
                    raw: cx.alloc_str(
                        literal_byte_literal
                            .byte_literal
                            .byte_literal
                            .text()
                            .trim_start_matches('b')
                            .trim_matches('"'),
                    ),
                    span: literal_byte_literal.byte_literal.byte_literal.span(),
                })
            }
            grammar_trait::Literal::HexByteLiteral(literal_hex_byte_literal) => {
                syn::Literal::HexBytes(syn::HexBytes {
                    raw: cx.alloc_str(
                        literal_hex_byte_literal
                            .hex_byte_literal
                            .hex_byte_literal
                            .text()
                            .trim_start_matches("bx")
                            .trim_matches('"'),
                    ),
                    span: literal_hex_byte_literal
                        .hex_byte_literal
                        .hex_byte_literal
                        .span(),
                })
            }
            grammar_trait::Literal::Numeric(literal_numeric) => {
                syn::Literal::Numeric(literal_numeric.numeric.process_token(cx))
            }
            grammar_trait::Literal::FilePathLiteral(literal_file_path_literal) => {
                syn::Literal::OsFilePath(syn::OsFilePath {
                    raw: cx.alloc_str(
                        literal_file_path_literal
                            .file_path_literal
                            .file_path_literal
                            .text()
                            .trim_start_matches("os")
                            .trim_matches('"'),
                    ),
                    span: literal_file_path_literal
                        .file_path_literal
                        .file_path_literal
                        .span(),
                })
            }
            grammar_trait::Literal::Rfc3339DateTime(literal_rfc3339_datetime) => {
                syn::Literal::DateTime(syn::DateTime {
                    raw: cx.alloc_str(
                        literal_rfc3339_datetime
                            .rfc3339_date_time
                            .rfc3339_date_time
                            .text(),
                    ),
                    span: literal_rfc3339_datetime
                        .rfc3339_date_time
                        .rfc3339_date_time
                        .span(),
                })
            }
        }
    }
}

impl<'cx> ProcessToken<'cx> for grammar_trait::Numeric<'_> {
    type Output = syn::Numeric<'cx>;

    fn process_token(&self, cx: &'cx Context<'cx>) -> Self::Output {
        macro_rules! extract_suffix {
            ($token:ident, $delimiter:expr) => {{
                let input = $token.text();
                if let Some((start, end)) = input.split_once($delimiter) {
                    (
                        start,
                        Some(syn::NumericSuffix(syn::Ident {
                            raw: cx.alloc_str(end),
                            span: $token.location.span(),
                        })),
                    )
                } else {
                    (input, None)
                }
            }};
        }

        let (raw, suffix, kind) = match self {
            grammar_trait::Numeric::BinaryInteger(binary_integer) => {
                let token = &binary_integer.binary_integer.binary_integer;
                let (raw, suffix) = extract_suffix!(token, |c: char| c.is_alphabetic());
                (
                    raw.trim_start_matches("0b"),
                    suffix,
                    syn::NumericKind::Integer(syn::IntegerPrefix::Binary),
                )
            }
            grammar_trait::Numeric::OctalInteger(octal_integer) => {
                let token = &octal_integer.octal_integer.octal_integer;
                let (raw, suffix) = extract_suffix!(token, |c: char| c.is_alphabetic());
                (
                    raw.trim_start_matches("0o"),
                    suffix,
                    syn::NumericKind::Integer(syn::IntegerPrefix::Octal),
                )
            }
            grammar_trait::Numeric::HexadecimalInteger(hexadecimal_integer) => {
                let token = &hexadecimal_integer.hexadecimal_integer.hexadecimal_integer;
                let (raw, suffix) =
                    extract_suffix!(token, |c: char| c.is_alphabetic() && !c.is_ascii_hexdigit());
                (
                    raw.trim_start_matches("0x"),
                    suffix,
                    syn::NumericKind::Integer(syn::IntegerPrefix::Hexadecimal),
                )
            }
            grammar_trait::Numeric::Ieee754Float(ieee754_float) => {
                let token = &ieee754_float.ieee754_float.ieee754_float;
                let (raw, suffix) = extract_suffix!(token, |c: char| {
                    c.is_alphabetic() && !matches!(c, 'e' | 'E')
                });
                (
                    raw,
                    suffix,
                    if raw.contains('.') || raw.contains(['e', 'E']) {
                        syn::NumericKind::Float
                    } else {
                        syn::NumericKind::Integer(syn::IntegerPrefix::None)
                    },
                )
            }
        };

        syn::Numeric {
            raw: cx.alloc_str(raw),
            kind,
            suffix,
        }
    }
}

impl<'cx> ProcessToken<'cx> for grammar_trait::Comment<'_> {
    type Output = &'cx syn::Comment<'cx>;

    fn process_token(&self, cx: &'cx Context<'cx>) -> Self::Output {
        let token = &self.comment_content.comment_content;
        cx.alloc_comment(syn::Comment {
            content: cx.alloc_str(token.text()),
            span: token.location.span(),
        })
    }
}

impl<'cx> ProcessToken<'cx> for grammar_trait::Ident<'_> {
    type Output = syn::Ident<'cx>;

    fn process_token(&self, cx: &'cx Context<'cx>) -> Self::Output {
        let token = &self.ident;
        syn::Ident {
            raw: cx.alloc_str(token.text()),
            span: token.location.span(),
        }
    }
}

impl<'cx> ProcessToken<'cx> for grammar_trait::Path<'_> {
    type Output = syn::Path<'cx>;

    fn process_token(&self, cx: &'cx Context<'cx>) -> Self::Output {
        let mut raw = String::new();
        let mut segments = Vec::with_capacity(self.path_list.len() + 1);
        raw.push_str(self.ident.ident.text());
        segments.push(self.ident.process_token(cx));
        for segment in &self.path_list {
            raw.push('.');
            raw.push_str(segment.ident.ident.text());
            segments.push(segment.ident.process_token(cx));
        }
        syn::Path {
            raw: cx.alloc_str(&raw),
            segments: Box::leak(segments.into_boxed_slice()),
        }
    }
}
