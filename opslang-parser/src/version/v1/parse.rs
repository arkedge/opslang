use super::generated::grammar_trait::{self, ActionTrait, Program};
use opslang_ast::{
    ScopeItem, V1Token as Token,
    token::{IntoPosition, IntoSpan},
    v1::{self as syn, context::Context},
};

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

/// Locally defined types, needed so that this crate is independent from `opslang-ast`.
struct Loc<T>(T);
impl IntoSpan<'_> for Loc<syn::Span> {
    fn into_span(self) -> syn::Span {
        self.0
    }
}
impl IntoPosition<'_> for Loc<syn::Position> {
    fn into_position(self) -> syn::Position {
        self.0
    }
}

/// Wraps a foreign type into a locally defined type.
trait WrapLoc<T> {
    fn wrap(&self) -> Loc<T>;
}

impl WrapLoc<syn::Span> for parol_runtime::Token<'_> {
    fn wrap(&self) -> Loc<syn::Span> {
        let parol_runtime::Location { start, end, .. } = self.location;
        Loc(syn::Span {
            start: syn::BytePos(start),
            end: syn::BytePos(end),
        })
    }
}

impl WrapLoc<syn::Position> for parol_runtime::Token<'_> {
    fn wrap(&self) -> Loc<syn::Position> {
        let parol_runtime::Location { start, .. } = self.location;
        Loc(syn::BytePos(start))
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

impl<'cx> ProcessToken<'cx> for grammar_trait::Program<'_> {
    type Output = syn::Program<'cx>;

    fn process_token(&self, cx: &'cx Context<'cx>) -> Self::Output {
        let mut definitions = Vec::new();
        let mut program = self;
        loop {
            match program {
                Program::DefinitionEndOfLineProgram(x) => {
                    definitions.push(x.definition.process_token(cx));
                    program = &x.program;
                }
                Program::Definition(x) => {
                    definitions.push(x.definition.process_token(cx));
                    break;
                }
            }
        }
        syn::Program {
            definitions: cx.alloc_definition_slice(definitions),
        }
    }
}

impl<'cx> ProcessToken<'cx> for grammar_trait::Definition<'_> {
    type Output = syn::Definition<'cx>;

    fn process_token(&self, cx: &'cx Context<'cx>) -> Self::Output {
        let kind = self
            .definition_opt
            .as_ref()
            .map(|def| match &*def.definition_opt_group {
                grammar_trait::DefinitionOptGroup::FunctionDef(function_def) => {
                    syn::DefinitionKind::Function(function_def.function_def.process_token(cx))
                }
                grammar_trait::DefinitionOptGroup::ConstantDef(constant_def) => {
                    syn::DefinitionKind::Constant(constant_def.constant_def.process_token(cx))
                }
            });
        let comment = self
            .definition_opt0
            .as_ref()
            .map(|c| c.comment.process_token(cx));
        syn::Definition { kind, comment }
    }
}

impl<'cx> ProcessToken<'cx> for grammar_trait::FunctionDef<'_> {
    type Output = syn::FunctionDef<'cx>;

    fn process_token(&self, cx: &'cx Context<'cx>) -> Self::Output {
        let params = if let Some(param_list) = &self.function_def_opt {
            let mut params = Vec::new();
            let mut current = &param_list.parameter_list;
            loop {
                params.push(current.parameter.process_token(cx));
                let Some(comma_param_list) = &current.parameter_list_opt else {
                    break;
                };
                let Some(next_param_list) = &comma_param_list
                    .comma_parameter_list
                    .comma_parameter_list_opt
                else {
                    break;
                };
                current = &next_param_list.parameter_list;
            }
            params
        } else {
            vec![]
        };

        syn::FunctionDef {
            proc_token: Token![prc](self.prc.wrap()),
            name: self.ident.process_token(cx),
            left_paren: syn::token::OpenParen(self.l_paren.wrap()),
            parameters: cx.alloc_parameter_slice(params),
            right_paren: syn::token::CloseParen(self.r_paren.wrap()),
            return_type: syn::FnReturnTy(self.function_def_opt0.as_ref().map(|return_opt| {
                (
                    Token![->](return_opt.minus_g_t.wrap()),
                    return_opt.path.process_token(cx),
                )
            })),
            body: cx.alloc_block(self.block.process_token(cx)),
        }
    }
}

impl<'cx> ProcessToken<'cx> for grammar_trait::Parameter<'_> {
    type Output = syn::Parameter<'cx>;

    fn process_token(&self, cx: &'cx Context<'cx>) -> Self::Output {
        syn::Parameter {
            name: self.ident.process_token(cx),
            colon: Token![:](self.colon.wrap()),
            ty: self.path.process_token(cx),
        }
    }
}

impl<'cx> ProcessToken<'cx> for grammar_trait::ConstantDef<'_> {
    type Output = syn::ConstantDef<'cx>;

    fn process_token(&self, cx: &'cx Context<'cx>) -> Self::Output {
        syn::ConstantDef {
            const_token: Token![const](self.r#const.wrap()),
            name: self.ident.process_token(cx),
            colon: Token![:](self.colon.wrap()),
            ty: self.path.process_token(cx),
            eq: Token![=](self.equ.wrap()),
            value: self.expr.process_token(cx),
        }
    }
}

impl<'cx> ProcessToken<'cx> for grammar_trait::Scope<'_> {
    type Output = syn::Scope<'cx>;

    fn process_token(&self, cx: &'cx Context<'cx>) -> Self::Output {
        let mut items = Vec::new();
        let mut scope = self;
        items.push(scope.scope_content.process_token(cx));
        while let Some(content) = &scope.scope_opt {
            scope = &content.scope;
            items.push(scope.scope_content.process_token(cx));
        }
        fn non_empty<'cx>(item: &ScopeItem<'cx>) -> bool {
            match item {
                ScopeItem::Row(row) => !row.is_empty(),
                ScopeItem::Block(_) => true,
            }
        }
        let begin = items.iter().position(non_empty).unwrap_or(items.len());
        let end = items
            .iter()
            .rposition(non_empty)
            .map(|i| i + 1)
            .unwrap_or(0);
        let len = end.saturating_sub(begin);
        syn::Scope {
            items: cx.alloc_scope_item_slice(items.into_iter().skip(begin).take(len)),
        }
    }
}

impl<'cx> ProcessToken<'cx> for grammar_trait::ScopeContentOpt<'_> {
    type Output = syn::token::Break<'cx>;

    fn process_token(&self, _cx: &'cx Context<'cx>) -> Self::Output {
        Token![.](self.r#break.r#break.wrap())
    }
}

impl<'cx> ProcessToken<'cx> for grammar_trait::ScopeContent<'_> {
    type Output = syn::ScopeItem<'cx>;

    fn process_token(&self, cx: &'cx Context<'cx>) -> Self::Output {
        let Self {
            scope_content_opt,
            scope_content_opt0,
            scope_content_opt1,
        } = self;
        let breaks = scope_content_opt.process_token(cx);
        let statement = if let Some(scope_content_kind) = scope_content_opt0 {
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
            statement,
            comment,
        }))
    }
}

impl<'cx> ProcessToken<'cx> for grammar_trait::Block<'_> {
    type Output = syn::Block<'cx>;

    fn process_token(&self, cx: &'cx Context<'cx>) -> Self::Output {
        syn::Block {
            left_brace: syn::token::OpenBrace(self.l_brace.wrap()),
            scope: self.scope.process_token(cx),
            right_brace: syn::token::CloseBrace(self.r_brace.wrap()),
        }
    }
}

impl<'cx> ProcessToken<'cx> for grammar_trait::Statement<'_> {
    type Output = syn::Statement<'cx>;

    fn process_token(&self, cx: &'cx Context<'cx>) -> Self::Output {
        let semi = Token![;](self.semi.semi.wrap());
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
                syn::Statement::Let(syn::Let {
                    let_token: Token![let](r#let.wrap()),
                    variable: ident.process_token(cx),
                    eq: Token![=](equ.wrap()),
                    rhs: expr.process_token(cx),
                    semi,
                })
            }
            grammar_trait::StatementKind::Expr(statement_kind_expr) => {
                syn::Statement::Expr(syn::ExprStatement {
                    expr: statement_kind_expr.expr.process_token(cx),
                    semi,
                })
            }
            grammar_trait::StatementKind::ReturnStmt(statement_kind_return_stmt) => {
                syn::Statement::Return(syn::ReturnStmt {
                    return_token: Token![return](
                        statement_kind_return_stmt.return_stmt.return_stmt.wrap(),
                    ),
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
            logical_or_expr,
        }) = &self.set_expr.set_expr_opt
        {
            cx.alloc_expr(syn::ExprKind::Set(syn::Set {
                lhs: self.set_expr.logical_or_expr.process_token(cx),
                colon_eq: Token![:=](colon_equ.wrap()),
                rhs: logical_or_expr.process_token(cx),
            }))
        } else {
            self.set_expr.logical_or_expr.process_token(cx)
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
                        op: syn::BinOp::Or(Token![||](expr.or_or.wrap())),
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
                        op: syn::BinOp::And(Token![&&](expr.amp_amp.wrap())),
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
                op: syn::BinOp::In(Token![in](
                    self.infix_in_expr_opt.as_ref().unwrap().r#in.wrap(),
                )),
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
                tail_with_op: cx.alloc_compare_op_expr_tuple_slice(
                    self.compare_expr_list.iter().map(|expr| {
                        (
                            expr.compare_op.process_token(cx),
                            expr.arithmetic_expr.process_token(cx),
                        )
                    }),
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
                syn::CompareOp::GreaterEq(Token![>=](compare_op_gtequ.g_t_equ.wrap()))
            }
            grammar_trait::CompareOp::LTEqu(compare_op_ltequ) => {
                syn::CompareOp::LessEq(Token![<=](compare_op_ltequ.l_t_equ.wrap()))
            }
            grammar_trait::CompareOp::GT(compare_op_gt) => {
                syn::CompareOp::Greater(Token![>](compare_op_gt.g_t.wrap()))
            }
            grammar_trait::CompareOp::LT(compare_op_lt) => {
                syn::CompareOp::Less(Token![<](compare_op_lt.l_t.wrap()))
            }
            grammar_trait::CompareOp::BangEqu(compare_op_bang_equ) => syn::CompareOp::NotEqual(
                syn::NotEqualToken::BangEqual(Token![!=](compare_op_bang_equ.bang_equ.wrap())),
            ),
            grammar_trait::CompareOp::SlashEqu(compare_op_slash_equ) => syn::CompareOp::NotEqual(
                syn::NotEqualToken::SlashEqual(Token![/=](compare_op_slash_equ.slash_equ.wrap())),
            ),
            grammar_trait::CompareOp::EquEqu(compare_op_equ_equ) => {
                syn::CompareOp::Equal(Token![==](compare_op_equ_equ.equ_equ.wrap()))
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
    type Output = syn::BinOp<'cx, syn::DefaultTypeFamily>;

    fn process_token(&self, _cx: &'cx Context<'cx>) -> Self::Output {
        match self {
            grammar_trait::ArithmeticOp::Plus(token) => {
                syn::BinOp::Add(Token![+](token.plus.wrap()))
            }
            grammar_trait::ArithmeticOp::Minus(token) => {
                syn::BinOp::Sub(Token![-](token.minus.wrap()))
            }
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
    type Output = syn::BinOp<'cx, syn::DefaultTypeFamily>;

    fn process_token(&self, _cx: &'cx Context<'cx>) -> Self::Output {
        match self {
            grammar_trait::FactorOp::Star(token) => syn::BinOp::Mul(Token![*](token.star.wrap())),
            grammar_trait::FactorOp::Slash(token) => syn::BinOp::Div(Token![/](token.slash.wrap())),
            grammar_trait::FactorOp::Percent(token) => {
                syn::BinOp::Mod(Token![%](token.percent.wrap()))
            }
        }
    }
}

impl<'cx> ProcessToken<'cx> for grammar_trait::PrefixExpr<'_> {
    type Output = syn::Expr<'cx>;

    fn process_token(&self, cx: &'cx Context<'cx>) -> Self::Output {
        match self {
            grammar_trait::PrefixExpr::MinusApplyExpr(prefix_expr_minus_apply_expr) => cx
                .alloc_expr(syn::ExprKind::Unary(syn::Unary {
                    op: syn::UnOp::Neg(Token![-](prefix_expr_minus_apply_expr.minus.wrap())),
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
                        qualifs: cx.alloc_qualif_slice(
                            prefix_expr_prefix_expr_list_apply_expr
                                .prefix_expr_list
                                .iter()
                                .map(|qualif| qualif.qualif.process_token(cx)),
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
            grammar_trait::Qualif::Modifier(qualif_mod) => {
                let e = &*qualif_mod.modifier;
                syn::Qualif::Modifier(syn::Modifier {
                    at_token: Token![@](e.at.wrap()),
                    id: e.path.process_token(cx),
                    arg: e.modifier_opt.as_ref().map(|k| syn::ModifierParam {
                        colon_token: Token![:](k.kind_arg.colon.wrap()),
                        value: k.kind_arg.callable.process_token(cx),
                    }),
                })
            }
            grammar_trait::Qualif::DefaultModifier(qualif_default) => {
                let t = &*qualif_default.default_modifier;
                syn::Qualif::DefaultModifier(syn::DefaultModifier {
                    tilde_token: Token![~](t.tilde.wrap()),
                    value: t.path.process_token(cx),
                })
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
                args: cx.alloc_expr_slice(
                    self.apply_expr_list
                        .iter()
                        .map(|arg| arg.atomic_expr.process_token(cx)),
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
                op: match &*lower_prefix.lower_prefix_op {
                    grammar_trait::LowerPrefixOp::Amp(lower_prefix_op_amp) => {
                        syn::UnOp::IdRef(Token![&](lower_prefix_op_amp.amp.wrap()))
                    }
                    grammar_trait::LowerPrefixOp::Dollar(lower_prefix_op_dollar) => {
                        syn::UnOp::Deref(Token![$](lower_prefix_op_dollar.dollar.wrap()))
                    }
                },
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
                    left_paren: syn::token::OpenParen(callable_lparen_expr_rparen.l_paren.wrap()),
                    expr: callable_lparen_expr_rparen.expr.process_token(cx),
                    right_paren: syn::token::CloseParen(callable_lparen_expr_rparen.r_paren.wrap()),
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
            grammar_trait::AtomicExpr::ImportExpr(atomic_expr_import) => {
                let expr = atomic_expr_import
                    .import_expr
                    .lower_prefix_expr
                    .process_token(cx);
                if let Some(import_expr_opt) = &atomic_expr_import.import_expr.import_expr_opt {
                    syn::Expr::import(
                        cx,
                        expr,
                        Token![?](import_expr_opt.quest.wrap()),
                        import_expr_opt.path.process_token(cx),
                    )
                } else {
                    expr
                }
            }
            grammar_trait::AtomicExpr::IfExpr(atomic_expr_if_expr) => {
                let e = &atomic_expr_if_expr.if_expr;
                syn::Expr::if_expr(
                    cx,
                    Token![if](e.r#if.wrap()),
                    e.expr.process_token(cx),
                    cx.alloc_block(e.block.process_token(cx)),
                    e.if_expr_opt.as_ref().map(|el| syn::IfElse {
                        else_kw: Token![else](el.r#else.wrap()),
                        else_clause: cx.alloc_block(el.block.process_token(cx)),
                    }),
                )
            }
        }
    }
}

impl<'cx> ProcessToken<'cx> for grammar_trait::Literal<'_> {
    type Output = syn::Literal<'cx>;

    fn process_token(&self, cx: &'cx Context<'cx>) -> Self::Output {
        match self {
            grammar_trait::Literal::Array(literal_array) => syn::Literal::Array(syn::Array {
                left_bracket: syn::token::OpenSquare(literal_array.array.l_bracket.wrap()),
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
                    cx.alloc_expr_slice(exprs)
                },
                right_bracket: syn::token::CloseSquare(literal_array.array.r_bracket.wrap()),
            }),
            grammar_trait::Literal::String(literal_string) => syn::Literal::String(syn::String {
                raw: cx.alloc_str(literal_string.string.string.text().trim_matches('"')),
                span: literal_string.string.string.wrap().0,
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
                    span: literal_byte_literal.byte_literal.byte_literal.wrap().0,
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
                        .wrap()
                        .0,
                })
            }
            grammar_trait::Literal::Numeric(literal_numeric) => {
                syn::Literal::Numeric(literal_numeric.numeric.process_token(cx))
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
                        .wrap()
                        .0,
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
                            span: $token.wrap().0,
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
        cx.alloc_comment(syn::Comment {
            content: cx.alloc_str(
                self.comment_opt
                    .as_ref()
                    .map(|c| c.comment_content.comment_content.text())
                    .unwrap_or(""),
            ),
            span: if let Some(comment) = &self.comment_opt {
                comment.comment_content.comment_content.wrap().0
            } else {
                let position = syn::BytePos(self.hash.hash.location.end + 1);
                syn::Span {
                    start: position,
                    end: position,
                }
            },
        })
    }
}

impl<'cx> ProcessToken<'cx> for grammar_trait::Ident<'_> {
    type Output = syn::Ident<'cx>;

    fn process_token(&self, cx: &'cx Context<'cx>) -> Self::Output {
        let token = &self.ident;
        syn::Ident {
            raw: cx.alloc_str(token.text()),
            span: token.wrap().0,
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
            segments: cx.alloc_ident_slice(segments),
        }
    }
}
