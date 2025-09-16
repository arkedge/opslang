use super::{
    Apply, Array, Binary, Bytes, Cast, Compare, CompareOp, DateTime, Expr, ExprKind, ExprMut,
    HexBytes, Ident, If, IfElse, InfixImport, IntegerPrefix, Literal, Numeric, NumericKind,
    NumericSuffix, Parened, Path, PreQualified, Row, Select, Set, String, ToplevelItem, TypeFamily,
    UnOp, Unary, context, token,
};

impl<'cx, F: TypeFamily<'cx>> Default for ToplevelItem<'cx, F> {
    fn default() -> Self {
        Self {
            kind: Default::default(),
            comment: Default::default(),
        }
    }
}

impl<'cx, F: TypeFamily<'cx>> Default for Row<'cx, F> {
    fn default() -> Self {
        Self {
            breaks: Default::default(),
            statement: Default::default(),
            comment: Default::default(),
        }
    }
}

impl<'cx, F: TypeFamily<'cx>> Expr<'cx, F> {
    #[inline]
    pub fn from_kind(ctx: &'cx context::Context<'cx, F>, kind: ExprKind<'cx, F>) -> Self {
        ctx.alloc_expr(kind)
    }
}

impl<'cx, F: TypeFamily<'cx>> ExprMut<'cx, F> {
    #[inline]
    pub fn from_kind(ctx: &'cx context::Context<'cx, F>, kind: ExprKind<'cx, F>) -> Self {
        ctx.alloc_expr_mut(kind)
    }
}

/// dupricates impl content into two impls.
macro_rules! impl_expr_and_expr_mut {
    ($($tt:tt)*) => {
        impl<'cx, F: TypeFamily<'cx>> Expr<'cx, F> {
            $($tt)*
        }
        impl<'cx, F: TypeFamily<'cx>> ExprMut<'cx, F> {
            $($tt)*
        }
    };
}

impl_expr_and_expr_mut! {
    #[inline]
    pub fn ident(ctx: &'cx context::Context<'cx, F>, name: &str, span: F::Span) -> Self
    where
        F: TypeFamily<'cx, Path = Path<'cx, F>, Ident = Ident<'cx, F>>,
    {
        Self::variable(ctx, Path::single(ctx, name, span))
    }

    #[inline]
    pub fn variable(ctx: &'cx context::Context<'cx, F>, path: F::Path) -> Self {
        Self::from_kind(ctx, ExprKind::Variable(path))
    }

    #[inline]
    pub fn literal(ctx: &'cx context::Context<'cx, F>, literal: F::Literal) -> Self {
        Self::from_kind(ctx, ExprKind::Literal(literal))
    }

    #[inline]
    pub fn parened(
        ctx: &'cx context::Context<'cx, F>,
        left_paren: token::OpenParen<'cx, F>,
        expr: F::Expr,
        right_paren: token::CloseParen<'cx, F>,
    ) -> Self
    where
        F: TypeFamily<'cx, Parened = Parened<'cx, F>>,
    {
        let parened = Parened {
            left_paren,
            expr,
            right_paren,
        };
        Self::from_kind(ctx, ExprKind::Parened(parened))
    }

    #[inline]
    pub fn apply(ctx: &'cx context::Context<'cx, F>, function: F::Expr, args: Vec<F::Expr>) -> Self
    where
        F: TypeFamily<'cx, Apply = Apply<'cx, F>>,
    {
        let apply = Apply {
            function,
            args: ctx.alloc_expr_slice(args),
        };
        Self::from_kind(ctx, ExprKind::Apply(apply))
    }

    #[inline]
    pub fn binary(
        ctx: &'cx context::Context<'cx, F>,
        lhs: F::Expr,
        op: F::BinOp,
        rhs: F::Expr,
    ) -> Self
    where
        F: TypeFamily<'cx, Binary = Binary<'cx, F>>,
    {
        let binary = Binary { lhs, op, rhs };
        Self::from_kind(ctx, ExprKind::Binary(binary))
    }

    #[inline]
    pub fn unary(ctx: &'cx context::Context<'cx, F>, op: UnOp<'cx, F>, expr: F::Expr) -> Self
    where
        F: TypeFamily<'cx, Unary = Unary<'cx, F>>,
    {
        let unary = Unary { op, expr };
        Self::from_kind(ctx, ExprKind::Unary(unary))
    }

    #[inline]
    pub fn compare(
        ctx: &'cx context::Context<'cx, F>,
        head: F::Expr,
        tail_with_op: Vec<(CompareOp<'cx, F>, F::Expr)>,
    ) -> Self
    where
        F: TypeFamily<'cx, Compare = Compare<'cx, F>>,
    {
        let compare = Compare {
            head,
            tail_with_op: ctx.alloc_compare_op_expr_tuple_slice(tail_with_op),
        };
        Self::from_kind(ctx, ExprKind::Compare(compare))
    }

    #[inline]
    pub fn compare_single(
        ctx: &'cx context::Context<'cx, F>,
        lhs: F::Expr,
        op: CompareOp<'cx, F>,
        rhs: F::Expr,
    ) -> Self
    where
        F: TypeFamily<'cx, Compare = Compare<'cx, F>>,
    {
        Self::compare(ctx, lhs, vec![(op, rhs)])
    }

    #[inline]
    pub fn set(
        ctx: &'cx context::Context<'cx, F>,
        lhs: F::Expr,
        colon_eq: token::ColonEq<'cx, F>,
        rhs: F::Expr,
    ) -> Self
    where
        F: TypeFamily<'cx, Set = Set<'cx, F>>,
    {
        let set = Set { lhs, colon_eq, rhs };
        Self::from_kind(ctx, ExprKind::Set(set))
    }

    #[inline]
    pub fn cast(
        ctx: &'cx context::Context<'cx, F>,
        expr: F::Expr,
        as_kw: token::As<'cx, F>,
        ty: F::Ty,
    ) -> Self
    where
        F: TypeFamily<'cx, Cast = Cast<'cx, F>>,
    {
        let cast = Cast { expr, as_kw, ty };
        Self::from_kind(ctx, ExprKind::Cast(cast))
    }

    #[inline]
    pub fn import(
        ctx: &'cx context::Context<'cx, F>,
        file: F::Expr,
        question: token::Question<'cx, F>,
        path: F::Path,
    ) -> Self
    where
        F: TypeFamily<'cx, InfixImport = InfixImport<'cx, F>>,
    {
        let import = InfixImport {
            file,
            question,
            path,
        };
        Self::from_kind(ctx, ExprKind::InfixImport(import))
    }

    #[inline]
    pub fn if_then_else(
        ctx: &'cx context::Context<'cx, F>,
        if_kw: token::If<'cx, F>,
        cond: F::Expr,
        then_clause: F::Block,
        else_kw: token::Else<'cx, F>,
        else_clause: F::Block,
    ) -> Self
    where
        F: TypeFamily<'cx, If = If<'cx, F>>,
    {
        let if_expr = If {
            if_kw,
            cond,
            then_clause,
            else_opt: Some(IfElse {
                else_kw,
                else_clause,
            }),
        };
        Self::from_kind(ctx, ExprKind::If(if_expr))
    }
    #[inline]
    pub fn if_then(
        ctx: &'cx context::Context<'cx, F>,
        if_kw: token::If<'cx, F>,
        cond: F::Expr,
        then_clause: F::Block,
    ) -> Self
    where
        F: TypeFamily<'cx, If = If<'cx, F>>,
    {
        let if_expr = If {
            if_kw,
            cond,
            then_clause,
            else_opt: None,
        };
        Self::from_kind(ctx, ExprKind::If(if_expr))
    }
    #[inline]
    pub fn if_expr(
        ctx: &'cx context::Context<'cx, F>,
        if_kw: token::If<'cx, F>,
        cond: F::Expr,
        then_clause: F::Block,
        else_opt: Option<IfElse<'cx, F>>,
    ) -> Self
    where
        F: TypeFamily<'cx, If = If<'cx, F>>,
    {
        let if_expr = If {
            if_kw,
            cond,
            then_clause,
            else_opt,
        };
        Self::from_kind(ctx, ExprKind::If(if_expr))
    }

    #[inline]
    pub fn select(
        ctx: &'cx context::Context<'cx, F>,
        select_kw: token::Select<'cx, F>,
        left_brace: token::OpenBrace<'cx, F>,
        items: F::SelectItems,
        right_brace: token::CloseBrace<'cx, F>,
    ) -> Self
    where
        F: TypeFamily<'cx, Select = Select<'cx, F>>,
    {
        let select = Select {
            select_kw,
            left_brace,
            items,
            right_brace,
        };
        Self::from_kind(ctx, ExprKind::Select(select))
    }

    #[inline]
    pub fn qualif(ctx: &'cx context::Context<'cx, F>, qualif: F::Qualif) -> Self {
        Self::from_kind(ctx, ExprKind::Qualif(qualif))
    }

    #[inline]
    pub fn pre_qualified(
        ctx: &'cx context::Context<'cx, F>,
        qualifs: Vec<F::Qualif>,
        expr: F::Expr,
    ) -> Self
    where
        F: TypeFamily<'cx, PreQualified = PreQualified<'cx, F>>,
    {
        let pre_qualified = PreQualified {
            qualifs: ctx.alloc_qualif_slice(qualifs),
            expr,
        };
        Self::from_kind(ctx, ExprKind::PreQualified(pre_qualified))
    }
}

impl<'cx, F: TypeFamily<'cx>> Path<'cx, F> {
    pub fn new(segments: &'cx [F::Ident]) -> Self {
        Path { segments }
    }

    pub fn single(ctx: &'cx context::Context<'cx, F>, name: &str, span: F::Span) -> Self
    where
        F: TypeFamily<'cx, Ident = Ident<'cx, F>>,
    {
        assert!(!name.contains('.'));
        let ident = Ident::new(ctx, name, span);
        let segments = ctx.alloc_ident_slice(vec![ident]);
        Path::new(segments)
    }
}

impl<'cx, F: TypeFamily<'cx>> Ident<'cx, F> {
    pub fn new(
        ctx: &'cx context::Context<'cx, impl TypeFamily<'cx>>,
        name: &str,
        span: F::Span,
    ) -> Self {
        let name_str = ctx.alloc_str(name);
        Ident {
            raw: name_str,
            span,
        }
    }
}

impl<'cx, F: TypeFamily<'cx>> Literal<'cx, F> {
    pub fn string(
        ctx: &'cx crate::syntax::v1::context::Context<'cx, F>,
        content: &str,
        span: F::Span,
    ) -> Self
    where
        F: TypeFamily<'cx, String = String<'cx, F>>,
    {
        let string_str = ctx.alloc_str(content);
        Literal::String(String {
            raw: string_str,
            span,
        })
    }

    pub fn bytes(
        ctx: &'cx crate::syntax::v1::context::Context<'cx, F>,
        content: &str,
        span: F::Span,
    ) -> Self
    where
        F: TypeFamily<'cx, Bytes = Bytes<'cx, F>>,
    {
        let bytes_str = ctx.alloc_str(content);
        Literal::Bytes(Bytes {
            raw: bytes_str,
            span,
        })
    }

    pub fn hex_bytes(
        ctx: &'cx crate::syntax::v1::context::Context<'cx, F>,
        content: &str,
        span: F::Span,
    ) -> Self
    where
        F: TypeFamily<'cx, HexBytes = HexBytes<'cx, F>>,
    {
        let hex_str = ctx.alloc_str(content);
        Literal::HexBytes(HexBytes { raw: hex_str, span })
    }

    pub fn numeric(numeric: F::Numeric) -> Self
    where
        F: TypeFamily<'cx, Numeric = Numeric<'cx>>,
    {
        Literal::Numeric(numeric)
    }

    pub fn date_time(
        ctx: &'cx crate::syntax::v1::context::Context<'cx, F>,
        content: &str,
        span: F::Span,
    ) -> Self
    where
        F: TypeFamily<'cx, DateTime = DateTime<'cx, F>>,
    {
        let date_str = ctx.alloc_str(content);
        Literal::DateTime(DateTime {
            raw: date_str,
            span,
        })
    }

    pub fn array(
        left_bracket: token::OpenSquare<'cx, F>,
        exprs: F::Exprs,
        right_bracket: token::CloseSquare<'cx, F>,
    ) -> Self
    where
        F: TypeFamily<'cx, Array = Array<'cx, F>>,
    {
        Literal::Array(Array {
            left_bracket,
            exprs,
            right_bracket,
        })
    }
}

impl<'cx, F: TypeFamily<'cx>> Numeric<'cx, F> {
    #[inline]
    pub fn integer(
        ctx: &'cx crate::syntax::v1::context::Context<'cx, impl TypeFamily<'cx>>,
        raw: &str,
        prefix: IntegerPrefix,
        suffix: Option<NumericSuffix<'cx, F>>,
    ) -> Self {
        let raw_str = ctx.alloc_str(raw);
        Numeric {
            raw: raw_str,
            kind: NumericKind::Integer(prefix),
            suffix,
        }
    }

    #[inline]
    pub fn float(
        ctx: &'cx crate::syntax::v1::context::Context<'cx, F>,
        raw: &str,
        suffix: Option<NumericSuffix<'cx, F>>,
    ) -> Self {
        let raw_str = ctx.alloc_str(raw);
        Numeric {
            raw: raw_str,
            kind: NumericKind::Float,
            suffix,
        }
    }

    #[inline]
    pub fn suffix(
        ctx: &'cx crate::syntax::v1::context::Context<'cx, F>,
        name: &str,
        span: F::Span,
    ) -> NumericSuffix<'cx, F>
    where
        F: TypeFamily<'cx, Ident = Ident<'cx, F>>,
    {
        let name_str = ctx.alloc_str(name);
        NumericSuffix(Ident {
            raw: name_str,
            span,
        })
    }
}
