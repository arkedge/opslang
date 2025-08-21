use super::{
    Apply, Array, BinOp, Binary, Bytes, Comment, Compare, CompareOp, DateTime, Debug, Definition,
    Expr, ExprKind, HexBytes, Ident, If, IfElse, InfixImport, IntegerPrefix, Literal, Numeric,
    NumericKind, NumericSuffix, Parened, Path, PreQualified, Row, Set, String, TypeFamily, UnOp,
    Unary, context, token,
};

impl<'cx, F: TypeFamily<'cx>> Definition<'cx, F> {
    pub fn is_empty(&self) -> bool {
        let Self { kind, comment } = self;
        kind.is_none() && comment.is_none()
    }
}

impl<'cx, F: TypeFamily<'cx>> Row<'cx, F> {
    pub fn is_empty(&self) -> bool {
        let Self {
            breaks,
            statement,
            comment,
        } = self;
        breaks.is_none() && statement.is_none() && comment.is_none()
    }
}

impl<'cx, F: TypeFamily<'cx>> Comment<'cx, F> {
    #[inline]
    pub fn is_meta(&self) -> bool {
        self.content.starts_with('!')
    }
}

impl<'cx, F: TypeFamily<'cx>> std::ops::Deref for Expr<'cx, F> {
    type Target = &'cx ExprKind<'cx, F>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'cx, F: TypeFamily<'cx>> Debug for Expr<'cx, F> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl<'cx, F: TypeFamily<'cx>> PartialEq for Expr<'cx, F> {
    fn eq(&self, other: &Self) -> bool {
        self.0.eq(other.0)
    }
}

impl<'cx, F: TypeFamily<'cx>> Clone for Expr<'cx, F> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<'cx, F: TypeFamily<'cx>> Copy for Expr<'cx, F> {}

impl<'cx, F: TypeFamily<'cx>> Expr<'cx, F> {
    #[inline]
    pub fn from_kind(ctx: &'cx context::Context<'cx, F>, kind: ExprKind<'cx, F>) -> Self {
        ctx.alloc_expr(kind)
    }

    #[inline]
    pub fn ident(ctx: &'cx context::Context<'cx, F>, name: &str, span: F::Span) -> Self
    where
        F: TypeFamily<'cx, Path = Path<'cx, F>>,
    {
        Expr::variable(ctx, Path::single(ctx, name, span))
    }

    #[inline]
    pub fn variable(ctx: &'cx context::Context<'cx, F>, path: F::Path) -> Self {
        ctx.alloc_expr(ExprKind::Variable(path))
    }

    #[inline]
    pub fn literal(ctx: &'cx context::Context<'cx, F>, literal: F::Literal) -> Self {
        ctx.alloc_expr(ExprKind::Literal(literal))
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
        ctx.alloc_expr(ExprKind::Parened(parened))
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
        ctx.alloc_expr(ExprKind::Apply(apply))
    }

    #[inline]
    pub fn binary(
        ctx: &'cx context::Context<'cx, F>,
        lhs: F::Expr,
        op: BinOp<'cx, F>,
        rhs: F::Expr,
    ) -> Self
    where
        F: TypeFamily<'cx, Binary = Binary<'cx, F>>,
    {
        let binary = Binary { lhs, op, rhs };
        ctx.alloc_expr(ExprKind::Binary(binary))
    }

    #[inline]
    pub fn unary(ctx: &'cx context::Context<'cx, F>, op: UnOp<'cx, F>, expr: F::Expr) -> Self
    where
        F: TypeFamily<'cx, Unary = Unary<'cx, F>>,
    {
        let unary = Unary { op, expr };
        ctx.alloc_expr(ExprKind::Unary(unary))
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
        ctx.alloc_expr(ExprKind::Compare(compare))
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
        ctx.alloc_expr(ExprKind::Set(set))
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
        ctx.alloc_expr(ExprKind::InfixImport(import))
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
        ctx.alloc_expr(ExprKind::If(if_expr))
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
        ctx.alloc_expr(ExprKind::If(if_expr))
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
        ctx.alloc_expr(ExprKind::If(if_expr))
    }

    #[inline]
    pub fn qualif(ctx: &'cx context::Context<'cx, F>, qualif: F::Qualif) -> Self {
        ctx.alloc_expr(ExprKind::Qualif(qualif))
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
        ctx.alloc_expr(ExprKind::PreQualified(pre_qualified))
    }
}

impl<'cx, F: TypeFamily<'cx>> Path<'cx, F> {
    pub fn new_unchecked(
        ctx: &'cx context::Context<'cx, F>,
        raw: &str,
        segments: &'cx [Ident<'cx, F>],
    ) -> Self {
        let raw_str = ctx.alloc_str(raw);
        Path {
            raw: raw_str,
            segments,
        }
    }

    pub fn single(ctx: &'cx context::Context<'cx, F>, name: &str, span: F::Span) -> Self {
        assert!(!name.contains('.'));
        let ident = Ident::new(ctx, name, span);
        let segments = ctx.alloc_ident_slice(vec![ident]);
        Path::new_unchecked(ctx, name, segments)
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
        exprs: &'cx [F::Expr],
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

impl<'cx, F: TypeFamily<'cx>> String<'cx, F> {
    pub fn unescape(&self) -> Result<std::string::String, escape8259::UnescapeError> {
        escape8259::unescape(self.raw)
    }
}

impl<'cx, F: TypeFamily<'cx>> Bytes<'cx, F> {
    pub fn as_bytes(&self) -> &'cx [u8] {
        self.raw.as_bytes()
    }
}

impl<'cx, F: TypeFamily<'cx>> HexBytes<'cx, F> {
    pub fn as_bytes(&self) -> Result<Vec<u8>, char> {
        self.raw
            .chars()
            .map(|c| match c {
                c @ '0'..='9' => Ok(c as u8 - b'0'),
                c @ 'a'..='f' => Ok(c as u8 - b'a' + 10),
                c @ 'A'..='F' => Ok(c as u8 - b'A' + 10),
                c => Err(c),
            })
            .collect()
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
    ) -> NumericSuffix<'cx, F> {
        let name_str = ctx.alloc_str(name);
        NumericSuffix(Ident {
            raw: name_str,
            span,
        })
    }
}
