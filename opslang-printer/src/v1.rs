use std::fmt::{self, Write};

mod prec;
mod strat_helper;

use crate::{
    CommentAligned, CommentGrouping, CommentPosition, Indent, Naive, Newline, PrettyPrint,
    PrintOptions, Strategy,
};
use opslang_ast::syntax::v1::*;
use prec::{HasPrecedence, Precedence};

#[derive(Debug, Clone, Copy, Default)]
pub struct Token;

impl Token {
    #[inline]
    /// Write a token to the writer.
    pub fn write<T: token::Token>(self, token: T, writer: &mut impl Write) -> fmt::Result {
        let _ = token;
        writer.write_str(T::REPR)
    }
}

/// Macro to define a trait alias for TypeFamily with specific associated types.
macro_rules! define_trait_alias {
    (
        $(#[$attr:meta])*
        pub trait $trait_name:ident<$lifetime:lifetime> = $base_trait:path
    ) => {
        pub trait $trait_name<$lifetime>: $base_trait {}

        impl<$lifetime, F: $base_trait> $trait_name<$lifetime> for F {}
    };
}

define_trait_alias!(
    /// Alias for printable family used in this module.
    ///
    /// Types that implement this trait looks very similar to [`DefaultTypeFamily`], but
    /// accepts any `Span` and `Position` types, which allows for more flexibility in
    /// printing operations.
    ///
    /// **IMPORTANT**: When modifying [`TypeFamily`] trait in opslang-ast, you must also
    /// update this trait to include the same associated types to maintain compatibility.
    /// This trait must be kept in sync with [`DefaultTypeFamily`] associated type definitions.
    pub trait PrintableFamily<'cx> = TypeFamily<
        'cx,
        Comment = &'cx Comment<'cx, Self>,
        ToplevelItem = ToplevelItem<'cx, Self>,
        Row = &'cx Row<'cx, Self>,
        Statement = Statement<'cx, Self>,
        Block = &'cx Block<'cx, Self>,
        Scope = Scope<'cx, Self>,
        ReturnStmt = ReturnStmt<'cx, Self>,
        Ident = Ident<'cx, Self>,
        Path = Path<'cx, Self>,
        Ty = Path<'cx, Self>,
        Expr = Expr<'cx, Self>,
        Exprs = &'cx [Expr<'cx, Self>],
        Qualif = Qualif<'cx, Self>,
        PreQualified = PreQualified<'cx, Self>,
        Parened = Parened<'cx, Self>,
        Literal = Literal<'cx, Self>,
        Array = Array<'cx, Self>,
        String = String<'cx, Self>,
        Bytes = Bytes<'cx, Self>,
        HexBytes = HexBytes<'cx, Self>,
        DateTime = DateTime<'cx, Self>,
        Numeric = Numeric<'cx, Self>,
        Apply = Apply<'cx, Self>,
        Unary = Unary<'cx, Self>,
        Binary = Binary<'cx, Self>,
        BinOp = BinOp<'cx, Self>,
        Compare = Compare<'cx, Self>,
        Set = Set<'cx, Self>,
        Cast = Cast<'cx, Self>,
        InfixImport = InfixImport<'cx, Self>,
        If = If<'cx, Self>,
        Select = Select<'cx, Self>,
        SelectItems = &'cx [SelectItem<'cx, Self>],
        FunctionDef = FunctionDef<'cx, Self>,
        ConstantDef = ConstantDef<'cx, Self>,
    >
);

#[doc(hidden)]
const _: () = {
    /// Assert that `opslang_ast::DefaultTypeFamily: for<'cx> PrintableFamily<'cx>`,
    /// which means `opslang-printer` can print AST.
    const fn check_impl<T: for<'cx> PrintableFamily<'cx>>() {}
    check_impl::<opslang_ast::DefaultTypeFamily>();
};

impl<'cx, F: PrintableFamily<'cx>> PrettyPrint<Naive> for Program<'cx, F> {
    fn pretty_print(&self, writer: &mut impl Write, options: &PrintOptions<Naive>) -> fmt::Result {
        for (i, definition) in self.toplevel_items.iter().enumerate() {
            if i > 0 {
                Newline.write(writer, options)?;
            }
            PrettyPrint::<Naive>::pretty_print(definition, writer, options)?;
        }
        Ok(())
    }
}

impl<'cx, F: PrintableFamily<'cx>> PrettyPrint<CommentAligned> for Program<'cx, F> {
    fn pretty_print(
        &self,
        writer: &mut impl Write,
        options: &PrintOptions<CommentAligned>,
    ) -> fmt::Result {
        for (i, definition) in self.toplevel_items.iter().enumerate() {
            if i > 0 {
                Newline.write(writer, options)?;
            }
            PrettyPrint::<CommentAligned>::pretty_print(definition, writer, options)?;
        }
        Ok(())
    }
}

impl<'cx, S: Strategy, F: PrintableFamily<'cx>> PrettyPrint<S> for ToplevelItem<'cx, F>
where
    Block<'cx, F>: PrettyPrint<S>,
    ExprKind<'cx, F>: PrettyPrint<S>,
{
    fn pretty_print(&self, writer: &mut impl Write, options: &PrintOptions<S>) -> fmt::Result {
        if let Some(kind) = &self.kind {
            match kind {
                DefinitionKind::Function(func_def) => {
                    PrettyPrint::<S>::pretty_print(func_def, writer, options)?;
                }
                DefinitionKind::Constant(const_def) => {
                    PrettyPrint::<S>::pretty_print(const_def, writer, options)?;
                }
            }
        }
        if let Some(comment) = self.comment {
            PrettyPrint::<S>::pretty_print(comment, writer, options)?;
        }
        Ok(())
    }
}

impl<'cx, S: Strategy, F: PrintableFamily<'cx>> PrettyPrint<S> for FunctionDef<'cx, F>
where
    Block<'cx, F>: PrettyPrint<S>,
{
    fn pretty_print(&self, writer: &mut impl Write, options: &PrintOptions<S>) -> fmt::Result {
        Token.write(self.prc_token, writer)?;
        writer.write_str(" ")?;
        PrettyPrint::<S>::pretty_print(&self.name, writer, options)?;
        Token.write(self.left_paren, writer)?;

        for (i, param) in self.parameters.iter().enumerate() {
            if i > 0 {
                writer.write_str(", ")?;
            }
            PrettyPrint::<S>::pretty_print(param, writer, options)?;
        }

        Token.write(self.right_paren, writer)?;
        writer.write_str(" ")?;
        PrettyPrint::<S>::pretty_print(self.body, writer, options)
    }
}

impl<'cx, S: Strategy, F: PrintableFamily<'cx>> PrettyPrint<S> for Parameter<'cx, F> {
    fn pretty_print(&self, writer: &mut impl Write, options: &PrintOptions<S>) -> fmt::Result {
        PrettyPrint::<S>::pretty_print(&self.name, writer, options)?;
        Token.write(self.colon, writer)?;
        writer.write_str(" ")?;
        PrettyPrint::<S>::pretty_print(&self.ty, writer, options)
    }
}

impl<'cx, S: Strategy, F: PrintableFamily<'cx>> PrettyPrint<S> for ConstantDef<'cx, F>
where
    ExprKind<'cx, F>: PrettyPrint<S>,
{
    fn pretty_print(&self, writer: &mut impl Write, options: &PrintOptions<S>) -> fmt::Result {
        Token.write(self.const_token, writer)?;
        writer.write_str(" ")?;
        PrettyPrint::<S>::pretty_print(&self.name, writer, options)?;
        Token.write(self.colon, writer)?;
        writer.write_str(" ")?;
        PrettyPrint::<S>::pretty_print(&self.ty, writer, options)?;
        writer.write_str(" ")?;
        Token.write(self.eq, writer)?;
        writer.write_str(" ")?;
        PrettyPrint::<S>::pretty_print(&self.value, writer, options)
    }
}

impl<'cx, F: PrintableFamily<'cx>> PrettyPrint<Naive> for Scope<'cx, F> {
    fn pretty_print(&self, writer: &mut impl Write, options: &PrintOptions<Naive>) -> fmt::Result {
        for item in self.items {
            match item {
                ScopeItem::Row(row) => PrettyPrint::<Naive>::pretty_print(*row, writer, options)?,
                ScopeItem::Block(block) => {
                    PrettyPrint::<Naive>::pretty_print(*block, writer, options)?
                }
            };
            Newline.write(writer, options)?;
        }
        Ok(())
    }
}

impl<'cx, F: PrintableFamily<'cx>> PrettyPrint<CommentAligned> for Scope<'cx, F> {
    fn pretty_print(
        &self,
        writer: &mut impl Write,
        options: &PrintOptions<CommentAligned>,
    ) -> fmt::Result {
        match options.comment_alignment.grouping {
            CommentGrouping::Consecutive => {
                strat_helper::pretty_print_consecutive(self, writer, options)
            }
            CommentGrouping::PerBlock => {
                strat_helper::pretty_print_per_block(self, writer, options)
            }
        }
    }
}

// Strategy-specific implementations for Row (where comment formatting matters)

fn indent_break<'cx, S: Strategy, F: PrintableFamily<'cx>>(
    row: &Row<'cx, F>,
    writer: &mut impl Write,
    options: &PrintOptions<S>,
) -> fmt::Result {
    if row.breaks.is_some() {
        let mut indent_str = options.current_indent();
        if let Some(c) = indent_str.pop() {
            writer.write_str(&indent_str)?;
            writer.write_char('.')?;
            if c == '\t' {
                // assuming tab size >= 2
                writer.write_char('\t')?;
            }
        } else {
            writer.write_char('.')?;
        }
    } else if options.reserve_for_break && !row.is_empty() && options.current_indent().is_empty() {
        writer.write_char(' ')?;
    } else {
        Indent.write(writer, options)?;
    }
    Ok(())
}

impl<'cx, F: PrintableFamily<'cx>> PrettyPrint<Naive> for Row<'cx, F> {
    fn pretty_print(&self, writer: &mut impl Write, options: &PrintOptions<Naive>) -> fmt::Result {
        // Handle breaks
        indent_break(self, writer, options)?;

        // Handle content
        if let Some(content) = &self.statement {
            PrettyPrint::<Naive>::pretty_print(content, writer, options)?;
        }

        // Handle comment (naive approach: just add space)
        if let Some(comment) = &self.comment {
            if self.statement.is_some() {
                writer.write_str(" ")?;
            }
            PrettyPrint::<Naive>::pretty_print(*comment, writer, options)?;
        }

        Ok(())
    }
}

impl<'cx, S: Strategy, F: PrintableFamily<'cx>> PrettyPrint<S> for Comment<'cx, F> {
    fn pretty_print(&self, writer: &mut impl Write, _options: &PrintOptions<S>) -> fmt::Result {
        writer.write_str("#")?;
        if !self.content.trim_end().is_empty() {
            writer.write_str(self.content)
        } else {
            Ok(())
        }
    }
}

impl<'cx, S: Strategy, F: PrintableFamily<'cx>> PrettyPrint<S> for Block<'cx, F>
where
    Scope<'cx, F>: PrettyPrint<S>,
{
    fn pretty_print(&self, writer: &mut impl Write, options: &PrintOptions<S>) -> fmt::Result {
        Token.write(self.left_brace, writer)?;
        Newline.write(writer, options)?;

        let nested_options = options.with_increased_indent();
        PrettyPrint::<S>::pretty_print(&self.scope, writer, &nested_options)?;

        Indent.write(writer, options)?;
        Token.write(self.right_brace, writer)
    }
}

impl<'cx, S: Strategy, F: PrintableFamily<'cx>> PrettyPrint<S> for Statement<'cx, F>
where
    ExprKind<'cx, F>: PrettyPrint<S>,
{
    fn pretty_print(&self, writer: &mut impl Write, options: &PrintOptions<S>) -> fmt::Result {
        match self {
            Statement::Let(let_stmt) => PrettyPrint::<S>::pretty_print(let_stmt, writer, options),
            Statement::Expr(expr_stmt) => {
                PrettyPrint::<S>::pretty_print(expr_stmt, writer, options)
            }
            Statement::Return(return_stmt) => {
                PrettyPrint::<S>::pretty_print(return_stmt, writer, options)
            }
        }
    }
}

impl<'cx, S: Strategy, F: PrintableFamily<'cx>> PrettyPrint<S> for Let<'cx, F>
where
    ExprKind<'cx, F>: PrettyPrint<S>,
{
    fn pretty_print(&self, writer: &mut impl Write, options: &PrintOptions<S>) -> fmt::Result {
        writer.write_str("let ")?;
        PrettyPrint::<S>::pretty_print(&self.variable, writer, options)?;
        writer.write_str(" = ")?;
        PrettyPrint::<S>::pretty_print(&self.rhs, writer, options)?;
        Token.write(self.semi, writer)
    }
}

impl<'cx, S: Strategy, F: PrintableFamily<'cx>> PrettyPrint<S> for ExprStatement<'cx, F>
where
    ExprKind<'cx, F>: PrettyPrint<S>,
{
    fn pretty_print(&self, writer: &mut impl Write, options: &PrintOptions<S>) -> fmt::Result {
        PrettyPrint::<S>::pretty_print(&self.expr, writer, options)?;
        Token.write(self.semi, writer)
    }
}

impl<'cx, S: Strategy, F: PrintableFamily<'cx>> PrettyPrint<S> for ReturnStmt<'cx, F> {
    fn pretty_print(&self, writer: &mut impl Write, _options: &PrintOptions<S>) -> fmt::Result {
        Token.write(self.return_token, writer)?;
        Token.write(self.semi, writer)
    }
}

impl<'cx, S: Strategy, F: PrintableFamily<'cx>> PrettyPrint<S> for Expr<'cx, F>
where
    ExprKind<'cx, F>: PrettyPrint<S>,
{
    fn pretty_print(&self, writer: &mut impl Write, options: &PrintOptions<S>) -> fmt::Result {
        self.0.pretty_print(writer, options)
    }
}

impl<'cx, S: Strategy, F: PrintableFamily<'cx>> PrettyPrint<S> for ExprKind<'cx, F>
where
    Block<'cx, F>: PrettyPrint<S>,
{
    fn pretty_print(&self, writer: &mut impl Write, options: &PrintOptions<S>) -> fmt::Result {
        match self {
            ExprKind::Variable(path) => PrettyPrint::<S>::pretty_print(path, writer, options),
            ExprKind::Literal(literal) => PrettyPrint::<S>::pretty_print(literal, writer, options),
            ExprKind::Parened(parened) => PrettyPrint::<S>::pretty_print(parened, writer, options),
            ExprKind::Qualif(qualif) => PrettyPrint::<S>::pretty_print(qualif, writer, options),
            ExprKind::PreQualified(pre_qual) => {
                PrettyPrint::<S>::pretty_print(pre_qual, writer, options)
            }
            ExprKind::Unary(unary) => PrettyPrint::<S>::pretty_print(unary, writer, options),
            ExprKind::Compare(compare) => PrettyPrint::<S>::pretty_print(compare, writer, options),
            ExprKind::Binary(binary) => PrettyPrint::<S>::pretty_print(binary, writer, options),
            ExprKind::Apply(apply) => PrettyPrint::<S>::pretty_print(apply, writer, options),
            ExprKind::Set(set) => PrettyPrint::<S>::pretty_print(set, writer, options),
            ExprKind::Cast(cast) => PrettyPrint::<S>::pretty_print(cast, writer, options),
            ExprKind::InfixImport(import) => {
                PrettyPrint::<S>::pretty_print(import, writer, options)
            }
            ExprKind::If(if_expr) => PrettyPrint::<S>::pretty_print(if_expr, writer, options),
            ExprKind::Select(select_expr) => {
                PrettyPrint::<S>::pretty_print(select_expr, writer, options)
            }
        }
    }
}

impl<'cx, S: Strategy, F: PrintableFamily<'cx>> PrettyPrint<S> for Path<'cx, F> {
    fn pretty_print(&self, writer: &mut impl Write, options: &PrintOptions<S>) -> fmt::Result {
        for (i, ident) in self.segments.iter().enumerate() {
            if i > 0 {
                writer.write_char('.')?;
            }
            PrettyPrint::<S>::pretty_print(ident, writer, options)?;
        }
        Ok(())
    }
}

impl<'cx, S: Strategy, F: PrintableFamily<'cx>> PrettyPrint<S> for Ident<'cx, F> {
    fn pretty_print(&self, writer: &mut impl Write, _options: &PrintOptions<S>) -> fmt::Result {
        writer.write_str(self.raw)
    }
}

impl<'cx, S: Strategy, F: PrintableFamily<'cx>> PrettyPrint<S> for Literal<'cx, F>
where
    ExprKind<'cx, F>: PrettyPrint<S>,
{
    fn pretty_print(&self, writer: &mut impl Write, options: &PrintOptions<S>) -> fmt::Result {
        match self {
            Literal::Array(array) => PrettyPrint::<S>::pretty_print(array, writer, options),
            Literal::String(string) => PrettyPrint::<S>::pretty_print(string, writer, options),
            Literal::Bytes(bytes) => PrettyPrint::<S>::pretty_print(bytes, writer, options),
            Literal::HexBytes(hex_bytes) => {
                PrettyPrint::<S>::pretty_print(hex_bytes, writer, options)
            }
            Literal::Numeric(numeric) => PrettyPrint::<S>::pretty_print(numeric, writer, options),
            Literal::DateTime(datetime) => {
                PrettyPrint::<S>::pretty_print(datetime, writer, options)
            }
        }
    }
}

impl<'cx, S: Strategy, F: PrintableFamily<'cx>> PrettyPrint<S> for Array<'cx, F>
where
    ExprKind<'cx, F>: PrettyPrint<S>,
{
    fn pretty_print(&self, writer: &mut impl Write, options: &PrintOptions<S>) -> fmt::Result {
        Token.write(self.left_bracket, writer)?;
        for (i, expr) in self.exprs.iter().enumerate() {
            if i > 0 {
                writer.write_str(", ")?;
            }
            PrettyPrint::<S>::pretty_print(expr, writer, options)?;
        }
        Token.write(self.right_bracket, writer)
    }
}

impl<'cx, S: Strategy, F: PrintableFamily<'cx>> PrettyPrint<S> for String<'cx, F> {
    fn pretty_print(&self, writer: &mut impl Write, _options: &PrintOptions<S>) -> fmt::Result {
        writer.write_str("\"")?;
        writer.write_str(self.raw)?;
        writer.write_str("\"")
    }
}

impl<'cx, S: Strategy, F: PrintableFamily<'cx>> PrettyPrint<S> for Bytes<'cx, F> {
    fn pretty_print(&self, writer: &mut impl Write, _options: &PrintOptions<S>) -> fmt::Result {
        writer.write_str("b\"")?;
        writer.write_str(self.raw)?;
        writer.write_str("\"")
    }
}

impl<'cx, S: Strategy, F: PrintableFamily<'cx>> PrettyPrint<S> for HexBytes<'cx, F> {
    fn pretty_print(&self, writer: &mut impl Write, _options: &PrintOptions<S>) -> fmt::Result {
        writer.write_str("bx\"")?;
        writer.write_str(self.raw)?;
        writer.write_str("\"")
    }
}

impl<'cx, S: Strategy, F: PrintableFamily<'cx>> PrettyPrint<S> for Numeric<'cx, F>
where
    Ident<'cx, F>: PrettyPrint<S>,
{
    fn pretty_print(&self, writer: &mut impl Write, options: &PrintOptions<S>) -> fmt::Result {
        match self.kind {
            NumericKind::Integer(IntegerPrefix::Hexadecimal) => writer.write_str("0x")?,
            NumericKind::Integer(IntegerPrefix::Octal) => writer.write_str("0o")?,
            NumericKind::Integer(IntegerPrefix::Binary) => writer.write_str("0b")?,
            NumericKind::Integer(IntegerPrefix::None) | NumericKind::Float => (),
        };
        writer.write_str(self.raw)?;
        if let Some(suffix) = &self.suffix {
            PrettyPrint::<S>::pretty_print(&suffix.0, writer, options)?;
        }
        Ok(())
    }
}

impl<'cx, S: Strategy, F: PrintableFamily<'cx>> PrettyPrint<S> for DateTime<'cx, F> {
    fn pretty_print(&self, writer: &mut impl Write, _options: &PrintOptions<S>) -> fmt::Result {
        writer.write_str(self.raw)
    }
}

impl<'cx, S: Strategy, F: PrintableFamily<'cx>> PrettyPrint<S> for Parened<'cx, F>
where
    ExprKind<'cx, F>: PrettyPrint<S>,
{
    fn pretty_print(&self, writer: &mut impl Write, options: &PrintOptions<S>) -> fmt::Result {
        writer.write_str("(")?;
        PrettyPrint::<S>::pretty_print(&self.expr, writer, options)?;
        writer.write_str(")")
    }
}

impl<'cx, S: Strategy, F: PrintableFamily<'cx>> PrettyPrint<S> for Qualif<'cx, F>
where
    ExprKind<'cx, F>: PrettyPrint<S>,
    DefaultModifier<'cx, F>: PrettyPrint<S>,
{
    fn pretty_print(&self, writer: &mut impl Write, options: &PrintOptions<S>) -> fmt::Result {
        match self {
            Qualif::Modifier(Modifier { at_token, id, arg }) => {
                Token.write(at_token, writer)?;
                PrettyPrint::<S>::pretty_print(id, writer, options)?;
                if let Some(expr) = arg {
                    Token.write(expr.colon_token, writer)?;
                    Precedence::ATOMIC.write_with_parens(&expr.value, writer, options)?;
                }
                Ok(())
            }
            Qualif::DefaultModifier(exec_comp) => {
                PrettyPrint::<S>::pretty_print(exec_comp, writer, options)
            }
        }
    }
}

impl<'cx, S: Strategy, F: PrintableFamily<'cx>> PrettyPrint<S> for DefaultModifier<'cx, F>
where
    Path<'cx, F>: PrettyPrint<S>,
{
    fn pretty_print(&self, writer: &mut impl Write, options: &PrintOptions<S>) -> fmt::Result {
        Token.write(self.tilde_token, writer)?;
        PrettyPrint::<S>::pretty_print(&self.value, writer, options)
    }
}

impl<'cx, S: Strategy, F: PrintableFamily<'cx>> PrettyPrint<S> for PreQualified<'cx, F>
where
    Qualif<'cx, F>: PrettyPrint<S>,
    ExprKind<'cx, F>: PrettyPrint<S>,
{
    fn pretty_print(&self, writer: &mut impl Write, options: &PrintOptions<S>) -> fmt::Result {
        for qualif in self.qualifs {
            PrettyPrint::<S>::pretty_print(qualif, writer, options)?;
            writer.write_str(" ")?;
        }
        PrettyPrint::<S>::pretty_print(&self.expr, writer, options)
    }
}

impl<'cx, S: Strategy, F: PrintableFamily<'cx>> PrettyPrint<S> for Unary<'cx, F>
where
    ExprKind<'cx, F>: PrettyPrint<S>,
{
    fn pretty_print(&self, writer: &mut impl Write, options: &PrintOptions<S>) -> fmt::Result {
        match self.op {
            UnOp::Neg(t) => Token.write(t, writer)?,
            UnOp::IdRef(t) => Token.write(t, writer)?,
            UnOp::Deref(t) => Token.write(t, writer)?,
        };
        self.op
            .precedence()
            .write_with_parens(self.expr.0, writer, options)
    }
}

impl<'cx, S: Strategy, F: PrintableFamily<'cx>> PrettyPrint<S> for Compare<'cx, F>
where
    ExprKind<'cx, F>: PrettyPrint<S>,
{
    fn pretty_print(&self, writer: &mut impl Write, options: &PrintOptions<S>) -> fmt::Result {
        Precedence::COMPARE.write_with_parens(self.head.0, writer, options)?;
        for (op, expr) in self.tail_with_op {
            writer.write_str(" ")?;
            match op {
                CompareOp::GreaterEq(t) => Token.write(t, writer),
                CompareOp::LessEq(t) => Token.write(t, writer),
                CompareOp::Greater(t) => Token.write(t, writer),
                CompareOp::Less(t) => Token.write(t, writer),
                CompareOp::NotEqual(NotEqualToken::BangEqual(t)) => Token.write(t, writer),
                CompareOp::NotEqual(NotEqualToken::SlashEqual(t)) => Token.write(t, writer),
                CompareOp::Equal(t) => Token.write(t, writer),
            }?;
            writer.write_str(" ")?;
            Precedence::COMPARE.write_with_parens(expr.0, writer, options)?;
        }
        Ok(())
    }
}

impl<'cx, S: Strategy, F: PrintableFamily<'cx>> PrettyPrint<S> for Binary<'cx, F>
where
    ExprKind<'cx, F>: PrettyPrint<S>,
{
    fn pretty_print(&self, writer: &mut impl Write, options: &PrintOptions<S>) -> fmt::Result {
        let op_prec = self.op.precedence();

        op_prec.write_with_parens(self.lhs.0, writer, options)?;
        writer.write_str(" ")?;
        match self.op {
            BinOp::And(_) => writer.write_str("&&")?,
            BinOp::Or(_) => writer.write_str("||")?,
            BinOp::Mul(_) => writer.write_str("*")?,
            BinOp::Div(_) => writer.write_str("/")?,
            BinOp::Mod(_) => writer.write_str("%")?,
            BinOp::Add(_) => writer.write_str("+")?,
            BinOp::Sub(_) => writer.write_str("-")?,
        }
        writer.write_str(" ")?;
        op_prec.write_with_parens(self.rhs.0, writer, options)
    }
}

impl<'cx, S: Strategy, F: PrintableFamily<'cx>> PrettyPrint<S> for Apply<'cx, F>
where
    ExprKind<'cx, F>: PrettyPrint<S>,
{
    fn pretty_print(&self, writer: &mut impl Write, options: &PrintOptions<S>) -> fmt::Result {
        // OCaml-like function application: f x y instead of f(x, y)
        Precedence::APPLY.write_with_parens(self.function.0, writer, options)?;
        for arg in self.args.iter() {
            writer.write_str(" ")?;
            Precedence::APPLY.write_with_parens(arg.0, writer, options)?;
        }
        Ok(())
    }
}

impl<'cx, S: Strategy, F: PrintableFamily<'cx>> PrettyPrint<S> for Set<'cx, F>
where
    ExprKind<'cx, F>: PrettyPrint<S>,
{
    fn pretty_print(&self, writer: &mut impl Write, options: &PrintOptions<S>) -> fmt::Result {
        Precedence::SET.write_with_parens(self.lhs.0, writer, options)?;
        writer.write_str(" ")?;
        Token.write(self.colon_eq, writer)?;
        writer.write_str(" ")?;
        Precedence::SET.write_with_parens(self.rhs.0, writer, options)
    }
}

impl<'cx, S: Strategy, F: PrintableFamily<'cx>> PrettyPrint<S> for Cast<'cx, F>
where
    ExprKind<'cx, F>: PrettyPrint<S>,
{
    fn pretty_print(&self, writer: &mut impl Write, options: &PrintOptions<S>) -> fmt::Result {
        Precedence::CAST.write_with_parens(self.expr.0, writer, options)?;
        writer.write_str(" ")?;
        Token.write(self.as_kw, writer)?;
        writer.write_str(" ")?;
        PrettyPrint::<S>::pretty_print(&self.ty, writer, options)
    }
}

impl<'cx, S: Strategy, F: PrintableFamily<'cx>> PrettyPrint<S> for InfixImport<'cx, F>
where
    ExprKind<'cx, F>: PrettyPrint<S>,
{
    fn pretty_print(&self, writer: &mut impl Write, options: &PrintOptions<S>) -> fmt::Result {
        Precedence::ATOMIC.write_with_parens(self.file.0, writer, options)?;
        Token.write(self.question, writer)?;
        PrettyPrint::<S>::pretty_print(&self.path, writer, options)
    }
}

impl<'cx, S: Strategy, F: PrintableFamily<'cx>> PrettyPrint<S> for If<'cx, F>
where
    Block<'cx, F>: PrettyPrint<S>,
{
    fn pretty_print(&self, writer: &mut impl Write, options: &PrintOptions<S>) -> fmt::Result {
        Token.write(self.if_kw, writer)?;
        PrettyPrint::<S>::pretty_print(self.cond.0, writer, options)?;
        PrettyPrint::<S>::pretty_print(self.then_clause, writer, options)?;
        if let Some(if_else) = &self.else_opt {
            Token.write(if_else.else_kw, writer)?;
            PrettyPrint::<S>::pretty_print(if_else.else_clause, writer, options)
        } else {
            Ok(())
        }
    }
}

impl<'cx, S: Strategy, F: PrintableFamily<'cx>> PrettyPrint<S> for Select<'cx, F>
where
    Block<'cx, F>: PrettyPrint<S>,
    ExprKind<'cx, F>: PrettyPrint<S>,
{
    fn pretty_print(&self, writer: &mut impl Write, options: &PrintOptions<S>) -> fmt::Result {
        Token.write(self.select_kw, writer)?;
        writer.write_str(" ")?;
        Token.write(self.left_brace, writer)?;
        Newline.write(writer, options)?;

        let nested_options = options.with_increased_indent();
        for item in self.items {
            Indent.write(writer, &nested_options)?;
            PrettyPrint::<S>::pretty_print(item, writer, &nested_options)?;
            Newline.write(writer, &nested_options)?;
        }

        Indent.write(writer, options)?;
        Token.write(self.right_brace, writer)
    }
}

impl<'cx, S: Strategy, F: PrintableFamily<'cx>> PrettyPrint<S> for SelectItem<'cx, F>
where
    Block<'cx, F>: PrettyPrint<S>,
    ExprKind<'cx, F>: PrettyPrint<S>,
{
    fn pretty_print(&self, writer: &mut impl Write, options: &PrintOptions<S>) -> fmt::Result {
        PrettyPrint::<S>::pretty_print(&self.expr, writer, options)?;
        writer.write_str(" ")?;
        Token.write(self.arrow, writer)?;
        writer.write_str(" ")?;
        PrettyPrint::<S>::pretty_print(self.body, writer, options)
    }
}
