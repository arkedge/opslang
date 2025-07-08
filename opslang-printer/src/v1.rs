use std::fmt::{self, Write};

use crate::{CommentAligned, Indent, Naive, Newline, PrettyPrint, PrintOptions, Strategy};
use opslang_ast::syntax::v1::*;

// Generic implementations for elements that don't depend on strategy

impl<'cx, S: Strategy> PrettyPrint<S> for Program<'cx>
where
    Scope<'cx>: PrettyPrint<S>,
{
    fn pretty_print(&self, writer: &mut impl Write, options: &PrintOptions<S>) -> fmt::Result {
        PrettyPrint::<S>::pretty_print(&self.content, writer, options)
    }
}

impl<'cx, S: Strategy> PrettyPrint<S> for Scope<'cx>
where
    ScopeItem<'cx>: PrettyPrint<S>,
{
    fn pretty_print(&self, writer: &mut impl Write, options: &PrintOptions<S>) -> fmt::Result {
        for (i, item) in self.items.iter().enumerate() {
            if i > 0 {
                Newline.write(writer, options)?;
            }
            PrettyPrint::<S>::pretty_print(item, writer, options)?;
        }
        Ok(())
    }
}

impl<'cx, S: Strategy> PrettyPrint<S> for ScopeItem<'cx>
where
    Row<'cx>: PrettyPrint<S>,
    Block<'cx>: PrettyPrint<S>,
{
    fn pretty_print(&self, writer: &mut impl Write, options: &PrintOptions<S>) -> fmt::Result {
        match self {
            ScopeItem::Row(row) => PrettyPrint::<S>::pretty_print(*row, writer, options),
            ScopeItem::Block(block) => PrettyPrint::<S>::pretty_print(*block, writer, options),
        }
    }
}

// Strategy-specific implementations for Row (where comment formatting matters)

impl<'cx> PrettyPrint<Naive> for Row<'cx> {
    fn pretty_print(&self, writer: &mut impl Write, options: &PrintOptions<Naive>) -> fmt::Result {
        Indent.write(writer, options)?;

        // Handle breaks
        if self.breaks.is_some() {
            writer.write_str(".")?;
        }

        // Handle content
        if let Some(content) = &self.content {
            PrettyPrint::<Naive>::pretty_print(content, writer, options)?;
        }

        // Handle comment (naive approach: just add space)
        if let Some(comment) = &self.comment {
            if self.content.is_some() {
                writer.write_str(" ")?;
            }
            PrettyPrint::<Naive>::pretty_print(*comment, writer, options)?;
        }

        Ok(())
    }
}

impl<'cx> PrettyPrint<CommentAligned> for Row<'cx> {
    fn pretty_print(
        &self,
        writer: &mut impl Write,
        options: &PrintOptions<CommentAligned>,
    ) -> fmt::Result {
        Indent.write(writer, options)?;

        // Handle breaks
        if self.breaks.is_some() {
            writer.write_str(".")?;
        }

        // Handle content
        if let Some(content) = &self.content {
            PrettyPrint::<CommentAligned>::pretty_print(content, writer, options)?;
        }

        // Handle comment (aligned approach: this is simplified for now)
        // In a real implementation, we'd need to analyze consecutive rows
        // and align comments properly
        if let Some(comment) = &self.comment {
            if self.content.is_some() {
                writer.write_str(" ")?;
            }
            PrettyPrint::<CommentAligned>::pretty_print(*comment, writer, options)?;
        }

        Ok(())
    }
}

impl<'cx, S: Strategy> PrettyPrint<S> for Comment<'cx> {
    fn pretty_print(&self, writer: &mut impl Write, _options: &PrintOptions<S>) -> fmt::Result {
        writer.write_str("#")?;
        writer.write_str(self.content)
    }
}

impl<'cx, S: Strategy> PrettyPrint<S> for Block<'cx>
where
    Scope<'cx>: PrettyPrint<S>,
{
    fn pretty_print(&self, writer: &mut impl Write, options: &PrintOptions<S>) -> fmt::Result {
        writer.write_str("{")?;
        Newline.write(writer, options)?;

        let nested_options = options.with_increased_indent();
        PrettyPrint::<S>::pretty_print(&self.scope, writer, &nested_options)?;

        Newline.write(writer, options)?;
        Indent.write(writer, options)?;
        writer.write_str("}")
    }
}

impl<'cx, S: Strategy> PrettyPrint<S> for StatementKind<'cx> {
    fn pretty_print(&self, writer: &mut impl Write, options: &PrintOptions<S>) -> fmt::Result {
        match self {
            StatementKind::Let(let_stmt) => {
                PrettyPrint::<S>::pretty_print(let_stmt, writer, options)
            }
            StatementKind::Expr(expr_stmt) => {
                PrettyPrint::<S>::pretty_print(expr_stmt, writer, options)
            }
            StatementKind::Return(return_stmt) => {
                PrettyPrint::<S>::pretty_print(return_stmt, writer, options)
            }
        }
    }
}

impl<'cx, S: Strategy> PrettyPrint<S> for Let<'cx> {
    fn pretty_print(&self, writer: &mut impl Write, options: &PrintOptions<S>) -> fmt::Result {
        writer.write_str("let ")?;
        PrettyPrint::<S>::pretty_print(&self.variable, writer, options)?;
        writer.write_str(" = ")?;
        PrettyPrint::<S>::pretty_print(self.rhs, writer, options)?;
        writer.write_str(";")
    }
}

impl<'cx, S: Strategy> PrettyPrint<S> for ExprStatement<'cx> {
    fn pretty_print(&self, writer: &mut impl Write, options: &PrintOptions<S>) -> fmt::Result {
        PrettyPrint::<S>::pretty_print(self.expr, writer, options)?;
        writer.write_str(";")
    }
}

impl<S: Strategy> PrettyPrint<S> for ReturnStmt {
    fn pretty_print(&self, writer: &mut impl Write, _options: &PrintOptions<S>) -> fmt::Result {
        writer.write_str("return;")
    }
}

impl<'cx, S: Strategy> PrettyPrint<S> for ExprKind<'cx> {
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
        }
    }
}

impl<'cx, S: Strategy> PrettyPrint<S> for Path<'cx> {
    fn pretty_print(&self, writer: &mut impl Write, _options: &PrintOptions<S>) -> fmt::Result {
        writer.write_str(self.raw)
    }
}

impl<'cx, S: Strategy> PrettyPrint<S> for Ident<'cx> {
    fn pretty_print(&self, writer: &mut impl Write, _options: &PrintOptions<S>) -> fmt::Result {
        writer.write_str(self.raw)
    }
}

impl<'cx, S: Strategy> PrettyPrint<S> for Literal<'cx> {
    fn pretty_print(&self, writer: &mut impl Write, options: &PrintOptions<S>) -> fmt::Result {
        match self {
            Literal::Array(array) => PrettyPrint::<S>::pretty_print(array, writer, options),
            Literal::String(string) => PrettyPrint::<S>::pretty_print(string, writer, options),
            Literal::Bytes(bytes) => PrettyPrint::<S>::pretty_print(bytes, writer, options),
            Literal::HexBytes(hex_bytes) => {
                PrettyPrint::<S>::pretty_print(hex_bytes, writer, options)
            }
            Literal::Numeric(numeric) => PrettyPrint::<S>::pretty_print(numeric, writer, options),
            Literal::OsFilePath(filepath) => {
                PrettyPrint::<S>::pretty_print(filepath, writer, options)
            }
            Literal::DateTime(datetime) => {
                PrettyPrint::<S>::pretty_print(datetime, writer, options)
            }
        }
    }
}

impl<'cx, S: Strategy> PrettyPrint<S> for Array<'cx>
where
    ExprKind<'cx>: PrettyPrint<S>,
{
    fn pretty_print(&self, writer: &mut impl Write, options: &PrintOptions<S>) -> fmt::Result {
        writer.write_str("[")?;
        for (i, expr) in self.exprs.iter().enumerate() {
            if i > 0 {
                writer.write_str(", ")?;
            }
            PrettyPrint::<S>::pretty_print(*expr, writer, options)?;
        }
        writer.write_str("]")
    }
}

impl<'cx, S: Strategy> PrettyPrint<S> for String<'cx> {
    fn pretty_print(&self, writer: &mut impl Write, _options: &PrintOptions<S>) -> fmt::Result {
        writer.write_str(self.raw)
    }
}

impl<'cx, S: Strategy> PrettyPrint<S> for Bytes<'cx> {
    fn pretty_print(&self, writer: &mut impl Write, _options: &PrintOptions<S>) -> fmt::Result {
        writer.write_str(self.raw)
    }
}

impl<'cx, S: Strategy> PrettyPrint<S> for HexBytes<'cx> {
    fn pretty_print(&self, writer: &mut impl Write, _options: &PrintOptions<S>) -> fmt::Result {
        writer.write_str(self.raw)
    }
}

impl<'cx, S: Strategy> PrettyPrint<S> for Numeric<'cx>
where
    Ident<'cx>: PrettyPrint<S>,
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

impl<'cx, S: Strategy> PrettyPrint<S> for OsFilePath<'cx> {
    fn pretty_print(&self, writer: &mut impl Write, _options: &PrintOptions<S>) -> fmt::Result {
        writer.write_str(self.raw)
    }
}

impl<'cx, S: Strategy> PrettyPrint<S> for DateTime<'cx> {
    fn pretty_print(&self, writer: &mut impl Write, _options: &PrintOptions<S>) -> fmt::Result {
        writer.write_str(self.raw)
    }
}

impl<'cx, S: Strategy> PrettyPrint<S> for Parened<'cx>
where
    ExprKind<'cx>: PrettyPrint<S>,
{
    fn pretty_print(&self, writer: &mut impl Write, options: &PrintOptions<S>) -> fmt::Result {
        writer.write_str("(")?;
        PrettyPrint::<S>::pretty_print(self.expr, writer, options)?;
        writer.write_str(")")
    }
}

impl<'cx, S: Strategy> PrettyPrint<S> for Qualif<'cx>
where
    ExprKind<'cx>: PrettyPrint<S>,
    ExecutorComponent<'cx>: PrettyPrint<S>,
{
    fn pretty_print(&self, writer: &mut impl Write, options: &PrintOptions<S>) -> fmt::Result {
        match self {
            Qualif::TimeIndicator(expr) => {
                writer.write_str(":")?;
                PrettyPrint::<S>::pretty_print(*expr, writer, options)
            }
            Qualif::ExecutorComponent(exec_comp) => {
                PrettyPrint::<S>::pretty_print(exec_comp, writer, options)
            }
        }
    }
}

impl<'cx, S: Strategy> PrettyPrint<S> for ExecutorComponent<'cx>
where
    Path<'cx>: PrettyPrint<S>,
{
    fn pretty_print(&self, writer: &mut impl Write, options: &PrintOptions<S>) -> fmt::Result {
        writer.write_str("@")?;
        PrettyPrint::<S>::pretty_print(&self.name, writer, options)
    }
}

impl<'cx, S: Strategy> PrettyPrint<S> for PreQualified<'cx>
where
    Qualif<'cx>: PrettyPrint<S>,
    ExprKind<'cx>: PrettyPrint<S>,
{
    fn pretty_print(&self, writer: &mut impl Write, options: &PrintOptions<S>) -> fmt::Result {
        for qualif in self.qualifs {
            PrettyPrint::<S>::pretty_print(qualif, writer, options)?;
            writer.write_str(" ")?;
        }
        PrettyPrint::<S>::pretty_print(self.expr, writer, options)
    }
}

impl<'cx, S: Strategy> PrettyPrint<S> for Unary<'cx>
where
    ExprKind<'cx>: PrettyPrint<S>,
{
    fn pretty_print(&self, writer: &mut impl Write, options: &PrintOptions<S>) -> fmt::Result {
        match self.op {
            UnOp::Neg(_) => writer.write_str("-")?,
            UnOp::Ref(_) => writer.write_str("&")?,
        }
        PrettyPrint::<S>::pretty_print(self.expr, writer, options)
    }
}

impl<'cx, S: Strategy> PrettyPrint<S> for Compare<'cx>
where
    ExprKind<'cx>: PrettyPrint<S>,
{
    fn pretty_print(&self, writer: &mut impl Write, options: &PrintOptions<S>) -> fmt::Result {
        PrettyPrint::<S>::pretty_print(self.head, writer, options)?;
        for (op, expr) in self.tail_with_op {
            writer.write_str(" ")?;
            match op {
                CompareOp::GreaterEq(_) => writer.write_str(">=")?,
                CompareOp::LessEq(_) => writer.write_str("<=")?,
                CompareOp::Greater(_) => writer.write_str(">")?,
                CompareOp::Less(_) => writer.write_str("<")?,
                CompareOp::NotEqual(NotEqualToken::BangEqual(_)) => writer.write_str("!=")?,
                CompareOp::NotEqual(NotEqualToken::SlashEqual(_)) => writer.write_str("/=")?,
                CompareOp::Equal(_) => writer.write_str("==")?,
            }
            writer.write_str(" ")?;
            PrettyPrint::<S>::pretty_print(*expr, writer, options)?;
        }
        Ok(())
    }
}

impl<'cx, S: Strategy> PrettyPrint<S> for Binary<'cx>
where
    ExprKind<'cx>: PrettyPrint<S>,
{
    fn pretty_print(&self, writer: &mut impl Write, options: &PrintOptions<S>) -> fmt::Result {
        PrettyPrint::<S>::pretty_print(self.lhs, writer, options)?;
        writer.write_str(" ")?;
        match self.op {
            BinOp::If => writer.write_str("if")?,
            BinOp::And => writer.write_str("&&")?,
            BinOp::Or => writer.write_str("||")?,
            BinOp::In => writer.write_str("in")?,
            BinOp::Mul => writer.write_str("*")?,
            BinOp::Div => writer.write_str("/")?,
            BinOp::Mod => writer.write_str("%")?,
            BinOp::Add => writer.write_str("+")?,
            BinOp::Sub => writer.write_str("-")?,
        }
        writer.write_str(" ")?;
        PrettyPrint::<S>::pretty_print(self.rhs, writer, options)
    }
}

impl<'cx, S: Strategy> PrettyPrint<S> for Apply<'cx>
where
    ExprKind<'cx>: PrettyPrint<S>,
{
    fn pretty_print(&self, writer: &mut impl Write, options: &PrintOptions<S>) -> fmt::Result {
        PrettyPrint::<S>::pretty_print(self.function, writer, options)?;
        writer.write_str("(")?;
        for (i, arg) in self.args.iter().enumerate() {
            if i > 0 {
                writer.write_str(", ")?;
            }
            PrettyPrint::<S>::pretty_print(*arg, writer, options)?;
        }
        writer.write_str(")")
    }
}

impl<'cx, S: Strategy> PrettyPrint<S> for Set<'cx>
where
    ExprKind<'cx>: PrettyPrint<S>,
{
    fn pretty_print(&self, writer: &mut impl Write, options: &PrintOptions<S>) -> fmt::Result {
        PrettyPrint::<S>::pretty_print(self.lhs, writer, options)?;
        writer.write_str(" := ")?;
        PrettyPrint::<S>::pretty_print(self.rhs, writer, options)
    }
}
