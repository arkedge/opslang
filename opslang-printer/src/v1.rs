use std::fmt::{self, Write};

use crate::{
    CommentAligned, CommentGrouping, CommentPosition, Indent, Naive, Newline, PrettyPrint,
    PrintOptions, Strategy,
};
use opslang_ast::syntax::v1::*;

// Helper structures for comment alignment calculations

/// Information about a single row for comment alignment calculation.
#[derive(Debug, Clone)]
struct RowInfo<'cx> {
    /// Pre-formatted content string (before comment).
    content: std::string::String,
    /// Raw comment reference (None if no comment).
    comment: Option<&'cx Comment<'cx>>,
}

/// Write a group of rows with aligned comments.
fn write_comment_group<'cx>(
    rows: &[RowInfo<'cx>],
    writer: &mut impl Write,
    options: &PrintOptions<CommentAligned>,
) -> fmt::Result {
    if rows.is_empty() {
        return Ok(());
    }

    // Find the target alignment position (excluding shebang comments)
    let longest_content = rows
        .iter()
        .filter(|row| row.comment.is_some() && !row.comment.unwrap().content.starts_with('!'))
        .map(|row| row.content.len())
        .max()
        .unwrap_or(0);

    let target_position =
        calculate_target_position(longest_content, options.comment_alignment.position);

    // Write each row with proper alignment
    for row in rows {
        writer.write_str(&row.content)?;

        if let Some(comment) = row.comment {
            let current_len = row.content.len();
            // Shebang comments (starting with '!') are not aligned
            if comment.content.starts_with('!') {
                writer.write_str(" ")?;
            } else if target_position > current_len {
                let spaces_needed = target_position - current_len;
                writer.write_str(&" ".repeat(spaces_needed))?;
            } else {
                writer.write_str(" ")?;
            }
            PrettyPrint::<CommentAligned>::pretty_print(comment, writer, options)?;
        }

        Newline.write(writer, options)?;
    }

    Ok(())
}

/// Implementation for consecutive grouping: group rows separated by empty lines
fn pretty_print_consecutive<'cx>(
    scope: &Scope<'cx>,
    writer: &mut impl Write,
    options: &PrintOptions<CommentAligned>,
) -> fmt::Result {
    let mut current_group = Vec::new();
    let mut temp_buffer = std::string::String::new();

    for item in scope.items {
        match item {
            ScopeItem::Row(row) => {
                temp_buffer.clear();

                // Format the content part
                Indent.write(&mut temp_buffer, options)?;
                if row.breaks.is_some() {
                    temp_buffer.push('.');
                }
                if let Some(content) = &row.content {
                    PrettyPrint::<CommentAligned>::pretty_print(
                        content,
                        &mut temp_buffer,
                        options,
                    )?;
                }

                let row_info = RowInfo {
                    content: temp_buffer.clone(),
                    comment: row.comment,
                };

                // If this is an empty line (default Row), output current group
                if **row == Row::default() {
                    if !current_group.is_empty() {
                        write_comment_group(&current_group, writer, options)?;
                        current_group.clear();
                    }
                    // Write the empty line
                    Newline.write(writer, options)?;
                } else {
                    current_group.push(row_info);
                }
            }
            ScopeItem::Block(block) => {
                // Output any pending group before the block
                if !current_group.is_empty() {
                    write_comment_group(&current_group, writer, options)?;
                    current_group.clear();
                }
                // Output the block
                PrettyPrint::<CommentAligned>::pretty_print(*block, writer, options)?;
                Newline.write(writer, options)?;
            }
        }
    }

    // Output any remaining group
    if !current_group.is_empty() {
        write_comment_group(&current_group, writer, options)?;
    }

    Ok(())
}

/// Implementation for per-block grouping: group all comments in the current block
fn pretty_print_per_block<'cx>(
    scope: &Scope<'cx>,
    writer: &mut impl Write,
    options: &PrintOptions<CommentAligned>,
) -> fmt::Result {
    let mut row_infos = Vec::new();
    let mut temp_buffer = std::string::String::new();

    // Collect all row information
    for item in scope.items {
        match item {
            ScopeItem::Row(row) => {
                temp_buffer.clear();

                // Format the content part
                Indent.write(&mut temp_buffer, options)?;
                if row.breaks.is_some() {
                    temp_buffer.push('.');
                }
                if let Some(content) = &row.content {
                    PrettyPrint::<CommentAligned>::pretty_print(
                        content,
                        &mut temp_buffer,
                        options,
                    )?;
                }

                row_infos.push(RowInfo {
                    content: temp_buffer.clone(),
                    comment: row.comment,
                });
            }
            ScopeItem::Block(block) => {
                // Output any pending rows before the block
                if !row_infos.is_empty() {
                    write_comment_group(&row_infos, writer, options)?;
                    row_infos.clear();
                }
                // Output the block (recursively handles its own alignment)
                PrettyPrint::<CommentAligned>::pretty_print(*block, writer, options)?;
                Newline.write(writer, options)?;
            }
        }
    }

    // Output any remaining rows
    if !row_infos.is_empty() {
        write_comment_group(&row_infos, writer, options)?;
    }

    Ok(())
}

/// Calculate the target position based on position strategy
fn calculate_target_position(longest_content: usize, position: CommentPosition) -> usize {
    match position {
        CommentPosition::ToLongest => longest_content + 1, // +1 for space before comment
        CommentPosition::ToFixed {
            column,
            fallback_to_longest,
        } => {
            if fallback_to_longest && longest_content >= column {
                longest_content + 1
            } else {
                column
            }
        }
        CommentPosition::ToTabMultiple {
            tab_size,
            fallback_to_longest,
        } => {
            let tab_position = ((longest_content / tab_size) + 1) * tab_size;
            if fallback_to_longest && longest_content >= tab_position {
                longest_content + 1
            } else {
                tab_position
            }
        }
    }
}

// Generic implementations for elements that don't depend on strategy

impl<'cx> PrettyPrint<Naive> for Program<'cx> {
    fn pretty_print(&self, writer: &mut impl Write, options: &PrintOptions<Naive>) -> fmt::Result {
        PrettyPrint::<Naive>::pretty_print(&self.content, writer, options)
    }
}

impl<'cx> PrettyPrint<CommentAligned> for Program<'cx> {
    fn pretty_print(
        &self,
        writer: &mut impl Write,
        options: &PrintOptions<CommentAligned>,
    ) -> fmt::Result {
        PrettyPrint::<CommentAligned>::pretty_print(&self.content, writer, options)
    }
}

// Naive implementation for Scope
impl<'cx> PrettyPrint<Naive> for Scope<'cx> {
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

// CommentAligned implementation for Scope
impl<'cx> PrettyPrint<CommentAligned> for Scope<'cx> {
    fn pretty_print(
        &self,
        writer: &mut impl Write,
        options: &PrintOptions<CommentAligned>,
    ) -> fmt::Result {
        match options.comment_alignment.grouping {
            CommentGrouping::Consecutive => pretty_print_consecutive(self, writer, options),
            CommentGrouping::PerBlock => pretty_print_per_block(self, writer, options),
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
        // Note: When Row is used individually (not through Scope),
        // we fall back to naive comment handling
        Indent.write(writer, options)?;

        // Handle breaks
        if self.breaks.is_some() {
            writer.write_str(".")?;
        }

        // Handle content
        if let Some(content) = &self.content {
            PrettyPrint::<CommentAligned>::pretty_print(content, writer, options)?;
        }

        // Handle comment (fallback to naive approach when not in scope context)
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

impl<'cx> PrettyPrint<Naive> for Block<'cx> {
    fn pretty_print(&self, writer: &mut impl Write, options: &PrintOptions<Naive>) -> fmt::Result {
        writer.write_str("{")?;
        Newline.write(writer, options)?;

        let nested_options = options.with_increased_indent();
        PrettyPrint::<Naive>::pretty_print(&self.scope, writer, &nested_options)?;

        Newline.write(writer, options)?;
        Indent.write(writer, options)?;
        writer.write_str("}")
    }
}

impl<'cx> PrettyPrint<CommentAligned> for Block<'cx> {
    fn pretty_print(
        &self,
        writer: &mut impl Write,
        options: &PrintOptions<CommentAligned>,
    ) -> fmt::Result {
        writer.write_str("{")?;
        Newline.write(writer, options)?;

        let nested_options = options.with_increased_indent();
        PrettyPrint::<CommentAligned>::pretty_print(&self.scope, writer, &nested_options)?;

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
