use std::fmt::{self, Write};

use crate::{
    CommentAligned, CommentGrouping, CommentPosition, Indent, Naive, Newline, PrettyPrint,
    PrintOptions, Strategy,
};
use opslang_ast::syntax::v1::*;

/// Operator precedence for proper parenthesization
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct Precedence(u8);

impl Precedence {
    const SET: Self = Self(0); // :=
    const INFIX_IF: Self = Self(1); // infix if
    const LOGICAL_OR: Self = Self(2); // ||
    const LOGICAL_AND: Self = Self(3); // &&
    const INFIX_IN: Self = Self(4); // infix in
    const COMPARE: Self = Self(5); // >=, <=, >, <, !=, /=, ==
    const ARITHMETIC: Self = Self(6); // +, -
    const FACTOR: Self = Self(7); // *, /, %
    const PREFIX: Self = Self(8); // unary -
    const APPLY: Self = Self(9); // function application
    const LOWER_PREFIX: Self = Self(10); // &, $
    const ATOMIC: Self = Self(11); // grouping, literals, etc.
}

/// Get the precedence of an expression kind
fn precedence_of<'cx, F: PrintableFamily<'cx>>(expr: &ExprKind<'cx, F>) -> Precedence {
    match expr {
        ExprKind::Set(_) => Precedence::SET,
        ExprKind::Binary(binary) => match binary.op {
            BinOp::If => Precedence::INFIX_IF,
            BinOp::Or => Precedence::LOGICAL_OR,
            BinOp::And => Precedence::LOGICAL_AND,
            BinOp::In => Precedence::INFIX_IN,
            BinOp::Add | BinOp::Sub => Precedence::ARITHMETIC,
            BinOp::Mul | BinOp::Div | BinOp::Mod => Precedence::FACTOR,
        },
        ExprKind::Compare(_) => Precedence::COMPARE,
        ExprKind::Unary(unary) => match unary.op {
            UnOp::Neg(_) => Precedence::PREFIX,
            UnOp::IdRef(_) | UnOp::Deref(_) => Precedence::LOWER_PREFIX,
        },
        ExprKind::Apply(_) => Precedence::APPLY,
        ExprKind::Variable(_)
        | ExprKind::Literal(_)
        | ExprKind::Parened(_)
        | ExprKind::Qualif(_)
        | ExprKind::PreQualified(_) => Precedence::ATOMIC,
    }
}

/// Print an expression with parentheses if needed.
///
/// This function will give parens when `parent_prec` is greater than the
/// precedence of the child expression. In other words, if the child expression is
/// the same or more prioritized than the parent, it will be printed without parens.
fn print_with_parens<'cx, S: Strategy, F: PrintableFamily<'cx>>(
    expr: &ExprKind<'cx, F>,
    parent_prec: Precedence,
    writer: &mut impl Write,
    options: &PrintOptions<S>,
) -> fmt::Result
where
    ExprKind<'cx, F>: PrettyPrint<S>,
{
    if precedence_of(expr) < parent_prec {
        writer.write_str("(")?;
        PrettyPrint::<S>::pretty_print(expr, writer, options)?;
        writer.write_str(")")
    } else {
        PrettyPrint::<S>::pretty_print(expr, writer, options)
    }
}

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

// Helper structures for comment alignment calculations

/// Information about a single row for comment alignment calculation.
#[derive(Debug, Clone)]
struct RowInfo<'cx, F: PrintableFamily<'cx>> {
    /// Pre-formatted content string (before comment).
    content: std::string::String,

    /// Whether this row has content.
    has_content: bool,

    /// Raw comment reference (None if no comment).
    comment: Option<&'cx Comment<'cx, F>>,
}

impl<'cx, F: PrintableFamily<'cx>> RowInfo<'cx, F> {
    fn need_align(&self) -> bool {
        if !self.has_content {
            return false;
        }
        let Some(comment) = self.comment else {
            return false;
        };
        // Shebang comments (starting with '!') are not aligned
        !comment.content.starts_with('!')
    }
}

/// Write a group of rows with aligned comments.
fn write_comment_group<'cx, F: PrintableFamily<'cx>>(
    rows: &[RowInfo<'cx, F>],
    writer: &mut impl Write,
    options: &PrintOptions<CommentAligned>,
) -> fmt::Result {
    if rows.is_empty() {
        return Ok(());
    }

    // Find the target alignment position (excluding shebang comments)
    let longest_content = rows
        .iter()
        .filter(|row| row.need_align())
        .map(|row| row.content.len())
        .max()
        .unwrap_or(0);

    let target_position =
        calculate_target_position(longest_content, options.comment_alignment.position);

    // Write each row with proper alignment
    for row in rows {
        writer.write_str(&row.content)?;

        if row.need_align() {
            let current_len = row.content.len();
            if target_position > current_len {
                let spaces_needed = target_position - current_len;
                writer.write_str(&" ".repeat(spaces_needed))?;
            }
        }

        if let Some(comment) = row.comment {
            PrettyPrint::<CommentAligned>::pretty_print(comment, writer, options)?;
        }

        Newline.write(writer, options)?;
    }

    Ok(())
}

/// Formats content part and extract information for comment alignment
fn format_row_before_comment<'cx, F: PrintableFamily<'cx>>(
    temp_buffer: &mut std::string::String,
    row: &Row<'cx, F>,
    options: &PrintOptions<CommentAligned>,
) -> Result<RowInfo<'cx, F>, fmt::Error> {
    // Handle meta comment first
    if row.breaks.is_none() && row.content.is_none() && row.comment.is_some_and(|c| c.is_meta()) {
        return Ok(RowInfo {
            content: Default::default(),
            has_content: row.content.is_some(),
            comment: row.comment,
        });
    }
    if !row.is_empty() {
        Indent.write(temp_buffer, options)?;
    }
    if row.breaks.is_some() {
        temp_buffer.push('.');
    } else if options.reserve_for_break && !row.is_empty() {
        temp_buffer.push(' ');
    }
    if let Some(content) = &row.content {
        PrettyPrint::<CommentAligned>::pretty_print(content, temp_buffer, options)?;
    }

    Ok(RowInfo {
        content: std::mem::take(temp_buffer),
        has_content: row.content.is_some(),
        comment: row.comment,
    })
}

/// Implementation for consecutive grouping: group rows separated by empty lines
fn pretty_print_consecutive<'cx, F: PrintableFamily<'cx>>(
    scope: &Scope<'cx, F>,
    writer: &mut impl Write,
    options: &PrintOptions<CommentAligned>,
) -> fmt::Result {
    let mut current_group = Vec::new();
    let mut temp_buffer = std::string::String::new();

    for item in scope.items {
        match item {
            ScopeItem::Row(row) => {
                // Format the content part
                let row_info = format_row_before_comment(&mut temp_buffer, row, options)?;

                // If this is an empty line (default Row), output current group
                if row.is_empty() {
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
fn pretty_print_per_block<'cx, F: PrintableFamily<'cx>>(
    scope: &Scope<'cx, F>,
    writer: &mut impl Write,
    options: &PrintOptions<CommentAligned>,
) -> fmt::Result {
    let mut row_infos = Vec::new();
    let mut temp_buffer = std::string::String::new();

    // Collect all row information
    for item in scope.items {
        match item {
            ScopeItem::Row(row) => {
                // Format the content part
                let row_info = format_row_before_comment(&mut temp_buffer, row, options)?;

                row_infos.push(row_info);
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
    if longest_content == 0 {
        return 0; // No content, no alignment needed
    }
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

/// Alias for printable family used in this module.
///
/// Types that implement this trait looks very similar to [`DefaultTypeFamily`], but
/// accepts any `Span` and `Position` types, which allows for more flexibility in
/// printing operations.
pub trait PrintableFamily<'cx>:
    TypeFamily<
        'cx,
        Comment = &'cx Comment<'cx, Self>,
        Row = &'cx Row<'cx, Self>,
        RowContent = StatementKind<'cx, Self>,
        Block = &'cx Block<'cx, Self>,
        ScopeItem = ScopeItem<'cx, Self>,
        ReturnStmt = ReturnStmt<'cx, Self>,
        Ident = Ident<'cx, Self>,
        Path = Path<'cx, Self>,
        Qualif = Qualif<'cx, Self>,
        PreQualified = PreQualified<'cx, Self>,
        Parened = Parened<'cx, Self>,
        Literal = Literal<'cx, Self>,
        Numeric = Numeric<'cx, Self>,
        Apply = Apply<'cx, Self>,
    >
{
}

// This implementation is needed to define `PrintableFamily` as an alias for
// such family. This can be removed using trait aliases feature, which is currently
// unstable.
impl<
    'cx,
    F: TypeFamily<
            'cx,
            Comment = &'cx Comment<'cx, Self>,
            Row = &'cx Row<'cx, Self>,
            RowContent = StatementKind<'cx, Self>,
            Block = &'cx Block<'cx, Self>,
            ScopeItem = ScopeItem<'cx, Self>,
            ReturnStmt = ReturnStmt<'cx, Self>,
            Ident = Ident<'cx, Self>,
            Path = Path<'cx, Self>,
            Qualif = Qualif<'cx, Self>,
            PreQualified = PreQualified<'cx, Self>,
            Parened = Parened<'cx, Self>,
            Literal = Literal<'cx, Self>,
            Numeric = Numeric<'cx, Self>,
            Apply = Apply<'cx, Self>,
        >,
> PrintableFamily<'cx> for F
{
}

// Generic implementations for elements that don't depend on strategy

impl<'cx, F: PrintableFamily<'cx>> PrettyPrint<Naive> for Program<'cx, F> {
    fn pretty_print(&self, writer: &mut impl Write, options: &PrintOptions<Naive>) -> fmt::Result {
        PrettyPrint::<Naive>::pretty_print(&self.content, writer, options)
    }
}

impl<'cx, F: PrintableFamily<'cx>> PrettyPrint<CommentAligned> for Program<'cx, F> {
    fn pretty_print(
        &self,
        writer: &mut impl Write,
        options: &PrintOptions<CommentAligned>,
    ) -> fmt::Result {
        PrettyPrint::<CommentAligned>::pretty_print(&self.content, writer, options)
    }
}

// Naive implementation for Scope
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

// CommentAligned implementation for Scope
impl<'cx, F: PrintableFamily<'cx>> PrettyPrint<CommentAligned> for Scope<'cx, F> {
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

impl<'cx, F: PrintableFamily<'cx>> PrettyPrint<Naive> for Row<'cx, F> {
    fn pretty_print(&self, writer: &mut impl Write, options: &PrintOptions<Naive>) -> fmt::Result {
        Indent.write(writer, options)?;

        // Handle breaks
        if self.breaks.is_some() {
            writer.write_char('.')?;
        } else if options.reserve_for_break && (self.content.is_some() || self.comment.is_some()) {
            writer.write_char(' ')?;
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

impl<'cx, F: PrintableFamily<'cx>> PrettyPrint<Naive> for Block<'cx, F> {
    fn pretty_print(&self, writer: &mut impl Write, options: &PrintOptions<Naive>) -> fmt::Result {
        Token.write(self.left_brace, writer)?;
        Newline.write(writer, options)?;

        let nested_options = options.with_increased_indent();
        PrettyPrint::<Naive>::pretty_print(&self.scope, writer, &nested_options)?;

        Newline.write(writer, options)?;
        Indent.write(writer, options)?;
        Token.write(self.right_brace, writer)
    }
}

impl<'cx, F: PrintableFamily<'cx>> PrettyPrint<CommentAligned> for Block<'cx, F> {
    fn pretty_print(
        &self,
        writer: &mut impl Write,
        options: &PrintOptions<CommentAligned>,
    ) -> fmt::Result {
        Token.write(self.left_brace, writer)?;
        Newline.write(writer, options)?;

        let nested_options = options.with_increased_indent();
        PrettyPrint::<CommentAligned>::pretty_print(&self.scope, writer, &nested_options)?;

        Newline.write(writer, options)?;
        Indent.write(writer, options)?;
        Token.write(self.right_brace, writer)
    }
}

impl<'cx, S: Strategy, F: PrintableFamily<'cx>> PrettyPrint<S> for StatementKind<'cx, F> {
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

impl<'cx, S: Strategy, F: PrintableFamily<'cx>> PrettyPrint<S> for Let<'cx, F> {
    fn pretty_print(&self, writer: &mut impl Write, options: &PrintOptions<S>) -> fmt::Result {
        writer.write_str("let ")?;
        PrettyPrint::<S>::pretty_print(&self.variable, writer, options)?;
        writer.write_str(" = ")?;
        PrettyPrint::<S>::pretty_print(&self.rhs, writer, options)?;
        Token.write(self.semi, writer)
    }
}

impl<'cx, S: Strategy, F: PrintableFamily<'cx>> PrettyPrint<S> for ExprStatement<'cx, F> {
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

impl<'cx, S: Strategy, F: PrintableFamily<'cx>> PrettyPrint<S> for Expr<'cx, F> {
    fn pretty_print(&self, writer: &mut impl Write, options: &PrintOptions<S>) -> fmt::Result {
        self.0.pretty_print(writer, options)
    }
}

impl<'cx, S: Strategy, F: PrintableFamily<'cx>> PrettyPrint<S> for ExprKind<'cx, F> {
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

impl<'cx, S: Strategy, F: PrintableFamily<'cx>> PrettyPrint<S> for Path<'cx, F> {
    fn pretty_print(&self, writer: &mut impl Write, _options: &PrintOptions<S>) -> fmt::Result {
        writer.write_str(self.raw)
    }
}

impl<'cx, S: Strategy, F: PrintableFamily<'cx>> PrettyPrint<S> for Ident<'cx, F> {
    fn pretty_print(&self, writer: &mut impl Write, _options: &PrintOptions<S>) -> fmt::Result {
        writer.write_str(self.raw)
    }
}

impl<'cx, S: Strategy, F: PrintableFamily<'cx>> PrettyPrint<S> for Literal<'cx, F> {
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

impl<'cx, S: Strategy, F: PrintableFamily<'cx>> PrettyPrint<S> for OsFilePath<'cx, F> {
    fn pretty_print(&self, writer: &mut impl Write, _options: &PrintOptions<S>) -> fmt::Result {
        writer.write_str("os\"")?;
        writer.write_str(self.raw)?;
        writer.write_str("\"")
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
    DefaultAttr<'cx, F>: PrettyPrint<S>,
{
    fn pretty_print(&self, writer: &mut impl Write, options: &PrintOptions<S>) -> fmt::Result {
        match self {
            Qualif::KindSpec(KindSpec {
                at_token,
                name,
                arg,
            }) => {
                Token.write(at_token, writer)?;
                PrettyPrint::<S>::pretty_print(name, writer, options)?;
                if let Some(expr) = arg {
                    Token.write(expr.colon_token, writer)?;
                    print_with_parens(&expr.value, Precedence::ATOMIC, writer, options)?;
                }
                Ok(())
            }
            Qualif::DefaultAttr(exec_comp) => {
                PrettyPrint::<S>::pretty_print(exec_comp, writer, options)
            }
        }
    }
}

impl<'cx, S: Strategy, F: PrintableFamily<'cx>> PrettyPrint<S> for DefaultAttr<'cx, F>
where
    Path<'cx, F>: PrettyPrint<S>,
{
    fn pretty_print(&self, writer: &mut impl Write, options: &PrintOptions<S>) -> fmt::Result {
        Token.write(self.tilde_token, writer)?;
        PrettyPrint::<S>::pretty_print(&self.name, writer, options)
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
        let op_prec = match self.op {
            UnOp::Neg(t) => {
                Token.write(t, writer)?;
                Precedence::PREFIX
            }
            UnOp::IdRef(t) => {
                Token.write(t, writer)?;
                Precedence::LOWER_PREFIX
            }
            UnOp::Deref(t) => {
                Token.write(t, writer)?;
                Precedence::LOWER_PREFIX
            }
        };
        print_with_parens(self.expr.0, op_prec, writer, options)
    }
}

impl<'cx, S: Strategy, F: PrintableFamily<'cx>> PrettyPrint<S> for Compare<'cx, F>
where
    ExprKind<'cx, F>: PrettyPrint<S>,
{
    fn pretty_print(&self, writer: &mut impl Write, options: &PrintOptions<S>) -> fmt::Result {
        print_with_parens(self.head.0, Precedence::COMPARE, writer, options)?;
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
            print_with_parens(expr.0, Precedence::COMPARE, writer, options)?;
        }
        Ok(())
    }
}

impl<'cx, S: Strategy, F: PrintableFamily<'cx>> PrettyPrint<S> for Binary<'cx, F>
where
    ExprKind<'cx, F>: PrettyPrint<S>,
{
    fn pretty_print(&self, writer: &mut impl Write, options: &PrintOptions<S>) -> fmt::Result {
        let op_prec = match self.op {
            BinOp::If => Precedence::INFIX_IF,
            BinOp::And => Precedence::LOGICAL_AND,
            BinOp::Or => Precedence::LOGICAL_OR,
            BinOp::In => Precedence::INFIX_IN,
            BinOp::Mul | BinOp::Div | BinOp::Mod => Precedence::FACTOR,
            BinOp::Add | BinOp::Sub => Precedence::ARITHMETIC,
        };

        print_with_parens(self.lhs.0, op_prec, writer, options)?;
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
        print_with_parens(self.rhs.0, op_prec, writer, options)
    }
}

impl<'cx, S: Strategy, F: PrintableFamily<'cx>> PrettyPrint<S> for Apply<'cx, F>
where
    ExprKind<'cx, F>: PrettyPrint<S>,
{
    fn pretty_print(&self, writer: &mut impl Write, options: &PrintOptions<S>) -> fmt::Result {
        // OCaml-like function application: f x y instead of f(x, y)
        print_with_parens(self.function.0, Precedence::APPLY, writer, options)?;
        for arg in self.args.iter() {
            writer.write_str(" ")?;
            print_with_parens(arg.0, Precedence::APPLY, writer, options)?;
        }
        Ok(())
    }
}

impl<'cx, S: Strategy, F: PrintableFamily<'cx>> PrettyPrint<S> for Set<'cx, F>
where
    ExprKind<'cx, F>: PrettyPrint<S>,
{
    fn pretty_print(&self, writer: &mut impl Write, options: &PrintOptions<S>) -> fmt::Result {
        print_with_parens(self.lhs.0, Precedence::SET, writer, options)?;
        writer.write_str(" := ")?;
        print_with_parens(self.rhs.0, Precedence::SET, writer, options)
    }
}
