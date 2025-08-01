use super::{
    Comment, CommentAligned, CommentPosition, Indent, Newline, PrettyPrint, PrintOptions,
    PrintableFamily, Row, Scope, ScopeItem, Write, fmt,
};

/// Implementation for consecutive grouping: group rows separated by empty lines
pub fn pretty_print_consecutive<'cx, F: PrintableFamily<'cx>>(
    scope: &Scope<'cx, F>,
    writer: &mut impl Write,
    options: &PrintOptions<CommentAligned>,
) -> fmt::Result {
    let mut current_group = Vec::new();
    let mut temp_buffer = String::new();

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
pub fn pretty_print_per_block<'cx, F: PrintableFamily<'cx>>(
    scope: &Scope<'cx, F>,
    writer: &mut impl Write,
    options: &PrintOptions<CommentAligned>,
) -> fmt::Result {
    let mut row_infos = Vec::new();
    let mut temp_buffer = String::new();

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
