use super::*;

impl<'cx> TypeChecker<'cx> {
    pub(super) fn typeck_block<'env>(
        &mut self,
        session: SecondPassSession<'cx, 'env>,
        env: &Scope<'cx, 'env>,
        subst: &mut Substitution<'cx>,
        block: &ast::Block<'cx>,
    ) -> Result<ir::Block<'cx>> {
        let mut local_env = env.extend_inherit();
        let mut ir_items: Vec<ir::ScopeItem<'cx>> = Vec::new();
        let mut pending_comments: Vec<&'cx ast::Comment<'cx>> = Vec::new();

        for item in block.scope.items {
            match item {
                ast::ScopeItem::Row(row) => {
                    match self.typeck_row(session, &mut local_env, subst, row)? {
                        RowProcessResult::Comment(comment) => {
                            pending_comments.push(comment);
                        }
                        RowProcessResult::ScopeItem(scope_item) => {
                            // Flush any pending comments before adding the regular item
                            self.flush_comments_to_items(&mut pending_comments, &mut ir_items)?;
                            ir_items.push(scope_item);
                        }
                    }
                }
                ast::ScopeItem::Block(nested_block) => {
                    // Flush any pending comments before adding the block
                    self.flush_comments_to_items(&mut pending_comments, &mut ir_items)?;
                    let ir_block = self.typeck_block(session, &local_env, subst, nested_block)?;
                    ir_items.push(ir::ScopeItem::Block(ir_block));
                }
            }
        }

        // Flush any remaining comments at the end
        self.flush_comments_to_items(&mut pending_comments, &mut ir_items)?;

        let ir_scope = ir::Scope { items: ir_items };

        let ir_block = ir::Block {
            left_brace: block.left_brace.into_token(),
            scope: ir_scope,
            right_brace: block.right_brace.into_token(),
        };

        Ok(ir_block)
    }
}

/// Result of [`TypeChecker::typeck_row`] - either a comment to be merged or a regular scope item
enum RowProcessResult<'cx> {
    Comment(&'cx ast::Comment<'cx>),
    ScopeItem(ir::ScopeItem<'cx>),
}

impl<'cx> TypeChecker<'cx> {
    fn typeck_row<'env>(
        &mut self,
        session: SecondPassSession<'cx, 'env>,
        env: &mut Scope<'cx, 'env>,
        subst: &mut Substitution<'cx>,
        row: &ast::Row<'cx>,
    ) -> Result<RowProcessResult<'cx>> {
        // Check if this row is only a comment (no content, no breaks)
        if row.breaks.is_none()
            && row.statement.is_none()
            && let Some(comment) = &row.comment
        {
            return Ok(RowProcessResult::Comment(comment));
        }

        // Process as a regular row
        let ir_content = if let Some(content) = &row.statement {
            let stmt = self.typeck_statement(session, env, subst, content)?;
            Some(stmt)
        } else {
            None
        };

        let ir_comment = row.comment.as_ref().map(|comment| ir::Comment {
            content: comment.content,
            span: comment.span,
            source_comments: self.ir_cx.alloc_ast_comment_slice(&[comment]),
        });

        let ir_row = ir::Row {
            breaks: row.breaks.map(|b| b.into_token()),
            statement: ir_content,
            comment: ir_comment,
        };

        Ok(RowProcessResult::ScopeItem(ir::ScopeItem::Row(ir_row)))
    }

    fn flush_comments_to_items(
        &self,
        comments: &mut Vec<&'cx ast::Comment<'cx>>,
        ir_items: &mut Vec<ir::ScopeItem<'cx>>,
    ) -> Result<()> {
        if !comments.is_empty() {
            let merged_comment = lower::merge_comments(self.ir_cx, comments)?;
            let comment_row = ir::Row {
                breaks: None,
                statement: None,
                comment: Some(merged_comment),
            };
            ir_items.push(ir::ScopeItem::Row(comment_row));
            comments.clear();
        }
        Ok(())
    }
}
