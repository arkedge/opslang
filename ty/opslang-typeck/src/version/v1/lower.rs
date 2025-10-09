use anyhow::anyhow;
use opslang_ast::v1::{self as ast};
use opslang_ir::version::v1::{self as ir};

pub fn merge_comments<'cx>(
    ir_cx: &'cx ir::Context<'cx>,
    comments: &[&'cx ast::Comment<'cx>],
) -> super::Result<ir::Comment<'cx>> {
    if comments.is_empty() {
        return Err(anyhow!("Cannot merge empty comment list"));
    }

    if comments.len() == 1 {
        // Single comment - simple case
        return Ok(ir::Comment {
            content: comments[0].content,
            span: comments[0].span,
            source_comments: ir_cx.alloc_ast_comment_slice(comments),
        });
    }

    // Multiple comments - merge content and spans
    let mut merged_content = String::new();
    let start = comments[0].span.start;
    let mut end = comments[0].span.end;

    for (i, comment) in comments.iter().enumerate() {
        if i > 0 {
            merged_content.push('\n');
        }
        merged_content.push_str(comment.content);

        // Track the overall span from first to last
        if i == comments.len() - 1 {
            end = comment.span.end;
        }
    }

    let merged_span = ast::Span { start, end };

    // Allocate the merged content in the context
    let content_ref = ir_cx.alloc_str(&merged_content);

    Ok(ir::Comment {
        content: content_ref,
        span: merged_span,
        source_comments: ir_cx.alloc_ast_comment_slice(comments),
    })
}
