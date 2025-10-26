// FIXME: implement LSP or something to remove infix import
use super::*;

impl<'cx> TypeChecker<'cx> {
    /// Performs type checking on an expression and converts it to IR.
    ///
    /// This function infers the type of an expression and converts it to its IR representation.
    /// It updates the provided substitution with any new type constraints discovered during checking.
    pub(super) fn typeck_infix_import<'env>(
        &mut self,
        session: SecondPassSession<'cx, 'env>,
        infix_import: &'cx ast::InfixImport<'cx>,
    ) -> Result<ir::Expr<'cx>> {
        let ast::InfixImport {
            file,
            question,
            path,
        } = infix_import;
        // InfixImport expressions are like "file ? path" operations
        let file_name = if let ast::Literal::String(s) = file {
            s.raw
        } else {
            return Err(anyhow::anyhow!(
                "Infix import file must be a string literal"
            ));
        };

        let parsed_segments = ParsedSegment::parse_segments(file_name)
            .ok_or_else(|| anyhow::anyhow!("Invalid module path in infix import: {file_name}"))?;

        // Determine if this is an absolute or relative path
        let (mut module_path, segments) = match parsed_segments.first() {
            Some(ParsedSegment::Ident(root_segment)) => {
                // Absolute path: starts with an identifier
                let root_path = self.gcx.module.alloc_root_path(root_segment);
                (root_path, &parsed_segments[1..])
            }
            Some(ParsedSegment::Current | ParsedSegment::Super) | None => {
                // Relative path: starts with "." or ".."
                (session.module_path, &parsed_segments[..])
            }
        };

        for segment in segments {
            segment
                .rebase_path(&mut module_path, self.gcx.module)
                .ok_or_else(|| {
                    anyhow::anyhow!("Invalid module path in infix import: {file_name}")
                })?;
        }

        let item_ident = path.is_ident().ok_or_else(|| {
            anyhow::anyhow!("Currently single identifier are supported in infix import paths")
        })?;

        let typed_ident = session
            .get_toplevel_items_of(&module_path)
            .and_then(|scope| scope.lookup_var(item_ident))
            .ok_or_else(|| {
                anyhow::anyhow!("Could not find item `{item_ident}` in file \"{file_name}\"")
            })?;

        // Create IR InfixImport expression
        let ir_expr = ir::Expr::new(
            ir::ExprMut::import(
                self.ir_cx,
                module_path,
                question.into_token(),
                typed_ident.id,
            ),
            typed_ident.ty,
        );
        Ok(ir_expr)
    }
}

#[derive(Clone, Copy)]
enum ParsedSegment<'cx> {
    Current,
    Super,
    Ident(&'cx str),
}

impl<'cx> ParsedSegment<'cx> {
    /// Parses a module path string into its constituent segments.
    ///
    /// Returns `None` if the path is invalid (contains empty segments).
    ///
    /// Path classification:
    /// - Absolute: starts with an identifier (e.g., "foo/bar", "module")
    /// - Relative: starts with "." or ".." (e.g., "./foo", "../bar")
    /// - Invalid: empty segments (e.g., "foo//bar", "/foo")
    fn parse_segments(path: &'cx str) -> Option<Vec<Self>> {
        let mut segments = Vec::new();

        for segment in path.split('/') {
            match segment {
                "" => return None, // Empty segments are not allowed
                "." => segments.push(ParsedSegment::Current),
                ".." => segments.push(ParsedSegment::Super),
                ident => segments.push(ParsedSegment::Ident(ident.trim_end_matches(".ops"))),
            }
        }

        Some(segments)
    }

    fn rebase_path(self, path: &mut ModulePath<'cx>, mcx: &'cx ModuleContext<'cx>) -> Option<()> {
        match self {
            ParsedSegment::Current => {}
            ParsedSegment::Super => {
                *path = path.parent?;
            }
            ParsedSegment::Ident(ident) => {
                *path = mcx.alloc_child_path(*path, ident);
            }
        };
        Some(())
    }
}
