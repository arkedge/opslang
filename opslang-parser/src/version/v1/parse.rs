use super::{
    ParseContext,
    generated::grammar_trait::{self, ActionTrait, Program},
};
use opslang_ast::v1::{self as syn, loc::Span};

use opslang_diag::DiagContext;
#[allow(unused_imports)]
use parol_runtime::{Result, Token};

pub struct Action<'cx> {
    context: &'cx ParseContext<'cx>,
    parsed: Option<syn::Program<'cx>>,
}

impl<'cx> Action<'cx> {
    pub fn new(context: &'cx ParseContext<'cx>) -> Self {
        Self {
            context,
            parsed: None,
        }
    }
}

impl<'t> ActionTrait<'t> for Action<'t> {
    fn program(&mut self, arg: &Program<'t>) -> Result<()> {
        todo!()
    }
}

/// A trait for processing tokens in the grammar.
///
/// This trait is used to process tokens in the grammar and convert them into
/// the appropriate types of the AST.
/// The implementations of this trait, which is the rest of this file, should be synced with the `parol` grammar file.
trait ProcessToken<'cx, 'dcx> {
    type Output;

    /// Processes a token into an [`Output`] type.
    ///
    /// This function may return an [`Err`] but should not.
    ///
    /// [`Output`]: ProcessToken::Output
    fn process_token(
        self,
        cx: &'cx ParseContext<'cx>,
        dcx: &'dcx DiagContext,
    ) -> Result<Self::Output>;
}

impl<'cx, 'dcx, T: ProcessToken<'cx, 'dcx>> ProcessToken<'cx, 'dcx> for Option<T> {
    type Output = Option<T::Output>;

    fn process_token(
        self,
        cx: &'cx ParseContext<'cx>,
        dcx: &'dcx DiagContext,
    ) -> Result<Self::Output> {
        if let Some(this) = self {
            Ok(Some(this.process_token(cx, dcx)?))
        } else {
            Ok(None)
        }
    }
}

/// Implementations for types that uniquely own their inner value.
macro_rules! impl_uniquely_own {
    () => {};
    ($([for $($bound:tt)*])? $([where $($bound2:tt)*])? for $t:ty, as $inner:ty, via $ident:ident => $e:expr; $($rest:tt)*) => {
        /// [`ProcessToken`] for types that uniquely own their inner value.
        impl<'cx, 'dcx, $($($bound)*)*> ProcessToken<'cx, 'dcx> for $t
        $(where $($bound2)*)?
        {
            type Output = <$inner as ProcessToken<'cx, 'dcx>>::Output;

            #[inline(always)]
            fn process_token(
                $ident,
                cx: &'cx ParseContext<'cx>,
                dcx: &'dcx DiagContext,
            ) -> Result<Self::Output> {
                <$inner as ProcessToken<'cx, 'dcx>>::process_token($e, cx, dcx)
            }
        }

        impl_uniquely_own! ($($rest)*);
    };
}

impl_uniquely_own! {
    [for T: ProcessToken<'cx, 'dcx>] for Box<T>, as T, via self => *self;
    [for 't]
        for grammar_trait::ScopeContentOpt1<'t>,
        as grammar_trait::Comment<'t>,
        via self => *self.comment;
}

trait TokenSpan {
    fn span(&self) -> syn::Span;
}

trait TokenLocation {
    fn location(&self) -> &'_ parol_runtime::Location;
}

impl TokenLocation for parol_runtime::Location {
    fn location(&self) -> &'_ parol_runtime::Location {
        self
    }
}

impl<T: TokenLocation> TokenSpan for T {
    fn span(&self) -> syn::Span {
        let parol_runtime::Location { start, end, .. } = self.location();
        syn::Span {
            start: syn::BytePos(*start),
            end: syn::BytePos(*end),
        }
    }
}

impl TokenLocation for grammar_trait::Break<'_> {
    fn location(&self) -> &'_ parol_runtime::Location {
        &self.r#break.location
    }
}

impl<'t, 'cx, 'dcx> ProcessToken<'cx, 'dcx> for grammar_trait::Scope<'t> {
    type Output = syn::Scope<'dcx>;

    fn process_token(
        self,
        cx: &'cx ParseContext<'cx>,
        dcx: &'dcx DiagContext,
    ) -> Result<Self::Output> {
        let vec: Vec<_> = self
            .scope_list
            .into_iter()
            .map(|x| x.scope_content.process_token(cx, dcx))
            .try_collect()?;
        Ok(syn::Scope {
            items: Box::leak(vec.into_boxed_slice()),
        })
    }
}

impl<'t, 'cx, 'dcx> ProcessToken<'cx, 'dcx> for grammar_trait::ScopeContentOpt<'t> {
    type Output = syn::token::Break;

    fn process_token(
        self,
        _cx: &'cx ParseContext<'cx>,
        _dcx: &'dcx DiagContext,
    ) -> Result<Self::Output> {
        Ok(syn::token::Break {
            position: syn::BytePos(self.r#break.r#break.location.start),
        })
    }
}

impl<'t, 'cx, 'dcx> ProcessToken<'cx, 'dcx> for grammar_trait::ScopeContent<'t> {
    type Output = syn::ScopeItem<'dcx>;

    fn process_token(
        self,
        cx: &'cx ParseContext<'cx>,
        dcx: &'dcx DiagContext,
    ) -> Result<Self::Output> {
        let Self {
            scope_content_opt,
            scope_content_opt0,
            scope_content_opt1,
            end_of_line: _end_of_line,
        } = self;
        let breaks = scope_content_opt.process_token(cx, dcx)?;
        let content = if let Some(scope_content_kind) = scope_content_opt0 {
            match *scope_content_kind.scope_content_kind {
                grammar_trait::ScopeContentKind::Block(block) => {
                    let block = block.block.process_token(dcx)?;
                    return Ok(syn::ScopeItem::Block(cx.alloc_block(block)));
                }
                grammar_trait::ScopeContentKind::Statement(stmt) => {
                    let stmt = stmt.statement.process_token(dcx)?;
                    Some(stmt)
                }
            }
        } else {
            None
        };
        let comment = scope_content_opt1.process_token(cx, dcx)?;
        Ok(syn::ScopeItem::Row(cx.alloc_row(syn::Row {
            breaks,
            content,
            comment,
        })))
    }
}

impl<'t, 'cx, 'dcx> ProcessToken<'cx, 'dcx> for grammar_trait::SuffixedNumeric<'t> {
    type Output = syn::Numeric<'cx>;

    fn process_token(
        self,
        cx: &'cx ParseContext<'cx>,
        dcx: &'dcx DiagContext,
    ) -> Result<Self::Output> {
        let numeric;
        let suffix;
        match self {
            grammar_trait::SuffixedNumeric::NumericIdent(suffixed_numeric_numeric_ident) => {
                numeric = suffixed_numeric_numeric_ident.numeric;
                suffix = Some(syn::NumericSuffix(
                    suffixed_numeric_numeric_ident
                        .ident
                        .process_token(cx, dcx)?,
                ));
            }
            grammar_trait::SuffixedNumeric::NumericWhiteSpace(
                suffixed_numeric_numeric_white_space,
            ) => {
                numeric = suffixed_numeric_numeric_white_space.numeric;
                suffix = None;
            }
        }
        Ok(match *numeric {
            grammar_trait::Numeric::BinaryInteger(binary_integer) => {
                let token = binary_integer.binary_integer.binary_integer;
                let prefix = syn::IntegerPrefix::Binary;
                syn::Numeric {
                    raw: cx.alloc_str(token.text()),
                    kind: syn::NumericKind::Integer(prefix),
                    suffix,
                }
            }
            grammar_trait::Numeric::OctalInteger(octal_integer) => {
                let token = octal_integer.octal_integer.octal_integer;
                let prefix = syn::IntegerPrefix::Octal;
                syn::Numeric {
                    raw: cx.alloc_str(token.text()),
                    kind: syn::NumericKind::Integer(prefix),
                    suffix,
                }
            }
            grammar_trait::Numeric::HexadecimalInteger(hexadecimal_integer) => {
                let token = hexadecimal_integer.hexadecimal_integer.hexadecimal_integer;
                let prefix = syn::IntegerPrefix::Hexadecimal;
                syn::Numeric {
                    raw: cx.alloc_str(token.text()),
                    kind: syn::NumericKind::Integer(prefix),
                    suffix,
                }
            }
            grammar_trait::Numeric::Ieee754Float(ieee754_float) => {
                let token = ieee754_float.ieee754_float.ieee754_float;
                syn::Numeric {
                    raw: cx.alloc_str(token.text()),
                    kind: syn::NumericKind::Float,
                    suffix,
                }
            }
        })
    }
}

impl<'t, 'cx, 'dcx> ProcessToken<'cx, 'dcx> for parol_runtime::Location {
    type Output = syn::Span;

    fn process_token(
        self,
        _cx: &'cx ParseContext<'cx>,
        _dcx: &'dcx DiagContext,
    ) -> Result<Self::Output> {
        Ok(syn::Span {
            start: syn::BytePos(self.start),
            end: syn::BytePos(self.end),
        })
    }
}

impl<'t, 'cx, 'dcx> ProcessToken<'cx, 'dcx> for grammar_trait::Comment<'t> {
    type Output = &'cx syn::Comment<'cx>;

    fn process_token(
        self,
        cx: &'cx ParseContext<'cx>,
        dcx: &'dcx DiagContext,
    ) -> Result<Self::Output> {
        Ok({
            let token = self.comment_content.comment_content;
            cx.alloc_comment(syn::Comment {
                content: cx.alloc_str(token.text()),
                span: token.location.process_token(cx, dcx)?,
            })
        })
    }
}

impl<'t, 'cx, 'dcx> ProcessToken<'cx, 'dcx> for grammar_trait::Ident<'t> {
    type Output = syn::Ident<'cx>;

    fn process_token(
        self,
        cx: &'cx ParseContext<'cx>,
        _: &'dcx DiagContext,
    ) -> Result<Self::Output> {
        let token = self.ident;
        Ok(syn::Ident {
            raw: cx.alloc_str(token.text()),
            span: token.location.span(),
        })
    }
}
