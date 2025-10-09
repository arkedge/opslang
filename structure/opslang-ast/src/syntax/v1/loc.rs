//! Extension traits for `opslang_ast` types.

use super::family::TypeFamily;

pub trait Span {
    fn span(&self) -> super::Span;

    fn span_start(&self) -> super::Position {
        self.span().start
    }

    fn span_end(&self) -> super::Position {
        self.span().end
    }
}

#[diagnostic::do_not_recommend]
impl Span for super::Span {
    fn span(&self) -> super::Span {
        *self
    }
}

pub trait Position {
    fn position(&self) -> super::Position;
}

#[diagnostic::do_not_recommend]
impl Position for super::Position {
    fn position(&self) -> super::Position {
        *self
    }
}

#[diagnostic::do_not_recommend]
impl<T: Position> Span for T {
    fn span(&self) -> super::Span {
        super::Span {
            start: self.position(),
            end: self.position(),
        }
    }

    fn span_start(&self) -> super::Position {
        self.position()
    }

    fn span_end(&self) -> super::Position {
        self.position()
    }
}

impl<'cx, F: TypeFamily<'cx, Span: Span>> Span for super::Comment<'cx, F> {
    fn span(&self) -> super::Span {
        self.span.span()
    }
}

impl Span for super::Ident<'_> {
    fn span(&self) -> super::Span {
        self.span
    }
}

impl Position for super::UnOp<'_> {
    fn position(&self) -> super::Position {
        match self {
            super::UnOp::Neg(hyphen) => hyphen.position,
            super::UnOp::IdRef(ampersand) => ampersand.position,
            super::UnOp::Deref(dollar) => dollar.position,
        }
    }
}
