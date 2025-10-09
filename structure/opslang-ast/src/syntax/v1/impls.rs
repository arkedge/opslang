use super::{
    Bytes, Comment, Debug, Expr, ExprKind, ExprMut, HexBytes, Ident, Path, Row, String,
    ToplevelItem, TypeFamily,
};
use std::fmt::Write;

impl<'cx, F: TypeFamily<'cx>> ToplevelItem<'cx, F> {
    pub fn is_empty(&self) -> bool {
        let Self { kind, comment } = self;
        kind.is_none() && comment.is_none()
    }
}

impl<'cx, F: TypeFamily<'cx>> Row<'cx, F> {
    pub fn is_empty(&self) -> bool {
        let Self {
            breaks,
            statement,
            comment,
        } = self;
        breaks.is_none() && statement.is_none() && comment.is_none()
    }
}

impl<'cx, F: TypeFamily<'cx>> Comment<'cx, F> {
    #[inline]
    pub fn is_meta(&self) -> bool {
        self.content.starts_with('!')
    }
}

impl<'cx, F: TypeFamily<'cx>> std::ops::Deref for Expr<'cx, F> {
    type Target = &'cx ExprKind<'cx, F>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'cx, F: TypeFamily<'cx>> Debug for Expr<'cx, F> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl<'cx, F: TypeFamily<'cx>> PartialEq for Expr<'cx, F> {
    fn eq(&self, other: &Self) -> bool {
        self.0.eq(other.0)
    }
}

impl<'cx, F: TypeFamily<'cx>> Clone for Expr<'cx, F> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<'cx, F: TypeFamily<'cx>> Copy for Expr<'cx, F> {}

impl<'cx, F: TypeFamily<'cx>> std::ops::Deref for ExprMut<'cx, F> {
    type Target = ExprKind<'cx, F>;

    fn deref(&self) -> &Self::Target {
        self.0
    }
}

impl<'cx, F: TypeFamily<'cx>> std::ops::DerefMut for ExprMut<'cx, F> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.0
    }
}

impl<'cx, F: TypeFamily<'cx>> Debug for ExprMut<'cx, F> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl<'cx, F: TypeFamily<'cx>> PartialEq for ExprMut<'cx, F> {
    fn eq(&self, other: &Self) -> bool {
        self.0.eq(&other.0)
    }
}

impl<'cx, F: TypeFamily<'cx>> Path<'cx, F> {
    pub fn is_ident(&self) -> Option<F::Ident> {
        if self.segments.len() == 1 {
            Some(self.segments[0])
        } else {
            None
        }
    }
}

impl<'cx, F: TypeFamily<'cx, Ident = Ident<'cx, F>>> std::fmt::Display for Path<'cx, F> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (i, segment) in self.segments.iter().enumerate() {
            if i > 0 {
                f.write_char('.')?;
            }
            f.write_str(segment.raw)?;
        }
        Ok(())
    }
}

impl<'cx, F: TypeFamily<'cx>> std::fmt::Display for Ident<'cx, F> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.raw)
    }
}

impl<'cx, F: TypeFamily<'cx>> String<'cx, F> {
    pub fn unescape(&self) -> Result<std::string::String, escape8259::UnescapeError> {
        escape8259::unescape(self.raw)
    }
}

impl<'cx, F: TypeFamily<'cx>> Bytes<'cx, F> {
    pub fn as_bytes(&self) -> &'cx [u8] {
        self.raw.as_bytes()
    }
}

impl<'cx, F: TypeFamily<'cx>> HexBytes<'cx, F> {
    pub fn as_bytes(&self) -> Result<Vec<u8>, char> {
        self.raw
            .chars()
            .map(|c| match c {
                c @ '0'..='9' => Ok(c as u8 - b'0'),
                c @ 'a'..='f' => Ok(c as u8 - b'a' + 10),
                c @ 'A'..='F' => Ok(c as u8 - b'A' + 10),
                c => Err(c),
            })
            .collect()
    }
}
