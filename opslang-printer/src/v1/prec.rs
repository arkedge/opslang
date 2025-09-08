use core::fmt;
use std::fmt::Write;

use opslang_ast::{BinOp, ExprKind, UnOp};

use crate::{PrettyPrint, PrintOptions, Strategy, v1::PrintableFamily};

/// Operator precedence for proper parenthesization
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Precedence(u8);

/// Constant definitions.
///
/// Note that `pub`lished constant leaks definition of `precedence`.
impl Precedence {
    pub const SET: Self = Self(0); // :=
    const LOGICAL_OR: Self = Self(2); // ||
    const LOGICAL_AND: Self = Self(3); // &&
    pub const COMPARE: Self = Self(5); // >=, <=, >, <, !=, /=, ==
    const ARITHMETIC: Self = Self(6); // +, -
    const FACTOR: Self = Self(7); // *, /, %
    const PREFIX: Self = Self(8); // unary -
    pub const APPLY: Self = Self(9); // function application
    const LOWER_PREFIX: Self = Self(10); // &, $
    pub const ATOMIC: Self = Self(11); // grouping, literals, etc.
}

pub trait HasPrecedence {
    fn precedence(&self) -> Precedence;
}

impl<'cx, F: PrintableFamily<'cx>> HasPrecedence for ExprKind<'cx, F> {
    /// Get the precedence of an expression kind.
    fn precedence(&self) -> Precedence {
        let expr = self;
        match expr {
            ExprKind::Set(_) => Precedence::SET,
            ExprKind::Binary(binary) => binary.op.precedence(),
            ExprKind::Compare(_) => Precedence::COMPARE,
            ExprKind::Unary(unary) => unary.op.precedence(),
            ExprKind::Apply(_) => Precedence::APPLY,
            ExprKind::Variable(_)
            | ExprKind::Literal(_)
            | ExprKind::Parened(_)
            | ExprKind::Qualif(_)
            | ExprKind::PreQualified(_)
            | ExprKind::InfixImport(_)
            | ExprKind::If(_) => Precedence::ATOMIC,
        }
    }
}

impl<'cx, F: PrintableFamily<'cx>> HasPrecedence for BinOp<'cx, F> {
    fn precedence(&self) -> Precedence {
        match self {
            BinOp::Or(_) => Precedence::LOGICAL_OR,
            BinOp::And(_) => Precedence::LOGICAL_AND,
            BinOp::Add(_) | BinOp::Sub(_) => Precedence::ARITHMETIC,
            BinOp::Mul(_) | BinOp::Div(_) | BinOp::Mod(_) => Precedence::FACTOR,
        }
    }
}

impl<'cx, F: PrintableFamily<'cx>> HasPrecedence for UnOp<'cx, F> {
    fn precedence(&self) -> Precedence {
        match self {
            UnOp::Neg(_) => Precedence::PREFIX,
            UnOp::IdRef(_) | UnOp::Deref(_) => Precedence::LOWER_PREFIX,
        }
    }
}

impl Precedence {
    /// Print an expression with parentheses if needed.
    ///
    /// This function will give parens when `self` is greater than the
    /// precedence of the child expression. In other words, if the child expression is
    /// the same or more prioritized than the parent, it will be printed without parens.
    pub fn write_with_parens<'cx, S: Strategy, F: PrintableFamily<'cx>>(
        self,
        expr: &ExprKind<'cx, F>,
        writer: &mut impl Write,
        options: &PrintOptions<S>,
    ) -> fmt::Result
    where
        ExprKind<'cx, F>: PrettyPrint<S>,
    {
        if expr.precedence() < self {
            writer.write_str("(")?;
            PrettyPrint::<S>::pretty_print(expr, writer, options)?;
            writer.write_str(")")
        } else {
            PrettyPrint::<S>::pretty_print(expr, writer, options)
        }
    }
}
