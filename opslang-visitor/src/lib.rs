#[diagnostic::on_unimplemented(
    message = "`{V}` do not know how `{Self}` is constructed",
    label = "lacking `{Self}: TemplateVisit<{V}>`",
    note = "add impl `for<V> TemplateVisit<V> for `{Self}` using macro"
)]
pub trait TemplateVisit<V: ?Sized> {
    #[inline]
    #[allow(unused_variables)]
    fn super_visit(&self, visitor: &mut V) {}
}

#[diagnostic::on_unimplemented(
    message = "`{V}` do not know how `{Self}` is constructed",
    label = "lacking `{Self}: TemplateVisitMut<{V}>`",
    note = "add impl `for<V> TemplateVisitMut<V> for `{Self}` using macro"
)]
pub trait TemplateVisitMut<V: ?Sized> {
    #[inline]
    #[allow(unused_variables)]
    fn super_visit_mut(&mut self, visitor: &mut V) {}
}

#[diagnostic::on_unimplemented(
    message = "`{Self}` does not visit `{T}`",
    label = "lacking `{Self}: Visitor<{T}>`",
    note = "add impl `Visitor<{T}> for `{Self}` using macro"
)]
pub trait Visitor<T: ?Sized> {
    fn visit(&mut self, node: &T);
}

#[diagnostic::on_unimplemented(
    message = "`{Self}` does not visit `T` mutably",
    label = "lacking `{Self}: VisitorMut<{T}>`",
    note = "add impl `VisitorMut<{T}> for `{Self}` using macro"
)]
pub trait VisitorMut<T: ?Sized> {
    fn visit_mut(&mut self, node: &mut T);
}

/// Generates base case implementations for [`TemplateVisit`] and [`TemplateVisitMut`].
///
/// This macro creates empty implementations for types that serve as base cases
/// in the visitor pattern, providing default no-op behavior for both immutable
/// and mutable visiting.
///
/// # Examples
///
/// ```ignore
/// impl_template_visit_base_case!(str);
/// impl_template_visit_base_case!(u32, i32, f64);
/// ```
#[macro_export]
macro_rules! impl_template_visit_base_case {
    ($($t:ty),+ $(,)?) => {
        $(
            impl<V: ?Sized> $crate::TemplateVisit<V> for $t {}
            impl<V: ?Sized> $crate::TemplateVisitMut<V> for $t {}
        )+
    };
}

/// Generates visitor trait implementations for visitor types.
///
/// This macro automatically implements the [`Visitor`] trait for a given visitor type and target type,
/// delegating to the target type's [`TemplateVisit`] implementation. It supports both immutable (`visit`)
/// and mutable (`visit_mut`) visitor patterns.
///
/// # Syntax
///
/// ```ignore
/// impl_visitor!(< generics > VisitorType [visit] TargetType where constraints);
/// impl_visitor!(< generics > VisitorType [visit_mut] TargetType where constraints);
/// ```
///
/// # Generated Implementation
///
/// For the `[visit]` variant, generates:
/// ```ignore
/// impl<generics> Visitor<TargetType> for VisitorType where constraints {
///     fn visit(&mut self, node: &TargetType) {
///         <TargetType as TemplateVisit<Self>>::super_visit(node, self);
///     }
/// }
/// ```
///
/// # Examples
///
/// ```ignore
/// // Basic usage for a simple visitor type
/// impl_visitor!(<> MyVisitor [visit] AstNode);
///
/// // With generic parameters and constraints
/// impl_visitor!(<V, T> Wrapper<V> [visit] Option<T> where Wrapper<V>: Visitor<T>);
///
/// // For reference types
/// // Tips: complex bounds such as `?Sized` is accepted by parenthesized
/// impl_visitor!(<V, T: (?Sized)> MyVisitor<V> [visit] &T where MyVisitor<V>: Visitor<T>);
/// ```
#[macro_export]
macro_rules! impl_visitor {
    (< $( $lt:tt $( : $clt:tt $(+ $dlt:tt )* )? ),* > $v:ty [visit] $t:ty $(where $($tt:tt)*)?) => {
        impl < $( $lt $( : $clt $(+ $dlt )* )? ),* > $crate::Visitor<$t> for $v $(where $($tt)*)? {
            fn visit(&mut self, node: &$t) {
                <$t as $crate::TemplateVisit<Self>>::super_visit(node, self);
            }
        }
    };
    (< $( $lt:tt $( : $clt:tt $(+ $dlt:tt )* )? ),* > $v:ty [visit_mut] $t:ty $(where $($tt:tt)*)?) => {
        impl < $( $lt $( : $clt $(+ $dlt )* )? ),* > $crate::VisitorMut<$t> for $v {
            fn visit_mut(&mut self, node: &mut $t) {
                <$t as $crate::TemplateVisitMut<Self>>::super_visit_mut(node, self);
            }
        }
    };
}

mod impls;
