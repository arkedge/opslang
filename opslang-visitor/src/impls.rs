use crate::{TemplateVisit, TemplateVisitMut, Visitor, VisitorMut};

impl_template_visit_base_case!(
    str,
    std::convert::Infallible,
    usize,
    u8,
    chrono::DateTime<chrono::Utc>
);

impl<V: ?Sized + Visitor<T>, T> TemplateVisit<V> for Option<T> {
    fn super_visit(&self, visitor: &mut V) {
        if let Some(node) = self {
            visitor.visit(node);
        }
    }
}

impl<V: ?Sized + VisitorMut<T>, T> TemplateVisitMut<V> for Option<T> {
    fn super_visit_mut(&mut self, visitor: &mut V) {
        if let Some(node) = self {
            visitor.visit_mut(node);
        }
    }
}

impl<V: ?Sized + Visitor<T>, T: ?Sized> TemplateVisit<V> for &T {
    fn super_visit(&self, visitor: &mut V) {
        <V as Visitor<T>>::visit(visitor, &**self);
    }
}

/// Enables immutable visitation during mutable visitor traversal.
///
/// This implementation is more important than it appears. When encountering an immutable reference
/// during a mutable visitor traversal, it allows the visitor to continue with immutable visitation
/// of the inner type, ensuring the visitor pattern remains consistent and type-safe.
impl<V: ?Sized + Visitor<T>, T: ?Sized> TemplateVisitMut<V> for &T {
    fn super_visit_mut(&mut self, visitor: &mut V) {
        <V as Visitor<T>>::visit(visitor, &**self);
    }
}

/// Enables immutable visitation of mutable references.
impl<V: ?Sized + Visitor<T>, T: ?Sized> TemplateVisit<V> for &mut T {
    fn super_visit(&self, visitor: &mut V) {
        <V as Visitor<T>>::visit(visitor, &**self);
    }
}

impl<V: ?Sized + VisitorMut<T>, T: ?Sized> TemplateVisitMut<V> for &mut T {
    fn super_visit_mut(&mut self, visitor: &mut V) {
        <V as VisitorMut<T>>::visit_mut(visitor, &mut **self);
    }
}

impl<V: ?Sized + Visitor<T>, T> TemplateVisit<V> for [T] {
    fn super_visit(&self, visitor: &mut V) {
        for node in self.iter() {
            visitor.visit(node);
        }
    }
}

impl<V: ?Sized + VisitorMut<T>, T> TemplateVisitMut<V> for [T] {
    fn super_visit_mut(&mut self, visitor: &mut V) {
        for node in self.iter_mut() {
            visitor.visit_mut(node);
        }
    }
}

impl<V: ?Sized + Visitor<T>, T> TemplateVisit<V> for Vec<T> {
    fn super_visit(&self, visitor: &mut V) {
        for node in self {
            visitor.visit(node);
        }
    }
}

impl<V: ?Sized + VisitorMut<T>, T> TemplateVisitMut<V> for Vec<T> {
    fn super_visit_mut(&mut self, visitor: &mut V) {
        for node in self {
            visitor.visit_mut(node);
        }
    }
}

impl<V: ?Sized + Visitor<T1> + Visitor<T2>, T1, T2> TemplateVisit<V> for (T1, T2) {
    fn super_visit(&self, visitor: &mut V) {
        let (x1, x2) = self;
        visitor.visit(x1);
        visitor.visit(x2);
    }
}

impl<V: ?Sized + VisitorMut<T1> + VisitorMut<T2>, T1, T2> TemplateVisitMut<V> for (T1, T2) {
    fn super_visit_mut(&mut self, visitor: &mut V) {
        let (x1, x2) = self;
        visitor.visit_mut(x1);
        visitor.visit_mut(x2);
    }
}
