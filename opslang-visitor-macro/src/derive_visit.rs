pub fn derive_visit(mut s: synstructure::Structure) -> proc_macro2::TokenStream {
    s.bind_with(|_| synstructure::BindStyle::Move);
    let visit_body = s.each(|bi| {
        quote::quote! {
            ::opslang_visitor::Visitor::visit(visitor, #bi);
        }
    });

    let visit_mut_body = s.each(|bi| {
        quote::quote! {
            ::opslang_visitor::VisitorMut::visit_mut(visitor, #bi);
        }
    });

    // Collect unique types of all binding fields to generate where clauses
    let mut field_types = std::collections::HashSet::new();
    for variant in s.variants() {
        for binding in variant.bindings() {
            field_types.insert(&binding.ast().ty);
        }
    }

    // Generate impl trait bounds for Visitor trait
    let visit_bounds = field_types.iter().map(|ty| {
        quote::quote! {
            ::opslang_visitor::Visitor<#ty>
        }
    });

    // Generate impl trait bounds for VisitorMut trait
    let visit_mut_bounds = field_types.iter().map(|ty| {
        quote::quote! {
            ::opslang_visitor::VisitorMut<#ty>
        }
    });

    s.gen_impl(quote::quote! {
        gen impl<V: #(#visit_bounds)+*> ::opslang_visitor::TemplateVisit<V> for @Self {
            fn super_visit(&self, visitor: &mut V) {
                match self { #visit_body }
            }
        }
        gen impl<V: #(#visit_mut_bounds)+*> ::opslang_visitor::TemplateVisitMut<V> for @Self {
            fn super_visit_mut(&mut self, visitor: &mut V) {
                match self { #visit_mut_body }
            }
        }
    })
}
