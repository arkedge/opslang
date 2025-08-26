use synstructure::decl_derive;

mod derive_visit;

decl_derive!([Visit] => derive_visit::derive_visit);
