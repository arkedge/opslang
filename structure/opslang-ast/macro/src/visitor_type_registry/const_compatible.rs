use std::fmt::Debug;

use proc_macro2::Span;
use syn::Ident;

use super::*;

mod sealed {
    pub trait Sealed {}
}

pub trait ExecPhase: Clone + Copy + Debug + sealed::Sealed + TypeMap<Ident> {}
impl<P> ExecPhase for P where P: Clone + Copy + Debug + sealed::Sealed + TypeMap<Ident> {}

#[derive(Clone, Copy, Debug)]
pub struct Const;
impl sealed::Sealed for Const {}

#[derive(Clone, Copy, Debug)]
pub struct Runtime;
impl sealed::Sealed for Runtime {}

pub trait TypeMap<T> {
    type Mapped: Debug;
}

impl<T: Debug> TypeMap<T> for Runtime {
    type Mapped = T;
}

impl TypeMap<Ident> for Const {
    type Mapped = &'static str;
}

pub type Mapped<T, P> = <P as TypeMap<T>>::Mapped;

impl AstNodeTy<Const> {
    pub(super) fn parse(&self) -> AstNodeTy {
        let Self {
            name,
            child: module_path,
        } = self;
        AstNodeTy {
            name: Ident::new(name, Span::call_site()),
            child: module_path.map(|child| Ident::new(child, Span::call_site())),
        }
    }
}

impl AstInterTy<Const> {
    pub(super) fn parse(&self) -> AstInterTy {
        let Self { name, has_lifetime } = self;
        AstInterTy {
            name: Ident::new(name, Span::call_site()),
            has_lifetime: *has_lifetime,
        }
    }
}
