use std::fmt::Debug;

use super::types::*;
use super::*;

mod sealed {
    pub trait Sealed {}
}

pub trait ExecPhase:
    Clone
    + Copy
    + Debug
    + sealed::Sealed
    + TypeMap<Ident>
    + TypeMap<Path>
    + TypeMap<IrNodeTyInstance>
    + TypeMap<InstanceKind>
{
}
impl<P> ExecPhase for P where
    P: Clone
        + Copy
        + Debug
        + sealed::Sealed
        + TypeMap<Ident>
        + TypeMap<Path>
        + TypeMap<IrNodeTyInstance>
        + TypeMap<InstanceKind>
{
}

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
impl TypeMap<Path> for Const {
    type Mapped = &'static str;
}
impl TypeMap<IrNodeTyInstance> for Const {
    type Mapped = Option<IrNodeTyInstance>;
}
impl TypeMap<InstanceKind> for Const {
    type Mapped = Option<InstanceKind>;
}

pub type Mapped<T, P> = <P as TypeMap<T>>::Mapped;

impl IrNodeTy<Const> {
    pub(super) fn parse(&self) -> IrNodeTy {
        let Self { name, child, ty } = self;
        IrNodeTy {
            name: Ident::new(name, Span::call_site()),
            child: child.map(|child| Ident::new(child, Span::call_site())),
            ty: ty.unwrap(),
        }
    }
}

impl IrInterTy<Const> {
    pub(super) fn parse(&self) -> IrInterTy {
        match self {
            IrInterTy::Instance(ir_inter_ty_instance) => {
                IrInterTy::Instance(ir_inter_ty_instance.parse())
            }
            IrInterTy::External(path) => IrInterTy::External(syn::parse_str(path).unwrap()),
        }
    }
}

impl IrInterTyInstance<Const> {
    fn parse(&self) -> IrInterTyInstance {
        let Self {
            name,
            child,
            ty,
            has_lifetime,
            wrapper,
        } = self;
        IrInterTyInstance {
            name: Ident::new(name, Span::call_site()),
            child: child.map(|child| Ident::new(child, Span::call_site())),
            ty: ty.unwrap(),
            has_lifetime: *has_lifetime,
            wrapper: wrapper.map(|child| syn::parse_str(child).unwrap()),
        }
    }
}
