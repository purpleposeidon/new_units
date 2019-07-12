//! Zero-hassle marker unit library.

use std::any::type_name;
use std::{fmt, mem, ops};
use std::cmp::Ordering;
use std::marker::PhantomData;

pub mod prelude {
    pub use crate::Unit;
    pub use crate::OfUnit as _;
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(transparent))]
#[repr(transparent)]
pub struct Unit<T, M>(
    pub T,
    #[cfg_attr(feature = "serde", serde(skip))]
    pub PhantomData<M>,
);
impl<T, M> Unit<T, M> {
    // Hopefully this compiles to just the answer.
    // It could be a const fn if type_name() were coöperating.
    fn suffix() -> (&'static str, bool) {
        let name = type_name::<M>();
        let trim = |name: &'static str| name.rsplit("::").next().unwrap_or(name);
        if name.starts_with("[") && name.ends_with("; 0]") {
            (trim(&name[1 .. name.len() - 4]), true)
        } else {
            (trim(name), false)
        }
    }
    pub fn map<R>(self, f: impl FnOnce(T) -> R) -> Unit<R, M> {
        Unit(f(self.0), PhantomData)
    }
    pub fn with_unit<N>(self, _n: N) -> Unit<T, N> {
        Unit(self.0, PhantomData)
    }
}

impl<T: Clone, M> Clone for Unit<T, M> {
    fn clone(&self) -> Self {
        Self(self.0.clone(), PhantomData)
    }
}
impl<T: Copy, M> Copy for Unit<T, M> {}

impl<T: fmt::Debug, M> fmt::Debug for Unit<T, M> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let (name, index) = Self::suffix();
        if index {
            write!(f, "{}[{:?}]", name, self.0)
        } else {
            write!(f, "{:?} {}", self.0, name)
        }
    }
}

impl<T> Unit<T, ()> {
    pub const fn new<M>(t: T) -> Unit<T, M> {
        Unit(t, PhantomData)
    }
    pub const fn index<M>(t: T) -> Unit<T, [M; 0]> {
        Unit(t, PhantomData)
    }
}

/// Utility trait for constructing [`OfUnit`].
pub trait OfUnit: Sized {
    #[inline]
    fn of<M>(self, _m: M) -> Unit<Self, M> {
        if mem::size_of::<M>() != 0 {
            assert_0sized();
        }
        Unit(self, PhantomData)
    }
    #[inline]
    fn over<M>(self, _m: M) -> Unit<Self, [M; 0]> {
        if mem::size_of::<M>() != 0 {
            assert_0sized();
        }
        Unit(self, PhantomData)
    }
}
impl<X> OfUnit for X {}

#[cold]
fn assert_0sized() {
    panic!("Unit types are 0-sized");
}


macro_rules! impl_ops {
    (
        $($Add:ident fn $add:ident & $AddAssign:ident fn $add_assign:ident,)*
    ) => {$(
        impl<T, M> ops::$Add for Unit<T, M>
        where
            T: ops::$Add<T, Output = T>,
        {
            type Output = Self;
            #[inline]
            fn $add(self, rhs: Self) -> Self::Output {
                Self(ops::$Add::$add(self.0, rhs.0), PhantomData)
            }
        }
        impl<T, M> ops::$Add<T> for Unit<T, M>
        where
            T: ops::$Add<T, Output = T>,
        {
            type Output = Self;
            #[inline]
            fn $add(self, rhs: T) -> Self::Output {
                Self(ops::$Add::$add(self.0, rhs), PhantomData)
            }
        }
        impl<T, M> ops::$AddAssign for Unit<T, M>
        where
            T: ops::$AddAssign,
        {
            #[inline]
            fn $add_assign(&mut self, rhs: Self) {
                ops::$AddAssign::$add_assign(&mut self.0, rhs.0);
            }
        }
        impl<T, M> ops::$AddAssign<T> for Unit<T, M>
        where
            T: ops::$AddAssign,
        {
            #[inline]
            fn $add_assign(&mut self, rhs: T) {
                ops::$AddAssign::$add_assign(&mut self.0, rhs);
            }
        }
    )*};
}
impl_ops! {
    Add fn add & AddAssign fn add_assign,
    Sub fn sub & SubAssign fn sub_assign,
    Mul fn mul & MulAssign fn mul_assign,
    Div fn div & DivAssign fn div_assign,
    BitAnd fn bitand & BitAndAssign fn bitand_assign,
    BitOr fn bitor & BitOrAssign fn bitor_assign,
    BitXor fn bitxor & BitXorAssign fn bitxor_assign,
    Rem fn rem & RemAssign fn rem_assign,
    Shl fn shl & ShlAssign fn shl_assign,
    Shr fn shr & ShrAssign fn shr_assign,
}
// FIXME: Neg Not
impl<T, M> PartialEq for Unit<T, M>
where
    T: PartialEq,
{
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        PartialEq::eq(&self.0, &other.0)
    }
}
impl<T, M> PartialEq<T> for Unit<T, M>
where
    T: PartialEq,
{
    #[inline]
    fn eq(&self, other: &T) -> bool {
        PartialEq::eq(&self.0, other)
    }
}
impl<T: Eq, M> Eq for Unit<T, M> {}
impl<T, M> PartialOrd for Unit<T, M>
where
    T: PartialOrd
{
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.0.partial_cmp(&other.0)
    }
}
impl<T, M> PartialOrd<T> for Unit<T, M>
where
    T: PartialOrd
{
    #[inline]
    fn partial_cmp(&self, other: &T) -> Option<Ordering> {
        self.0.partial_cmp(other)
    }
}

use std::hash::{Hash, Hasher};
impl<T: Hash, M> Hash for Unit<T, M> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.hash(state)
    }
}
impl<T: Default, M> Default for Unit<T, M> {
    fn default() -> Self {
        Unit(T::default(), PhantomData)
    }
}


// We don't use TryInto because:
//  - usize is probably u64
//  - You're not likely to be indexing by u128
pub trait IntoIndex {
    fn into(self) -> usize;
    fn from(idx: usize) -> Self;
}
use std::convert::TryInto;
macro_rules! impl_into_index {
    ($($ty:ty),*) => {$(
        impl IntoIndex for $ty {
            #[inline(always)]
            fn into(self) -> usize { self as usize }
            fn from(idx: usize) -> Self {
                let crash = |_e| {
                    panic!("index {} does not fit in {}", idx, type_name::<Self>())
                };
                idx.try_into().unwrap_or_else(crash)
            }
        }
    )*}
}
impl_into_index![usize, u8, u16, u32];
// We don't add u64 because you'd just use usize.
impl<M> Unit<usize, M> {
    pub fn shrink<U: IntoIndex>(self) -> Unit<U, M> {
        Unit(U::from(self.0), PhantomData)
    }
}

impl<T, M, I> ops::Index<Unit<I, [M; 0]>> for Unit<T, M>
where
    T: ops::Index<usize>,
    I: IntoIndex,
{
    type Output = <T as ops::Index<usize>>::Output;
    fn index(&self, index: Unit<I, [M; 0]>) -> &Self::Output {
        let index: usize = index.0.into();
        &self.0[index]
    }
}
impl<T, M, I> ops::IndexMut<Unit<I, [M; 0]>> for Unit<T, M>
where
    T: ops::IndexMut<usize>,
    I: IntoIndex,
{
    fn index_mut(&mut self, index: Unit<I, [M; 0]>) -> &mut Self::Output {
        let index: usize = index.0.into();
        &mut self.0[index]
    }
}
// FIXME: Ranged indexing
// FIXME: Vec.len() ?
// FIXME: IntoIterator/Iter

impl<T, M> ops::Deref for Unit<T, M> {
    type Target = T;
    fn deref(&self) -> &Self::Target { &self.0 }
}
impl<T, M> ops::DerefMut for Unit<T, M> {
    fn deref_mut(&mut self) -> &mut T { &mut self.0 }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn introduction_example() {
        struct Inches;
        assert_eq!(
            3.of(Inches) * 10.of(Inches),
            30.of(Inches) * 1,
            "The answer isn't `30 Inches²` because this crate is not for units of measure.",
        );
        // Tho this doesn't compile:
        //      struct Meters;
        //      1.of(Inches) + 1.of(Meters);
        assert_eq!(3.of(Inches), 3);
    }

    #[test]
    fn indexing_example() {
        struct Cheese;
        let mut cheeses = vec![].of(Cheese);
        let mut register = |name| {
            let index = cheeses.len().over(Cheese);
            // You can transparently use a smaller index.
            let index = index.shrink::<u8>();
            cheeses.push(name);
            index
        };
        let gouda = register("gouda");
        let goata = register("goata");
        let bahda = register("bahda");
        dbg!(gouda);
        dbg!(cheeses[goata]);
        cheeses[bahda] = "greata";
        dbg!(&cheeses);

        struct Goats;
        let mut goats = vec![].of(Goats);
        let mut register = |name| {
            let index = goats.len().over(Goats);
            goats.push(name);
            index
        };
        let _billy = register("billy");
        let goatb = register("goatb");
        goats[goatb];
        cheeses[goata];
        // So confusing! Thankfully Rust lets us do deep type checking.
    }

    struct Meters;

    #[test]
    fn operations() {
        let zero = 0.0.of(Meters);
        let one = 1.0.of(Meters);
        dbg!(zero + one);
        let mut hey = zero;
        hey += one;
        hey += one;
        hey += one;

        assert_eq!(hey, 3.0);
        assert_eq!(hey, hey);
        assert_eq!(hey, one * 3.0);

        let mut foo = 1.0f32.of(Meters);
        foo *= 2.5;
        foo *= foo;
        assert_eq!(foo, 6.25);
        *foo = foo.floor();
        assert_eq!(foo, 6.0);
    }

    #[test]
    #[cfg(feature = "serde")]
    fn serializing() {
        fn assert<S: serde::Serialize>() {}
        assert::<Unit<i8, Meters>>();
    }
}
