use super::*;

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

impl<T, M> ops::Deref for Unit<T, M> {
    type Target = T;
    fn deref(&self) -> &Self::Target { &self.0 }
}
impl<T, M> ops::DerefMut for Unit<T, M> {
    fn deref_mut(&mut self) -> &mut T { &mut self.0 }
}

impl<T, M, I> ops::Index<Unit<I, [M; 0]>> for Unit<T, M>
where
    T: ops::Index<usize>,
    I: self::into_index::IntoIndex,
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
    I: self::into_index::IntoIndex,
{
    fn index_mut(&mut self, index: Unit<I, [M; 0]>) -> &mut Self::Output {
        let index: usize = index.0.into();
        &mut self.0[index]
    }
}
// FIXME: Ranged indexing
// FIXME: Vec.len() ?
// FIXME: IntoIterator/Iter

impl<T, Marker> From<T> for Unit<T, Marker> {
    fn from(t: T) -> Self {
        Unit(t, PhantomData)
    }
}

#[cfg(test)]
#[test]
fn from() {
    struct Meters;
    let _: Unit<f32, Meters> = 0.0.into();
}
