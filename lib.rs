//! This crate provides a simple & easy way to distinguish types, eg in `HashMap<TypeId, _>`.
//! If you want to do [dimensional analysis](https://crates.io/keywords/dimensional-analysis),
//! there are better crates for that.
use std::{fmt, ops};
use std::marker::PhantomData;
use std::cmp::Ordering;
use std::hash::{Hash, Hasher};

/// Boiler-plate reducing macro.
///
/// # Usage
/// ```no_compile
/// new_unit! { pub type Meter = NewUnit<f32, MeterUnit>; }
/// new_unit! { pub type Meter = NewUnit<f32, MeterUnit, "meters">; }
/// new_unit! { pub type Meter<T> = NewUnit<T, MeterUnit>; }
/// new_unit! { pub type Meter<T> = NewUnit<T, MeterUnit, "meters">; }
/// ```
///
/// This creates a type alias. It may have a generic parameter.
/// You may provide a visibility for the item.
/// `Debug` will be derived, or it will use the string if given.
#[macro_export]
macro_rules! new_unit {
    (
        $pub:vis type $Name:ident = NewUnit<$ty:ty, $BaseName:ident>;
    ) => {
        $pub type $Name = $crate::NewUnit<$ty, $BaseName>;
        #[derive(Default, Copy, Clone, Debug)]
        $pub struct $BaseName;
    };
    (
        $pub:vis type $Name:ident = NewUnit<$ty:ty, $BaseName:ident, $name:literal>;
    ) => {
        $pub type $Name = $crate::NewUnit<$ty, $BaseName>;
        #[derive(Default, Copy, Clone)]
        $pub struct $BaseName;
        impl std::fmt::Debug for $BaseName {
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                write!(f, $name)
            }
        }
    };
    (
        $pub:vis type $Name:ident<$T:ident> = NewUnit<$_T:ident, $BaseName:ident>;
    ) => {
        $pub type $Name<$T> = $crate::NewUnit<$T, $BaseName>;
        #[derive(Default, Copy, Clone, Debug)]
        $pub struct $BaseName;
    };
    (
        $pub:vis type $Name:ident<$T:ident> = NewUnit<$_T:ident, $BaseName:ident, $name:literal>;
    ) => {
        $pub type $Name<$T> = $crate::NewUnit<$T, $BaseName>;
        #[derive(Default, Copy, Clone)]
        $pub struct $BaseName;
        impl std::fmt::Debug for $BaseName {
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                write!(f, $name)
            }
        }
    };
}

/// The preferred way of instantiating is to use `let distance: Meter = 0.0.into()`.
///
/// You can do basic math (`+-*/`) using either a matching `NewUnit`, or with the raw inner type.
/// It implements `Deref`/`DerefMut`.
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(transparent))]
// #[cfg_attr(feature = "serde", serde(bound(deserialize = "Inner: serde::de::DeserializeOwned")))]
// #[cfg_attr(feature = "serde", serde(bound(  serialize = "Inner: serde::Serialize")))]
#[repr(transparent)]
pub struct NewUnit<Inner, Marker> {
    pub inner: Inner,
    #[cfg_attr(feature = "serde", serde(skip))]
    pub marker: PhantomData<Marker>,
}
impl<T, M> ops::Deref for NewUnit<T, M> {
    type Target = T;
    fn deref(&self) -> &T { &self.inner }
}
impl<T, M> ops::DerefMut for NewUnit<T, M> {
    fn deref_mut(&mut self) -> &mut T { &mut self.inner }
}
impl<T: Copy, M> Copy for NewUnit<T, M> {}
impl<T: Eq, M> Eq for NewUnit<T, M> {}
impl<T: PartialEq, M> PartialEq for NewUnit<T, M> {
    fn eq(&self, other: &Self) -> bool {
        self.inner.eq(&other.inner)
    }
}
impl<T: PartialEq, M> PartialEq<T> for NewUnit<T, M> {
    fn eq(&self, other: &T) -> bool {
        self.inner.eq(other)
    }
}
impl<T: PartialOrd, M> PartialOrd for NewUnit<T, M> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.inner.partial_cmp(&other.inner)
    }
}
impl<T: PartialOrd, M> PartialOrd<T> for NewUnit<T, M> {
    fn partial_cmp(&self, other: &T) -> Option<Ordering> {
        self.inner.partial_cmp(other)
    }
}
impl<T: Ord, M> Ord for NewUnit<T, M> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.inner.cmp(&other.inner)
    }
}
impl<T, M> Clone for NewUnit<T, M>
where
    T: Clone,
{
    fn clone(&self) -> Self {
        NewUnit {
            inner: self.inner.clone(),
            marker: self.marker,
        }
    }
}
impl<T: fmt::Display, M: fmt::Debug + Default> fmt::Display for NewUnit<T, M> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} {:?}", self.inner, M::default())
    }
}
impl<T: fmt::Debug, M: fmt::Debug + Default> fmt::Debug for NewUnit<T, M> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?} {:?}", self.inner, M::default())
    }
}
impl<T, M> From<T> for NewUnit<T, M> {
    fn from(inner: T) -> Self {
        NewUnit {
            inner,
            marker: PhantomData,
        }
    }
}
impl<T, M> Hash for NewUnit<T, M>
where
    T: Hash
{
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.inner.hash(state)
    }
}

macro_rules! impls {
    ($(
        $trait:ident fn $func:ident & $incr_trait:ident fn $incr_func:ident,
    )*) => {$(
        impl<T, M> ops::$trait for NewUnit<T, M>
        where
            T: ops::$trait<Output=T>,
        {
            type Output = Self;
            #[inline]
            fn $func(self, rhs: Self) -> Self::Output {
                self.inner.$func(rhs.inner).into()
            }
        }
        impl<T, M> ops::$trait<T> for NewUnit<T, M>
        where
            T: ops::$trait<Output=T>,
        {
            type Output = Self;
            #[inline]
            fn $func(self, t: T) -> Self::Output {
                self.inner.$func(t).into()
            }
        }
        impl<T, M> ops::$incr_trait for NewUnit<T, M>
        where
            T: ops::$incr_trait,
        {
            #[inline]
            fn $incr_func(&mut self, rhs: Self) {
                self.inner.$incr_func(rhs.inner);
            }
        }
        impl<T, M> ops::$incr_trait<T> for NewUnit<T, M>
        where
            T: ops::$incr_trait,
        {
            #[inline]
            fn $incr_func(&mut self, t: T) {
                self.inner.$incr_func(t);
            }
        }
    )*};
}
impls! {
    Add fn add & AddAssign fn add_assign,
    Sub fn sub & SubAssign fn sub_assign,
    Mul fn mul & MulAssign fn mul_assign,
    Div fn div & DivAssign fn div_assign,
}


#[cfg(test)]
mod test {
    use super::*;

    new_unit! { pub type Meters = NewUnit<f32, MeterUnit, "meters">; }
    new_unit! { pub type Feet = NewUnit<f32, FeetUnit>; }

    #[test]
    fn stuff() {
        let zero: Meters = 0.0.into();
        let one: Meters = 1.0.into();
        println!("{:?}", zero + one);
        let mut hey = zero;
        hey += one;
        hey += one;
        hey += one;

        assert_eq!(hey, 3.0);
        assert_eq!(hey, hey);
        assert_eq!(hey, one * 3.0);

        let mut foo: Meters = 1.0.into();
        foo *= 2.5;
        foo *= foo;
        assert_eq!(foo, 6.25);
        *foo = foo.floor();
        assert_eq!(foo, 6.0);

        let _: Feet = 3.0.into();

        let _: Length<bool> = true.into();
        let _: Rotation<()> = ().into();
    }

    new_unit! { pub type Length<T> = NewUnit<T, LengthUnit>; }
    new_unit! { pub type Rotation<T> = NewUnit<T, RotationUnit, "rotated">; }

    #[test]
    #[cfg(feature = "serde")]
    fn serializing() {
        fn assert<S: serde::Serialize>() {}
        assert::<Meters>();
    }
}
