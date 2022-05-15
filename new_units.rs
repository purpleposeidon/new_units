//! Zero-hassle marker unit library. Correct code made easy.
//!
//! # Example
//!
//! ```
//! use new_units::*;
//!
//! struct Inches;
//! assert_eq!(
//!     3.of(Inches) * 10.of(Inches),
//!     30.of(Inches) * 1,
//!     "The answer isn't `30 Inches²` because this crate is not for units of measure.",
// ------- Okay? Show us what you're good at then.
//! );
// Tho this doesn't compile:
//      struct Meters;
//      1.of(Inches) + 1.of(Meters);
//! assert_eq!(3.of(Inches), 3);
//! ```

use std::any::type_name;
use std::{fmt, mem, ops};
use std::cmp::Ordering;
use std::marker::PhantomData;

/// Add a marker to another type, eg `Unit<Point3, LastPosition>`.
///
/// `Unit`s try to be as transparent as possible: they deref to `T`, and the various operators
/// forward to it.
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(transparent))]
#[repr(transparent)]
pub struct Unit<T, Marker>(
    pub T,
    #[doc(hidden)]
    #[cfg_attr(feature = "serde", serde(skip))]
    pub PhantomData<Marker>,
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
    /// Change the contained value. Note that `Unit` also implements `Deref`, `DerefMut`, etc; or
    /// you can use `self.0`.
    pub fn map<R>(self, f: impl FnOnce(T) -> R) -> Unit<R, M> {
        Unit(f(self.0), PhantomData)
    }
    /// Change the unit.
    /// # Example
    /// ```
    /// use new_units::*;
    /// struct Position;
    /// struct LastPosition;
    /// let position = (0, 0).of(Position);
    /// // (And then some stuff happens...)
    /// let previous_position = position.with_unit(LastPosition);
    /// ```
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
        if mem::size_of::<M>() != 0 { assert_0sized(); }
        Unit(t, PhantomData)
    }
    /// Constructs an index. You can use this to prevent using an index on the wrong value.
    ///
    /// See [`OfUnit::over`](trait.OfUnit.html#method.over) for a more pleasant API.
    ///
    /// # Example
    /// ```
    /// # use new_units::Unit;
    /// let data1 = vec![0];
    /// struct Marked;
    /// let data2 = Unit::new::<Marked>(data1.clone());
    /// assert_eq!(
    ///     data1[0],
    ///     data2[Unit::index::<Marked>(0usize)],
    /// );
    /// ```
    pub const fn index<M>(t: T) -> Unit<T, [M; 0]> {
        if mem::size_of::<M>() != 0 { assert_0sized(); }
        Unit(t, PhantomData)
    }
}

impl<T, M> Unit<T, M> {
    // If you want Unit::<T, M>::new.
    // Unit::<T, ()>::new::<M> might not cut it in certain situations.
    // Possibly unnecessary.
    #[doc(hidden)]
    pub const fn make(t: T) -> Unit<T, M> {
        if mem::size_of::<M>() != 0 { assert_0sized(); }
        Unit(t, PhantomData)
    }
}

/// Utility trait for constructing [`Unit`]s.
pub trait OfUnit: Sized {
    /// Ascribe a unit onto this value.
    ///
    /// # Example
    /// ```
    /// # use new_units::*;
    /// struct Diamonds;
    /// 4.of(Diamonds);
    /// ```
    #[inline]
    fn of<M>(self, _m: M) -> Unit<Self, M> {
        if mem::size_of::<M>() != 0 { assert_0sized(); }
        Unit(self, PhantomData)
    }
    /// Let this value be an index into `M`s.
    ///
    /// # Example
    /// ```
    /// # use new_units::*;
    ///
    /// // The marker type.
    /// struct Cheese;
    /// let mut cheeses = vec![].of(Cheese);
    /// // made cheeses: Unit<Vec<&str>, Cheese>
    ///
    /// // Adds a new cheese to the list, and return its index.
    /// let mut register = |name| {
    ///     let index: IndexTo<Cheese, usize> = cheeses.len().over(Cheese);
    ///     cheeses.push(name);
    ///     // Save space by using u8 instead of usize.
    ///     index.shrink::<u8>()
    /// };
    ///
    /// let gouda = register("gouda");
    /// let goata = register("goata");
    /// let bahda = register("bahda");
    ///
    /// dbg!(gouda);
    /// dbg!(cheeses[goata]);
    /// cheeses[bahda] = "greata";
    /// dbg!(&cheeses);
    ///
    ///
    /// struct Goats;
    /// let mut goats = vec![].of(Goats);
    /// let mut register = |name| {
    ///     let index = goats.len().over(Goats);
    ///     goats.push(name);
    ///     index
    /// };
    /// let billy = register("billy");
    /// let goatb = register("goatb");
    ///
    /// // So confusing! Thankfully Rust lets us do deep type checking.
    /// goats[goatb];
    /// cheeses[goata];
    /// ```
    #[inline]
    fn over<M>(self, _m: M) -> Unit<Self, [M; 0]> {
        if mem::size_of::<M>() != 0 { assert_0sized(); }
        Unit(self, PhantomData)
    }
}
impl<X> OfUnit for X {}

#[cold]
const fn assert_0sized() -> ! { panic!("Unit types are 0-sized") }

mod impl_ops;
mod into_index;

// Weird name so that it's less likely to hit something.
#[doc(hidden)]
pub use self::into_index::IntoIndex as NewUnits_Secret_IntoIndex;

impl<M> Unit<usize, M> {
    /// Reduce the size. Panics if too large.
    ///
    /// # See Also
    /// [`IntoIndex`](../src/new_units/into_index.rs.html)
    pub fn shrink<U: self::into_index::IntoIndex>(self) -> Unit<U, M> {
        Unit(U::from(self.0), PhantomData)
    }
}


/// `IndexTo<Cars>` indexes `Vec<_, Cars>`.
pub type IndexTo<Marker, IndexType = usize> = Unit<IndexType, [Marker; 0]>;


#[cfg(test)]
mod test {
    use super::*;

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
