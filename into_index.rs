/// Converts between `usize`.
/// Exposed as `new_units::NewUnits_Secret_IntoIndex`.
#[doc(hidden)]
pub trait IntoIndex {
    fn into(self) -> usize;
    fn from(idx: usize) -> Self;
}
// We don't use TryInto because:
//  - usize is probably u64
//  - You're not likely to be indexing by u128

use std::any::type_name;
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
// We don't add u64 because you can just use usize on 64-bit platforms,
// and it'd be buggy on 32-bit platforms.

impl IntoIndex for bool {
    #[inline(always)]
    fn into(self) -> usize { self as usize }
    fn from(idx: usize) -> Self {
        match idx {
            0 => false,
            1 => true,
            _ => panic!("0 or 1 becomes bool, not {}", idx),
        }
    }
}

