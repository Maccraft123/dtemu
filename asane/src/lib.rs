pub mod intel8085;

use static_assertions::const_assert_eq;
use core::fmt;

mod cpu_prelude {
    pub use crate::{Bus, BusRo, BusWo, Cpu, TwoBytes};
    pub use bitfield_struct::bitfield;
    pub use futures_lite::future::yield_now;
}

#[derive(Copy, Clone)]
#[repr(C)]
pub union TwoBytes {
    b: (u8, u8),
    word: u16,
}

const_assert_eq!(TwoBytes::with_word(0xff00).hi(), 0xff);
const_assert_eq!(TwoBytes::with_word(0x00ff).lo(), 0xff);
const_assert_eq!(TwoBytes::with_word(0x1234).word(), 0x1234);

impl TwoBytes {
    #[inline(always)]
    pub const fn with_word(word: u16) -> Self {
        Self { word }
    }
    #[inline(always)]
    pub const fn zeroed() -> Self {
        Self { word: 0 }
    }
    #[inline(always)]
    pub const fn hi_ref(&self) -> &u8 {
        #[cfg(target_endian = "little")]
        unsafe { &self.b.1 }
        #[cfg(target_endian = "big")]
        unsafe { &self.b.0 }
    }
    #[inline(always)]
    pub const fn lo_ref(&self) -> &u8 {
        #[cfg(target_endian = "little")]
        unsafe { &self.b.0 }
        #[cfg(target_endian = "big")]
        unsafe { &self.b.1 }
    }
    #[inline(always)]
    pub const fn word_ref(&self) -> &u16 {
        unsafe { &self.word }
    }

    #[inline(always)]
    pub fn hi_ref_mut(&mut self) -> &mut u8 {
        #[cfg(target_endian = "little")]
        unsafe { &mut self.b.1 }
        #[cfg(target_endian = "big")]
        unsafe { &mut self.b.0 }
    }
    #[inline(always)]
    pub fn lo_ref_mut(&mut self) -> &mut u8 {
        #[cfg(target_endian = "little")]
        unsafe { &mut self.b.0 }
        #[cfg(target_endian = "big")]
        unsafe { &mut self.b.1 }
    }
    #[inline(always)]
    pub fn word_ref_mut(&mut self) -> &mut u16 {
        unsafe { &mut self.word }
    }

    #[inline(always)]
    pub const fn hi(&self) -> u8 {
        *self.hi_ref()
    }
    #[inline(always)]
    pub const fn lo(&self) -> u8 {
        *self.lo_ref()
    }
    #[inline(always)]
    pub const fn word(&self) -> u16 {
        *self.word_ref()
    }

    #[inline(always)]
    pub fn set_hi(&mut self, v: u8) {
        *self.hi_ref_mut() = v
    }
    #[inline(always)]
    pub fn set_lo(&mut self, v: u8) {
        *self.lo_ref_mut() = v
    }
    #[inline(always)]
    pub fn set_word(&mut self, v: u16) {
        *self.word_ref_mut() = v
    }
}

#[cfg(test)]
mod tests {
    use super::TwoBytes;
    #[test]
    fn twobytes_ok() {
        let mut b = TwoBytes::zeroed();
        b.set_hi(0x01);
        b.set_lo(0x02);
        assert_eq!(b.hi(), 0x01);
        assert_eq!(b.lo(), 0x02);
        assert_eq!(b.word(), 0x0102);
        b.set_word(0x1234);
        assert_eq!(b.hi(), 0x12);
        assert_eq!(b.lo(), 0x34);
    }
}

impl fmt::Debug for TwoBytes {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt.debug_struct("TwoBytes")
            .field("hi", &self.hi())
            .field("lo", &self.lo())
            .finish()
    }
}

impl PartialEq for TwoBytes {
    fn eq(&self, other: &TwoBytes) -> bool {
        self.word() == other.word()
    }
}

pub trait BusRo<Address: UnsignedInteger>: Send + Sync {
    fn read8(&self, _: Address) -> u8;
    #[inline(always)]
    fn read16le(&self, a: Address) -> u16 {
        u16::from_le_bytes([
            self.read8(a),
            self.read8(a.off(1)),
        ])
    }
}

pub trait BusWo<Address: UnsignedInteger>: Send + Sync {
    fn write8(&mut self, _: Address, _: u8);
    #[inline(always)]
    fn write16le(&mut self, a: Address, v: u16) {
        let [lo, hi] = v.to_le_bytes();
        self.write8(a, lo);
        self.write8(a.off(1), hi);
    }
}

pub trait Bus<Address: UnsignedInteger>: BusWo<Address> + BusRo<Address> {}
impl<Address: UnsignedInteger, T: BusRo<Address> + BusWo<Address>> Bus<Address> for T {}

impl<T: UnsignedInteger> BusWo<T> for &mut [u8] {
    #[inline(always)]
    fn write8(&mut self, addr: T, val: u8) {
        self[addr.to_usize()] = val;
    }
}

impl<T: UnsignedInteger> BusRo<T> for &mut [u8] {
    #[inline(always)]
    fn read8(&self, addr: T) -> u8 {
        self[addr.to_usize()]
    }
}

impl<T: UnsignedInteger> BusRo<T> for &[u8] {
    #[inline(always)]
    fn read8(&self, addr: T) -> u8 {
        self[addr.to_usize()]
    }
}

impl<T: UnsignedInteger> BusRo<T> for Vec<u8> {
    #[inline(always)]
    fn read8(&self, addr: T) -> u8 {
        self[addr.to_usize()]
    }
}

impl<T: UnsignedInteger> BusWo<T> for Vec<u8> {
    #[inline(always)]
    fn write8(&mut self, addr: T, val: u8) {
        self[addr.to_usize()] = val
    }
}

pub trait UnsignedInteger: sealed_impl::Sealed + Sized + Copy {
    fn to_usize(self) -> usize;
    fn from_usize(_: usize) -> Self;
    #[inline(always)]
    fn off(self, offset: isize) -> Self {
        Self::from_usize((self.to_usize() as isize + offset) as usize)
    }
}

macro_rules! impl_unsigned_int {
    ($u: ty) => {
        impl UnsignedInteger for $u {
            #[inline(always)]
            fn to_usize(self) -> usize { self as usize }
            #[inline(always)]
            fn from_usize(v: usize) -> Self { v as Self }
        }
    }
}

impl_unsigned_int!(u8);
impl_unsigned_int!(u16);
impl_unsigned_int!(u32);
impl_unsigned_int!(u64);

mod sealed_impl {
    pub trait Sealed {}
    impl Sealed for u8 {}
    impl Sealed for u16 {}
    impl Sealed for u32 {}
    impl Sealed for u64 {}
}

pub trait Cpu: Sized {
    type AddressWidth: UnsignedInteger;
    type Instruction: Sized;
    fn new() -> Self;
    fn next_instruction(&self, memory: &impl Bus<Self::AddressWidth>) -> Self::Instruction;
    fn step_instruction(&mut self, memory: &mut impl Bus<Self::AddressWidth>) -> impl std::future::Future<Output = Self::AddressWidth> + Send;
}
