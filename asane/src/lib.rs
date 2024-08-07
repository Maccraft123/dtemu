#![cfg_attr(not(feature = "std"), no_std)]

#![allow(clippy::unused_async)]
#[cfg(any(feature = "alloc", test))]
extern crate alloc;

pub mod intel8085;
pub mod mos6502;

use static_assertions::const_assert_eq;
use core::fmt;
use core::convert::Infallible;
use core::future::Future;

mod cpu_prelude {
    pub use crate::{Bus, BusRead, BusWrite, Cpu, TwoBytes, cycles, define_cycles, take_cycles};
}

#[cfg(not(feature = "cycle_counting"))]
#[macro_export]
macro_rules! take_cycles {
    () => { 0 }
}

#[cfg(feature = "cycle_counting")]
#[macro_export]
macro_rules! take_cycles {
    () => {
        CYCLES.take()
    }
}

#[cfg(all(feature = "cycle_counting", not(feature = "std")))]
mod cyclescell {
    use core::cell::Cell;

    pub(crate) struct CyclesCell {
        c: Cell<usize>,
    }

    impl CyclesCell {
        #[inline(always)]
        pub(crate) const fn new() -> Self {
            Self {
                c: Cell::new(0),
            }
        }
        #[inline(always)]
        pub(crate) fn take(&'static self) -> usize {
            unsafe {
                self.c.take()
                //let ptr = self.c.get();
                //let old = *ptr;
                //*ptr = 0;
                //old
            }
        }
        #[inline(always)]
        pub(crate) fn add(&'static self, v: usize) {
            unsafe {
                self.c.set(self.c.get() + v);
                //let ptr = self.c.get();
                //*ptr = *ptr + v;
            }
        }
    }

    // safe as long as we're poking at it from one thread
    unsafe impl Sync for CyclesCell {}
}

#[macro_export]
macro_rules! define_cycles {
    () => {
        #[cfg(all(feature = "cycle_counting", feature = "std"))]
        std::thread_local! { // why is this faster????
            static CYCLES: std::cell::Cell<usize> = const { std::cell::Cell::new(0) }
        }
        #[cfg(all(feature = "cycle_counting", not(feature = "std")))]
        static CYCLES: crate::cyclescell::CyclesCell = crate::cyclescell::CyclesCell::new();
    }
}

#[macro_export]
macro_rules! cycles {
    ($num: expr) => {
        #[cfg(feature = "cycle_stepping")]
        {
            let mut amount = $num;
            while amount > 0 {
                amount -= 1;
                cassette::yield_now().await;
            }
        }
        #[cfg(feature = "cycle_counting")]
        {
            #[cfg(feature = "std")]
            CYCLES.set(CYCLES.get() + $num as usize);
            #[cfg(not(feature = "std"))]
            CYCLES.add($num as usize);
        }
    }
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
    #[inline]
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt.debug_struct("TwoBytes")
            .field("hi", &self.hi())
            .field("lo", &self.lo())
            .finish()
    }
}

impl PartialEq for TwoBytes {
    #[inline]
    fn eq(&self, other: &TwoBytes) -> bool {
        self.word() == other.word()
    }
}

pub trait BusRead<Address: UnsignedInteger> {
    fn read8(&mut self, _: Address) -> u8;
    #[inline(always)]
    fn read16le(&mut self, a: Address) -> u16 {
        u16::from_le_bytes([
            self.read8(a),
            self.read8(a.off(1)),
        ])
    }
}

pub trait BusWrite<Address: UnsignedInteger> {
    fn write8(&mut self, _: Address, _: u8);
    #[inline(always)]
    fn write16le(&mut self, a: Address, v: u16) {
        let [lo, hi] = v.to_le_bytes();
        self.write8(a, lo);
        self.write8(a.off(1), hi);
    }
}

pub trait Bus<Address: UnsignedInteger>: BusWrite<Address> + BusRead<Address> {}
impl<Address: UnsignedInteger, T: BusRead<Address> + BusWrite<Address>> Bus<Address> for T {}

impl BusRead<Infallible> for () {
    #[inline(always)]
    fn read8(&mut self, _: Infallible) -> u8 {
        unreachable!()
    }
}

impl BusWrite<Infallible> for () {
    #[inline(always)]
    fn write8(&mut self, _: Infallible, _: u8) {
        unreachable!()
    }
}

/// A "no-op" bus implementation that ignores any writes and on reads returns value stored in self.0
pub struct NoopBus(pub u8);

impl<T: UnsignedInteger> BusRead<T> for NoopBus {
    #[inline(always)]
    fn read8(&mut self, _: T) -> u8 {
        self.0
    }
}

impl<T: UnsignedInteger> BusWrite<T> for NoopBus {
    #[inline(always)]
    fn write8(&mut self, _: T, _: u8) {
    }
}

impl<T: UnsignedInteger, const LEN: usize> BusRead<T> for [u8; LEN] {
    #[inline(always)]
    fn read8(&mut self, addr: T) -> u8 {
        self[addr.to_usize()]
    }
}

impl<T: UnsignedInteger, const LEN: usize> BusWrite<T> for [u8; LEN] {
    #[inline(always)]
    fn write8(&mut self, addr: T, val: u8) {
        self[addr.to_usize()] = val
    }
}

impl<T: UnsignedInteger> BusRead<T> for [u8] {
    #[inline(always)]
    fn read8(&mut self, addr: T) -> u8 {
        self[addr.to_usize()]
    }
}

impl<T: UnsignedInteger> BusWrite<T> for [u8] {
    #[inline(always)]
    fn write8(&mut self, addr: T, val: u8) {
        self[addr.to_usize()] = val;
    }
}

#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq)]
pub enum BusAccess {
    Read,
    Write(u8),
}

impl<T: FnMut(BusAccess, U) -> u8, U: UnsignedInteger> BusWrite<U> for T {
    #[inline(always)]
    fn write8(&mut self, addr: U, value: u8) {
        self(BusAccess::Write(value), addr);
    }
}

impl<T: FnMut(BusAccess, U) -> u8, U: UnsignedInteger> BusRead<U> for T {
    #[inline(always)]
    fn read8(&mut self, addr: U) -> u8 {
        self(BusAccess::Read, addr)
    }
}

/*#[cfg(feature = "alloc")]
impl<T: UnsignedInteger> BusRead<T> for alloc::vec::Vec<u8> {
    #[inline(always)]
    fn read8(&self, addr: T) -> u8 {
        self[addr.to_usize()]
    }
}

#[cfg(feature = "alloc")]
impl<T: UnsignedInteger> BusWrite<T> for alloc::vec::Vec<u8> {
    #[inline(always)]
    fn write8(&mut self, addr: T, val: u8) {
        self[addr.to_usize()] = val
    }
}*/

pub trait UnsignedInteger: sealed_impl::Sealed + Sized + Copy {
    fn to_usize(self) -> usize;
    fn from_usize(_: usize) -> Self;
    #[inline(always)]
    fn off(self, offset: isize) -> Self {
        Self::from_usize((self.to_usize() as isize + offset) as usize)
    }
}

pub trait OptionalUnsignedInteger: UnsignedInteger {}
impl<T: UnsignedInteger> OptionalUnsignedInteger for T {}

macro_rules! impl_unsigned_int {
    ($u: ty) => {
        impl UnsignedInteger for $u {
            #[allow(clippy::cast_possible_truncation)]
            #[inline(always)]
            fn to_usize(self) -> usize { self as usize }
            #[allow(clippy::cast_possible_truncation)]
            #[inline(always)]
            fn from_usize(v: usize) -> Self { v as Self }
        }
    }
}

impl_unsigned_int!(u8);
impl_unsigned_int!(u16);
impl_unsigned_int!(u32);
impl_unsigned_int!(u64);

impl UnsignedInteger for core::convert::Infallible {
    fn to_usize(self) -> usize {
        unreachable!()
    }
    fn from_usize(_: usize) -> Self {
        panic!()
    }
}

mod sealed_impl {
    pub trait Sealed {}
    impl Sealed for u8 {}
    impl Sealed for u16 {}
    impl Sealed for u32 {}
    impl Sealed for u64 {}
    impl Sealed for core::convert::Infallible {}
}

pub trait Cpu: Sized {
    type AddressWidth: UnsignedInteger;
    type IoAddressWidth: OptionalUnsignedInteger;
    type Interrupt: Sized;
    type Instruction: Sized;
    /// Returns the program counter
    fn pc(&self) -> Self::AddressWidth;
    /// Sets the program counter
    fn set_pc(&mut self, _: Self::AddressWidth);
    /// Creates a new instance of cpu
    fn new(memory: &mut impl Bus<Self::AddressWidth>, io: &mut impl Bus<Self::IoAddressWidth>) -> Self;
    /// Returns the next instruction to be executed
    fn next_instruction(&self, memory: &mut impl Bus<Self::AddressWidth>) -> Self::Instruction;
    /// Steps one instruction, with every clock cycle represented by polling the future, return
    /// value being the (cycles taken, program counter)
    fn step_block(&mut self, memory: &mut impl Bus<Self::AddressWidth>, io: &mut impl Bus<Self::IoAddressWidth>) -> impl Future<Output = (usize, Self::AddressWidth)>;
    /// Steps `num` instructions, retuning a tuple of (cycles taken, program counter), the value of
    /// cycles taken is reliable if, and only if, feature `cycle_stepping` is enabled
    #[inline]
    fn step_blocks_sync(&mut self, num: usize, memory: &mut impl Bus<Self::AddressWidth>, io: &mut impl Bus<Self::IoAddressWidth>) -> (usize, Self::AddressWidth) {
        use core::pin::pin;
        let pin = pin!(async {
            let mut cycles = self.step_block(memory, io).await;
            for _ in 1..num {
                cycles = self.step_block(memory, io).await;
            }
            cycles
        });
        let mut future = cassette::Cassette::new(pin);
        loop {
            if let Some(cycles) = future.poll_on() {
                return cycles;
            }
        }
    }
    /// Steps one instruction, retuning a tuple of (program counter, cycles taken), the value of
    /// cycles taken is reliable if, and only if, feature `cycle_stepping` is enabled
    #[inline]
    fn step_block_sync(&mut self, memory: &mut impl Bus<Self::AddressWidth>, io: &mut impl Bus<Self::IoAddressWidth>) -> (usize, Self::AddressWidth) {
        self.step_blocks_sync(1, memory, io)
    }
    fn irq(&mut self, _: Self::Interrupt, memory: &mut impl Bus<Self::AddressWidth>, io: &mut impl Bus<Self::IoAddressWidth>) -> impl Future<Output = usize>;
    #[inline]
    fn irq_sync(&mut self, _: Self::Interrupt, memory: &mut impl Bus<Self::AddressWidth>, io: &mut impl Bus<Self::IoAddressWidth>) -> usize {
        use core::pin::pin;
        let pin = pin!(self.step_block(memory, io));
        let mut future = cassette::Cassette::new(pin);
        loop {
            if let Some(cycles) = future.poll_on() {
                return cycles.0;
            }
        }
    }
}
