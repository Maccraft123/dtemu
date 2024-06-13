#![cfg_attr(not(feature = "std"), no_std)]
#![cfg_attr(not(feature = "std"), feature(error_in_core))]
extern crate alloc;
use alloc::boxed::Box;
use alloc::vec::Vec;
use core::convert::Infallible;
use core::future::Future;

mod inner_prelude {
    #[cfg(feature = "std")]
    pub use std::error::Error;
    #[cfg(not(feature = "std"))]
    pub use core::error::Error;
    pub use asane::Cpu;
    //pub use cassette::block_on;
    pub use crate::{Backend, Machine};
    pub use alloc::boxed::Box;
    pub use alloc::vec::Vec;
}
use inner_prelude::*;

pub mod cpm;
pub mod nestrace;
//pub mod spaceinvaders;

#[derive(Copy, Clone, Eq, PartialEq)]
pub enum Key {
    Character(char),
    Left,
    Right,
    Up,
    Down,
}

impl TryFrom<Key> for u8 {
    type Error = ();
    #[inline]
    fn try_from(value: Key) -> Result<u8, ()> {
        if let Key::Character(ch) = value {
            Ok(ch as u8)
        } else {
            Err(())
        }
    }
}

impl From<u8> for Key {
    #[inline]
    fn from(value: u8) -> Self {
        Self::Character(value as char)
    }
}

impl From<char> for Key {
    #[inline]
    fn from(value: char) -> Self {
        Self::Character(value)
    }
}

pub trait Input {}
pub trait Output {}

pub trait Backend {
    type Input: Input;
    type Output: Output;
    fn should_exit(&mut self) -> bool;
    fn request_firmware(&mut self, name: &str) -> Vec<u8>;
    fn i(&mut self) -> &mut Self::Input;
    fn o(&mut self) -> &mut Self::Output;
}

pub trait KeyboardInput {
    type Error: Error + 'static;
    /// Reads a character from the keyboard, blocking until a keypress
    fn read_key(&mut self) -> Result<Key, Self::Error>;
    /// Reads a keyacter from the keyboard, returning None when there isn't one ready
    fn poll_key(&mut self) -> Result<Option<Key>, Self::Error>;
    /// Reads a keyacter from the keyboard, without removing it from internal queue
    fn peek_key(&mut self) -> Result<Option<Key>, Self::Error>;
    /// Returns whether there is an input keyacter in internal queue
    #[inline(always)]
    fn has_key(&mut self) -> Result<bool, Self::Error> {
        Ok(self.peek_key()?.is_some())
    }
}
impl<T: KeyboardInput> Input for T {}

/// A KeyboardInput implementation that never returns a key and panics on a blocking operation
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct BrokenKeyboard;

impl BrokenKeyboard {
    pub fn ref_mut() -> &'static mut Self {
        // Safety: miri doesn't complain
        unsafe {
            core::ptr::NonNull::dangling().as_mut()
        }
    }
}

impl KeyboardInput for BrokenKeyboard {
    type Error = Infallible;
    fn read_key(&mut self) -> Result<Key, Self::Error> {
        panic!("Called read_key on BrokenKeyboard")
    }
    fn poll_key(&mut self) -> Result<Option<Key>, Self::Error> {
        Ok(None)
    }
    fn peek_key(&mut self) -> Result<Option<Key>, Self::Error> {
        Ok(None)
    }
}

pub trait TerminalOutput {
    type Error: Error + 'static;
    /// Writes a character onto the screen
    fn write_key(&mut self, _: Key) -> Result<(), Self::Error>;
    /// Writes a string to the display
    #[inline(always)]
    fn write_string(&mut self, s: &str) -> Result<(), Self::Error> {
        for ch in s.chars() {
            self.write_key(ch.into())?;
        }
        Ok(())
    }
    /// Writes every value yielded by an iterator to display
    #[inline(always)]
    fn write_iter(&mut self, iter: impl Iterator<Item = Key>) -> Result<(), Self::Error> {
        for ch in iter {
            self.write_key(ch)?;
        }
        Ok(())
    }
}
impl<T: TerminalOutput> Output for T {}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Rgb888 {
    r: u8,
    g: u8,
    b: u8,
}

impl Rgb888 {
    fn new(r: u8, g: u8, b: u8) -> Self {
        Self { r, g, b }
    }
}

impl From<bool> for Rgb888 {
    fn from(v: bool) -> Rgb888 {
        if v {
            Rgb888::new(0xff, 0xff, 0xff)
        } else {
            Rgb888::new(0x0, 0x0, 0x0)
        }
    }
}

pub trait FramebufferOutput {
    type Error: Error + 'static;
    fn blit_pixel(&mut self, _: Rgb888, _: (u16, u16)) -> Result<(), Self::Error>;
    fn set_resolution(&mut self, _: (usize, usize)) -> Result<(), Self::Error>;
    fn refresh(&mut self) -> Result<(), Self::Error>;
}

pub trait Machine<T: Backend> {
    fn new(_: T) -> Self where Self: Sized;
    /// Runs a 'tick' of the machine, which is an undefined period of time, usually less than some
    /// milliseconds..
    /// The returned `bool` is the "should the application quit" value.
    /// i.e. `while machine.tick()? {}` is enough of a main loop for most cases
    fn tick(&mut self) -> Result<bool, Box<dyn Error>>;
    /// Returns the amount of clock cycles the machine executed
    fn cycles(&self) -> usize;
}
