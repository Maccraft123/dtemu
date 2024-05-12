#![cfg_attr(not(feature = "std"), no_std)]
#![cfg_attr(not(feature = "std"), feature(error_in_core))]
extern crate alloc;
use alloc::boxed::Box;
use alloc::vec::Vec;

mod inner_prelude {
    #[cfg(feature = "std")]
    pub use std::error::Error;
    #[cfg(not(feature = "std"))]
    pub use core::error::Error;
    pub use asane::Cpu;
    pub use cassette::block_on;
    pub use crate::{Backend, Machine};
    pub use alloc::boxed::Box;
}
use inner_prelude::*;

pub mod cpm;

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

pub trait Backend {
    type TtyError: Error + 'static;
    /// Writes a character onto the screen
    fn write_key(&mut self, _: Key) -> Result<(), Self::TtyError>;
    /// Reads a character from the keyboard, blocking until a keypress
    fn read_key(&mut self) -> Result<Key, Self::TtyError>;
    /// Reads a keyacter from the keyboard, returning None when there isn't one ready
    fn poll_key(&mut self) -> Result<Option<Key>, Self::TtyError>;
    /// Reads a keyacter from the keyboard, without removing it from internal queue
    fn peek_key(&mut self) -> Result<Option<Key>, Self::TtyError>;
    /// Returns whether there is an input keyacter in internal queue
    #[inline(always)]
    fn has_key(&mut self) -> Result<bool, Self::TtyError> {
        Ok(self.peek_key()?.is_some())
    }
    /// Writes every value yielded by an iterator to display
    #[inline(always)]
    fn write_iter(&mut self, iter: impl Iterator<Item = Key>) -> Result<(), Self::TtyError> {
        for ch in iter {
            self.write_key(ch)?;
        }
        Ok(())
    }
    fn should_exit(&mut self) -> bool;
    fn request_firmware(&mut self, name: &str) -> Vec<u8>;
}

pub trait Machine<T: Backend> {
    fn new(_: T) -> Self where Self: Sized;
    /// Runs a 'tick' of the machine, usually a single CPU instruction, however that is
    /// implementation-defined
    fn tick(&mut self) -> Result<bool, Box<dyn Error>>;
    /// Returns the amount of clock cycles the machine executed
    fn cycles(&self) -> usize;
}
