#![cfg_attr(not(feature = "std"), no_std)]

#[cfg(feature = "alloc")]
extern crate alloc;

pub mod i8080;
pub mod mos6502;
pub mod mc6809;
pub mod m68k;

#[cfg(feature = "parse")]
pub mod parse;
#[cfg(not(feature = "parse"))]
pub mod parse {
    pub use unasm_derive::NoOperand as Operand;
    pub use unasm_derive::NoParsableInstruction as ParsableInstruction;
    pub use unasm_derive::NoParsableOpcode as ParsableOpcode;
}

#[cfg(feature = "encode")]
pub trait EncodableInstruction: core::fmt::Debug {
    /// Encodes this instruction, returning a Vec with the binary form
    fn encode(&self) -> Vec<u8>;
    /// Returns length of this instruction
    fn len(&self) -> usize;
}

pub trait DecodableInstruction {
    type Error;
    /// Decodes a string of bytes into an instruction
    fn decode(_: &[u8]) -> Result<Self, Self::Error> where Self: Sized;
    /// Returns length of this instruction
    fn len(&self) -> usize;
}
