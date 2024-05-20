use super::DecodableInstruction;
use super::parse::{Operand, ParsableInstruction};
use core::convert::Infallible;
use core::mem;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Chip8Revision {
    Chip8,
    Schip8,
    Chip8X,
}

pub enum RegOrU8 {
    Reg(u8),
    U8(u8),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Instruction {
}

impl Instruction {
    #[inline]
    pub fn decode_from(bytes: &[u8]) -> Self {
        let x = bytes[0] & 0x0f;
        let y = (bytes[1] & 0xf0) >> 4;
        //let v8 = bytes[0] & 0x0f;
        let v16 = u16::from_be_bytes([bytes[0] & 0x0f, bytes[1]]);
        match bytes[0] & 0xf0 {
            0x00 => match bytes[1] {
                0xe0 => Cls,
                0xee => Ret,
                _ => Jmp(v16),
            },
            0x10 => Jmp(v16),
            0x20 => Jsr(v16),
            0x30 => SkipEq(x, RegOrU8::U8(bytes[1])),
            0x40 => SkipNe(x, bytes[1]),
            0x50 => SkipEq(x, RegOrU8::Reg(y)),
            0x60 => Load(x, bytes[1]),
            0x60 => Add(x, bytes[1]),
            0x80 => Mov(x, y),

        }
    }
}
