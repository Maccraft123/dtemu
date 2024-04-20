use bitvec::prelude::*;
use super::{Operand, ParsableInstruction, EncodableInstruction, DecodableInstruction};
use core::convert::Infallible;
use core::mem;

// MSB first
fn b3_to_u8(b1: bool, b2: bool, b3: bool) -> u8 {
    b1 as u8 * 0b100 | b2 as u8 * 0b010 | b3 as u8 * 0b001
}

fn l(val: u16) -> u8 {
    (val & 0xff) as u8
}

fn b(val: u16) -> u8 {
    ((val & 0xff00) >> 8) as u8
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Operand)]
#[repr(u8)]
/// Represents the Source/Destination register in the opcode
pub enum Reg {
    /// The A register
    A = 0b111,
    /// The B register
    B = 0b000,
    /// The C register
    C = 0b001,
    /// The D register
    D = 0b010,
    /// The E register
    E = 0b011,
    /// The H register
    H = 0b100,
    /// The L register
    L = 0b101,
    /// Memory reference through address in H:L
    M = 0b110,
}

impl Reg {
    fn from_byte(byte: u8) -> Reg {
        if byte & 0b1000 != 0 {
            panic!()
        } else {
            unsafe { mem::transmute(byte) }
        }
    }
    fn from_bits(b1: bool, b2: bool, b3: bool) -> Reg {
        Reg::from_byte(b3_to_u8(b1, b2, b3))
    }
    pub fn to_dst_byte(self) -> u8 {
        (self as u8) << 3
    }
    pub fn to_src_byte(self) -> u8 {
        self as u8
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Operand)]
#[repr(u8)]
/// Represents the Register Pair field in the opcode
pub enum RegPair {
    /// BC register pair
    Bc = 0b00,
    /// DE register pair
    De = 0b01,
    /// HL register pair
    Hl = 0b10,
    /// SP register pair, or PSW on push/pop operations
    #[operand(alias = "psw")]
    Sp = 0b11,
}

impl RegPair {
    fn from_bits(b1: bool, b2: bool) -> RegPair {
        match (b1, b2) {
            (false, false) => Self::Bc,
            (false, true) => Self::De,
            (true, false) => Self::Hl,
            (true, true) => Self::Sp,
        }
    }
    fn to_byte(self) -> u8 {
        (self as u8) << 4
    }
    pub fn low(&self) -> Reg {
        match self {
            Self::Bc => Reg::C,
            Self::De => Reg::E,
            Self::Hl => Reg::L,
            Self::Sp => panic!("what no don't"),
        }
    }
    pub fn high(&self) -> Reg {
        match self {
            Self::Bc => Reg::B,
            Self::De => Reg::D,
            Self::Hl => Reg::H,
            Self::Sp => panic!("what no don't"),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Copy, Operand)]
#[repr(u8)]
/// Condition field in conditional CALL, JMP and RET instructions
pub enum Condition {
    /// Zero flag not set
    #[operand(rename = "nz")]
    NonZero = 0b000,
    /// Zero flag set
    #[operand(rename = "z")]
    Zero = 0b001,
    /// Carry flag not set
    #[operand(rename = "nc")]
    NoCarry = 0b010,
    /// Carry flag set
    #[operand(rename = "c")]
    Carry = 0b011,
    /// Parity flag not set
    #[operand(rename = "po")]
    Odd = 0b100,
    /// Parity flag set
    #[operand(rename = "pe")]
    Even = 0b101,
    /// Sign flag not set
    #[operand(rename = "p")]
    Plus = 0b110,
    /// Sign flag set
    #[operand(rename = "m")]
    Minus = 0b111,
}

impl Condition {
    fn from_bits(b1: bool, b2: bool, b3: bool) -> Condition {
        let byte = b3_to_u8(b1, b2, b3);
        if byte & 0b1000 != 0 {
            panic!()
        } else {
            unsafe { mem::transmute(byte) }
        }
    }
    pub fn to_byte(self) -> u8 {
        (self as u8) << 3
    }
}

/// A form of a decoded 8080/8085 instruction that's easy to work with in Rust.
#[derive(Debug, Clone, PartialEq, Eq, ParsableInstruction)]
pub enum Instruction {
    /// Move register to register
    Mov(Reg, #[instruction(comma)] Reg),
    /// Move immediate value to register
    Mvi(Reg, #[instruction(comma)] u8),
    /// Load register pair with immediate value
    Lxi(RegPair, #[instruction(comma)] u16),
    /// Load A from memory
    Lda(u16),
    /// Store A to memory
    Sta(u16),
    /// Load H:L from memory
    Lhld(u16),
    /// Store H:L to memory
    Shld(u16),
    /// Load A using address in BC or DE
    Ldax(RegPair),
    /// Store A using address in BC or DE
    Stax(RegPair),
    /// Exchange DE and HL content
    Xchg,
    /// Add register to A
    Add(Reg),
    /// Add immediate to A
    Adi(u8),
    /// Add register to A with carry
    Adc(Reg),
    /// Add immediate to A with carry
    Aci(u8),
    /// Subtract register from A
    Sub(Reg),
    /// Subtract immediate from A
    Sui(u8),
    /// Subtract register from A with borrow
    Sbb(Reg),
    /// Subtract immediate from A with borrow
    Sbi(u8),
    /// Increment register, setting aux carry if low nibble of A is 0x0 after addition, and
    /// updating Zero, Sign and Parity flags based on resulting value
    Inr(Reg),
    /// Decrement register, setting aux carry if low nibble of A is not equal to 0xf after
    /// addition, and updating Zero, Sign and Parity flags based on resulting value
    Dcr(Reg),
    /// Increment register pair
    Inx(RegPair),
    /// Decrement register pair
    Dcx(RegPair),
    /// (Double Add) Add register pair to HL, setting carry on overflow
    Dad(RegPair),
    /// Decimal adjust accumulator, converting a binary value to a BCD value.
    /// If aux carry is set or low nibble of A is bigger than 9, add 0x06 to A
    /// If carry is set, high nibble of A is bigger than 9 add 0x60 to A
    /// If there was a carry out of low nibble of A, set aux carry, otherwise reset it
    /// Copy resulting value to A and set Zero, Sign and Parity flags based on result
    Daa,
    /// AND register with A
    Ana(Reg),
    /// AND immediate with A
    Ani(u8),
    /// OR register with A
    Ora(Reg),
    /// OR immediate with A
    Ori(u8),
    /// XOR register with A
    Xra(Reg),
    /// XOR immediate with A
    Xri(u8),
    /// Compare register with A by subtracting and discarding resulting value
    Cmp(Reg),
    /// Compare immediate with A by subtracting and discarding resulting value
    Cpi(u8),
    /// Rotate left circular
    Rlc,
    /// Rotate right circular
    Rrc,
    /// Rotate left through carry
    Ral,
    /// Rotate right through carry
    Rar,
    /// Compliment(invert/XOR 0xff) A
    Cma,
    /// Compliment(invert) carry flag
    Cmc,
    /// Set carry flag
    Stc,
    /// Unconditional jump
    Jmp(u16),
    /// Conditional jump
    J(#[instruction(nospace)] Condition, u16),
    /// Unconditional subroutine call, pushing PC+3 to stack
    Call(u16),
    /// Conditional subroutine call, pushing PC+3 to stack
    C(#[instruction(nospace)] Condition, u16),
    /// Unconditional return from subroutine, Pops PC from stack
    Ret,
    /// Conditional return from subroutine, Pops PC from stack
    R(#[instruction(nospace)] Condition),
    /// Calls location in memory specified in the u8 parameter
    Rst(u8),
    /// Copies value from HL to PC
    Pchl,
    /// Push register pair on the stack, pushing A/B/D/H first. When SP is pushed, the CPU pushes
    /// PSW(A + flags) instead
    Push(RegPair),
    /// Pop register pair from the stack, popping flags/C/E/L first. When SP is pushed, the CPU
    /// pops PSW(A + flags) instead
    Pop(RegPair),
    /// Swap H:L with top word on stack
    Xthl,
    /// Copies value from HL to SP
    Sphl,
    /// Read input port into A
    In(u8),
    /// Write A to output port
    Out(u8),
    /// Enable interrupts
    Ei,
    /// Disable interrupts
    Di,
    /// Halt processor
    Hlt,
    /// No operation
    Nop,
}

impl EncodableInstruction for Instruction {
    fn len(&self) -> usize {
        self.len()
    }
    fn encode(&self) -> Vec<u8> {
        use Instruction::*;
        match *self {
            Mov(dst, src)   => vec![0b01000000 | dst.to_dst_byte() | src.to_src_byte()],
            Mvi(dst, op)    => vec![0b00000110 | dst.to_dst_byte(), op],
            Lxi(rp, op)     => vec![0b00000001 | rp.to_byte(), l(op), b(op)],
            J(cond, op)     => vec![0b11000010 | cond.to_byte(), l(op), b(op)],
            C(cond, op)     => vec![0b11000100 | cond.to_byte(), l(op), b(op)],
            R(cond)         => vec![0b11000000 | cond.to_byte()],
            Jmp(op)     => vec![0b11000011, l(op), b(op)],
            Call(op)    => vec![0b11001101, l(op), b(op)],
            Rst(dst)    => vec![0b11000111 | (dst << 3) & 0b111],
            Lda(op)     => vec![0b00111010, l(op), b(op)],
            Sta(op)     => vec![0b00110010, l(op), b(op)],
            Ldax(rp)    => vec![0b00001010 | rp.to_byte()],
            Stax(rp)    => vec![0b00000010 | rp.to_byte()],
            Lhld(op)    => vec![0b00101010, l(op), b(op)],
            Shld(op)    => vec![0b00100010, l(op), b(op)],
            Add(src)    => vec![0b10000000 | src.to_src_byte()],
            Adi(op)     => vec![0b11000110, op],
            Adc(src)    => vec![0b10001000 | src.to_src_byte()],
            Aci(op)     => vec![0b11001110, op],
            Sub(src)    => vec![0b10010000 | src.to_src_byte()],
            Sui(op)     => vec![0b11010110, op],
            Sbb(src)    => vec![0b10011000 | src.to_src_byte()],
            Sbi(op)     => vec![0b11011110, op],
            Inr(dst)    => vec![0b00000100 | dst.to_dst_byte()],
            Dcr(dst)    => vec![0b00000101 | dst.to_dst_byte()],
            Inx(rp)     => vec![0b00000011 | rp.to_byte()],
            Dcx(rp)     => vec![0b00001011 | rp.to_byte()],
            Dad(rp)     => vec![0b00001001 | rp.to_byte()],
            Ana(src)    => vec![0b10100000 | src.to_src_byte()],
            Ani(op)     => vec![0b11100110, op],
            Ora(src)    => vec![0b10110000 | src.to_src_byte()],
            Ori(op)     => vec![0b11110110, op],
            Xra(src)    => vec![0b10101000 | src.to_src_byte()],
            Xri(op)     => vec![0b11101110, op],
            Cmp(src)    => vec![0b10111000 | src.to_src_byte()],
            Cpi(op)     => vec![0b11111110, op],
            Push(rp)    => vec![0b11000101 | rp.to_byte()],
            Pop(rp)     => vec![0b11000001 | rp.to_byte()],
            In(op)      => vec![0b11011011, op],
            Out(op)     => vec![0b11010011, op],
            Hlt     => vec![0x76],
            Xchg    => vec![0xeb],
            Daa     => vec![0x27],
            Rlc     => vec![0x07],
            Rrc     => vec![0x0f],
            Ral     => vec![0x17],
            Rar     => vec![0x1f],
            Cma     => vec![0x2f],
            Cmc     => vec![0x3f],
            Stc     => vec![0x37],
            Ret     => vec![0xc9],
            Pchl    => vec![0xe9],
            Xthl    => vec![0xe3],
            Sphl    => vec![0xf9],
            Ei      => vec![0xfb],
            Di      => vec![0xf3],
            Nop     => vec![0x00],
        }
    }
}

impl DecodableInstruction for Instruction {
    type Error = Infallible;
    fn decode(b: &[u8]) -> Result<Self, Self::Error>{
        Ok(Instruction::decode_from(b))
    }
    fn len(&self) -> usize {
        self.len()
    }
}

impl Instruction {
    pub fn len(&self) -> usize {
        use Instruction::*;
        match self {
            Hlt | Mov(..) | Xchg | Add(..) | Adc(..) | Sub(..) | Sbb(..) | Inr(..) | Dcr(..)
            | Inx(..) | Dcx(..) | Dad(..) | Daa | Ana(..) | Ora(..) | Xra(..) | Cmp(..) | Rlc
            | Rrc | Ral | Rar | Cma | Cmc | Stc | Ret | R(..) | Rst(..) | Pchl | Push(..)
            | Pop(..) | Xthl | Sphl | Ei | Di | Nop | Ldax(..) | Stax(..) => 1,
            Mvi(..) | Adi(..) | Aci(..) | Sui(..) | Sbi(..) | Ani(..) | Ori(..) | Xri(..)
            | Cpi(..) | In(..) | Out(..) => 2,
            Lxi(..) | Lda(..) | Sta(..) | Lhld(..) | Shld(..) | Jmp(..)
            | J(..) | Call(..) | C(..) => 3,
        }
    }
    pub fn decode_from(bytes: &[u8]) -> Self {
        use Instruction::*;
        let b = bytes[0].view_bits::<Msb0>();
        let bits = (
            b[0] as u8, b[1] as u8, b[2] as u8, b[3] as u8, b[4] as u8, b[5] as u8, b[6] as u8,
            b[7] as u8,
        );

        let dst = Reg::from_bits(b[2], b[3], b[4]);
        let src = Reg::from_bits(b[5], b[6], b[7]);
        let rp = RegPair::from_bits(b[2], b[3]);
        let op = bytes[1];
        let op16 = u16::from_le_bytes([bytes[1], bytes[2]]);
        let cond = Condition::from_bits(b[2], b[3], b[4]);

        match bits {
            (0, 1, 1, 1, 0, 1, 1, 0) => Hlt,
            (0, 1, _, _, _, _, _, _) => Mov(dst, src),
            (0, 0, _, _, _, 1, 1, 0) => Mvi(dst, op),
            (0, 0, _, _, 0, 0, 0, 1) => Lxi(rp, op16),
            (0, 0, 1, 1, 1, 0, 1, 0) => Lda(op16),
            (0, 0, 1, 1, 0, 0, 1, 0) => Sta(op16),
            (0, 0, 1, 0, 1, 0, 1, 0) => Lhld(op16),
            (0, 0, 1, 0, 0, 0, 1, 0) => Shld(op16),
            (1, 1, 1, 0, 1, 0, 1, 1) => Xchg,
            (1, 0, 0, 0, 0, _, _, _) => Add(src),
            (1, 1, 0, 0, 0, 1, 1, 0) => Adi(op),
            (1, 0, 0, 0, 1, _, _, _) => Adc(src),
            (1, 1, 0, 0, 1, 1, 1, 0) => Aci(op),
            (1, 0, 0, 1, 0, _, _, _) => Sub(src),
            (1, 1, 0, 1, 0, 1, 1, 0) => Sui(op),
            (1, 0, 0, 1, 1, _, _, _) => Sbb(src),
            (1, 1, 0, 1, 1, 1, 1, 0) => Sbi(op),
            (0, 0, _, _, _, 1, 0, 0) => Inr(dst),
            (0, 0, _, _, _, 1, 0, 1) => Dcr(dst),
            (0, 0, _, _, 0, 0, 1, 1) => Inx(rp),
            (0, 0, _, _, 1, 0, 1, 1) => Dcx(rp),
            (0, 0, _, _, 1, 0, 0, 1) => Dad(rp),
            (0, 0, _, _, 0, 0, 1, 0) => Stax(rp),
            (0, 0, _, _, 1, 0, 1, 0) => Ldax(rp),
            (0, 0, 1, 0, 0, 1, 1, 1) => Daa,
            (1, 0, 1, 0, 0, _, _, _) => Ana(src),
            (1, 1, 1, 0, 0, 1, 1, 0) => Ani(op),
            (1, 0, 1, 1, 0, _, _, _) => Ora(src),
            (1, 1, 1, 1, 0, 1, 1, 0) => Ori(op),
            (1, 0, 1, 0, 1, _, _, _) => Xra(src),
            (1, 1, 1, 0, 1, 1, 1, 0) => Xri(op),
            (1, 0, 1, 1, 1, _, _, _) => Cmp(src),
            (1, 1, 1, 1, 1, 1, 1, 0) => Cpi(op),
            (0, 0, 0, 0, 0, 1, 1, 1) => Rlc,
            (0, 0, 0, 0, 1, 1, 1, 1) => Rrc,
            (0, 0, 0, 1, 0, 1, 1, 1) => Ral,
            (0, 0, 0, 1, 1, 1, 1, 1) => Rar,
            (0, 0, 1, 0, 1, 1, 1, 1) => Cma,
            (0, 0, 1, 1, 1, 1, 1, 1) => Cmc,
            (0, 0, 1, 1, 0, 1, 1, 1) => Stc,
            (1, 1, 0, 0, _, 0, 1, 1) => Jmp(op16),
            (1, 1, _, _, _, 0, 1, 0) => J(cond, op16),
            (1, 1, _, _, 1, 1, 0, 1) => Call(op16),
            (1, 1, _, _, _, 1, 0, 0) => C(cond, op16),
            (1, 1, 0, _, 1, 0, 0, 1) => Ret,
            (1, 1, _, _, _, 0, 0, 0) => R(cond),
            (1, 1, _, _, _, 1, 1, 1) => Rst(dst as u8),
            (1, 1, 1, 0, 1, 0, 0, 1) => Pchl,
            (1, 1, _, _, 0, 1, 0, 1) => Push(rp),
            (1, 1, _, _, 0, 0, 0, 1) => Pop(rp),
            (1, 1, 1, 0, 0, 0, 1, 1) => Xthl,
            (1, 1, 1, 1, 1, 0, 0, 1) => Sphl,
            (1, 1, 0, 1, 1, 0, 1, 1) => In(op),
            (1, 1, 0, 1, 0, 0, 1, 1) => Out(op),
            (1, 1, 1, 1, 1, 0, 1, 1) => Ei,
            (1, 1, 1, 1, 0, 0, 1, 1) => Di,
            (0, 0, _, _, _, 0, 0, 0) => Nop,
            _ => unreachable!(),
        }
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn code_decode_ok() {
        use super::Instruction;
        for i in 0..=u8::MAX {
            let instruction = Instruction::decode_from(&vec![i, 0, 0]);
            let mut encoded = instruction.clone().encode();
            encoded.push(0);
            encoded.push(0);
            let re_decoded = Instruction::decode_from(&encoded);
            assert_eq!(instruction, re_decoded);
        }
    }
}
