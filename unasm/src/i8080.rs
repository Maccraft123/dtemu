use bitvec::prelude::*;
use core::mem;

// MSB first
fn b2_to_u8(b1: u8, b2: u8) -> u8 {
    b1 * 0b010 | b2 * 0b001
}

// MSB first
fn b3_to_u8(b1: bool, b2: bool, b3: bool) -> u8 {
    b1 as u8 * 0b100 | b2 as u8 * 0b010 | b3 as u8 * 0b001
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
#[repr(u8)]
/// Represents the Source/Destination register in the opcode
pub enum Reg {
    A = 0b111,
    B = 0b000,
    C = 0b001,
    D = 0b010,
    E = 0b011,
    H = 0b100,
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
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
#[repr(u8)]
/// Represents the Register Pair field in the opcode
pub enum RegPair {
    Bc = 0b00,
    De = 0b01,
    Hl = 0b10,
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

#[derive(Debug, Clone, Eq, PartialEq, Copy)]
#[repr(u8)]
/// Condition field in conditional CALL, JMP and RET instructions
pub enum Condition {
    /// Zero flag not set
    NonZero = 0b000,
    /// Zero flag set
    Zero = 0b001,
    /// Carry flag not set
    NoCarry = 0b010,
    /// Carry flag set
    Carry = 0b011,
    /// Parity flag not set
    Odd = 0b100,
    /// Parity flag set
    Even = 0b101,
    /// Sign flag not set
    Plus = 0b110,
    /// Sign flag set
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
}

/// A form of decoded 8080/8085/Z80 instruction that's easy to work with in Rust.
#[derive(Debug, Clone)]
pub enum Instruction {
    /// Move register to register
    Mov(Reg, Reg),
    /// Move immediate value to register
    Mvi(Reg, u8),
    /// Load register pair with immediate value
    Lxi(RegPair, u16),
    /// Load A from memory
    Lda(u16),
    /// Store A from memory
    Sta(u16),
    /// Load H:L from memory
    Lhld(u16),
    /// Store H:L to memory
    Shld(u16),
    /// Load indirect through BC or DE
    Ldax(RegPair),
    /// Store indirect through BC or DE
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
    /// Increment register
    Inr(Reg),
    /// Decrement registser
    Dcr(Reg),
    /// Increment register pair
    Inx(RegPair),
    /// Decrement register pair
    Dcx(RegPair),
    /// Add register pair to HL
    Dad(RegPair),
    /// Decimal adjust accumulator
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
    /// Compare register with A
    Cmp(Reg),
    /// Compare immediate with A
    Cpi(u8),
    /// Rotate A left
    Rlc,
    /// Rotate A right
    Rrc,
    /// Rotate A left through carry
    Ral,
    /// Rorate A right through carry
    Rar,
    /// Compliment A
    Cma,
    /// Compliment carry flag
    Cmc,
    /// Set carry flag
    Stc,
    /// Unconditional jump
    Jmp(u16),
    /// Conditional jump
    J(Condition, u16),
    /// Unconditional subroutine call
    Call(u16),
    /// Conditional subroutine call
    C(Condition, u16),
    /// Unconditional return from subroutine
    Ret,
    /// Conditional return from subroutine
    R(Condition),
    /// Restart
    Rst(u8),
    /// Jump to address in H:L
    Pchl,
    /// Push register pair on the stack
    Push(RegPair),
    /// Pop register pair from the stack
    Pop(RegPair),
    /// Swap H:L with top word on stack
    Xthl,
    /// Set SP to content of H:L
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

impl Instruction {
    pub fn len(&self) -> usize {
        use Instruction::*;
        match self {
            Hlt | Mov(..) | Xchg | Add(..) | Adc(..) | Sub(..) | Sbb(..) | Inr(..) | Dcr(..)
            | Inx(..) | Dcx(..) | Dad(..) | Daa | Ana(..) | Ora(..) | Xra(..) | Cmp(..) | Rlc
            | Rrc | Ral | Rar | Cma | Cmc | Stc | Ret | R(..) | Rst(..) | Pchl | Push(..)
            | Pop(..) | Xthl | Sphl | Ei | Di | Nop => 1,
            Mvi(..) | Adi(..) | Aci(..) | Sui(..) | Sbi(..) | Ani(..) | Ori(..) | Xri(..)
            | Cpi(..) | In(..) | Out(..) => 2,
            Lxi(..) | Lda(..) | Sta(..) | Lhld(..) | Shld(..) | Ldax(..) | Stax(..) | Jmp(..)
            | J(..) | Call(..) | C(..) => 3,
        }
    }
    pub fn decode_from(bytes: &[u8]) -> Self {
        // TODO: is this good? helps me not fuck up the pattern matching below
        #![deny(unreachable_patterns)]

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
            (1, 1, 0, 0, 0, 0, 1, 1) => Jmp(op16),
            (1, 1, _, _, _, 0, 1, 0) => J(cond, op16),
            (1, 1, 0, 0, 1, 1, 0, 1) => Call(op16),
            (1, 1, _, _, _, 1, 0, 0) => C(cond, op16),
            (1, 1, 0, 0, 1, 0, 0, 1) => Ret,
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
            (0, 0, 0, 0, 0, 0, 0, 0) => Nop,
            _ => panic!("unknown opcode {:x}", bytes[0]),
        }
    }
}
