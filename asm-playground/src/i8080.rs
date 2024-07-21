use super::parse::{Operand, ParsableInstruction};

#[cfg(feature = "encode")]
fn l(val: u16) -> u8 {
    (val & 0xff) as u8
}

#[cfg(feature = "encode")]
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
    #[inline(always)]
    pub fn to_dst_byte(self) -> u8 {
        (self as u8) << 3
    }
    #[inline(always)]
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
    #[inline]
    #[cfg(feature = "encode")]
    fn to_byte(self) -> u8 {
        (self as u8) << 4
    }
    #[inline(always)]
    pub fn low(&self) -> Reg {
        match self {
            Self::Bc => Reg::C,
            Self::De => Reg::E,
            Self::Hl => Reg::L,
            Self::Sp => panic!("what no don't"),
        }
    }
    #[inline(always)]
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
    #[inline(always)]
    pub fn to_byte(self) -> u8 {
        (self as u8) << 3
    }
}

/// A form of a decoded 8080/8085 instruction that's easy to work with in Rust.
#[derive(Debug, Clone, PartialEq, Eq, ParsableInstruction)]
#[repr(u8)]
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

#[cfg(feature = "encode")]
impl super::EncodableInstruction for Instruction {
    fn len(&self) -> usize {
        panic!("i forgor")
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
            Rst(dst)    => vec![0b11000111 | (dst & 0b111000)],
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

impl Instruction {
    // it's faster if we do both at once
    #[inline]
    pub fn decode_and_len(bytes: &[u8]) -> (Self, usize) {
        use Instruction::*;
        use RegPair::*;
        use Condition::*;

        // it's faster this way as it doesn't waste time calling it when it's not needed
        macro_rules! d16 {
            () => {u16::from_le_bytes([bytes[1], bytes[2]])}
        }
        // this one is for consistency reasons
        macro_rules! d8 {
            () => {bytes[1]}
        }
        match bytes[0] {
            0x00 => (Nop, 1),
            0x01 => (Lxi(Bc, d16!()), 3),
            0x02 => (Stax(Bc), 1),
            0x03 => (Inx(Bc), 1),
            0x04 => (Inr(Reg::B), 1),
            0x05 => (Dcr(Reg::B), 1),
            0x06 => (Mvi(Reg::B, d8!()), 2),
            0x07 => (Rlc, 1),
            0x08 => (Nop, 1),
            0x09 => (Dad(Bc), 1),
            0x0a => (Ldax(Bc), 1),
            0x0b => (Dcx(Bc), 1),
            0x0c => (Inr(Reg::C), 1),
            0x0d => (Dcr(Reg::C), 1),
            0x0e => (Mvi(Reg::C, d8!()), 2),
            0x0f => (Rrc, 1),
            0x10 => (Nop, 1),
            0x11 => (Lxi(De, d16!()), 3),
            0x12 => (Stax(De), 1),
            0x13 => (Inx(De), 1),
            0x14 => (Inr(Reg::D), 1),
            0x15 => (Dcr(Reg::D), 1),
            0x16 => (Mvi(Reg::D, d8!()), 2),
            0x17 => (Ral, 1),
            0x18 => (Nop, 1),
            0x19 => (Dad(De), 1),
            0x1a => (Ldax(De), 1),
            0x1b => (Dcx(De), 1),
            0x1c => (Inr(Reg::E), 1),
            0x1d => (Dcr(Reg::E), 1),
            0x1e => (Mvi(Reg::E, d8!()), 2),
            0x1f => (Rar, 1),
            0x20 => (Nop, 1),
            0x21 => (Lxi(Hl, d16!()), 3),
            0x22 => (Shld(d16!()), 3),
            0x23 => (Inx(Hl), 1),
            0x24 => (Inr(Reg::H), 1),
            0x25 => (Dcr(Reg::H), 1),
            0x26 => (Mvi(Reg::H, d8!()), 2),
            0x27 => (Daa, 1),
            0x28 => (Nop, 1),
            0x29 => (Dad(Hl), 1),
            0x2a => (Lhld(d16!()), 3),
            0x2b => (Dcx(Hl), 1),
            0x2c => (Inr(Reg::L), 1),
            0x2d => (Dcr(Reg::L), 1),
            0x2e => (Mvi(Reg::L, d8!()), 2),
            0x2f => (Cma, 1),
            0x30 => (Nop, 1),
            0x31 => (Lxi(Sp, d16!()), 3),
            0x32 => (Sta(d16!()), 3),
            0x33 => (Inx(Sp), 1),
            0x34 => (Inr(Reg::M), 1),
            0x35 => (Dcr(Reg::M), 1),
            0x36 => (Mvi(Reg::M, d8!()), 2),
            0x37 => (Stc, 1),
            0x38 => (Nop, 1),
            0x39 => (Dad(Sp), 1),
            0x3a => (Lda(d16!()), 3),
            0x3b => (Dcx(Sp), 1),
            0x3c => (Inr(Reg::A), 1),
            0x3d => (Dcr(Reg::A), 1),
            0x3e => (Mvi(Reg::A, d8!()), 2),
            0x3f => (Cmc, 1),
            0x40 => (Mov(Reg::B, Reg::B), 1),
            0x41 => (Mov(Reg::B, Reg::C), 1),
            0x42 => (Mov(Reg::B, Reg::D), 1),
            0x43 => (Mov(Reg::B, Reg::E), 1),
            0x44 => (Mov(Reg::B, Reg::H), 1),
            0x45 => (Mov(Reg::B, Reg::L), 1),
            0x46 => (Mov(Reg::B, Reg::M), 1),
            0x47 => (Mov(Reg::B, Reg::A), 1),
            0x48 => (Mov(Reg::C, Reg::B), 1),
            0x49 => (Mov(Reg::C, Reg::C), 1),
            0x4a => (Mov(Reg::C, Reg::D), 1),
            0x4b => (Mov(Reg::C, Reg::E), 1),
            0x4c => (Mov(Reg::C, Reg::H), 1),
            0x4d => (Mov(Reg::C, Reg::L), 1),
            0x4e => (Mov(Reg::C, Reg::M), 1),
            0x4f => (Mov(Reg::C, Reg::A), 1),

            0x50 => (Mov(Reg::D, Reg::B), 1),
            0x51 => (Mov(Reg::D, Reg::C), 1),
            0x52 => (Mov(Reg::D, Reg::D), 1),
            0x53 => (Mov(Reg::D, Reg::E), 1),
            0x54 => (Mov(Reg::D, Reg::H), 1),
            0x55 => (Mov(Reg::D, Reg::L), 1),
            0x56 => (Mov(Reg::D, Reg::M), 1),
            0x57 => (Mov(Reg::D, Reg::A), 1),
            0x58 => (Mov(Reg::E, Reg::B), 1),
            0x59 => (Mov(Reg::E, Reg::C), 1),
            0x5a => (Mov(Reg::E, Reg::D), 1),
            0x5b => (Mov(Reg::E, Reg::E), 1),
            0x5c => (Mov(Reg::E, Reg::H), 1),
            0x5d => (Mov(Reg::E, Reg::L), 1),
            0x5e => (Mov(Reg::E, Reg::M), 1),
            0x5f => (Mov(Reg::E, Reg::A), 1),
            
            0x60 => (Mov(Reg::H, Reg::B), 1),
            0x61 => (Mov(Reg::H, Reg::C), 1),
            0x62 => (Mov(Reg::H, Reg::D), 1),
            0x63 => (Mov(Reg::H, Reg::E), 1),
            0x64 => (Mov(Reg::H, Reg::H), 1),
            0x65 => (Mov(Reg::H, Reg::L), 1),
            0x66 => (Mov(Reg::H, Reg::M), 1),
            0x67 => (Mov(Reg::H, Reg::A), 1),
            0x68 => (Mov(Reg::L, Reg::B), 1),
            0x69 => (Mov(Reg::L, Reg::C), 1),
            0x6a => (Mov(Reg::L, Reg::D), 1),
            0x6b => (Mov(Reg::L, Reg::E), 1),
            0x6c => (Mov(Reg::L, Reg::H), 1),
            0x6d => (Mov(Reg::L, Reg::L), 1),
            0x6e => (Mov(Reg::L, Reg::M), 1),
            0x6f => (Mov(Reg::L, Reg::A), 1),
            
            0x70 => (Mov(Reg::M, Reg::B), 1),
            0x71 => (Mov(Reg::M, Reg::C), 1),
            0x72 => (Mov(Reg::M, Reg::D), 1),
            0x73 => (Mov(Reg::M, Reg::E), 1),
            0x74 => (Mov(Reg::M, Reg::H), 1),
            0x75 => (Mov(Reg::M, Reg::L), 1),
            0x76 => (Hlt, 1),
            0x77 => (Mov(Reg::M, Reg::A), 1),
            0x78 => (Mov(Reg::A, Reg::B), 1),
            0x79 => (Mov(Reg::A, Reg::C), 1),
            0x7a => (Mov(Reg::A, Reg::D), 1),
            0x7b => (Mov(Reg::A, Reg::E), 1),
            0x7c => (Mov(Reg::A, Reg::H), 1),
            0x7d => (Mov(Reg::A, Reg::L), 1),
            0x7e => (Mov(Reg::A, Reg::M), 1),
            0x7f => (Mov(Reg::A, Reg::A), 1),
            
            0x80 => (Add(Reg::B), 1),
            0x81 => (Add(Reg::C), 1),
            0x82 => (Add(Reg::D), 1),
            0x83 => (Add(Reg::E), 1),
            0x84 => (Add(Reg::H), 1),
            0x85 => (Add(Reg::L), 1),
            0x86 => (Add(Reg::M), 1),
            0x87 => (Add(Reg::A), 1),
            0x88 => (Adc(Reg::B), 1),
            0x89 => (Adc(Reg::C), 1),
            0x8a => (Adc(Reg::D), 1),
            0x8b => (Adc(Reg::E), 1),
            0x8c => (Adc(Reg::H), 1),
            0x8d => (Adc(Reg::L), 1),
            0x8e => (Adc(Reg::M), 1),
            0x8f => (Adc(Reg::A), 1),

            0x90 => (Sub(Reg::B), 1),
            0x91 => (Sub(Reg::C), 1),
            0x92 => (Sub(Reg::D), 1),
            0x93 => (Sub(Reg::E), 1),
            0x94 => (Sub(Reg::H), 1),
            0x95 => (Sub(Reg::L), 1),
            0x96 => (Sub(Reg::M), 1),
            0x97 => (Sub(Reg::A), 1),
            0x98 => (Sbb(Reg::B), 1),
            0x99 => (Sbb(Reg::C), 1),
            0x9a => (Sbb(Reg::D), 1),
            0x9b => (Sbb(Reg::E), 1),
            0x9c => (Sbb(Reg::H), 1),
            0x9d => (Sbb(Reg::L), 1),
            0x9e => (Sbb(Reg::M), 1),
            0x9f => (Sbb(Reg::A), 1),

            0xa0 => (Ana(Reg::B), 1),
            0xa1 => (Ana(Reg::C), 1),
            0xa2 => (Ana(Reg::D), 1),
            0xa3 => (Ana(Reg::E), 1),
            0xa4 => (Ana(Reg::H), 1),
            0xa5 => (Ana(Reg::L), 1),
            0xa6 => (Ana(Reg::M), 1),
            0xa7 => (Ana(Reg::A), 1),
            0xa8 => (Xra(Reg::B), 1),
            0xa9 => (Xra(Reg::C), 1),
            0xaa => (Xra(Reg::D), 1),
            0xab => (Xra(Reg::E), 1),
            0xac => (Xra(Reg::H), 1),
            0xad => (Xra(Reg::L), 1),
            0xae => (Xra(Reg::M), 1),
            0xaf => (Xra(Reg::A), 1),

            0xb0 => (Ora(Reg::B), 1),
            0xb1 => (Ora(Reg::C), 1),
            0xb2 => (Ora(Reg::D), 1),
            0xb3 => (Ora(Reg::E), 1),
            0xb4 => (Ora(Reg::H), 1),
            0xb5 => (Ora(Reg::L), 1),
            0xb6 => (Ora(Reg::M), 1),
            0xb7 => (Ora(Reg::A), 1),
            0xb8 => (Cmp(Reg::B), 1),
            0xb9 => (Cmp(Reg::C), 1),
            0xba => (Cmp(Reg::D), 1),
            0xbb => (Cmp(Reg::E), 1),
            0xbc => (Cmp(Reg::H), 1),
            0xbd => (Cmp(Reg::L), 1),
            0xbe => (Cmp(Reg::M), 1),
            0xbf => (Cmp(Reg::A), 1),

            0xc0 => (R(NonZero), 1),
            0xc1 => (Pop(Bc), 1),
            0xc2 => (J(NonZero, d16!()), 3),
            0xc3 | 0xcb => (Jmp(d16!()), 3),
            0xc4 => (C(NonZero, d16!()), 3),
            0xc5 => (Push(Bc), 1),
            0xc6 => (Adi(d8!()), 2),
            0xc7 => (Rst(0x00), 1),
            0xc8 => (R(Zero), 1),
            0xc9 | 0xd9 => (Ret, 1),
            0xca => (J(Zero, d16!()), 3),
            0xcc => (C(Zero, d16!()), 3),
            0xcd | 0xdd | 0xed | 0xfd => (Call(d16!()), 3),
            0xce => (Aci(d8!()), 2),
            0xcf => (Rst(0x08), 1),

            0xd0 => (R(NoCarry), 1),
            0xd1 => (Pop(De), 1),
            0xd2 => (J(NoCarry, d16!()), 3),
            0xd3 => (Out(d8!()), 2),
            0xd4 => (C(NoCarry, d16!()), 3),
            0xd5 => (Push(De), 1),
            0xd6 => (Sui(d8!()), 2),
            0xd7 => (Rst(0x10), 1),
            0xd8 => (R(Carry), 1),
            0xda => (J(Carry, d16!()), 3),
            0xdb => (In(d8!()), 2),
            0xdc => (C(Carry, d16!()), 3),
            0xde => (Sbi(d8!()), 2),
            0xdf => (Rst(0x18), 1),

            0xe0 => (R(Odd), 1),
            0xe1 => (Pop(Hl), 1),
            0xe2 => (J(Odd, d16!()), 3),
            0xe3 => (Xthl, 1),
            0xe4 => (C(Odd, d16!()), 3),
            0xe5 => (Push(Hl), 1),
            0xe6 => (Ani(d8!()), 2),
            0xe7 => (Rst(0x20), 1),
            0xe8 => (R(Even), 1),
            0xe9 => (Pchl, 1),
            0xea => (J(Even, d16!()), 3),
            0xeb => (Xchg, 1),
            0xec => (C(Even, d16!()), 3),
            0xee => (Xri(d8!()), 2),
            0xef => (Rst(0x28), 1),

            0xf0 => (R(Plus), 1),
            0xf1 => (Pop(Sp), 1),
            0xf2 => (J(Plus, d16!()), 3),
            0xf3 => (Di, 1),
            0xf4 => (C(Plus, d16!()), 3),
            0xf5 => (Push(Sp), 1),
            0xf6 => (Ori(d8!()), 2),
            0xf7 => (Rst(0x30), 1),
            0xf8 => (R(Minus), 1),
            0xf9 => (Sphl, 1),
            0xfa => (J(Minus, d16!()), 3),
            0xfb => (Ei, 1),
            0xfc => (C(Minus, d16!()), 3),
            0xfe => (Cpi(d8!()), 2),
            0xff => (Rst(0x38), 1),
        }
    }
}
