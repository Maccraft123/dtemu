use super::{Operand, ParsableOpcode, FixupInstruction, ParsableInstruction, EncodableInstruction, Patch};
use nom::bytes::complete::{tag, tag_no_case};
use nom::character::complete::line_ending;
use nom::branch::alt;
use nom::combinator::map;
use nom::sequence::{preceded, terminated, delimited};
use std::collections::HashSet;
use crate::OperandPatches;
use core::mem;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Addressing {
    Accumulator,
    Absolute,
    AbsoluteX,
    AbsoluteY,
    Immediate,
    Implied,
    Indirect,
    IndirectX,
    IndirectY,
    Relative,
    ZeroPage,
    ZeroPageX,
    ZeroPageY,
}

#[derive(Debug, Clone)]
pub enum Mos6502Operand {
    Accumulator,
    Absolute(u16),
    AbsoluteX(u16),
    AbsoluteY(u16),
    Immediate(u8),
    Implied,
    Indirect(u16),
    IndirectX(u16),
    IndirectY(u16),
    Relative(i8),
    ZeroPage(u8),
    ZeroPageX(u8),
    ZeroPageY(u8),
}

impl Mos6502Operand {
    pub fn addressing(&self) -> Addressing {
        match self {
            Self::Accumulator => Addressing::Accumulator,
            Self::Absolute(_) => Addressing::Absolute,
            Self::AbsoluteX(_) => Addressing::AbsoluteX,
            Self::AbsoluteY(_) => Addressing::AbsoluteY,
            Self::Immediate(_) => Addressing::Immediate,
            Self::Implied => Addressing::Implied,
            Self::Indirect(_) => Addressing::Indirect,
            Self::IndirectX(_) => Addressing::IndirectX,
            Self::IndirectY(_) => Addressing::IndirectY,
            Self::Relative(_) => Addressing::Relative,
            Self::ZeroPage(_) => Addressing::ZeroPage,
            Self::ZeroPageX(_) => Addressing::ZeroPageX,
            Self::ZeroPageY(_) => Addressing::ZeroPageY,
        }
    }
    pub fn to_u8(&self) -> Option<u8> {
        match self {
            Self::Immediate(val) => Some(*val),
            Self::Relative(val) => Some(*val as u8),
            Self::ZeroPage(val) => Some(*val),
            Self::ZeroPageX(val) => Some(*val),
            Self::ZeroPageY(val) => Some(*val),
            _ => None,
        }
    }
    pub fn to_u16(&self) -> Option<u16> {
        match self {
            Self::Absolute(val) => Some(*val),
            Self::AbsoluteX(val) => Some(*val),
            Self::AbsoluteY(val) => Some(*val),
            Self::Indirect(val) => Some(*val),
            Self::IndirectX(val) => Some(*val),
            Self::IndirectY(val) => Some(*val),
            _ => None,
        }
    }
}

impl Operand for Mos6502Operand {
    fn set(&mut self, val: f64) {
         match self {
            Self::Absolute(cur) => *cur = val as u16,
            Self::AbsoluteX(cur) => *cur = val as u16,
            Self::AbsoluteY(cur) => *cur = val as u16,
            Self::Immediate(cur) => *cur = val as u8,
            Self::Indirect(cur) => *cur = val as u16,
            Self::IndirectX(cur) => *cur = val as u16,
            Self::IndirectY(cur) => *cur = val as u16,
            Self::Relative(cur) => *cur = val as i8,
            Self::ZeroPage(cur) => *cur = val as u8,
            Self::ZeroPageX(cur) => *cur = val as u8,
            Self::ZeroPageY(cur) => *cur = val as u8,
            _ => (),
        }
    }
    fn parse(l: &HashSet<String>, i: u32, p: &OperandPatches) -> impl FnMut(&str) -> nom::IResult<&str, Mos6502Operand> {
        move |s| {
            alt((
                map(
                    preceded(
                        tag("#"),
                        u8::parse(l, i, p),
                    ),
                    |v| Mos6502Operand::Immediate(v),
                ),
                map(
                    terminated(
                        u8::parse(l, i, p),
                        tag_no_case(",x"),
                    ),
                    |v| Mos6502Operand::ZeroPageX(v),
                ),
                map(
                    terminated(
                        u8::parse(l, i, p),
                        tag_no_case(",y"),
                    ),
                    |v| Mos6502Operand::ZeroPageY(v),
                ),
                map(
                    delimited(
                        tag_no_case("("),
                        u16::parse(l, i, p),
                        tag_no_case(",x)"),
                    ),
                    |v| Mos6502Operand::IndirectX(v),
                ),
                map(
                    delimited(
                        tag_no_case("("),
                        u16::parse(l, i, p),
                        tag_no_case("),y"),
                    ),
                    |v| Mos6502Operand::IndirectY(v),
                ),
                map(
                    delimited(
                        tag_no_case("("),
                        u16::parse(l, i, p),
                        tag_no_case(")"),
                    ),
                    |v| Mos6502Operand::Indirect(v),
                ),
                map(
                    terminated(
                        u16::parse(l, i, p),
                        tag_no_case(",x"),
                    ),
                    |v| Mos6502Operand::AbsoluteX(v),
                ),
                map(
                    terminated(
                        u16::parse(l, i, p),
                        tag_no_case(",y"),
                    ),
                    |v| Mos6502Operand::AbsoluteY(v),
                ),
                map(
                    u8::parse(l, i, p),
                    |v| Mos6502Operand::ZeroPage(v),
                ),
                map(
                    u16::parse(l, i, p),
                    |v| Mos6502Operand::Absolute(v),
                ),
                map(
                    tag_no_case("a"),
                    |_| Mos6502Operand::Accumulator,
                ),
                map(
                    alt((
                        line_ending,
                        tag_no_case(";"),
                    )),
                    |_| Mos6502Operand::Implied,
                ),
            ))(s)
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, ParsableOpcode)]
pub enum Opcode {
    /// Load A
    Lda,
    /// Load X
    Ldx,
    /// Load Y
    Ldy,
    /// Store A
    Sta,
    /// Store X
    Stx,
    /// Store Y
    Sty,
    /// Copy A to X
    Tax,
    /// Copy A to Y
    Tay,
    /// Copy X to SP
    Txs,
    /// Copy X to A
    Txa,
    /// Copy Y to A
    Tya,
    /// Decrement Y
    Dey,
    /// Branch on carry clear
    Bcc,
    /// Jmp
    Jmp,
    /// Software interrupt
    Brk,
    /// Push Accumulator on stack
    Pha,
    /// Pull Accumulator from stack
    Pla,
    /// Jump to subroutine
    Jsr,
    /// Clear decimal flag
    Cld,
    /// Set interrupt disable
    Sei,
    /// Return from interrupt handler
    Rti,
    /// Arithmetic shift left
    Asl,
    /// Logical shift left
    Lsr,
    /// Bit compare with memory, AND-ing A with operand. 7th bit of result goes into Negative
    /// flag, 6th bit of result goes into Overflow flag, Zero gets set if result is 0x00
    Bit,
    /// Branch on zero flag not set
    Bne,
    /// Branch on zero flag set
    Beq,
    /// Increment X
    Inx,
    /// Operand OR Accumulator
    Ora,
    /// Operand AND Accumulator
    And,
    /// Operand XOR Accumulator
    Eor,
    /// Add with carry
    Adc,
    /// Compare(subtract without storing the result) Operand with accumulator
    Cmp,
    /// Subtract with carry, equal to adding negated operand to A
    Sbc,
    /// Return from subroutine
    Rts,
    /// Branch on overflow flag set
    Bvs,
    /// Branch on overflow flag not set
    Bvc,
    /// Branch on sign flag not set
    Bpl,
    /// Branch on sign flag set
    Bmi,
    /// Branch on carry flag not set
    Bcs,
    /// Set decimal flag
    Sed,
    /// Set carry flag
    Sec,
    /// Clear overflow flag
    Clv,
    /// Clear interrupt-disable flag
    Cli,
    /// Clear carry flag
    Clc,
    /// Increment Y
    Iny,
    /// Decrement X
    Dex,
    /// Copy SP to X
    Tsx,
    /// Do nothing
    Nop,
    /// Rotate left
    Rol,
    /// Rotate right
    Ror,
    /// Compare(like CMP) X
    Cpx,
    /// Compare(like CMP) Y
    Cpy,
    /// Pull flags from stack
    Plp,
    /// Push flgas to stack
    Php,
    /// Decrement operand
    Dec,
    /// Increment operand
    Inc,
    /// Undocumented, Freeze the CPU
    Jam,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DatalessInstruction(Opcode, Addressing);

macro_rules! inst {
    ($opcode: ident, $addr: ident) => {
        Self(Opcode::$opcode, Addressing::$addr)
    };
}

impl DatalessInstruction {
    const LUT: &'static [(u8, DatalessInstruction)] = &[
        (0x00, inst!(Brk, Implied)),
        (0x01, inst!(Ora, IndirectX)),
        (0x02, inst!(Jam, Implied)),
        //(0x03, inst!(Slo, IndirectX)),
        //(0x04, inst!(Nop, ZeroPage)),
        (0x05, inst!(Ora, ZeroPage)),
        (0x06, inst!(Asl, ZeroPage)),
        //(0x07, inst!(Slo, ZeroPage)),
        (0x08, inst!(Php, Implied)),
        (0x09, inst!(Ora, Immediate)),
        (0x0a, inst!(Asl, Accumulator)),
        //(0x0b, inst!(Anc, Immediate)),
        //(0x0c, inst!(Nop, Absolute)),
        (0x0d, inst!(Ora, Absolute)),
        (0x0e, inst!(Asl, Absolute)),
        //(0x0f, inst!(Slo, Absolute)),
        (0x10, inst!(Bpl, Relative)),
        (0x11, inst!(Ora, IndirectY)),
        //(0x12, inst!(Jam, Implied)),
        //(0x13, inst!(Slo, IndirectY)),
        //(0x14, inst!(Nop, ZeroPageX)),
        (0x15, inst!(Ora, ZeroPageX)),
        (0x16, inst!(Asl, ZeroPageX)),
        //(0x17, inst!(Slo, ZeroPageX)),
        (0x18, inst!(Clc, Implied)),
        (0x19, inst!(Ora, AbsoluteY)),
        //(0x1a, inst!(Nop, Implied)),
        //(0x1b, inst!()),
        //(0x1c, inst!(Nop, Absolute)),
        (0x1d, inst!(Ora, AbsoluteX)),
        (0x1e, inst!(Asl, AbsoluteX)),
        //(0x1f, inst!()),
        (0x20, inst!(Jsr, Absolute)),
        (0x21, inst!(And, ZeroPageX)),
        //(0x22, inst!(Jam, Implied)),
        //(0x23, inst!()),
        (0x24, inst!(Bit, ZeroPage)),
        (0x25, inst!(And, ZeroPage)),
        (0x26, inst!(Rol, ZeroPage)),
        //(0x27, inst!()),
        (0x28, inst!(Plp, Implied)),
        (0x29, inst!(And, Immediate)),
        (0x2a, inst!(Rol, Accumulator)),
        //(0x2b, inst!()),
        (0x2c, inst!(Bit, Absolute)),
        (0x2d, inst!(And, Absolute)),
        (0x2e, inst!(Rol, Absolute)),
        //(0x2f, inst!()),
        (0x30, inst!(Bmi, Relative)),
        (0x31, inst!(And, ZeroPageY)),
        (0x32, inst!(Jam, Implied)),
        //(0x33, inst!()),
        //(0x34, inst!()),
        (0x35, inst!(And, ZeroPageX)),
        (0x36, inst!(Rol, ZeroPageX)),
        //(0x37, inst!()),
        (0x38, inst!(Sec, Implied)),
        (0x39, inst!(And, AbsoluteY)),
        //(0x3a, inst!(Nop, )),
        //(0x3b, inst!()),
        (0x3c, inst!(Nop, Absolute)),
        (0x3d, inst!(Ora, AbsoluteX)),
        (0x3e, inst!(Asl, AbsoluteX)),
        //(0x3f, inst!()),
        (0x40, inst!(Rti, Implied)),
        (0x41, inst!(Eor, ZeroPageX)),
        (0x42, inst!(Jam, Implied)),
        //(0x43, inst!()),
        (0x44, inst!(Nop, ZeroPage)),
        (0x45, inst!(Eor, ZeroPage)),
        (0x46, inst!(Lsr, ZeroPage)),
        //(0x47
        (0x48, inst!(Pha, Implied)),
        (0x49, inst!(Eor, Immediate)),
        (0x4a, inst!(Lsr, Accumulator)),
        //(0x4b, inst!()),
        (0x4c, inst!(Jmp, Absolute)),
        (0x4d, inst!(Eor, Absolute)),
        (0x4e, inst!(Lsr, Accumulator)),
        //(0x4f, inst!()),
        (0x50, inst!(Bvc, Relative)),
        (0x51, inst!(Eor, ZeroPageY)),
        (0x52, inst!(Jam, Implied)),
        //(0x53, 
        //(0x54
        (0x55, inst!(Eor, ZeroPageX)),
        (0x56, inst!(Lsr, ZeroPageX)),
        //(0x57, inst!()
        (0x58, inst!(Cli, Implied)),
        (0x59, inst!(Eor, AbsoluteX)),
        //(0x5a, inst!(Nop)),
        //(0x5b, inst!()),
        //(0x5c, inst!()),
        (0x5d, inst!(Eor, AbsoluteX)),
        (0x5e, inst!(Lsr, AbsoluteX)),
        //(0x5f, inst!()),
        (0x60, inst!(Rts, Implied)),
        (0x61, inst!(Adc, ZeroPageX)),
        (0x62, inst!(Jam, Implied)),
        //(0x63, inst!()),
        (0x64, inst!(Nop, ZeroPage)),
        (0x65, inst!(Adc, ZeroPage)),
        (0x66, inst!(Ror, ZeroPage)),
        //(0x67, inst!()),
        (0x68, inst!(Pla, Implied)),
        (0x69, inst!(Adc, Immediate)),
        (0x6a, inst!(Ror, Accumulator)),
        //(0x6b, inst!()),
        (0x6c, inst!(Jmp, Indirect)),
        (0x6d, inst!(Adc, Absolute)),
        (0x6e, inst!(Ror, Absolute)),
        //(0x6f, inst!()),
        (0x70, inst!(Bcs, Relative)),
        (0x71, inst!(Adc, ZeroPageY)),
        (0x72, inst!(Jam, Implied)),
        //(0x73, inst!()),
        (0x74, inst!(Nop, ZeroPage)),
        (0x75, inst!(Adc, ZeroPageX)),
        (0x76, inst!(Ror, ZeroPageX)),
        //(0x77, inst!()),
        (0x78, inst!(Sei, Implied)),
        (0x79, inst!(Adc, AbsoluteY)),
        //(0x7a, inst!(Nop)),
        (0x7d, inst!(Adc, AbsoluteX)),
        (0x7e, inst!(Ror, AbsoluteX)),
        (0x81, inst!(Sta, ZeroPageX)),
        (0x84, inst!(Sty, ZeroPage)),
        (0x85, inst!(Sta, ZeroPage)),
        (0x86, inst!(Stx, ZeroPage)),
        (0x88, inst!(Dey, Implied)),
        (0x89, inst!(Nop, ZeroPage)),
        (0x8a, inst!(Txa, Implied)),
        (0x8c, inst!(Sty, Absolute)),
        (0x8d, inst!(Sta, Absolute)),
        (0x8e, inst!(Stx, Absolute)),
        (0x90, inst!(Bcc, Relative)),
        (0x91, inst!(Sta, ZeroPageY)),
        (0x94, inst!(Sty, ZeroPage)),
        (0x95, inst!(Sta, ZeroPageX)),
        (0x96, inst!(Stx, ZeroPageY)),
        (0x98, inst!(Tya, Implied)),
        (0x99, inst!(Sta, AbsoluteY)),
        (0x9a, inst!(Txs, Implied)),
        (0x9d, inst!(Sta, AbsoluteX)),
        (0xa0, inst!(Ldy, Immediate)),
        (0xa1, inst!(Lda, ZeroPageX)),
        (0xa2, inst!(Ldx, Immediate)),
        (0xa4, inst!(Ldy, ZeroPage)),
        (0xa5, inst!(Lda, ZeroPage)),
        (0xa6, inst!(Ldx, ZeroPage)),
        (0xa8, inst!(Tay, Implied)),
        (0xa9, inst!(Lda, Immediate)),
        (0xaa, inst!(Tax, Implied)),
        (0xac, inst!(Ldy, Absolute)),
        (0xad, inst!(Lda, Absolute)),
        (0xae, inst!(Ldx, Absolute)),
        (0xb0, inst!(Bcs, Relative)),
        (0xb1, inst!(Lda, IndirectY)),
        (0xb4, inst!(Ldy, ZeroPage)),
        (0xb5, inst!(Lda, ZeroPageX)),
        (0xb6, inst!(Ldx, ZeroPageY)),
        (0xb8, inst!(Clv, Implied)),
        (0xb9, inst!(Lda, AbsoluteY)),
        (0xba, inst!(Tsx, Implied)),
        (0xbc, inst!(Ldy, AbsoluteX)),
        (0xbd, inst!(Lda, AbsoluteX)),
        (0xbe, inst!(Ldx, AbsoluteY)),
        (0xc0, inst!(Cpy, Immediate)),
        (0xc1, inst!(Cmp, ZeroPageX)),
        (0xc4, inst!(Cpy, ZeroPage)),
        (0xc5, inst!(Cmp, ZeroPage)),
        (0xc6, inst!(Dec, ZeroPage)),
        (0xc8, inst!(Iny, Implied)),
        (0xc9, inst!(Cmp, Immediate)),
        (0xca, inst!(Dex, Implied)),
        (0xcc, inst!(Cpy, Absolute)),
        (0xcd, inst!(Cmp, Absolute)),
        (0xce, inst!(Dec, Absolute)),
        (0xd0, inst!(Bne, Relative)),
        (0xd1, inst!(Cmp, ZeroPageY)),
        (0xd5, inst!(Cmp, ZeroPageX)),
        (0xd6, inst!(Dec, ZeroPageX)),
        (0xd8, inst!(Cld, Implied)),
        (0xd9, inst!(Cmp, AbsoluteY)),
        (0xdd, inst!(Cmp, AbsoluteX)),
        (0xde, inst!(Dec, AbsoluteX)),
        (0xe0, inst!(Cpx, Immediate)),
        (0xe1, inst!(Sbc, ZeroPageX)),
        (0xe4, inst!(Cpx, ZeroPage)),
        (0xe5, inst!(Sbc, ZeroPage)),
        (0xe6, inst!(Inc, ZeroPage)),
        (0xe8, inst!(Inx, Implied)),
        (0xe9, inst!(Sbc, Immediate)),
        (0xea, inst!(Nop, Implied)),
        //(0xeb, inst!(Sbc, Immediate)),
        (0xec, inst!(Cpx, Absolute)),
        (0xed, inst!(Sbc, Absolute)),
        (0xee, inst!(Inc, Absolute)),
        (0xf0, inst!(Beq, Relative)),
        (0xf1, inst!(Sbc, ZeroPageY)),
        (0xf5, inst!(Sbc, ZeroPageX)),
        (0xf6, inst!(Inc, ZeroPageX)),
        (0xf8, inst!(Sed, Implied)),
        (0xf9, inst!(Sbc, AbsoluteY)),
        (0xfd, inst!(Sbc, AbsoluteX)),
        (0xfe, inst!(Inc, AbsoluteX)),
    ];
    
    pub fn from_u8(val: u8) -> Option<Self> {
        for (i, ret) in Self::LUT {
            if val == *i {
                return Some(ret.clone())
            }
        }
        None
    }

    pub fn to_u8(&self) -> Option<u8> {
        for (ret, i) in Self::LUT {
            if self == i {
                return Some(*ret);
            }
        }
        None
    }

    pub fn opcode(&self) -> Opcode {
        self.0
    }

    pub fn addressing(&self) -> Addressing {
        self.1
    }

    pub fn add_data(&self, bytes: &[u8]) -> Instruction {
        Instruction(self.0, op(self.1, bytes))
    }

    pub fn len(&self) -> u16 {
        use Addressing::*;
        match self.1 {
        Accumulator | Implied => 1,
        Immediate | Relative | ZeroPage | ZeroPageX | ZeroPageY => 2,
        Absolute | AbsoluteX | AbsoluteY |
            Indirect | IndirectX | IndirectY => 3,
        }
    }
}

fn op(addressing: Addressing, bytes: &[u8]) -> Mos6502Operand {
    let op16 = u16::from_le_bytes([bytes[0], bytes[1]]);
    let op8 = bytes[0];
    match addressing {
        Addressing::Accumulator => Mos6502Operand::Accumulator,
        Addressing::Absolute => Mos6502Operand::Absolute(op16),
        Addressing::AbsoluteX => Mos6502Operand::AbsoluteY(op16),
        Addressing::AbsoluteY => Mos6502Operand::AbsoluteX(op16),
        Addressing::Immediate => Mos6502Operand::Immediate(op8),
        Addressing::Implied => Mos6502Operand::Implied,
        Addressing::Indirect => Mos6502Operand::Indirect(op16),
        Addressing::IndirectX => Mos6502Operand::IndirectY(op16),
        Addressing::IndirectY => Mos6502Operand::IndirectX(op16),
        Addressing::Relative => Mos6502Operand::Relative(op8 as i8),
        Addressing::ZeroPage => Mos6502Operand::ZeroPage(op8),
        Addressing::ZeroPageX => Mos6502Operand::ZeroPageX(op8),
        Addressing::ZeroPageY => Mos6502Operand::ZeroPageY(op8),
    }
}

#[derive(Debug, Clone, ParsableInstruction)]
#[instruction(fixup)]
pub struct Instruction(Opcode, Mos6502Operand);

impl FixupInstruction for Instruction {
    fn fixup(&mut self, p: &OperandPatches) {
        match self.0 {
            Opcode::Bcc | Opcode::Bcs | Opcode::Beq | Opcode::Bmi |
            Opcode::Bne | Opcode::Bpl | Opcode::Bvc | Opcode::Bvs => {
                let val;
                if let Some(v) = self.1.to_u8() {
                    val = v as u32;
                } else if let Some(v) = self.1.to_u16() {
                    val = v as u32;
                } else {
                    panic!("Branch instructions must have an operand");
                }

                self.1 = Mos6502Operand::Relative(0);
                if let Some(patch) = p.get_patch(0) {
                    p.add_or_replace_patch(0, patch.into_relative());
                } else {
                    p.add_or_replace_patch(0, Patch::ConstRelative(val));
                }
            },
            _ => (),
        }
    }
}

impl EncodableInstruction for Instruction {
    fn encode(&self) -> Vec<u8> {
        let mut ret = Vec::with_capacity(3);
        println!("{:x?}", self);
        if let Some(b) = self.erase_data().to_u8() {
            ret.push(b);
        } else {
            panic!("addressing {:x?} is invalid for {:?}", self.1, self.0);
        }
        if let Some(op8) = self.1.to_u8() {
            ret.push(op8);
        } else if let Some(op16) = self.1.to_u16() {
            let [hi, lo] = op16.to_le_bytes();
            ret.push(hi);
            ret.push(lo);
        }
        ret
    }
}

impl Instruction {
    fn erase_data(&self) -> DatalessInstruction {
        DatalessInstruction(self.0, self.1.addressing())
    }
    pub fn decode_from(bytes: &[u8]) -> Self {
        DatalessInstruction::from_u8(bytes[0])
            .unwrap()
            .add_data(&bytes[1..])
    }
}
