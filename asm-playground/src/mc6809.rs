#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Addressing {
    Immediate8,
    Immediate16,
    Relative8,
    Relative16,
    Direct,
    Extended,
    Inherent,
    AccumulatorA,
    AccumulatorB,
    Indexed(IndexedData),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct IndexedData {
    flavor: IndexedFlavor,
    indirect: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IndexedFlavor {
    NoOffset(IndexRegister),
    Offset5(IndexRegister, i8),
    Offset8(IndexRegister, i8),
    Offset16(IndexRegister, i16),
    AOffset(IndexRegister),
    BOffset(IndexRegister),
    DOffset(IndexRegister),
    Increment1(IndexRegister),
    Decrement1(IndexRegister),
    Increment2(IndexRegister),
    Decrement2(IndexRegister),
    PcOffset8(i8),
    PcOffset16(i16),
    ExtendedIndirect(i16),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IndexRegister {
    X,
    Y,
    U,
    S,
}

impl IntoRegister16 for IndexRegister {
    fn to_reg(&self) -> Register16 {
        match self {
            Self::S => Register16::S,
            Self::U => Register16::U,
            Self::X => Register16::X,
            Self::Y => Register16::Y,
        }
    }
}

impl IndexedData {
    pub fn flavor(&self) -> IndexedFlavor { self.flavor }
    pub fn indirect(&self) -> bool { self.indirect }
    fn new(flavor: IndexedFlavor, indirect: bool) -> Self {
        Self { flavor, indirect }
    }
    const fn len(&self) -> usize {
        use IndexedFlavor::*;
        match self.flavor {
            NoOffset(..) | Offset5(..) | AOffset(..) | BOffset(..) | DOffset(..) | Increment1(..) | Decrement1(..) | Increment2(..) | Decrement2(..) => 1,
            Offset8(..) | PcOffset8(..) => 2,
            Offset16(..) | PcOffset16(..) | ExtendedIndirect(..) => 3,
        }
    }
    // https://www.maddes.net/m6809pm/appendix_f.htm
    #[inline]
    fn from_bytes(b: &[u8]) -> Option<Self> {
        let reg = match (b[0] & 0x60) >> 5 {
            0 => IndexRegister::X,
            1 => IndexRegister::Y,
            2 => IndexRegister::U,
            3 => IndexRegister::S,
            _ => unreachable!(),
        };
        let o8 = b.get(1).map(|&v| v as i8);
        let o5 = b.get(0).map(|&v| {
            let sign = v & 0x10 != 0;
            (v | (sign as u8 * !0b11111)) as i8
        });
        let o16 = b.get(1)
            .zip(b.get(2))
            .map(|(&l, &h)| i16::from_be_bytes([l, h]));
        let indirect = b[0] & 0b00010000 != 0;
        Some(match b[0] & !0x70 {
            0x80 if !indirect => Self::new(IndexedFlavor::Increment1(reg), false),
            0x81 => Self::new(IndexedFlavor::Increment2(reg), indirect),
            0x82 if !indirect => Self::new(IndexedFlavor::Decrement1(reg), false),
            0x83 => Self::new(IndexedFlavor::Decrement2(reg), indirect),
            0x84 => Self::new(IndexedFlavor::NoOffset(reg), indirect),
            0x85 => Self::new(IndexedFlavor::BOffset(reg), indirect),
            0x86 => Self::new(IndexedFlavor::AOffset(reg), indirect),
            0x88 => Self::new(IndexedFlavor::Offset8(reg, o8?), indirect),
            0x89 => Self::new(IndexedFlavor::Offset16(reg, o16?), indirect),
            0x8b => Self::new(IndexedFlavor::DOffset(reg), indirect),
            0x8c => Self::new(IndexedFlavor::PcOffset8(o8?), indirect),
            0x8d => Self::new(IndexedFlavor::PcOffset16(o16?), indirect),
            0x9f => Self::new(IndexedFlavor::ExtendedIndirect(o16?), true),
            0x00..=0x7f => Self::new(IndexedFlavor::Offset5(reg, o5?), false),
            _ => panic!("Unknown/Invalid indexed byte {:x}", b[0]),
        })
    }
}

impl Addressing {
    /// Converts a u8 into Self, based on this:
    /// https://www.maddes.net/m6809pm/appendix_f.htm
    #[inline]
    fn from_bytes(b: &[u8], is_16bit: bool) -> Option<Self> {
        match b[0] & 0xf0 {
            0x00 => Some(Self::Direct),
            0x10 => None,
            0x20 if !is_16bit => Some(Self::Relative8),
            0x20 if is_16bit => Some(Self::Relative16),
            0x30 => match b[0] & 0b1110 {
                0x00 | 0x02 => Some(Self::Indexed(IndexedData::from_bytes(&b[1..])?)),
                0x04 => Some(Self::Immediate8),
                0x08 | 0x0a | 0xe => Some(Self::Inherent),
                _ => unreachable!(),
            },
            0x40 => Some(Self::AccumulatorA),
            0x50 => Some(Self::AccumulatorB),
            0x80 | 0xc0 => match b[0] & 0x0f {
                0x3 | 0xc | 0xe => Some(Self::Immediate16),
                0x7 | 0xf => None,
                0xd if b[0] & 0xf0 == 0x80 => Some(Self::Relative8),
                _ => Some(Self::Immediate8),
            },
            0x90 | 0xd0 => Some(Self::Direct),
            0x60 | 0xa0 | 0xe0 => Some(Self::Indexed(IndexedData::from_bytes(&b[1..])?)),
            0x70 | 0xb0 | 0xf0 => Some(Self::Extended),
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Prefix {
    None,
    Ten,
    Eleven,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StackRegister {
    S,
    U,
}

impl IntoRegister16 for StackRegister {
    fn to_reg(&self) -> Register16 {
        match self {
            Self::S => Register16::S,
            Self::U => Register16::U,
        }
    }
}

pub trait IntoRegister8 {
    fn to_reg(&self) -> Register8;
}

pub trait IntoRegister16 {
    fn to_reg(&self) -> Register16;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Accumulator8 {
    A,
    B,
}

impl IntoRegister8 for Accumulator8 {
    fn to_reg(&self) -> Register8 {
        match self {
            Self::A => Register8::A,
            Self::B => Register8::B,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Reg16 {
    D,
    S,
    U,
    X,
    Y,
}

impl IntoRegister16 for Reg16 {
    fn to_reg(&self) -> Register16 {
        match self {
            Self::D => Register16::D,
            Self::S => Register16::S,
            Self::U => Register16::U,
            Self::X => Register16::X,
            Self::Y => Register16::Y,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Register8 {
    A,
    B,
    Dp,
    Cc,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Register16 {
    D,
    X,
    Y,
    Pc,
    S,
    U,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Condition {
    CarryClear,
    CarrySet,
    Equal,
    NotEqual,
    GreaterOrEqual,
    Greater,
    Higher,
    LessOrEqual,
    LowerOrSame,
    LessThan,
    Minus,
    Plus,
    Always,
    OverflowClear,
    OverflowSet,
    Never,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Opcode {
    Neg,
    Com,
    Lsr,
    Ror,
    Asr,
    Asl, // Also known as Lsl
    Rol,
    Dec,
    Inc,
    Tst,
    Jmp,
    Clr,
    Nop,
    Sync,
    Lbra,
    Lbsr,
    Daa,
    Orcc,
    Andcc,
    Sex,
    Exg,
    Tfr,
    B(Condition),
    Lea(IndexRegister),
    Psh(StackRegister),
    Pul(StackRegister),
    Rts,
    Abx,
    Rti,
    Cwai,
    Mul,
    Swi,
    Swi2,
    Swi3,
    Subd,
    Cmp16(Reg16),
    And(Accumulator8),
    Bit(Accumulator8),
    Ld(Accumulator8),
    Ld16(Reg16),
    Or(Accumulator8),
    Add(Accumulator8),
    Bsr,
    Jsr,
    Sub(Accumulator8),
    Cmp(Accumulator8),
    Sbc(Accumulator8),
    Addd,
    Eor(Accumulator8),
    Adc(Accumulator8),
    St(Accumulator8),
    St16(Reg16),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DatalessInstruction(Opcode, Addressing);

macro_rules! inst {
    ($opcode: expr, $addr: expr) => {
        Some(Self($opcode, $addr))
    };
}

impl DatalessInstruction {
    #[inline]
    pub fn from_bytes(b: &[u8]) -> Option<Self> {
        use Opcode::*;
        use Addressing::*;
        let (b, p): (&[u8], Prefix) = {
            if b[0] == 0x10 {
                (&b[1..], Prefix::Ten)
            } else if b[0] == 0x11 {
                (&b[1..], Prefix::Eleven)
            } else {
                (b, Prefix::None)
            }
        };
        let byte = b[0];
        let hi = b[0] & 0xf0;
        let lo = b[0] & 0x0f;
        match hi {
            // Misc
            0x10 => {
                match lo {
                    0x02 => inst!(Nop, Inherent),
                    0x03 => inst!(Sync, Inherent),
                    0x06 => inst!(B(Condition::Always), Relative16),
                    0x07 => inst!(Bsr, Relative16),
                    0x09 => inst!(Daa, Inherent),
                    0x0a => inst!(Orcc, Immediate8),
                    0x0c => inst!(Andcc, Immediate8),
                    0x0d => inst!(Sex, Inherent),
                    0x0e => inst!(Exg, Immediate8),
                    0x0f => inst!(Tfr, Immediate8),
                    _ => panic!("Unknown 6809 opcode {:x}", byte),
                }
            },
            // Relative
            0x20 => {
                let op = match lo {
                    0x00 => B(Condition::Always),
                    0x01 => B(Condition::Never),
                    0x02 => B(Condition::Higher),
                    0x03 => B(Condition::LowerOrSame),
                    0x04 => B(Condition::CarryClear),
                    0x05 => B(Condition::CarrySet),
                    0x06 => B(Condition::NotEqual),
                    0x07 => B(Condition::Equal),
                    0x08 => B(Condition::OverflowClear),
                    0x09 => B(Condition::OverflowSet),
                    0x0a => B(Condition::Plus),
                    0x0b => B(Condition::Minus),
                    0x0c => B(Condition::GreaterOrEqual),
                    0x0d => B(Condition::LessThan),
                    0x0e => B(Condition::Greater),
                    0x0f => B(Condition::LessOrEqual),
                    _ => unreachable!(),
                };
                match p {
                    Prefix::Ten => {
                        if matches!(byte & 0x0f, 0 | 4 | 5) {
                            panic!("Unknown 6809 opcode 10 {:x}", byte);
                        } else {
                            inst!(op, Relative16)
                        }
                    },
                    Prefix::None => inst!(op, Relative8),
                    Prefix::Eleven => panic!("Unknown 6809 opcode 11 {:x}", byte),
                }
            }, 
            0x30 => {
                let op = match byte & 0x0f {
                    0x00 => Lea(IndexRegister::X),
                    0x01 => Lea(IndexRegister::Y),
                    0x02 => Lea(IndexRegister::S),
                    0x03 => Lea(IndexRegister::U),
                    0x04 => Psh(StackRegister::S),
                    0x05 => Pul(StackRegister::S),
                    0x06 => Psh(StackRegister::U),
                    0x07 => Pul(StackRegister::U),
                    0x09 => Rts,
                    0x0a => Abx,
                    0x0b => Rti,
                    0x0c => Cwai,
                    0x0d => Mul,
                    0x0f => match p {
                        Prefix::None => Swi,
                        Prefix::Ten => Swi2,
                        Prefix::Eleven => Swi3,
                    },
                    _ => panic!("Unknown 6809 opcode {:x}", byte),
                };
                inst!(op, Addressing::from_bytes(b, false)?)
            },
            // Direct | A | B | Indexed | Extended
            0x00 | 0x40 | 0x50 | 0x60 | 0x70 => {
                let opcode = match lo {
                    0x0 => Neg,
                    0x3 => Com,
                    0x4 => Lsr,
                    0x6 => Ror,
                    0x7 => Asr,
                    0x8 => Asl,
                    0x9 => Rol,
                    0xa => Dec,
                    0xc => Inc,
                    0xd => Tst,
                    0xe if hi != 0x40 && hi != 0x50 => Jmp,
                    0xf => Clr,
                    _ => panic!("Unknown 6809 opcode {:x}", byte),
                };
                inst!(opcode, Addressing::from_bytes(b, false)?)
            },
            // Immediate | Direct | Indexed | Extended
            0x80 | 0x90 | 0xa0 | 0xb0 => {
                let imm = hi == 0x80;
                let op = match lo {
                    0x00 => Sub(Accumulator8::A),
                    0x01 => Cmp(Accumulator8::A),
                    0x02 => Sbc(Accumulator8::A),
                    0x03 if p == Prefix::None => Subd,
                    0x03 if p == Prefix::Ten => Cmp16(Reg16::D),
                    0x03 if p == Prefix::Eleven => Cmp16(Reg16::U),
                    0x04 => And(Accumulator8::A),
                    0x05 => Bit(Accumulator8::A),
                    0x06 => Ld(Accumulator8::A),
                    0x07 if !imm => St(Accumulator8::A),
                    0x08 => Eor(Accumulator8::A),
                    0x09 => Adc(Accumulator8::A),
                    0x0a => Or(Accumulator8::A),
                    0x0b => Add(Accumulator8::A),
                    0x0c if p == Prefix::None => Cmp16(Reg16::X),
                    0x0c if p == Prefix::Ten => Cmp16(Reg16::Y),
                    0x0c if p == Prefix::Eleven => Cmp16(Reg16::S),
                    0x0d if imm => Bsr,
                    0x0d if !imm => Jsr,
                    0x0e if p == Prefix::None => Ld16(Reg16::X),
                    0x0e if p == Prefix::Ten => Ld16(Reg16::Y),
                    0x0f if p == Prefix::None && !imm => St16(Reg16::X),
                    0x0f if p == Prefix::Ten && !imm => St16(Reg16::Y),
                    _ => panic!("Unknown 6809 opcode {:x}", byte),
                };
                inst!(op, Addressing::from_bytes(b, false)?)
            },
            // Immediate | Direct | Indexed | Extended
            0xc0 | 0xd0 | 0xe0 | 0xf0 => {
                let imm = hi == 0x80;
                let op = match lo {
                    0x00 => Sub(Accumulator8::B),
                    0x01 => Cmp(Accumulator8::B),
                    0x02 => Sbc(Accumulator8::B),
                    0x03 => Addd,
                    0x04 => And(Accumulator8::B),
                    0x05 => Bit(Accumulator8::B),
                    0x06 => Ld(Accumulator8::B),
                    0x07 if !imm => St(Accumulator8::B),
                    0x08 => Eor(Accumulator8::B),
                    0x09 => Adc(Accumulator8::B),
                    0x0a => Or(Accumulator8::B),
                    0x0b => Add(Accumulator8::B),
                    0x0c => Ld16(Reg16::D),
                    0x0d if !imm => St16(Reg16::D),
                    0x0e if p == Prefix::None => Ld16(Reg16::U),
                    0x0e if p == Prefix::Ten => Ld16(Reg16::S),
                    0x0f if p == Prefix::None && !imm => St16(Reg16::U),
                    0x0f if p == Prefix::Ten && !imm => St16(Reg16::S),
                    _ => panic!("Unknown 6809 opcode {:x}", byte),
                };
                inst!(op, Addressing::from_bytes(b, false)?)
            },
            _ => unreachable!(),
        }
    }
    #[inline(always)]
    pub const fn unprefixed_len(&self) -> usize {
        use Addressing::*;
        match self.1 {
            AccumulatorA | AccumulatorB | Inherent => 1,
            Immediate8 | Relative8 | Direct => 2,
            Immediate16 | Relative16 | Extended => 3,
            Indexed(d) => d.len() + 1,
        }
    }
    #[inline(always)]
    pub const fn has_prefix(&self) -> bool {
        use Opcode::*;
        match self.1 {
            Addressing::Relative16 => true,
            _ => match self.0 {
                Ld16(Reg16::Y) | St16(Reg16::Y) | Ld16(Reg16::S) | St16(Reg16::S) |
                Cmp16(Reg16::D) | Cmp16(Reg16::Y) | Cmp16(Reg16::U) | Cmp16(Reg16::S) |
                Swi2 | Swi3 => true,
                _ => false,
            },
        }
    }
    #[inline(always)]
    pub const fn opcode(&self) -> Opcode {
        self.0
    }
    #[inline(always)]
    pub const fn addressing(&self) -> Addressing {
        self.1
    }
}
