

enum Version {
    M68000,
}

#[derive(Debug, Clone, Eq, PartialEq)]
enum Size {
    /// 8 bits, 1 byte
    Byte,
    /// 16 bits, 2 bytes
    Word,
    /// 32 bits, 4 bytes
    Long,
}

impl Size {
    fn from_bit(bit: bool) -> Self {
        match bit {
            true => Self::Long,
            false => Self::Word,
        }
    }
    fn from_bits(v: u8) -> Option<Self> {
        match v {
            0 => Some(Self::Byte),
            1 => Some(Self::Word),
            2 => Some(Self::Long),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
enum Addressing {
    /// The address is stored in a data register
    DReg(u8),
    /// The address is stored in an address register
    AReg(u8),
    /// The pointer to an address is stored in address register
    ARegIndirect(u8),
    AddressPostincrement(u8),
    AddressPredecrement(u8),
    AddressWithDisplacement(u8),
    AddressWithIndex(u8),
    PcDisplacement,
    PcIndex,
    AbsoluteShort,
    AbsoluteLong,
    Immediate,
}

impl Addressing {
    fn from_mx(mx: u8) -> Option<Self> {
        let x = mx & 0o07;
        let m = (mx & 0o70) >> 3;
        println!("mx {:x?} x {:x?} m {:x?}", mx, x, m);
        match m {
            0 => Some(Self::DReg(x)),
            1 => Some(Self::AReg(x)),
            2 => Some(Self::ARegIndirect(x)),
            3 => Some(Self::AddressPostincrement(x)),
            4 => Some(Self::AddressPredecrement(x)),
            5 => Some(Self::AddressWithDisplacement(x)),
            6 => Some(Self::AddressWithIndex(x)),
            7 => match x {
                0 => Some(Self::AbsoluteShort),
                1 => Some(Self::AbsoluteLong),
                2 => Some(Self::PcDisplacement),
                3 => Some(Self::PcIndex),
                4 => Some(Self::Immediate),
                5..8 => None,
                _ => unreachable!(),
            },
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
struct AReg(u8);

impl AReg {
    fn from_bits(b0: bool, b1: bool, b2: bool) -> Self {
        Self(b0 as u8 * 0x04 | b1 as u8 * 0x2 | b2 as u8)
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
enum Opcode {
    ATrap(u16),
    FTrap(u16),
    Addi(Size, Addressing),
    Adda(Size, AReg, Addressing),
}

trait OpcodeFields: Copy {
    fn msb4(self) -> u8;
    fn ea(self) -> u8;
    fn opmode(self) -> u8;
    fn areg(self) -> AReg { AReg(self.reg()) }
    fn reg(self) -> u8 { self.oct0() }
    fn oct0(self) -> u8;
    fn oct1(self) -> u8;
    fn oct1_bit0(self) -> bool { self.oct1() & 0b100 != 0 }
}

impl OpcodeFields for u16 {
    fn msb4(self) -> u8 { (self >> 12) as u8 }
    fn ea(self) -> u8 { (self & 0o77) as u8 }
    fn opmode(self) -> u8 { (self >> 6 & 0o07) as u8 }
    fn oct0(self) -> u8 { (self >> 9 & 0o7) as u8 }
    fn oct1(self) -> u8 { (self >> 6 & 0o7) as u8 }
}

impl Opcode {
    fn decode(data: &[u8]) -> Option<Self> {
        use Opcode::*;
        //let b = split_into_array(u16::from_be_bytes([data[0], data[1]]));
        let b = u16::from_be_bytes([data[0], data[1]]);
        println!("{:x?} {:x?}", b.msb4(), b);

        Some(match b.msb4() {
            0x0 => {
                if b.oct1_bit0() {
                    todo!("bit instructins");
                } else {
                    match b.oct0() {
                        0b011 => Addi(Size::from_bits(b.oct1())?, Addressing::from_mx(b.ea())?),
                        _ => todo!("{:x} octet", b.oct0()),
                    }
                }
            },
            0xd => {
                match dbg!(b.opmode()) {
                    0b011 => Self::Adda(Size::Word, b.areg(), Addressing::from_mx(b.ea())?),
                    0b111 => Self::Adda(Size::Long, b.areg(), Addressing::from_mx(b.ea())?),
                    _ => None?,
                }
            },
            _ => todo!(),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::{Opcode, Size, AReg, Addressing};
    #[test]
    fn adda() {

        assert_eq!(Opcode::decode(&[0xd0, 0xfc]), Some(Opcode::Adda(Size::Word, AReg(0), Addressing::Immediate)));
        assert_eq!(Opcode::decode(&[0xd4, 0xf8]), Some(Opcode::Adda(Size::Word, AReg(2), Addressing::AbsoluteShort)));
        assert_eq!(Opcode::decode(&[0xd7, 0xf9]), Some(Opcode::Adda(Size::Long, AReg(3), Addressing::AbsoluteLong)));
        assert_eq!(Opcode::decode(&[0xd4, 0xc0]), Some(Opcode::Adda(Size::Word, AReg(2), Addressing::DReg(0))));
        assert_eq!(Opcode::decode(&[0xd2, 0xd0]), Some(Opcode::Adda(Size::Word, AReg(1), Addressing::ARegIndirect(0))));
        assert_eq!(Opcode::decode(&[0xd2, 0xe8]), Some(Opcode::Adda(Size::Word, AReg(1), Addressing::AddressWithDisplacement(0))));
        assert_eq!(Opcode::decode(&[0xd2, 0xd8]), Some(Opcode::Adda(Size::Word, AReg(1), Addressing::AddressPostincrement(0))));
        
        assert_eq!(Opcode::decode(&[0xd2, 0xe0]), Some(Opcode::Adda(Size::Word, AReg(1), Addressing::AddressPredecrement(0))));
        assert_eq!(Opcode::decode(&[0xd8, 0xfc]), Some(Opcode::Adda(Size::Word, AReg(4), Addressing::Immediate)));
        assert_eq!(Opcode::decode(&[0xd6, 0xf3, 0x10, 0x00]), Some(Opcode::Adda(Size::Word, AReg(3), Addressing::AddressWithIndex(3))));
        assert_eq!(Opcode::decode(&[0xd6, 0xf3, 0x40, 0x24]), Some(Opcode::Adda(Size::Word, AReg(3), Addressing::AddressWithIndex(3))));
    }
    #[test]
    fn addi() {
        assert_eq!(Opcode::decode(&[0x06, 0x01]), Some(Opcode::Addi(Size::Byte, Addressing::DReg(1))));
        assert_eq!(Opcode::decode(&[0x06, 0x80]), Some(Opcode::Addi(Size::Long, Addressing::DReg(0))));
        assert_eq!(Opcode::decode(&[0x06, 0x78]), Some(Opcode::Addi(Size::Word, Addressing::AbsoluteShort)));
        assert_eq!(Opcode::decode(&[0x06, 0x79]), Some(Opcode::Addi(Size::Word, Addressing::AbsoluteLong)));
        assert_eq!(Opcode::decode(&[0x06, 0xb8]), Some(Opcode::Addi(Size::Long, Addressing::AbsoluteShort)));
        assert_eq!(Opcode::decode(&[0x06, 0x11]), Some(Opcode::Addi(Size::Byte, Addressing::ARegIndirect(1))));
    }
}
