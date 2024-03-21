use bitfield_struct::bitfield;
use std::fmt;

#[bitfield(u8)]
pub struct RawOpcode {
    #[bits(2)]
    c: u8,
    #[bits(3)]
    b: u8,
    #[bits(3)]
    a: u8,
}

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

#[derive(Debug, Clone, Copy)]
pub enum Opcode {
    Lda,
    Ldx,
    Ldy,
    Sta,
    Stx,
    Sty,
    Tax,
    Tay,
    Txs,
    Txa,
    Tya,
    Dey,
    Bcc,
    Jmp,
    Brk,
    Pha,
    Pla,
    Jsr,
    Cld,
    Sei,
    Rti,
    Asl,
    Lsr,
    Bit,
    Bne,
    Beq,
    Inx,
    Ora,
    And,
    Eor,
    Adc,
    Cmp,
    Sbc,
    Rts,
    Bvs,
    Bvc,
    Bpl,
    Bmi,
    Bcs,
    Sed,
    Sec,
    Clv,
    Cli,
    Clc,
    Iny,
    Dex,
    Tsx,
    Nop,
    Rol,
    Ror,
    Cpx,
    Cpy,
    Plp,
    Php,
    Dec,
    Inc,
}

#[derive(Copy, Clone, Debug)]
pub enum Operand {
    U8(u8),
    U16(u16),
}

impl Operand {
    fn u8(self) -> Option<u8> {
        match self {
            Operand::U8(v) => Some(v),
            _ => None,
        }
    }
    fn u16(self) -> Option<u16> {
        match self {
            Operand::U16(v) => Some(v),
            _ => None,
        }
    }
}

pub struct Instruction {
    opcode: Opcode,
    addressing: Addressing,
    operand: Option<Operand>,
    addr: Option<u16>,
}

#[inline]
pub fn disasm(bytes: &[u8], addr: Option<u16>) -> Instruction {
    let dataless = decode_u8(bytes[0]);
    match dataless.len() {
        1 => Instruction {
            opcode: dataless.opcode(),
            addressing: dataless.addressing(),
            operand: None,
            addr,
        },
        2 => Instruction {
            opcode: dataless.opcode(),
            addressing: dataless.addressing(),
            operand: Some(Operand::U8(bytes[1])),
            addr,
        },
        3 => Instruction {
            opcode: dataless.opcode(),
            addressing: dataless.addressing(),
            operand: Some(Operand::U16(bytes[1] as u16 | (bytes[2] as u16) << 8)),
            addr,
        },
        _ => unreachable!(),
    }
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Addressing::*;
        let opcode = format!("{:?}", self.opcode).to_uppercase();
        let op8 = self.operand.map(|v| v.u8().unwrap_or(0)).unwrap_or(0);
        let op16 = self.operand.map(|v| v.u16().unwrap_or(0)).unwrap_or(0);

        let operand = match self.addressing {
            Immediate => format!("#${:02x?}", op8),
            Absolute => format!("${:04x}", op16),
            ZeroPage => format!("${:02x}", op8),
            AbsoluteX => format!("${:04x},X", op16),
            AbsoluteY => format!("${:04x},Y", op16),
            ZeroPageX => format!("${:02x},X", op8),
            ZeroPageY => format!("${:02x},Y", op8),
            Indirect => format!("(${:04x})", op16),
            IndirectX => format!("(${:02x},X)", op8),
            IndirectY => format!("(${:02x}),Y", op8),
            Relative => {
                if let Some(addr) = self.addr {
                    // need to cast first to i8 then to i16 because otherwise it won't catch
                    // negative values properly
                    format!("${:x}", (addr as i16).wrapping_add(i16::from_le_bytes([op8, 0x0])))
                } else {
                    // custom syntax for disassembling branch instructions with unknown
                    // instruction address, and also rust can't do signed hex format
                    if op8 == 0 {
                        format!("$00") // what?
                    } else if op8 & 0x80 != 0 { // negative
                        format!("$-{:02x}", !(op8.wrapping_sub(1)))
                    } else if op8 & 0x80 == 0 { // positive
                        format!("$+{:02x}", op8)
                    } else {
                        unreachable!()
                    }
                }
            },
            Accumulator => "A".into(),
            Implied => "".into(),
        };


        f.write_str(&opcode)?;
        if operand != "" {
            f.write_str(" ")?;
            f.write_str(&operand)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, Copy)]
pub struct DatalessInstruction(Opcode, Addressing);

impl DatalessInstruction {
    pub fn opcode(&self) -> Opcode { self.0 }
    pub fn addressing(&self) -> Addressing { self.1 }
    pub fn len(&self) -> u16 {
        use Addressing::*;
        match self.1 {
            Indirect | Absolute | AbsoluteX | AbsoluteY => 3,
            Relative | Immediate | ZeroPage | ZeroPageX | ZeroPageY => 2,
            IndirectX | IndirectY => 2,
            Accumulator | Implied => 1,
        }
    }
}

fn di(opcode: Opcode, addressing: Addressing) -> DatalessInstruction {
    DatalessInstruction(opcode, addressing)
}

#[inline]
pub fn decode_u8(val: u8) -> DatalessInstruction {
    use Opcode::*;
    use Addressing::*;
    let raw = RawOpcode::from(val);
    match (raw.a(), raw.b(), raw.c()) {
        (a, b, 0) => {
            let opcode = match (a, b) {
                (0, 0) => Brk,
                (0, 2) => Php,
                (0, 4) => Bpl,
                (0, 6) => Clc,
                
                (1, 0) => Jsr,
                (1, 1) | (1, 3) => Bit,
                (1, 2) => Plp,
                (1, 4) => Bmi,
                (1, 6) => Sec,
                
                (2, 0) => Rti,
                (2, 2) => Pha,
                (2, 3) => Jmp,
                (2, 4) => Bvc,
                (2, 6) => Cli,

                (3, 0) => Rts,
                (3, 2) => Pla,
                (3, 3) => Jmp,
                (3, 4) => Bvs,
                (3, 6) => Sei,

                (4, _) if b % 2 != 0 => Sty,
                (4, 2) => Dey,
                (4, 4) => Bcc,
                (4, 6) => Tya,


                (5, 0) => Ldy,
                (5, _) if b % 2 != 0 => Ldy,
                (5, 2) => Tay,
                (5, 4) => Bcs,
                (5, 6) => Clv,

                (6, 0) | (6, 1) | (6, 3) => Cpy,
                (6, 2) => Iny,
                (6, 4) => Bne,
                (6, 6) => Cld,

                (7, 0) | (7, 1) | (7, 3) => Cpx,
                (7, 2) => Inx,
                (7, 4) => Beq,
                (7, 6) => Sed,

                other => unimplemented!("missing {:?} for c == 0", other),
            };
            let addressing = match b {
                0 if a == 1 => Absolute,
                0 if a >= 4 => Immediate,
                0 => Implied,
                1 => ZeroPage,
                2 => Implied,
                3 if a == 3 => Indirect,
                3 => Absolute,
                4 => Relative,
                5 => ZeroPageX,
                6 => Implied,
                7 => AbsoluteX,
                _ => unreachable!(),
            };
            di(opcode, addressing)
        },

        (a, b, 1) => {
            let opcode = match a {
                0 => Ora,
                1 => And,
                2 => Eor,
                3 => Adc,
                4 => Sta,
                5 => Lda,
                6 => Cmp,
                7 => Sbc,
                _ => unreachable!(),
            };
            let addressing = match b {
                0 => IndirectX,
                1 => ZeroPage,
                2 => Immediate,
                3 => Absolute,
                4 => IndirectY,
                5 => ZeroPageX,
                6 => AbsoluteY,
                7 => AbsoluteX,
                _ => unreachable!(),
            };
            di(opcode, addressing)
        },

        (a, b, 2) if a < 4 => {
            let opcode = match a {
                0 => Asl,
                1 => Rol,
                2 => Lsr,
                3 => Ror,
                _ => unreachable!(),
            };
            let addressing = match b {
                0 => Immediate,
                1 => ZeroPage,
                2 => Accumulator,
                3 => Absolute,
                4 => unimplemented!(),
                5 => ZeroPageX,
                6 => unimplemented!(),
                7 => AbsoluteX,
                _ => unreachable!(),
            };
            di(opcode, addressing)
        },
        (a, b, 2) if a >= 4 => {
            let opcode = match (a, b) {
                (4, _) if b % 2 != 0 => Stx,
                (4, 2) => Txa,
                (4, 6) => Txs,
                (5, 2) => Tax,
                (5, 6) => Tsx,
                (5, _) => Ldx,
                (6, 2) => Dex,
                (6, _) => Dec,
                (7, 2) => Nop,
                (7, _) => Inc,
                other => unimplemented!("missing {:?} for c == 2", other),
            };
            let addressing = match b {
                0 => Immediate,
                1 => ZeroPage,
                2 => Implied,
                3 => Absolute,
                4 => unimplemented!(),
                5 if a == 4 || a == 5 => ZeroPageY,
                5 if a == 6 || a == 7 => ZeroPageX,
                6 => Implied,
                7 if a == 4 || a == 5 => AbsoluteY,
                7 if a == 6 || a == 7 => AbsoluteX,
                _ => unreachable!(),
            };
            di(opcode, addressing)
        }
        (7, 2, 3) => di(Sbc, Immediate),
        other => panic!("unknown/invalid opcode {:?}/{:x}", other, val),
    }
}


#[cfg(test)]
mod tests {
    fn dis(bytes: &[u8]) -> String {
        crate::disasm(bytes, None).to_string()
    }
    fn dis_addr(bytes: &[u8], addr: u16) -> String {
        crate::disasm(bytes, Some(addr)).to_string()
    }
    #[test]
    fn lda() {
        assert_eq!("LDA #$21",      dis(&[0xa9, 0x21]));
        assert_eq!("LDA $55",       dis(&[0xa5, 0x55]));
        assert_eq!("LDA $44,X",     dis(&[0xb5, 0x44]));
        assert_eq!("LDA $aa55",     dis(&[0xad, 0x55, 0xaa]));
        assert_eq!("LDA $aa55,X",   dis(&[0xbd, 0x55, 0xaa]));
        assert_eq!("LDA $aa55,Y",   dis(&[0xb9, 0x55, 0xaa]));
        assert_eq!("LDA ($22,X)",   dis(&[0xa1, 0x22]));
        assert_eq!("LDA ($11),Y",   dis(&[0xb1, 0x11]));
    }
    #[test]
    fn ldx() {
        assert_eq!("LDX #$21",      dis(&[0xa2, 0x21]));
        assert_eq!("LDX $55",       dis(&[0xa6, 0x55]));
        assert_eq!("LDX $44,Y",     dis(&[0xb6, 0x44]));
        assert_eq!("LDX $aa55",     dis(&[0xae, 0x55, 0xaa]));
        assert_eq!("LDX $aa55,Y",   dis(&[0xbe, 0x55, 0xaa]));
    }
    #[test]
    fn ldy() {
        assert_eq!("LDY #$21",      dis(&[0xa0, 0x21]));
        assert_eq!("LDY $55",       dis(&[0xa4, 0x55]));
        assert_eq!("LDY $44,X",     dis(&[0xb4, 0x44]));
        assert_eq!("LDY $aa55",     dis(&[0xac, 0x55, 0xaa]));
        assert_eq!("LDY $aa55,X",   dis(&[0xbc, 0x55, 0xaa]));
    }
    #[test]
    fn sta() {
        assert_eq!("STA $55",       dis(&[0x85, 0x55]));
        assert_eq!("STA $44,X",     dis(&[0x95, 0x44]));
        assert_eq!("STA $aa55",     dis(&[0x8d, 0x55, 0xaa]));
        assert_eq!("STA $aa55,X",   dis(&[0x9d, 0x55, 0xaa]));
        assert_eq!("STA $aa55,Y",   dis(&[0x99, 0x55, 0xaa]));
        assert_eq!("STA ($22,X)",   dis(&[0x81, 0x22]));
        assert_eq!("STA ($11),Y",   dis(&[0x91, 0x11]));
    }
    #[test]
    fn stx() {
        assert_eq!("STX $55",       dis(&[0x86, 0x55]));
        assert_eq!("STX $44,Y",     dis(&[0x96, 0x44]));
        assert_eq!("STX $aa55",     dis(&[0x8e, 0x55, 0xaa]));
    }
    #[test]
    fn sty() {
        assert_eq!("STY $55",       dis(&[0x84, 0x55]));
        assert_eq!("STY $44,X",     dis(&[0x94, 0x44]));
        assert_eq!("STY $aa55",     dis(&[0x8c, 0x55, 0xaa]));
    }
    #[test]
    fn transfer() {
        assert_eq!("TAX",           dis(&[0xaa]));
        assert_eq!("TAY",           dis(&[0xa8]));
        assert_eq!("TSX",           dis(&[0xba]));
        assert_eq!("TXA",           dis(&[0x8a]));
        assert_eq!("TXS",           dis(&[0x9a]));
        assert_eq!("TYA",           dis(&[0x98]));
    }
    #[test]
    fn stack_instructions() {
        assert_eq!("PHA",           dis(&[0x48]));
        assert_eq!("PHP",           dis(&[0x08]));
        assert_eq!("PLA",           dis(&[0x68]));
        assert_eq!("PLP",           dis(&[0x28]));
    }
    #[test]
    fn decrements() {
        assert_eq!("DEC $21",       dis(&[0xc6, 0x21]));
        assert_eq!("DEC $44,X",     dis(&[0xd6, 0x44]));
        assert_eq!("DEC $aa55",     dis(&[0xce, 0x55, 0xaa]));
        assert_eq!("DEC $aa55,X",   dis(&[0xde, 0x55, 0xaa]));
        assert_eq!("DEX",           dis(&[0xca]));
        assert_eq!("DEY",           dis(&[0x88]));
    }
    #[test]
    fn increments() {
        assert_eq!("INC $21",       dis(&[0xe6, 0x21]));
        assert_eq!("INC $44,X",     dis(&[0xf6, 0x44]));
        assert_eq!("INC $aa55",     dis(&[0xee, 0x55, 0xaa]));
        assert_eq!("INC $aa55,X",   dis(&[0xfe, 0x55, 0xaa]));
        assert_eq!("INX",           dis(&[0xe8]));
        assert_eq!("INY",           dis(&[0xc8]));
    }
    #[test]
    fn adc() {
        assert_eq!("ADC #$21",      dis(&[0x69, 0x21]));
        assert_eq!("ADC $55",       dis(&[0x65, 0x55]));
        assert_eq!("ADC $44,X",     dis(&[0x75, 0x44]));
        assert_eq!("ADC $aa55",     dis(&[0x6d, 0x55, 0xaa]));
        assert_eq!("ADC $aa55,X",   dis(&[0x7d, 0x55, 0xaa]));
        assert_eq!("ADC $aa55,Y",   dis(&[0x79, 0x55, 0xaa]));
        assert_eq!("ADC ($22,X)",   dis(&[0x61, 0x22]));
        assert_eq!("ADC ($11),Y",   dis(&[0x71, 0x11]));
    }
    #[test]
    fn sbc() {
        assert_eq!("SBC #$21",      dis(&[0xe9, 0x21]));
        assert_eq!("SBC $55",       dis(&[0xe5, 0x55]));
        assert_eq!("SBC $44,X",     dis(&[0xf5, 0x44]));
        assert_eq!("SBC $aa55",     dis(&[0xed, 0x55, 0xaa]));
        assert_eq!("SBC $aa55,X",   dis(&[0xfd, 0x55, 0xaa]));
        assert_eq!("SBC $aa55,Y",   dis(&[0xf9, 0x55, 0xaa]));
        assert_eq!("SBC ($22,X)",   dis(&[0xe1, 0x22]));
        assert_eq!("SBC ($11),Y",   dis(&[0xf1, 0x11]));
    }
    #[test]
    fn and() {
        assert_eq!("AND #$21",      dis(&[0x29, 0x21]));
        assert_eq!("AND $55",       dis(&[0x25, 0x55]));
        assert_eq!("AND $44,X",     dis(&[0x35, 0x44]));
        assert_eq!("AND $aa55",     dis(&[0x2d, 0x55, 0xaa]));
        assert_eq!("AND $aa55,X",   dis(&[0x3d, 0x55, 0xaa]));
        assert_eq!("AND $aa55,Y",   dis(&[0x39, 0x55, 0xaa]));
        assert_eq!("AND ($22,X)",   dis(&[0x21, 0x22]));
        assert_eq!("AND ($11),Y",   dis(&[0x31, 0x11]));
    }
    #[test]
    fn eor() {
        assert_eq!("EOR #$21",      dis(&[0x49, 0x21]));
        assert_eq!("EOR $55",       dis(&[0x45, 0x55]));
        assert_eq!("EOR $44,X",     dis(&[0x55, 0x44]));
        assert_eq!("EOR $aa55",     dis(&[0x4d, 0x55, 0xaa]));
        assert_eq!("EOR $aa55,X",   dis(&[0x5d, 0x55, 0xaa]));
        assert_eq!("EOR $aa55,Y",   dis(&[0x59, 0x55, 0xaa]));
        assert_eq!("EOR ($22,X)",   dis(&[0x41, 0x22]));
        assert_eq!("EOR ($11),Y",   dis(&[0x51, 0x11]));
    }
    #[test]
    fn ora() {
        assert_eq!("ORA #$21",      dis(&[0x09, 0x21]));
        assert_eq!("ORA $55",       dis(&[0x05, 0x55]));
        assert_eq!("ORA $44,X",     dis(&[0x15, 0x44]));
        assert_eq!("ORA $aa55",     dis(&[0x0d, 0x55, 0xaa]));
        assert_eq!("ORA $aa55,X",   dis(&[0x1d, 0x55, 0xaa]));
        assert_eq!("ORA $aa55,Y",   dis(&[0x19, 0x55, 0xaa]));
        assert_eq!("ORA ($22,X)",   dis(&[0x01, 0x22]));
        assert_eq!("ORA ($11),Y",   dis(&[0x11, 0x11]));
    }
    #[test]
    fn asl() {
        assert_eq!("ASL A",         dis(&[0x0a]));
        assert_eq!("ASL $21",       dis(&[0x06, 0x21]));
        assert_eq!("ASL $44,X",     dis(&[0x16, 0x44]));
        assert_eq!("ASL $aa55",     dis(&[0x0e, 0x55, 0xaa]));
        assert_eq!("ASL $aa55,X",   dis(&[0x1e, 0x55, 0xaa]));
    }
    #[test]
    fn lsr() {
        assert_eq!("LSR A",         dis(&[0x4a]));
        assert_eq!("LSR $21",       dis(&[0x46, 0x21]));
        assert_eq!("LSR $44,X",     dis(&[0x56, 0x44]));
        assert_eq!("LSR $aa55",     dis(&[0x4e, 0x55, 0xaa]));
        assert_eq!("LSR $aa55,X",   dis(&[0x5e, 0x55, 0xaa]));
    }
    #[test]
    fn rol() {
        assert_eq!("ROL A",         dis(&[0x2a]));
        assert_eq!("ROL $21",       dis(&[0x26, 0x21]));
        assert_eq!("ROL $44,X",     dis(&[0x36, 0x44]));
        assert_eq!("ROL $aa55",     dis(&[0x2e, 0x55, 0xaa]));
        assert_eq!("ROL $aa55,X",   dis(&[0x3e, 0x55, 0xaa]));
    }
    #[test]
    fn ror() {
        assert_eq!("ROR A",         dis(&[0x6a]));
        assert_eq!("ROR $21",       dis(&[0x66, 0x21]));
        assert_eq!("ROR $44,X",     dis(&[0x76, 0x44]));
        assert_eq!("ROR $aa55",     dis(&[0x6e, 0x55, 0xaa]));
        assert_eq!("ROR $aa55,X",   dis(&[0x7e, 0x55, 0xaa]));
    }
    #[test]
    fn flag_instructions() {
        assert_eq!("CLC",           dis(&[0x18]));
        assert_eq!("CLD",           dis(&[0xd8]));
        assert_eq!("CLI",           dis(&[0x58]));
        assert_eq!("CLV",           dis(&[0xb8]));
        assert_eq!("SEC",           dis(&[0x38]));
        assert_eq!("SED",           dis(&[0xf8]));
        assert_eq!("SEI",           dis(&[0x78]));
    }
    #[test]
    fn comparisons() {
        assert_eq!("CMP #$21",      dis(&[0xc9, 0x21]));
        assert_eq!("CMP $55",       dis(&[0xc5, 0x55]));
        assert_eq!("CMP $44,X",     dis(&[0xd5, 0x44]));
        assert_eq!("CMP $aa55",     dis(&[0xcd, 0x55, 0xaa]));
        assert_eq!("CMP $aa55,X",   dis(&[0xdd, 0x55, 0xaa]));
        assert_eq!("CMP $aa55,Y",   dis(&[0xd9, 0x55, 0xaa]));
        assert_eq!("CMP ($22,X)",   dis(&[0xc1, 0x22]));
        assert_eq!("CMP ($11),Y",   dis(&[0xd1, 0x11]));

        assert_eq!("CPX #$21",      dis(&[0xe0, 0x21]));
        assert_eq!("CPX $21",       dis(&[0xe4, 0x21]));
        assert_eq!("CPX $aa55",     dis(&[0xec, 0x55, 0xaa]));

        assert_eq!("CPY #$21",      dis(&[0xc0, 0x21]));
        assert_eq!("CPY $21",       dis(&[0xc4, 0x21]));
        assert_eq!("CPY $aa55",     dis(&[0xcc, 0x55, 0xaa]));
    }
    #[test]
    fn branch() {
        // ok we can be custom but we have to maintain consistency regardless
        assert_eq!("BCC $+55",      dis(&[0x90, 0x55]));
        assert_eq!("BCC $-55",      dis(&[0x90, 0xab]));


        assert_eq!("BCC $1010",     dis_addr(&[0x90, 0x10], 0x1000));
        assert_eq!("BCS $1010",     dis_addr(&[0xb0, 0x10], 0x1000));
        assert_eq!("BEQ $1010",     dis_addr(&[0xf0, 0x10], 0x1000));
        assert_eq!("BMI $1010",     dis_addr(&[0x30, 0x10], 0x1000));
        assert_eq!("BNE $1010",     dis_addr(&[0xd0, 0x10], 0x1000));
        assert_eq!("BPL $1010",     dis_addr(&[0x10, 0x10], 0x1000));
        assert_eq!("BVC $1010",     dis_addr(&[0x50, 0x10], 0x1000));
        assert_eq!("BVS $1010",     dis_addr(&[0x70, 0x10], 0x1000));
    }
    #[test]
    fn jumps() {
        assert_eq!("JMP $aa55",     dis(&[0x4c, 0x55, 0xaa]));
        assert_eq!("JMP ($aa55)",   dis(&[0x6c, 0x55, 0xaa]));
        assert_eq!("JSR $aa55",     dis(&[0x20, 0x55, 0xaa]));
        assert_eq!("RTS",           dis(&[0x60]));
    }
    #[test]
    fn interrupts() {
        assert_eq!("BRK",           dis(&[0x00]));
        assert_eq!("RTI",           dis(&[0x40]));
    }
    #[test]
    fn others() {
        assert_eq!("BIT $22",       dis(&[0x24, 0x22]));
        assert_eq!("BIT $aa55",     dis(&[0x2c, 0x55, 0xaa]));
        assert_eq!("NOP",           dis(&[0xea]));
    }
    // one day...
    //#[test]
    //fn disasm_coverage() {
    //    for x in 0x00..=0xff {
    //        dis(&[x, 0, 0, 0, 0]);
    //    }
    //}
}
