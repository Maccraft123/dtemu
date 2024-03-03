use bitfield_struct::bitfield;
use super::Addressing;

#[bitfield(u8)]
struct RawOpcode {
    #[bits(2)]
    c: u8,
    #[bits(3)]
    b: u8,
    #[bits(3)]
    a: u8,
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

#[derive(Debug, Clone, Copy)]
pub struct DatalessInstruction(Opcode, Addressing);

impl DatalessInstruction {
    pub fn opcode(&self) -> Opcode { self.0 }
    pub fn addressing(&self) -> Addressing { self.1 }
    pub fn len(&self) -> u16 {
        use Opcode::*;
        use super::Addressing::*;
        //match self.0 {
            //Lda | Ldx | Ldy | Sta | Stx | Sty | Tax | Tay | Tsx | Txa | Txs | Tya |
                //Adc | Sbc |
                //And | Eor | Ora |
                //Asl | Lsr |
                //Cmp |
                //Bcc | Bcs | Beq | Bmi | Bne | Bpl | Bvc | Bvs |
                //Jmp | Jsr |
                //Bit =>
                match self.1 {
                    Absolute | AbsoluteX | AbsoluteY => 3,
                    Relative | Immediate | ZeroPage | ZeroPageX | ZeroPageY => 2,
                    Indirect | IndirectX | IndirectY => 2,
                    Accumulator | Implied => 1,
                }
            //Pha | Pla |
            //    Dex | Dey | Inx | Iny |
            //    Clc | Cld | Cli | Clv | Sec | Sed | Sei |
            //    Rts | 
            //    Brk | Rti |
            //    Nop
            //    => 1,
        //}
    }
}

fn di(opcode: Opcode, addressing: Addressing) -> DatalessInstruction {
    DatalessInstruction(opcode, addressing)
}

pub fn decode_u8(val: u8) -> DatalessInstruction {
    use Opcode::*;
    use super::Addressing::*;
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
        other => panic!("unknown/invalid opcode {:?}/{:x}", other, val),
    }
}
