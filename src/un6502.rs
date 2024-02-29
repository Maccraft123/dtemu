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
    Tax,
    Tay,
    Txs,
    Jmp,
    Brk,
}

#[derive(Debug, Clone, Copy)]
pub struct DatalessInstruction(Opcode, Addressing);

impl DatalessInstruction {
    pub fn opcode(&self) -> Opcode { self.0 }
    pub fn addressing(&self) -> Addressing { self.1 }
    pub fn len(&self) -> u16 {
        use Opcode::*;
        use super::Addressing::*;
        match self.0 {
            Lda | Ldx | Ldy => match self.1 {
                Absolute | AbsoluteX | AbsoluteY => 3,
                Immediate | ZeroPage | ZeroPageX | IndirectX | IndirectY => 2,
                other => panic!("invalid addressing {:?} for {:?}", other, self.0),
            },
            Jmp => 3,
            Brk | Tax | Tay | Txs => 1,
        }
    }
}

fn di(opcode: Opcode, addressing: Addressing) -> DatalessInstruction {
    DatalessInstruction(opcode, addressing)
}

pub fn decode_u8(val: u8) -> DatalessInstruction {
    use Opcode::*;
    use super::Addressing::*;

    let raw = RawOpcode::from(val);

    println!("{:x} is {} {} {}", val, raw.a(), raw.b(), raw.c());
    match (raw.a(), raw.b(), raw.c()) {
        (0, 0, 0) => di(Brk, Implied),
        //(1, 0, 0) => di(Jsr, Implied),
        //(2, 0, 0) => di(Rti, Implied),
        //(3, 0, 0) => di(Rts, Implied),
        //(4 0, 0) => ,
        //(5, 0, 0) => di(Ldy, Immediate),
        //(6, 0, 0) => di(Cpy, Immediate),
        //(7, 0, 0) => di(Cpx, Immediate),
        //(0, 1, 0) => ,
        //(1, 1, 0) => di(Bit, ZeroPage),
        ////(2, 1, 0)
        //(3, 1, 0)
        //(4, 1, 0) => di(Sty, ZeroPage),
        //(5, 1, 0) => di(Ldy, ZeroPage),
        //(6, 1, 0) => di(Cpy, ZeroPage),
        //(7, 1, 0) => di(Cpx, ZeroPage),
        //(0, 2, 0) => di(Php, Implied),
        //(1, 2, 0) => di(Plp, Implied),
        //(2, 2, 0) => di(Pha, Implied),
        
        (4, 6, 2) => di(Txs, Implied),

        (5, 2, 0) => di(Tay, Implied),

        (5, 0, 1) => di(Lda, IndirectX),
        (5, 1, 1) => di(Lda, ZeroPage),
        (5, 2, 1) => di(Lda, Immediate),
        (5, 3, 1) => di(Lda, Absolute),
        (5, 4, 1) => di(Lda, IndirectY),
        (5, 5, 1) => di(Lda, ZeroPageX),
        (5, 6, 1) => di(Lda, AbsoluteY),
        (5, 7, 1) => di(Lda, AbsoluteX),

        (5, 0, 2) => di(Ldx, Immediate),
        (5, 1, 2) => di(Ldx, ZeroPage),
        (5, 2, 2) => di(Tax, Implied),
        (5, 3, 2) => di(Ldx, Absolute),
        //(5, 4, 2) => di(JamB2, Implied),
        (5, 5, 2) => di(Ldx, ZeroPageY),
        //(5, 5, 2) => di(Tsx, Implied),
        (5, 6, 2) => di(Ldx, AbsoluteY),

        (5, 0, 0) => di(Ldy, Immediate),
        (5, 1, 0) => di(Ldy, ZeroPage),
        //(5, 2, 0) => di(Tay, Implied),
        (5, 3, 0) => di(Ldy, Absolute),
        //(5, 4, 0) => di(Bcs, Relative),
        (5, 5, 0) => di(Ldy, ZeroPageX),
        //(5, 6, 0) => di(Clv, Implied),
        (5, 6, 0) => di(Ldy, AbsoluteX),

        (2, 3, 0) => di(Jmp, Absolute),
        other => panic!("unknown/invalid opcode {:?}/{:x}", other, val),
    }
}
