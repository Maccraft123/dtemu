use crate::cpu_prelude::*;
use asm_playground::i8080::{Instruction, Instruction::*, Condition};
pub use asm_playground::i8080::{Reg, RegPair};
use core::mem;

#[derive(Clone, Debug)]
pub struct Intel8085 {
    pc: u16,
    psw: (u8, Flags),
    bc: TwoBytes,
    de: TwoBytes,
    hl: TwoBytes,
    sp: u16,
}

#[bitfield(u8, order = Msb)]
struct Flags {
    sign: bool,
    zero: bool,
    k: bool,
    half_carry: bool,
    unused_zero: bool,
    parity: bool,
    signed_overflow: bool,
    carry: bool,
}

impl Flags {
    fn to_8080(&self) -> u8 {
        const MASK: u8 = 0xd5;
        (self.into_bits() & MASK) | 0x2
    }
}

impl Cpu for Intel8085 {
    type AddressWidth = u16;
    #[inline]
    fn new() -> Self {
        Self {
            pc: 0x100,
            sp: 0x500,
            psw: (0, Flags::from(0x02)),
            bc: TwoBytes::zeroed(),
            de: TwoBytes::zeroed(),
            hl: TwoBytes::zeroed(),
        }
    }
    #[inline]
    fn step_instruction(&mut self, m: &mut impl Bus<u16>) -> impl std::future::Future<Output = Self::AddressWidth> + Send {
        async {
            let bytes = [m.read8(self.pc), m.read8(self.pc+1), m.read8(self.pc+2)];
            let inst = Instruction::decode_from(&bytes);
            println!("I: {:x?} PC: {:04x} A: {:02x} BC: {:04x} DE: {:04x} F: {:02x}",
                inst, self.pc, self.psw.0, self.bc.word(), self.de.word(), self.psw.1.to_8080());
            self.pc += inst.len() as u16;
            match inst {
                Jmp(addr) => self.pc = addr,
                Rst(addr) => self.pc = addr as u16,
                J(cond, addr) => {
                    if self.has_cond(cond) {
                        self.pc = addr;
                    }
                },
                Call(addr) => {
                    self.push16(m, self.pc).await;
                    self.pc = addr;
                },
                C(cond, addr) => {
                    if self.has_cond(cond) {
                        self.push16(m, self.pc).await;
                        self.pc = addr;
                    }
                },
                Ret => self.pc = self.pop16(m).await,
                R(cond) => {
                    if self.has_cond(cond) {
                        self.pc = self.pop16(m).await;
                    }
                },
                Xchg => mem::swap(self.de.word_ref_mut(), self.hl.word_ref_mut()),
                Mov(dst, src) => self.set_reg(m, dst, self.reg(m, src)),
                Mvi(dst, val) => self.set_reg(m, dst, val),
                Lxi(rp, val) => self.set_rp(rp, val),
                Lda(addr) => self.psw.0 = m.read8(addr),
                Sta(addr) => m.write8(addr, self.psw.0),
                Ldax(rp) => self.psw.0 = m.read8(self.rp(rp)),
                Stax(rp) => m.write8(self.rp(rp), self.psw.0),
                Lhld(addr) => self.hl.set_word(m.read16le(addr)),
                Shld(addr) => m.write16le(self.hl.word(), addr),
                Sphl => self.set_rp(RegPair::Sp, self.rp(RegPair::Hl)),
                Push(rp) => {
                    if rp == RegPair::Sp {
                        self.push8(m, self.psw.0).await;
                        self.push8(m, self.psw.1.to_8080()).await;
                    } else {
                        self.push16(m, self.rp(rp)).await;
                    }
                },
                Pop(rp) => {
                    if rp == RegPair::Sp {
                        self.psw.1 = self.pop8(m).await.into();
                        self.psw.0 = self.pop8(m).await;
                    } else {
                        let tmp = self.pop16(m).await;
                        self.set_rp(rp, tmp);
                    }
                },
                Xthl => {
                    let new = m.read16le(self.sp);
                    m.write16le(self.sp, self.hl.word());
                    self.hl.set_word(new);
                },
                Pchl => self.pc = self.hl.word(),
                Xri(val) => self.psw.0 = self.alu(AluOp::Xor, self.psw.0, val),
                Xra(reg) => self.psw.0 = self.alu(AluOp::Xor, self.psw.0, self.reg(m, reg)),
                Ori(val) => self.psw.0 = self.alu(AluOp::Or, self.psw.0, val),
                Ora(reg) => self.psw.0 = self.alu(AluOp::Or, self.psw.0, self.reg(m, reg)),
                Ani(val) => self.psw.0 = self.alu(AluOp::And, self.psw.0, val),
                Ana(reg) => self.psw.0 = self.alu(AluOp::And, self.psw.0, self.reg(m, reg)),
                Add(reg) => self.psw.0 = self.alu(AluOp::Add(false), self.psw.0, self.reg(m, reg)),
                Adc(reg) => self.psw.0 = self.alu(AluOp::Add(true), self.psw.0, self.reg(m, reg)),
                Adi(val) => self.psw.0 = self.alu(AluOp::Add(false), self.psw.0, val),
                Aci(val) => self.psw.0 = self.alu(AluOp::Add(true), self.psw.0, val),
                Sub(reg) => self.psw.0 = self.alu(AluOp::Sub(false), self.psw.0, self.reg(m, reg)),
                Sbb(reg) => self.psw.0 = self.alu(AluOp::Sub(true), self.psw.0, self.reg(m, reg)),
                Sui(val) => self.psw.0 = self.alu(AluOp::Sub(false), self.psw.0, val),
                Sbi(val) => self.psw.0 = self.alu(AluOp::Sub(true), self.psw.0, val),
                Cmp(reg) => { self.alu(AluOp::Sub(false), self.psw.0, self.reg(m, reg)); },
                Cpi(val) => { self.alu(AluOp::Sub(false), self.psw.0, val); },
                Dad(rp) => {
                    let (result, carry) = self.hl.word()
                        .overflowing_add(self.rp(rp));
                    self.psw.1.set_carry(carry);
                    self.hl.set_word(result);
                },
                Stc => self.psw.1.set_carry(true),
                Cmc => self.psw.1.set_carry(!self.psw.1.carry()),
                Cma => self.psw.0 = self.psw.0 ^ 0xff,
                Daa => {
                    // HEAVILY inspired by https://github.com/GunshipPenguin/lib8080/blob/master/src/i8080.c#L405
                    let mut add = 0;

                    if self.psw.1.half_carry() || self.psw.0 & 0x0f > 9 {
                        add = 0x06
                    }

                    if self.psw.1.carry() || (self.psw.0 & 0xf0) > 0x90
                        || ((self.psw.0 & 0xf0) >= 0x90 && self.psw.0 & 0x0F > 9)
                    {
                        add |= 0x60;
                        self.psw.1.set_carry(true);
                    }
                    self.psw.1.set_half_carry((self.psw.0 & 0x0F) + (add as u8 & 0x0F) > 0x0F);
                    self.psw.0 = self.update_zsp(self.psw.0.wrapping_add(add));
                },
                Dcr(reg) => {
                    self.set_reg(m, reg, self.reg(m, reg).wrapping_sub(1));
                    self.update_zsp(self.reg(m, reg));
                    self.psw.1.set_half_carry(!((self.reg(m, reg) & 0xf) == 0xf)); // whaaaa
                },
                Inr(reg) => {
                    self.set_reg(m, reg, self.reg(m, reg).wrapping_add(1));
                    self.psw.1.set_half_carry(self.reg(m, reg) & 0xf == 0x00);
                    self.update_zsp(self.reg(m, reg));
                },
                Inx(rp) => self.set_rp(rp, self.rp(rp).wrapping_add(1)),
                Dcx(rp) => self.set_rp(rp, self.rp(rp).wrapping_sub(1)),
                Rrc => {
                    self.psw.0 = self.psw.0.rotate_right(1);
                    self.psw.1.set_carry(self.psw.0 & 0x80 != 0);
                },
                Rlc => {
                    self.psw.0 = self.psw.0.rotate_left(1);
                    self.psw.1.set_carry(self.psw.0 & 0x01 != 0);
                },
                Rar => {
                    let carry = self.psw.0 & 0x1 != 0;
                    self.psw.0 = (self.psw.0 >> 1) | (self.psw.1.carry() as u8 * 0x80);
                    self.psw.1.set_carry(carry);
                },
                Ral => {
                    let carry = self.psw.0 & 0x80 != 0;
                    self.psw.0 = (self.psw.0 << 1) | (self.psw.1.carry() as u8);
                    self.psw.1.set_carry(carry);
                },
                Di | Ei | Nop => (),
                _ => todo!("{:x?}", inst),
            }

            self.pc
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq)]
enum AluOp {
    Xor,
    Or,
    And,
    Add(bool),
    Sub(bool),
}

impl Intel8085 {
    #[inline]
    pub fn set_reg(&mut self, m: &mut impl BusWo<u16>, reg: Reg, val: u8) {
        use Reg::*;
        match reg {
            A => self.psw.0 = val,
            B => self.bc.set_hi(val),
            C => self.bc.set_lo(val),
            D => self.de.set_hi(val),
            E => self.de.set_lo(val),
            H => self.hl.set_hi(val),
            L => self.hl.set_lo(val),
            M => m.write8(self.hl.word(), val),
        }
    }
    #[inline]
    pub fn reg(&self, m: &impl BusRo<u16>, reg: Reg) -> u8 {
        use Reg::*;
        match reg {
            A => self.psw.0,
            B => self.bc.hi(),
            C => self.bc.lo(),
            D => self.de.hi(),
            E => self.de.lo(),
            H => self.hl.hi(),
            L => self.hl.lo(),
            M => m.read8(self.hl.word()),
        }
    }
    #[inline]
    pub fn rp(&self, rp: RegPair) -> u16 {
        use RegPair::*;
        match rp {
            Bc => self.bc.word(),
            De => self.de.word(),
            Hl => self.hl.word(),
            Sp => self.sp,
        }
    }
    #[inline]
    pub fn set_rp(&mut self, rp: RegPair, val: u16) {
        use RegPair::*;
        match rp {
            Bc => self.bc.set_word(val),
            De => self.de.set_word(val),
            Hl => self.hl.set_word(val),
            Sp => self.sp = val,
        }
    }
    #[inline]
    fn update_zsp(&mut self, val: u8) -> u8 {
        self.psw.1.set_zero(val == 0);
        self.psw.1.set_sign(val & 0x80 != 0);
        self.psw.1.set_parity(val.count_ones() % 2 == 0);
        val
    }
    #[inline]
    async fn push8(&mut self, m: &mut impl BusWo<u16>, v: u8) {
        self.sp = self.sp.wrapping_sub(1);
        m.write8(self.sp, v);
        yield_now().await;
    }
    #[inline]
    async fn pop8(&mut self, m: &mut impl BusRo<u16>) -> u8 {
        let v = m.read8(self.sp);
        self.sp = self.sp.wrapping_add(1);
        yield_now().await;
        v
    }
    #[inline]
    async fn pop16(&mut self, mem: &mut impl BusRo<u16>) -> u16 {
        let lo = self.pop8(mem).await;
        let hi = self.pop8(mem).await;
        u16::from_le_bytes([lo, hi])
    }
    #[inline]
    async fn push16(&mut self, mem: &mut impl BusWo<u16>, val: u16) {
        let [lo, hi] = val.to_le_bytes();
        self.push8(mem, hi).await;
        self.push8(mem, lo).await;
    }
    #[inline]
    fn has_cond(&self, cond: Condition) -> bool {
        match cond {
            Condition::NonZero => !self.psw.1.zero(),
            Condition::Zero => self.psw.1.zero(),
            Condition::NoCarry => !self.psw.1.carry(),
            Condition::Carry => self.psw.1.carry(),
            Condition::Odd => !self.psw.1.parity(),
            Condition::Even => self.psw.1.parity(),
            Condition::Plus => !self.psw.1.sign(),
            Condition::Minus => self.psw.1.sign(),
        }
    }
    fn alu(&mut self, op: AluOp, val1: u8, val2: u8) -> u8 {
        use AluOp::*;
        let ret = match op {
            Xor | Or | And => {
                let result = match op {
                    Xor => val1 ^ val2,
                    Or => val1 | val2,
                    And => val1 & val2,
                    _ => unreachable!(),
                };
                if op == And {
                    self.psw.1.set_half_carry(((val1 | val2) & 0x08) != 0);
                } else {
                    self.psw.1.set_half_carry(false);
                }
                self.psw.1.set_carry(false);
                result
            },
            Add(use_carry) => {
                let carry = (use_carry & self.psw.1.carry()) as u8;
                let result = val1
                    .wrapping_add(val2)
                    .wrapping_add(carry);
                //let result = val1 as u16 + val2 as u16 + carry as u16;
                self.psw.1.set_carry(val1 as u16 + val2 as u16 + carry as u16 > 0xff);
                self.psw.1.set_half_carry(((val1 & 0x0f) + (val2 & 0x0f) + carry) > 0xf);
                result
            },
            Sub(use_carry) => {
                let carry = (use_carry & self.psw.1.carry()) as u8;
                //let result = (val1 as u16).wrapping_sub(val2.wrapping_add(carry) as u16);
                //self.psw.set_carry(result & 0xff00 != 0);
                //self.psw.1.set_half_carry((val2 & 0x0f) <= (val1 & 0x0f));
                let result = val1
                    .wrapping_sub(val2)
                    .wrapping_sub(carry);
                self.psw.1.set_half_carry(val1 as i8 & 0xf - val2 as i8 & 0xf - carry as i8 >= 0);
                self.psw.1.set_carry(val1 < val2 + carry);
                result
            },
        };
        self.update_zsp(ret)
    }
}
