use crate::cpu_prelude::*;
use asm_playground::i8080::{self, Instruction, Instruction::*, Condition};
pub use asm_playground::i8080::{Reg, RegPair};
use core::mem;

#[bitfield(u8, order = Msb)]
pub struct Flags {
    sign: bool,
    zero: bool,
    k: bool,
    half_carry: bool,
    unused_zero: bool,
    parity_even: bool,
    signed_overflow: bool,
    carry: bool,
}

impl Flags {
    fn to_8080(&self) -> u8 {
        const MASK: u8 = 0xd7;
        (self.into_bits() & MASK) | 0x2
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

#[derive(Clone, Debug)]
pub struct Intel8080 {
    pc: u16,
    a: u8,
    f: Flags,
    bc: TwoBytes,
    de: TwoBytes,
    hl: TwoBytes,
    sp: u16,
}

impl Intel8080 {
    #[inline(always)]
    pub fn flags(&self) -> Flags {
        self.f
    }
    #[inline(always)]
    pub fn pc(&self) -> u16 {
        self.pc
    }
    #[inline(always)]
    pub fn set_reg(&mut self, m: &mut impl BusWo<u16>, reg: Reg, val: u8) {
        use Reg::*;
        match reg {
            A => self.a = val,
            B => self.bc.set_hi(val),
            C => self.bc.set_lo(val),
            D => self.de.set_hi(val),
            E => self.de.set_lo(val),
            H => self.hl.set_hi(val),
            L => self.hl.set_lo(val),
            M => m.write8(self.hl.word(), val),
        }
    }
    #[inline(always)]
    pub fn reg(&self, m: &impl BusRo<u16>, reg: Reg) -> u8 {
        use Reg::*;
        match reg {
            A => self.a,
            B => self.bc.hi(),
            C => self.bc.lo(),
            D => self.de.hi(),
            E => self.de.lo(),
            H => self.hl.hi(),
            L => self.hl.lo(),
            M => m.read8(self.hl.word()),
        }
    }
    #[inline(always)]
    pub fn rp(&self, rp: RegPair) -> u16 {
        use RegPair::*;
        match rp {
            Bc => self.bc.word(),
            De => self.de.word(),
            Hl => self.hl.word(),
            Sp => self.sp,
        }
    }
    #[inline(always)]
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
        self.f.set_zero(val == 0);
        self.f.set_sign(val & 0x80 != 0);
        self.f.set_parity_even(val.count_ones() % 2 == 0);
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
    #[inline(always)]
    fn has_cond(&self, cond: Condition) -> bool {
        match cond {
            Condition::NonZero => !self.f.zero(),
            Condition::Zero => self.f.zero(),
            Condition::NoCarry => !self.f.carry(),
            Condition::Carry => self.f.carry(),
            Condition::Odd => !self.f.parity_even(),
            Condition::Even => self.f.parity_even(),
            Condition::Plus => !self.f.sign(),
            Condition::Minus => self.f.sign(),
        }
    }
    #[inline(always)]
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
                    self.f.set_half_carry(((val1 | val2) & 0x08) != 0);
                } else {
                    self.f.set_half_carry(false);
                }
                self.f.set_carry(false);
                result
            },
            Add(use_carry) => {
                let carry = (use_carry & self.f.carry()) as u8;
                let result = val1.wrapping_add(val2).wrapping_add(carry);
                self.f.set_carry(self.carry(8, val1, val2, carry));
                self.f.set_half_carry((val1 ^ result as u8 ^ val2) & 0x10 != 0);
                result as u8
            },
            Sub(use_carry) => {
                let carry = (use_carry & self.f.carry()) as u8;
                let result = val1.wrapping_sub(val2).wrapping_sub(carry);
                self.f.set_carry((val1 as u16) < val2 as u16 + carry as u16);
                self.f.set_half_carry((val1 as i8 & 0x0f) - (val2 as i8 & 0x0f) - (carry as i8) >= 0x00);
                result as u8
            },
        };
        self.update_zsp(ret)
    }
    #[inline(always)]
    fn carry(&self, num: u8, val1: u8, val2: u8, carry: u8) -> bool {
        let result = val1 as u16 + val2 as u16 + carry as u16;
        let carry = result ^ val1 as u16 ^ val2 as u16;
        (carry & (1 << num)) != 0
    }
}

impl Cpu for Intel8080 {
    type AddressWidth = u16;
    type Instruction = i8080::Instruction;
    #[inline(always)]
    fn pc(&self) -> u16 {
        self.pc
    }
    #[inline]
    fn next_instruction(&self, m: &impl Bus<u16>) -> Self::Instruction {
        let bytes = [m.read8(self.pc), m.read8(self.pc+1), m.read8(self.pc+2)];
        Instruction::decode_from(&bytes)
    }
    #[inline]
    fn new() -> Self {
        Self {
            pc: 0x0,
            sp: 0x0,
            a: 0x0,
            f: Flags::from(0x02),
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
            self.pc += inst.len() as u16;
            match inst {
                Jmp(addr) => self.pc = addr,
                Rst(addr) => {
                    self.push16(m, self.pc).await;
                    self.pc = addr as u16;
                },
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
                Lda(addr) => self.a = m.read8(addr),
                Sta(addr) => m.write8(addr, self.a),
                Ldax(rp) => self.a = m.read8(self.rp(rp)),
                Stax(rp) => m.write8(self.rp(rp), self.a),
                Lhld(addr) => self.hl.set_word(m.read16le(addr)),
                Shld(addr) => m.write16le(addr, self.hl.word()),
                Sphl => self.set_rp(RegPair::Sp, self.rp(RegPair::Hl)),
                Push(rp) => {
                    if rp == RegPair::Sp {
                        self.push8(m, self.a).await;
                        self.push8(m, self.f.to_8080()).await;
                    } else {
                        self.push16(m, self.rp(rp)).await;
                    }
                },
                Pop(rp) => {
                    if rp == RegPair::Sp {
                        self.f = self.pop8(m).await.into();
                        self.a = self.pop8(m).await;
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
                Xri(val) => self.a = self.alu(AluOp::Xor, self.a, val),
                Xra(reg) => self.a = self.alu(AluOp::Xor, self.a, self.reg(m, reg)),
                Ori(val) => self.a = self.alu(AluOp::Or, self.a, val),
                Ora(reg) => self.a = self.alu(AluOp::Or, self.a, self.reg(m, reg)),
                Ani(val) => self.a = self.alu(AluOp::And, self.a, val),
                Ana(reg) => self.a = self.alu(AluOp::And, self.a, self.reg(m, reg)),
                Add(reg) => self.a = self.alu(AluOp::Add(false), self.a, self.reg(m, reg)),
                Adc(reg) => self.a = self.alu(AluOp::Add(true), self.a, self.reg(m, reg)),
                Adi(val) => self.a = self.alu(AluOp::Add(false), self.a, val),
                Aci(val) => self.a = self.alu(AluOp::Add(true), self.a, val),
                Sub(reg) => self.a = self.alu(AluOp::Sub(false), self.a, self.reg(m, reg)),
                Sbb(reg) => self.a = self.alu(AluOp::Sub(true), self.a, self.reg(m, reg)),
                Sui(val) => self.a = self.alu(AluOp::Sub(false), self.a, val),
                Sbi(val) => self.a = self.alu(AluOp::Sub(true), self.a, val),
                //Cmp(reg) => { self.alu(AluOp::Sub(false), self.a, self.reg(m, reg)); },
                //Cpi(val) => { self.alu(AluOp::Sub(false), self.a, val); },
                Cpi(_) | Cmp(_) => {
                    let val = match inst {
                        Cpi(imm) => imm,
                        Cmp(reg) => self.reg(m, reg),
                        _ => unreachable!(),
                    };
                    let (result, carry) = self.a.overflowing_sub(val);
                    //self.f.set_carry(val > self.a);
                    self.f.set_carry(carry);
                    let carry4 = !(self.a ^ result as u8 ^ val) & 0x10;
                    self.f.set_half_carry(carry4 != 0);
                    self.update_zsp(result as u8);
                },
                Dad(rp) => {
                    let (result, carry) = self.hl.word()
                        .overflowing_add(self.rp(rp));
                    self.f.set_carry(carry);
                    self.hl.set_word(result);
                },
                Stc => self.f.set_carry(true),
                Cmc => self.f.set_carry(!self.f.carry()),
                Cma => self.a = self.a ^ 0xff,
                Daa => {
                    // HEAVILY inspired by https://github.com/GunshipPenguin/lib8080/blob/master/src/i8080.c#L405
                    let mut add = 0;

                    if self.f.half_carry() || self.a & 0x0f > 9 {
                        add = 0x06
                    }

                    if self.f.carry() || (self.a & 0xf0) > 0x90
                        || ((self.a & 0xf0) >= 0x90 && self.a & 0x0F > 9)
                    {
                        add |= 0x60;
                        self.f.set_carry(true);
                    }
                    self.f.set_half_carry((self.a & 0x0F) + (add as u8 & 0x0F) > 0x0F);
                    self.a = self.update_zsp(self.a.wrapping_add(add));
                },
                Dcr(reg) => {
                    self.set_reg(m, reg, self.reg(m, reg).wrapping_sub(1));
                    self.update_zsp(self.reg(m, reg));
                    self.f.set_half_carry(!((self.reg(m, reg) & 0xf) == 0xf));
                },
                Inr(reg) => {
                    self.set_reg(m, reg, self.reg(m, reg).wrapping_add(1));
                    self.update_zsp(self.reg(m, reg));
                    self.f.set_half_carry(self.reg(m, reg) & 0xf == 0x00);
                },
                Inx(rp) => self.set_rp(rp, self.rp(rp).wrapping_add(1)),
                Dcx(rp) => self.set_rp(rp, self.rp(rp).wrapping_sub(1)),
                Rrc => {
                    self.a = self.a.rotate_right(1);
                    self.f.set_carry(self.a & 0x80 != 0);
                },
                Rlc => {
                    self.a = self.a.rotate_left(1);
                    self.f.set_carry(self.a & 0x01 != 0);
                },
                Rar => {
                    let carry = self.a & 0x1 != 0;
                    self.a = (self.a >> 1) | (self.f.carry() as u8 * 0x80);
                    self.f.set_carry(carry);
                },
                Ral => {
                    let carry = self.a & 0x80 != 0;
                    self.a = (self.a << 1) | (self.f.carry() as u8);
                    self.f.set_carry(carry);
                },
                Di | Ei | Nop => (),
                _ => todo!("{:x?}", inst),
            }

            self.pc
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{Intel8080, Reg, RegPair};
    use crate::{Cpu, BusRo, BusWo, Bus};
    use futures_lite::future::block_on;
    #[test]
    fn reg_m() {
        block_on(async {
            let mut cpu = Intel8080::new();
            let mut m = vec![0u8; 0x10000];
            m[0] = 0x26; // mvi h
            m[1] = 0xaa;
            m[2] = 0x2e; // mvi l
            m[3] = 0xbb;
            m[4] = 0x36; // mvi m
            m[5] = 0xdd;
            m[0xaabb] = 0xcc;
            cpu.step_instruction(&mut m).await;
            cpu.step_instruction(&mut m).await;
            cpu.step_instruction(&mut m).await;
            assert_eq!(m[0xaabb], m[5]);
        })
    }
    #[test]
    fn mvi_and_reg_pairing() {
        block_on(async {
            let mut cpu = Intel8080::new();
            let mut m = vec![0u8; 0x10000];
            m[0] = 0x06; // mvi b
            m[1] = 0x01;
            m[2] = 0x0e; // mvi c
            m[3] = 0x02;
            m[4] = 0x16; // mvi d
            m[5] = 0x03;
            m[6] = 0x1e; // mvi e
            m[7] = 0x04;
            m[8] = 0x26; // mvi h
            m[9] = 0x05;
            m[10] = 0x2e; // mvi l
            m[11] = 0x06;
            m[12] = 0x3e; // mvi a
            m[13] = 0x07;

            cpu.step_instruction(&mut m).await;
            cpu.step_instruction(&mut m).await;
            cpu.step_instruction(&mut m).await;
            cpu.step_instruction(&mut m).await;
            cpu.step_instruction(&mut m).await;
            cpu.step_instruction(&mut m).await;
            cpu.step_instruction(&mut m).await;

            assert_eq!(cpu.reg(&m, Reg::B), 0x01);
            assert_eq!(cpu.reg(&m, Reg::C), 0x02);
            assert_eq!(cpu.rp(RegPair::Bc), 0x0102);
            assert_eq!(cpu.reg(&m, Reg::D), 0x03);
            assert_eq!(cpu.reg(&m, Reg::E), 0x04);
            assert_eq!(cpu.rp(RegPair::De), 0x0304);
            assert_eq!(cpu.reg(&m, Reg::H), 0x05);
            assert_eq!(cpu.reg(&m, Reg::L), 0x06);
            assert_eq!(cpu.rp(RegPair::Hl), 0x0506);
            assert_eq!(cpu.reg(&m, Reg::A), 0x07);
        });
    }
    /*#[test]
    fn cpi_tst8080() {
        block_on(async {
            let mut cpu = Intel8080::new();
            let mut m = vec![0u8; 0x10000];
            m[0] = 0x3e; // mvi a, 0xf5
            m[1] = 0xf5;
            m[2] = 0xfe; // cpi 0x0
            m[3] = 0x00;
            m[4] = 0xfe; // cpi 0xf5
            m[5] = 0xf5;
            m[6] = 0xf3; // cpi 0xff
            m[7] = 0xff;
            cpu.step_instruction(&mut m).await; // mvi a, 0xf5
            cpu.step_instruction(&mut m).await; // cpi 0x0
            assert!(!cpu.flags().carry());
            assert!(!cpu.flags().zero());
            cpu.step_instruction(&mut m).await; // cpi 0xf5
            assert!(!cpu.flags().carry());
            assert!(cpu.flags().zero());
            cpu.step_instruction(&mut m).await; // cpi 0xff
            assert!(!cpu.flags().carry());
            assert!(cpu.flags().zero());
        });
    }*/
}
