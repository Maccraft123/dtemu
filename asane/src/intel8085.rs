use crate::cpu_prelude::*;
use asm_playground::i8080::{self, Instruction, Instruction::*, Condition};
pub use asm_playground::i8080::{Reg, RegPair};
use core::mem;
use core::future::Future;
/*
#[bitfield(u8, order = Msb)]
pub struct Flags {
    sign: bool,
    zero: bool,
    k: bool,
    half_carry: bool,
    unused_zero: bool,
    parity_even: bool,
    overflow: bool,
    carry: bool,
}

impl Flags {
    fn to_8080(&self) -> u8 {
        const MASK: u8 = 0xd7;
        (self.into_bits() & MASK) | 0x2
    }
}*/

#[derive(Clone, Debug)]
pub struct Flags {
    sign: bool,
    zero: bool,
    half_carry: bool,
    parity_even: bool,
    carry: bool,
}

impl Flags {
    #[inline(always)] pub fn to_8080(&self) -> u8 {
        self.sign as u8 * 0x80 |
            self.zero as u8 * 0x40 |
            self.half_carry as u8 * 0x10 |
            self.parity_even as u8 * 0x04 |
            0x02 |
            self.carry as u8
    }
    #[inline(always)] pub fn sign(&self) -> bool { self.sign }
    #[inline(always)] pub fn zero(&self) -> bool { self.zero }
    #[inline(always)] pub fn half_carry(&self) -> bool { self.half_carry }
    #[inline(always)] pub fn parity_even(&self) -> bool { self.parity_even }
    #[inline(always)] pub fn carry(&self) -> bool { self.carry }
    #[inline(always)] pub fn set_sign(&mut self, val: bool) { self.sign = val }
    #[inline(always)] pub fn set_zero(&mut self, val: bool) { self.zero = val }
    #[inline(always)] pub fn set_half_carry(&mut self, val: bool) { self.half_carry = val }
    #[inline(always)] pub fn set_parity_even(&mut self, val: bool) { self.parity_even = val }
    #[inline(always)] pub fn set_carry(&mut self, val: bool) { self.carry = val }
}

impl From<u8> for Flags {
    #[inline(always)]
    fn from(v: u8) -> Self {
        Self {
            sign: v & 0x80 != 0,
            zero: v & 0x40 != 0,
            half_carry: v & 0x10 != 0,
            parity_even: v & 0x04 != 0,
            carry: v & 0x01 != 0,
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq)]
enum AluOp {
    Xor,
    Or,
    And,
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
        self.f.clone()
    }
    #[inline(always)]
    pub fn pc(&self) -> u16 {
        self.pc
    }
    #[inline(always)]
    pub fn set_reg(&mut self, m: &mut impl BusWrite<u16>, reg: Reg, val: u8) {
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
    pub fn reg(&self, m: &impl BusRead<u16>, reg: Reg) -> u8 {
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
    /// Gets value of an 8080 register, taking 3 cycles for getting M
    #[inline(always)]
    async fn r(&self, m: &impl BusRead<u16>, reg: Reg) -> u8 {
        if reg == Reg::M {
            yield_for!(3);
        }
        self.reg(m, reg)
    }
    /// Sets an 8080 register, taking 3 cycles for setting M
    #[inline(always)]
    async fn sr(&mut self, m: &mut impl BusWrite<u16>, reg: Reg, val: u8) {
        if reg == Reg::M {
            yield_for!(3);
        }
        self.set_reg(m, reg, val)
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
    async fn push8(&mut self, m: &mut impl BusWrite<u16>, v: u8) {
        self.sp = self.sp.wrapping_sub(1);
        m.write8(self.sp, v);
        yield_for!(3);
    }
    #[inline]
    async fn pop8(&mut self, m: &mut impl BusRead<u16>) -> u8 {
        let v = m.read8(self.sp);
        self.sp = self.sp.wrapping_add(1);
        yield_for!(3);
        v
    }
    #[inline]
    async fn pop16(&mut self, mem: &mut impl BusRead<u16>) -> u16 {
        yield_for!(6);
        let val = mem.read16le(self.sp);
        self.sp += 2;
        val
    }
    #[inline]
    async fn push16(&mut self, mem: &mut impl BusWrite<u16>, val: u16) {
        self.sp -= 2;
        mem.write16le(self.sp, val);
        yield_for!(6);
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
    fn add(&mut self, carry: bool, v1: u8, v2: u8) -> u8 {
        let result = v1.wrapping_add(v2).wrapping_add(carry as u8);
        self.f.set_carry(self.carry(8, v1,  v2, carry));
        self.f.set_half_carry(self.carry(4, v1,  v2, carry));
        self.update_zsp(result)
    }
    #[inline(always)]
    fn sub(&mut self, carry: bool, val1: u8, val2: u8) -> u8 {
        let tmp = self.add(!carry, val1, !val2);
        self.f.set_carry(!self.f.carry());
        tmp
    }
    #[inline]
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
        };
        self.update_zsp(ret)
    }
    #[inline(always)]
    fn carry(&self, num: u8, val1: u8, val2: u8, carry: bool) -> bool {
        let result = val1 as u16 + val2 as u16 + carry as u16;
        let carry = result ^ val1 as u16 ^ val2 as u16;
        (carry & (1 << num)) != 0
    }
    #[inline]
    async fn call(&mut self, m: &mut impl Bus<u16>, addr: u16, cond: bool) {
        if cond {
            self.push16(m, self.pc).await;
            self.pc = addr;
        }
        yield_for!(1);
    }
    #[inline(always)]
    async fn rd8(&self, m: &impl BusRead<u16>, addr: u16) -> u8 {
        yield_for!(3);
        m.read8(addr)
    }
    #[inline(always)]
    async fn rd16(&self, m: &impl BusRead<u16>, addr: u16) -> u16 {
        yield_for!(6);
        m.read16le(addr)
    }
    #[inline(always)]
    async fn w8(&self, m: &mut impl BusWrite<u16>, addr: u16, val: u8) {
        yield_for!(3);
        m.write8(addr, val)
    }
    #[inline(always)]
    async fn w16(&self, m: &mut impl BusWrite<u16>, addr: u16, val: u16) {
        yield_for!(6);
        m.write16le(addr, val)
    }
}

impl Cpu for Intel8080 {
    type AddressWidth = u16;
    type IoAddressWidth = u8;
    type Interrupt = u8;
    type Instruction = i8080::Instruction;
    #[inline(always)]
    fn set_pc(&mut self, new: u16) {
        self.pc = new;
    }
    #[inline(always)]
    fn pc(&self) -> u16 {
        self.pc
    }
    #[inline]
    fn irq(&mut self, num: u8, m: &mut impl Bus<u16>, _: &mut impl Bus<u8>) -> impl Future<Output = Self::AddressWidth> + Send {
        async move {
            match num {
                0xc7 => self.call(m, 0x00, true).await,
                0xcf => self.call(m, 0x08, true).await,
                0xd7 => self.call(m, 0x10, true).await,
                0xdf => self.call(m, 0x18, true).await,
                0xe7 => self.call(m, 0x20, true).await,
                0xef => self.call(m, 0x28, true).await,
                0xf7 => self.call(m, 0x30, true).await,
                0xff => self.call(m, 0x38, true).await,
                _ => unimplemented!("less hacky way to do 8080 irqs"),
            }
            self.pc
        }
    }
    #[inline]
    fn next_instruction(&self, m: &impl Bus<u16>) -> Self::Instruction {
        let bytes = [m.read8(self.pc), m.read8(self.pc+1), m.read8(self.pc+2)];
        Instruction::decode_and_len(&bytes).0
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
    fn step_instruction(&mut self, m: &mut impl Bus<u16>, io: &mut impl Bus<u8>) -> impl Future<Output = Self::AddressWidth> + Send {
        async {
            let bytes = [m.read8(self.pc), m.read8(self.pc+1), m.read8(self.pc+2)];
            let (inst, len) = Instruction::decode_and_len(&bytes);
            self.pc += len as u16;
            // memory fetch is 3 cycles, every byte of instruction has to be fetched and 1 cycle
            // presumably for decoding
            yield_for!(1 + len as u8);
            match inst {
                Jmp(addr) => self.pc = addr,
                Rst(addr) => self.call(m, addr as u16, true).await,
                J(cond, addr) => {
                    if self.has_cond(cond) {
                        self.pc = addr;
                    }
                },
                Call(addr) => self.call(m, addr, true).await,
                C(cond, addr) => self.call(m, addr, self.has_cond(cond)).await,
                Ret => self.pc = self.pop16(m).await,
                R(cond) => {
                    yield_for!(1); // for checking condition?
                    if self.has_cond(cond) {
                        self.pc = self.pop16(m).await;
                    }
                },
                Xchg => mem::swap(self.de.word_ref_mut(), self.hl.word_ref_mut()),
                Mov(dst, src) => {
                    yield_for!(1);
                    self.sr(m, dst, self.r(m, src).await).await;
                },
                Mvi(dst, val) => self.sr(m, dst, val).await,
                Lxi(rp, val) => self.set_rp(rp, val),
                Lda(addr) => self.a = self.rd8(m, addr).await,
                Sta(addr) => self.w8(m, addr, self.a).await,
                Ldax(rp) => self.a = self.rd8(m, self.rp(rp)).await,
                Stax(rp) => self.w8(m, self.rp(rp), self.a).await,
                Lhld(addr) => {
                    yield_for!(6); // ???
                    self.hl.set_word(self.rd16(m, addr).await);
                },
                Shld(addr) => {
                    yield_for!(6); // ???
                    self.w16(m, addr, self.hl.word()).await;
                },
                Sphl => { yield_for!(1); self.set_rp(RegPair::Sp, self.rp(RegPair::Hl)); },
                Push(rp) => {
                    yield_for!(1);
                    if rp == RegPair::Sp {
                        self.push8(m, self.a).await;
                        //if false {// if self.is_8080
                            self.push8(m, self.f.to_8080()).await;
                        //} else {
                            //self.push8(m, self.f.to_8085()).await;
                        //}
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
                    yield_for!(2);
                    let new = self.rd16(m, self.sp).await;
                    self.w16(m, self.sp, self.hl.word()).await;
                    self.hl.set_word(new);
                },
                Pchl => { yield_for!(1); self.pc = self.hl.word(); },
                Xri(val) => self.a = self.alu(AluOp::Xor, self.a, val),
                Xra(reg) => self.a = self.alu(AluOp::Xor, self.a, self.reg(m, reg)),
                Ori(val) => self.a = self.alu(AluOp::Or, self.a, val),
                Ora(reg) => self.a = self.alu(AluOp::Or, self.a, self.reg(m, reg)),
                Ani(val) => self.a = self.alu(AluOp::And, self.a, val),
                Ana(reg) => self.a = self.alu(AluOp::And, self.a, self.reg(m, reg)),
                Add(reg) => self.a = self.add(false, self.a, self.reg(m, reg)),
                Adc(reg) => self.a = self.add(self.f.carry(), self.a, self.reg(m, reg)),
                Adi(val) => self.a = self.add(false, self.a, val),
                Aci(val) => self.a = self.add(self.f.carry(), self.a, val),
                Sub(reg) => self.a = self.sub(false, self.a, self.reg(m, reg)),
                Sbb(reg) => self.a = self.sub(self.f.carry(), self.a, self.reg(m, reg)),
                Sui(val) => self.a = self.sub(false, self.a, val),
                Sbi(val) => self.a = self.sub(self.f.carry(), self.a, val),
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
                    yield_for!(6);
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
                    if reg != Reg::M { yield_for!(1); }
                    self.sr(m, reg, self.r(m, reg).await.wrapping_sub(1)).await;
                    self.update_zsp(self.reg(m, reg));
                    self.f.set_half_carry(!((self.reg(m, reg) & 0xf) == 0xf));
                },
                Inr(reg) => {
                    if reg != Reg::M { yield_for!(1); }
                    self.sr(m, reg, self.r(m, reg).await.wrapping_add(1)).await;
                    self.update_zsp(self.reg(m, reg));
                    self.f.set_half_carry(self.reg(m, reg) & 0xf == 0x00);
                },
                Inx(rp) => { yield_for!(1); self.set_rp(rp, self.rp(rp).wrapping_add(1)); },
                Dcx(rp) => { yield_for!(1); self.set_rp(rp, self.rp(rp).wrapping_sub(1))} ,
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
                In(addr) => self.a = io.read8(addr),
                Out(addr) => io.write8(addr, self.a),
                Hlt => self.pc -= 1,
            }

            self.pc
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{Intel8080, Reg, RegPair};
    use crate::{Cpu, BusRead, BusWrite, Bus};
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
