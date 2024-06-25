use crate::cpu_prelude::*;
use asm_playground::i8080::{self, Instruction, Instruction::*, Condition};
pub use asm_playground::i8080::{Reg, RegPair};
use core::mem;

define_cycles!();

const fn make_zsp_lut() -> [(bool, bool, bool); 0x100] {
    let mut lut = [(false, false, false); 0x100];
    let mut v: usize = 0;
    while v < 0x100 {
        lut[v] = (
            v == 0,
            v & 0x80 != 0,
            v.count_ones() % 2 == 0,
        );
        v += 1;
    }
    lut
}

#[derive(Copy, Clone, Debug)]
pub struct Flags {
    sign: bool,
    zero: bool,
    half_carry: bool,
    parity_even: bool,
    carry: bool,
}

/*const fn make_flags_lut() -> [Flags; 0x100] {
    let mut lut = [Flags { sign: false, zero: false, half_carry: false, parity_even: false, carry: false}; 0x100];
    let mut v = 0usize;
    while v < 0x100 {
        lut[v] = Flags {
            sign: v & 0x80 != 0,
            zero: v & 0x40 != 0,
            half_carry: v & 0x10 != 0,
            parity_even: v & 0x04 != 0,
            carry: v & 0x01 != 0,
        };
        v += 1;
    }

    lut
}

static FLAGS_LUT: [Flags; 0x100] = make_flags_lut();*/

impl Flags {
    #[inline(always)] pub fn to_8080(&self) -> u8 {
        (self.sign as u8 * 0x80) |
            (self.zero as u8 * 0x40) |
            (self.half_carry as u8 * 0x10) |
            (self.parity_even as u8 * 0x04) |
            0x02 |
            (self.carry as u8)
    }
    #[inline] pub fn sign(&self) -> bool { self.sign }
    #[inline] pub fn zero(&self) -> bool { self.zero }
    #[inline] pub fn half_carry(&self) -> bool { self.half_carry }
    #[inline] pub fn parity_even(&self) -> bool { self.parity_even }
    #[inline] pub fn carry(&self) -> bool { self.carry }
    #[inline] pub fn set_sign(&mut self, val: bool) { self.sign = val }
    #[inline] pub fn set_zero(&mut self, val: bool) { self.zero = val }
    #[inline] pub fn set_half_carry(&mut self, val: bool) { self.half_carry = val }
    #[inline] pub fn set_parity_even(&mut self, val: bool) { self.parity_even = val }
    #[inline] pub fn set_carry(&mut self, val: bool) { self.carry = val }
}

impl From<u8> for Flags {
    #[inline(always)]
    fn from(v: u8) -> Self {
        //FLAGS_LUT[v as usize]
        Self {
            sign: v & 0x80 != 0,
            zero: v & 0x40 != 0,
            half_carry: v & 0x10 != 0,
            parity_even: v & 0x04 != 0,
            carry: v & 0x01 != 0,
        }
    }
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
    pub fn reg(&self, m: &mut impl BusRead<u16>, reg: Reg) -> u8 {
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
    async fn r(&self, m: &mut impl BusRead<u16>, reg: Reg) -> u8 {
        if reg == Reg::M {
            cycles!(3);
        }
        self.reg(m, reg)
    }
    /// Sets an 8080 register, taking 3 cycles for setting M
    #[inline(always)]
    async fn sr(&mut self, m: &mut impl BusWrite<u16>, reg: Reg, val: u8) {
        if reg == Reg::M {
            cycles!(3);
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
        // lol
        static ZSP_LUT: [(bool, bool, bool); 0x100] = make_zsp_lut();
        (self.f.zero, self.f.sign, self.f.parity_even) = ZSP_LUT[val as usize];
        //self.f.zero = val == 0;
        //self.f.sign = val & 0x80 != 0;
        //self.f.parity_even = val.count_ones() % 2 == 0;
        val
    }
    #[inline]
    async fn push8(&mut self, m: &mut impl BusWrite<u16>, v: u8) {
        self.sp = self.sp.wrapping_sub(1);
        self.w8(m, self.sp, v).await;
    }
    #[inline]
    async fn pop8(&mut self, m: &mut impl BusRead<u16>) -> u8 {
        let v = self.rd8(m, self.sp).await;
        self.sp = self.sp.wrapping_add(1);
        v
    }
    #[inline]
    async fn pop16(&mut self, m: &mut impl BusRead<u16>) -> u16 {
        let val = self.rd16(m, self.sp).await;
        self.sp = self.sp.wrapping_add(2);
        val
    }
    #[inline]
    async fn push16(&mut self, m: &mut impl BusWrite<u16>, val: u16) {
        self.sp = self.sp.wrapping_sub(2);
        self.w16(m, self.sp, val).await;
    }
    #[inline(always)]
    fn has_cond(&self, cond: Condition) -> bool {
        match cond {
            Condition::NonZero => !self.f.zero,
            Condition::Zero => self.f.zero,
            Condition::NoCarry => !self.f.carry,
            Condition::Carry => self.f.carry,
            Condition::Odd => !self.f.parity_even,
            Condition::Even => self.f.parity_even,
            Condition::Plus => !self.f.sign,
            Condition::Minus => self.f.sign,
        }
    }
    #[inline]
    fn add(&mut self, carry: bool, v1: u8, v2: u8) -> u8 {
        // a lil bit faster than u8 math
        let result = v1 as usize + v2 as usize + carry as usize;
        self.f.carry = result > 0xff;
        //self.f.set_half_carry((v1 & 0x0f) + (v2 & 0x0f) + (carry as u8) > 0x0f);
        self.f.half_carry = self.carry(4, v1,  v2, carry);
        self.update_zsp(result as u8)
    }
    #[inline]
    fn sub(&mut self, carry: bool, val1: u8, val2: u8) -> u8 {
        let ret = self.add(!carry, val1, !val2);
        self.f.carry = !self.f.carry;
        ret
    }
    #[inline(always)]
    fn xor(&mut self, val1: u8, val2: u8) {
        self.f.carry = false;
        self.f.half_carry = false;
        self.a = self.update_zsp(val1 ^ val2);
    }
    #[inline(always)]
    fn or(&mut self, val1: u8, val2: u8) {
        self.f.carry = false;
        self.f.half_carry = false;
        self.a = self.update_zsp(val1 | val2);
    }
    #[inline(always)]
    fn and(&mut self, val1: u8, val2: u8) {
        self.f.carry = false;
        self.f.half_carry = ((val1 | val2) & 0x08) != 0;
        self.a = self.update_zsp(val1 & val2);
    }
    #[inline(always)]
    fn carry(&self, num: u8, val1: u8, val2: u8, carry: bool) -> bool {
        let result = val1 as usize + val2 as usize + carry as usize;
        let carry = result ^ val1 as usize ^ val2 as usize;
        (carry & (1 << num)) != 0
    }
    #[inline]
    async fn call(&mut self, m: &mut impl Bus<u16>, addr: u16, cond: bool) -> bool {
        if cond {
            self.push16(m, self.pc).await;
            self.pc = addr;
        }
        cycles!(1);
        cond
    }
    #[inline(always)]
    async fn rd8(&self, m: &mut impl BusRead<u16>, addr: u16) -> u8 {
        cycles!(3);
        m.read8(addr)
    }
    #[inline(always)]
    async fn rd16(&self, m: &mut impl BusRead<u16>, addr: u16) -> u16 {
        cycles!(6);
        m.read16le(addr)
    }
    #[inline(always)]
    async fn w8(&self, m: &mut impl BusWrite<u16>, addr: u16, val: u8) {
        cycles!(3);
        m.write8(addr, val)
    }
    #[inline(always)]
    async fn w16(&self, m: &mut impl BusWrite<u16>, addr: u16, val: u16) {
        cycles!(6);
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
    async fn irq(&mut self, num: u8, m: &mut impl Bus<u16>, _: &mut impl Bus<u8>) -> usize {
        match num {
            0xc7 => { self.call(m, 0x00, true).await; },
            0xcf => { self.call(m, 0x08, true).await; },
            0xd7 => { self.call(m, 0x10, true).await; },
            0xdf => { self.call(m, 0x18, true).await; },
            0xe7 => { self.call(m, 0x20, true).await; },
            0xef => { self.call(m, 0x28, true).await; },
            0xf7 => { self.call(m, 0x30, true).await; },
            0xff => { self.call(m, 0x38, true).await; },
            _ => unimplemented!("less hacky way to do 8080 irqs"),
        }
        take_cycles!()
    }
    #[inline]
    fn next_instruction(&self, m: &mut impl Bus<u16>) -> Self::Instruction {
        let bytes = [m.read8(self.pc), m.read8(self.pc+1), m.read8(self.pc+2)];
        Instruction::decode_and_len(&bytes).0
    }
    #[inline]
    fn new(_: &mut impl Bus<u16>, _: &mut impl Bus<u8>) -> Self {
        Self {
            pc: 0x0,
            sp: 0x0,
            a: 0x0,
            f: 0x0.into(),
            bc: TwoBytes::zeroed(),
            de: TwoBytes::zeroed(),
            hl: TwoBytes::zeroed(),
        }
    }
    #[inline]
    async fn step_block(&mut self, m: &mut impl Bus<u16>, io: &mut impl Bus<u8>) -> (usize, u16) {
        macro_rules! set_pc {
            ($new_pc: expr) => {
                { self.pc = ($new_pc); return (take_cycles!(), self.pc) }
            }
        }
        macro_rules! brk_if {
            ($cond: expr) => {
                { if ($cond) { return (take_cycles!(), self.pc) } }
            }
        }
        loop {
            let bytes = [m.read8(self.pc), m.read8(self.pc+1), m.read8(self.pc+2)];
            let (inst, len) = Instruction::decode_and_len(&bytes);
            self.pc += len as u16;
            // memory fetch is 3 cycles, every byte of instruction has to be fetched and 1 cycle
            // presumably for decoding
            cycles!(1 + (3*len) as u8);
            match inst {
                Jmp(addr) => set_pc!(addr),
                Rst(addr) => brk_if!(self.call(m, addr as u16, true).await),
                J(cond, addr) => {
                    if self.has_cond(cond) {
                        set_pc!(addr);
                    }
                },
                Call(addr) => brk_if!(self.call(m, addr, true).await),
                C(cond, addr) => brk_if!(self.call(m, addr, self.has_cond(cond)).await),
                Ret => set_pc!(self.pop16(m).await),
                R(cond) => {
                    cycles!(1); // for checking condition?
                    if self.has_cond(cond) {
                        set_pc!(self.pop16(m).await);
                    }
                },
                Xchg => { cycles!(1); mem::swap(self.de.word_ref_mut(), self.hl.word_ref_mut()); },
                Mov(dst, src) => {
                    if !(src == Reg::M || dst == Reg::M) {
                        cycles!(1);
                    }
                    let tmp = self.r(m, src).await;
                    self.sr(m, dst, tmp).await;
                },
                Mvi(dst, val) => self.sr(m, dst, val).await,
                Lxi(rp, val) => self.set_rp(rp, val),
                Lda(addr) => self.a = self.rd8(m, addr).await,
                Sta(addr) => self.w8(m, addr, self.a).await,
                Ldax(rp) => self.a = self.rd8(m, self.rp(rp)).await,
                Stax(rp) => self.w8(m, self.rp(rp), self.a).await,
                Lhld(addr) => {
                    cycles!(6); // ???
                    self.hl.set_word(self.rd16(m, addr).await);
                },
                Shld(addr) => {
                    cycles!(6); // ???
                    self.w16(m, addr, self.hl.word()).await;
                },
                Sphl => { cycles!(1); self.set_rp(RegPair::Sp, self.rp(RegPair::Hl)); },
                Push(rp) => {
                    cycles!(1);
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
                    cycles!(2);
                    let new = self.rd16(m, self.sp).await;
                    self.w16(m, self.sp, self.hl.word()).await;
                    self.hl.set_word(new);
                },
                Pchl => { cycles!(1); set_pc!(self.hl.word()); },
                Xri(val) => self.xor(self.a, val),
                Xra(reg) => self.xor(self.a, self.reg(m, reg)),
                Ori(val) => self.or(self.a, val),
                Ora(reg) => self.or(self.a, self.reg(m, reg)),
                Ani(val) => self.and(self.a, val),
                Ana(reg) => self.and(self.a, self.reg(m, reg)),
                Add(reg) => self.a = self.add(false, self.a, self.reg(m, reg)),
                Adc(reg) => self.a = self.add(self.f.carry, self.a, self.reg(m, reg)),
                Adi(val) => self.a = self.add(false, self.a, val),
                Aci(val) => self.a = self.add(self.f.carry, self.a, val),
                Sub(reg) => self.a = self.sub(false, self.a, self.reg(m, reg)),
                Sbb(reg) => self.a = self.sub(self.f.carry, self.a, self.reg(m, reg)),
                Sui(val) => self.a = self.sub(false, self.a, val),
                Sbi(val) => self.a = self.sub(self.f.carry, self.a, val),
                Cmp(reg) => { self.sub(false, self.a, self.reg(m, reg)); },
                Cpi(val) => { self.sub(false, self.a, val); },
                Dad(rp) => {
                    cycles!(6);
                    let result = self.hl.word() as usize + self.rp(rp) as usize;
                    self.f.carry = result > 0xffff;
                    self.hl.set_word(result as u16);
                },
                Stc => self.f.carry = true,
                Cmc => self.f.carry = !self.f.carry,
                Cma => self.a = !self.a,
                // i wonder if i can make this into a LUT
                Daa => {
                    // HEAVILY inspired by https://github.com/GunshipPenguin/lib8080/blob/master/src/i8080.c#L405
                    let mut add = 0;

                    if self.f.half_carry || self.a & 0x0f > 9 {
                        add = 0x06
                    }

                    if self.f.carry || (self.a & 0xf0) > 0x90
                        || ((self.a & 0xf0) >= 0x90 && self.a & 0x0F > 9)
                    {
                        add |= 0x60;
                        self.f.carry = true;
                    }
                    self.f.half_carry = (self.a & 0x0F) + (add & 0x0F) > 0x0F;
                    self.a = self.update_zsp(self.a.wrapping_add(add));
                },
                Dcr(reg) => {
                    if reg != Reg::M { cycles!(1); }
                    let v = self.r(m, reg).await.wrapping_sub(1);
                    self.sr(m, reg, v).await;
                    self.update_zsp(self.reg(m, reg));
                    self.f.half_carry = self.reg(m, reg) & 0xf != 0xf;
                },
                Inr(reg) => {
                    if reg != Reg::M { cycles!(1); }
                    let v = self.r(m, reg).await.wrapping_add(1);
                    self.sr(m, reg, v).await;
                    self.update_zsp(self.reg(m, reg));
                    self.f.half_carry = self.reg(m, reg) & 0xf == 0x00;
                },
                Inx(rp) => { cycles!(1); self.set_rp(rp, self.rp(rp).wrapping_add(1))},
                Dcx(rp) => { cycles!(1); self.set_rp(rp, self.rp(rp).wrapping_sub(1))},
                Rrc => {
                    self.a = RRC_LUT[self.a as usize];
                    //self.a = self.a.rotate_right(1);
                    self.f.carry = self.a & 0x80 != 0;
                },
                Rlc => {
                    self.a = RLC_LUT[self.a as usize];
                    //self.a = self.a.rotate_left(1);
                    self.f.carry = self.a & 0x01 != 0;
                },
                Rar => {
                    let carry = self.a & 0x1 != 0;
                    self.a = (self.a >> 1) | (self.f.carry as u8 * 0x80);
                    self.f.carry = carry;
                },
                Ral => {
                    let carry = self.a & 0x80 != 0;
                    self.a = (self.a << 1) | (self.f.carry as u8);
                    self.f.carry = carry;
                },
                Di | Ei | Nop => (),
                In(addr) => self.a = io.read8(addr),
                Out(addr) => io.write8(addr, self.a),
                Hlt => set_pc!(self.pc - 1),
            }
        }
    }
}

static RLC_LUT: [u8; 0x100] = make_rlc_lut();
static RRC_LUT: [u8; 0x100] = make_rrc_lut();

const fn make_rrc_lut() -> [u8; 0x100] {
    let mut lut = [0u8; 0x100];
    let mut v = 0usize;
    while v < 0x100 {
        lut[v] = (v as u8).rotate_right(1);
        v += 1;
    }
    lut
}

const fn make_rlc_lut() -> [u8; 0x100] {
    let mut lut = [0u8; 0x100];
    let mut v = 0usize;
    while v < 0x100 {
        lut[v] = (v as u8).rotate_left(1);
        v += 1;
    }
    lut
}
