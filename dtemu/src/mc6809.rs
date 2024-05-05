use bitfield_struct::bitfield;
use futures::pending;
use crate::MemoryWrapper;
use parking_lot::Mutex;
use std::sync::atomic::{AtomicBool, Ordering};
use crossbeam_channel::Sender;
    
use asm_playground::mc6809::{Opcode, DatalessInstruction, Condition, Addressing, IndexedFlavor, IndexRegister, StackRegister, IntoRegister8, IntoRegister16, Register8, Register16};
use crate::cpu::{Cpu, CpuRegs, DisasmFn};

struct Mc6809Stuff<'a> {
    mem: MemoryWrapper,
    cached_state: &'a Mutex<Mc6809State>,
    instruction_done: &'a AtomicBool,
    trace_tx: &'a Mutex<Option<Sender<String>>>
}

#[bitfield(u8, order = Msb)]
struct StackSet {
    pc: bool,
    su: bool,
    y: bool,
    x: bool,
    dp: bool,
    b: bool,
    a: bool,
    cc: bool,
}

impl StackSet {
    fn entire() -> Self {
        0xff.into()
    }
}

#[bitfield(u8, order = Msb)]
struct ConditionCode {
    entire: bool,
    firq_enable: bool,
    half_carry: bool,
    irq_enable: bool,
    negative: bool,
    zero: bool,
    overflow: bool,
    carry: bool,
}

#[derive(Debug, Clone)]
pub struct Mc6809State {
    a: u8,
    b: u8,
    x: u16,
    y: u16,
    u: u16,
    s: u16,
    pc: u16,
    dp: u8,
    i: DatalessInstruction,
    cc: ConditionCode,
}

impl CpuRegs for Mc6809State {
    fn next_instruction(&self) -> u32 { self.pc as u32 }
    fn stack_bot(&self) -> u32 { self.s.wrapping_sub(0x20) as u32 }
    fn stack_top(&self) -> u32 { self.s as u32 }
}

pub struct Mc6809 {
    state: Mutex<Mc6809State>,
    cached_state: Mutex<Mc6809State>,
    instruction_done: AtomicBool,
    mem: MemoryWrapper,
    tx: Mutex<Option<Sender<String>>>,
}

impl<'me> Cpu<'me> for Mc6809 {
    fn new(mem: MemoryWrapper) -> Box<Self> {
        Box::new(Self {
            state: Mutex::new(Mc6809State::new()),
            cached_state: Mutex::new(Mc6809State::new()),
            instruction_done: AtomicBool::new(false),
            mem,
            tx: Mutex::new(None),
        })
    }
    fn tick(&'me self) -> std::pin::Pin<Box<dyn futures::Future<Output = ()> + 'me>> {
        let stuff = Mc6809Stuff {
            mem: self.mem.clone(),
            cached_state: &self.cached_state,
            instruction_done: &self.instruction_done,
            trace_tx: &self.tx
        };
        Box::pin(async { self.state.lock().run(stuff).await })
    }
    fn disasm_fn(&self) -> &'static DisasmFn {
        &{|b, _| format!("{:x?}", DatalessInstruction::from_bytes(b)) }
    }
    fn instruction_done(&self) -> bool {
        self.instruction_done.load(Ordering::SeqCst)
    }
    fn regs(&self) -> &Mutex<dyn CpuRegs + Send + Sync> {
        &self.cached_state
    }
    fn trace_start(&self, tx: Sender<String>) {
        *self.tx.lock() = Some(tx);
    }
    fn trace_end(&self) {
        *self.tx.lock() = None;
    }
}

enum Vector {
    Swi3 = 0xfff2,
    Swi2 = 0xfff4,
    Firq = 0xfff6,
    Irq = 0xfff8,
    Swi = 0xfffa,
    Nmi = 0xfffc,
    Reset = 0xfffe,
}

impl Mc6809State {
    fn idxreg(&self, reg: IndexRegister) -> u16 {
        match reg {
            IndexRegister::X => self.x,
            IndexRegister::Y => self.y,
            IndexRegister::S => self.s,
            IndexRegister::U => self.u,
        }
    }
    fn setidxreg(&mut self, reg: IndexRegister, val: u16) {
        match reg {
            IndexRegister::X => self.x = val,
            IndexRegister::Y => self.y = val,
            IndexRegister::S => self.s = val,
            IndexRegister::U => self.u = val,
        }
    }
    fn reg8_ref_mut<R: IntoRegister8>(&mut self, reg: R) -> &mut u8 {
        match reg.to_reg() {
            Register8::A => &mut self.a,
            Register8::B => &mut self.b,
            _ => todo!(),
        }
    }
    fn reg8<R: IntoRegister8>(&self, reg: R) -> u8 {
        match reg.to_reg() {
            Register8::A => self.a,
            Register8::B => self.b,
            Register8::Cc => self.cc.into(),
            _ => todo!(),
        }
    }
    fn set_reg16<R: IntoRegister16>(&mut self, reg: R, val: u16) {
        match reg.to_reg() {
            Register16::S => self.s = val,
            Register16::U => self.u = val,
            Register16::X => self.x = val,
            Register16::Y => self.y = val,
            Register16::D => [self.b, self.a] = val.to_le_bytes(),
            Register16::Pc => self.pc = val,
        }
    }
    fn reg16<R: IntoRegister16>(&self, reg: R) -> u16 {
        match reg.to_reg() {
            Register16::S => self.s,
            Register16::U => self.u,
            Register16::X => self.x,
            Register16::Y => self.y,
            Register16::D => u16::from_le_bytes([self.b, self.a]),
            Register16::Pc => self.pc,
        }
    }
    fn new() -> Self {
        Self {
            a: 0,
            b: 0,
            x: 0,
            y: 0,
            u: 0,
            s: 0,
            pc: 0,
            dp: 0,
            i: DatalessInstruction::from_bytes(&[0x12]).unwrap(), // NOP
            cc: ConditionCode::new(),
        }
    }
    
    async fn operand16(&mut self, a: Addressing, m: &MemoryWrapper) -> u16 {
        let opaddr = self.pc as u32 + 1;
        match a {
            Addressing::AccumulatorA => panic!(),
            Addressing::AccumulatorB => panic!(),
            _ => m.fetch16_be(self.calc_addr(m, a).await as u32).await,
        }
    }

    async fn operand8(&mut self, a: Addressing, m: &MemoryWrapper) -> u8 {
        let opaddr = self.pc as u32 + 1;
        match a {
            Addressing::AccumulatorA => self.a,
            Addressing::AccumulatorB => self.b,
            _ => {
                let addr = self.calc_addr(m, a).await;
                let tmp = m.fetch8(addr as u32).await;
                tmp
            }
        }
    }

    async fn cmp(&mut self, op1: u8, op2: u8) {
        self.sub(op1, op2, false).await;
    }

    async fn cmp16(&mut self, op1: u16, op2: u16) {
        self.sub16(op1, op2, false).await;
    }

    async fn sub(&mut self, op1: u8, op2: u8, carry: bool) -> u8 {
        let carry = (self.cc.carry() && carry) as u8;
        let result = op1.wrapping_sub(op2).wrapping_sub(carry);
        self.cc.set_carry(op1 < op2 + carry);
        self.cc.set_zero(result == 0);
        self.cc.set_negative(result & 0x80 != 0);
        result
    }

    async fn sub16(&mut self, op1: u16, op2: u16, carry: bool) -> u16 {
        self.add16(op1, (op2 ^ 0xffff).wrapping_add(1), carry).await
    }

    async fn add(&mut self, op1: u8, op2: u8, carry: bool) -> u8 {
        let carry = self.cc.carry() && carry;
        let result = op1 as u16 + op2 as u16 + carry as u16;
        self.cc.set_carry(result > 0xff);
        self.cc.set_negative(result & 0x80 != 0);
        self.cc.set_zero(result & 0xff == 0);
        self.cc.set_overflow((!(op1 ^ op2) & (op1 ^ (result & 0xff) as u8) & 0x80) != 0);
        self.cc.set_half_carry((op1 & 0xf + op2 & 0xf + carry as u8) > 0xf);
        result as u8
    }

    async fn add16(&mut self, op1: u16, op2: u16, carry: bool) -> u16 {
        let carry = self.cc.carry() && carry;
        let result = op1 as u32 + op2 as u32 + carry as u32;
        self.cc.set_carry(result > 0xffff);
        self.cc.set_negative(result & 0x8000 != 0);
        self.cc.set_zero(result & 0xffff == 0);
        self.cc.set_overflow((!(op1 ^ op2) & (op1 ^ (result & 0xffff) as u16) & 0x8000) != 0);
        result as u16
    }

    fn update_zn16(&mut self, val: u16) -> u16 {
        self.cc.set_zero(val == 0);
        self.cc.set_negative((val as i16) < 0);
        val
    }

    fn update_nzv16(&mut self, val: u16) -> u16 {
        self.cc.set_zero(val == 0);
        self.cc.set_negative(val & 0x8000 != 0);
        self.cc.set_overflow(false);
        val
    }

    fn update_nzv(&mut self, val: u8) -> u8 {
        self.cc.set_zero(val == 0);
        self.cc.set_negative(val & 0x80 != 0);
        self.cc.set_overflow(false);
        val
    }

    fn d(&self) -> u16 {
        u16::from_le_bytes([self.b, self.a])
    }

    fn set_d(&mut self, v: u16) {
        let [lo, hi] = v.to_le_bytes();
        self.a = hi;
        self.b = lo;
    }

    async fn pull8(&mut self, m: &MemoryWrapper, s: StackRegister) -> u8 {
        match s {
            StackRegister::S => {
                let val = m.fetch8(self.s as u32).await;
                self.s = self.s.wrapping_add(1);
                eprintln!("pull {:x}\r", val);
                val
            },
            StackRegister::U => {
                let val = m.fetch8(self.u as u32).await;
                self.u = self.u.wrapping_add(1);
                val
            },
        }
    }

    async fn push8(&mut self, m: &MemoryWrapper, s: StackRegister, val: u8) {
        match s {
            StackRegister::S => {
                self.s = self.s.wrapping_sub(1);
                eprintln!("push {:x}\r", val);
                m.set8(self.s as u32, val).await;
            },
            StackRegister::U => {
                self.u = self.u.wrapping_sub(1);
                m.set8(self.u as u32, val).await;
            },
        }
    }

    async fn pull16(&mut self, m: &MemoryWrapper, s: StackRegister) -> u16 {
        let hi = self.pull8(m, s).await;
        let lo = self.pull8(m, s).await;
        u16::from_le_bytes([lo, hi])
    }

    async fn push16(&mut self, m: &MemoryWrapper, s: StackRegister, val: u16) {
        let [lo, hi] = val.to_le_bytes();
        self.push8(m, s, lo).await;
        self.push8(m, s, hi).await;
    }

    async fn pullset(&mut self, m: &MemoryWrapper, s: StackSet, sr: StackRegister) {
        if s.cc() { self.cc = self.pull8(&m, sr).await.into() }
        if s.a() { self.a = self.pull8(&m, sr).await }
        if s.b() { self.b = self.pull8(&m, sr).await }
        if s.dp() { self.dp = self.pull8(&m, sr).await }
        if s.x() { self.x = self.pull16(&m, sr).await }
        if s.y() { self.y = self.pull16(&m, sr).await }
        if s.su() {
            match sr {
                StackRegister::U => self.s = self.pull16(&m, sr).await,
                StackRegister::S => self.u = self.pull16(&m, sr).await,
            }
        }
        if s.pc() { self.pc = self.pull16(&m, sr).await - self.i.unprefixed_len() as u16 }
    }

    async fn pushset(&mut self, m: &MemoryWrapper, s: StackSet, sr: StackRegister) {
        if s.pc() { self.push16(&m, sr, self.pc + self.i.unprefixed_len() as u16).await }
        if s.su() { self.push16(&m, sr, self.u).await }
        if s.su() {
            match sr {
                StackRegister::U => self.push16(&m, sr, self.s).await,
                StackRegister::S => self.push16(&m, sr, self.u).await,
            }
        }
        if s.y() { self.push16(&m, sr, self.y).await }
        if s.x() { self.push16(&m, sr, self.x).await }
        if s.dp() { self.push8(&m, sr, self.dp).await }
        if s.b() { self.push8(&m, sr, self.b).await }
        if s.a() { self.push8(&m, sr, self.a).await }
        if s.cc() { self.push8(&m, sr, self.cc.into()).await }
    }

    async fn vector(&self, m: &MemoryWrapper, v: Vector) -> u16 {
        m.fetch16_be(v as u32).await
    }

    async fn branch(&self, m: &MemoryWrapper, cond: bool) -> i16 {
        if cond {
            // force rust to sign-extend
            (m.fetch8(self.pc as u32 + 1).await as i8) as i16
        } else {
            0
        }
    }

    fn has_cond(&self, condition: Condition) -> bool {
        match condition {
            Condition::Always => true,
            Condition::Never => false,
            Condition::Higher => !self.cc.zero() && !self.cc.carry(),
            Condition::LowerOrSame => self.cc.zero() || self.cc.carry(),
            Condition::CarryClear => !self.cc.carry(),
            Condition::CarrySet => self.cc.carry(),
            Condition::NotEqual => !self.cc.zero(),
            Condition::Equal => self.cc.zero(),
            Condition::OverflowClear => !self.cc.overflow(),
            Condition::OverflowSet => self.cc.overflow(),
            Condition::Plus => !self.cc.negative(),
            Condition::Minus => self.cc.negative(),
            Condition::GreaterOrEqual => self.cc.negative() == self.cc.overflow(),
            Condition::LessThan => self.cc.negative() != self.cc.overflow(),
            Condition::Greater => (self.cc.negative() == self.cc.overflow()) && !self.cc.zero(),
            Condition::LessOrEqual => (self.cc.negative() != self.cc.overflow()) || self.cc.zero(),
        }
    }

    async fn run(&mut self, stuff: Mc6809Stuff<'_>) {
        let Mc6809Stuff {
            mut mem,
            cached_state: other_state,
            instruction_done: inst_done,
            trace_tx: _trace_tx,
        } = stuff;

        self.pc = self.vector(&mem, Vector::Reset).await;
        loop {
            let mut jump: Option<u16> = None;
            let mut branch: i16 = 0;
            use Opcode::*;
            let pc = self.pc as u32;
            println!("pc = {:x}\r", pc);
            let bytes = [
                mem.fetch8_fast(pc), mem.fetch8_fast(pc + 1),
                mem.fetch8_fast(pc + 2), mem.fetch8_fast(pc + 3)];
            //println!("decoding {:x?}\r", bytes);
            self.i = DatalessInstruction::from_bytes(&bytes)
                .expect("Failed to decode instruction");
            let inst = self.i.clone();
            inst_done.store(false, Ordering::SeqCst);
            if self.i.has_prefix() {
                self.pc += 1;
            }
            println!("running {:?}, len {:x}\r\n", self.i, self.i.unprefixed_len());
            match self.i.opcode() {
                // Load/Store
                Ld(reg) => {
                    *self.reg8_ref_mut(reg) = self.operand8(inst.addressing(), &mem).await;
                    self.update_nzv(self.reg8(reg));
                },
                Ld16(reg) => {
                    let op = self.operand16(inst.addressing(), &mem).await;
                    self.set_reg16(reg, op);
                    self.update_nzv16(op);
                }
                St(reg) => {
                    self.update_nzv(self.reg8(reg));
                    self.store8(&mem, inst.addressing(), self.reg8(reg)).await
                },
                St16(reg) => {
                    self.update_nzv16(self.reg16(reg));
                    self.store16(&mem, inst.addressing(), self.reg16(reg)).await;
                },
                Clr => {
                    self.cc.set_carry(false);
                    self.update_nzv(0);
                    self.store8(&mem, inst.addressing(), 0).await;
                },
                Lea(reg) => {
                    let val = self.calc_addr(&mem, inst.addressing()).await;
                    if reg == IndexRegister::X || reg == IndexRegister::Y {
                        self.cc.set_zero(val == 0);
                    }
                    self.set_reg16(reg, val);
                },

                // ALU ops
                Cmp(reg) => {
                    let v = self.operand8(inst.addressing(), &mem).await;
                    self.cmp(self.reg8(reg), v).await;
                },
                Cmp16(reg) => {
                    let v = self.operand16(inst.addressing(), &mem).await;
                    self.cmp16(self.reg16(reg), v).await;
                },
                Sub(reg) => {
                    let v = self.operand8(inst.addressing(), &mem).await;
                    *self.reg8_ref_mut(reg) = self.sub(self.reg8(reg), v, false).await;
                },
                Add(reg) => {
                    let v = self.operand8(inst.addressing(), &mem).await;
                    *self.reg8_ref_mut(reg) = self.add(self.reg8(reg), v, false).await;
                },
                Adc(reg) => {
                    let v = self.operand8(inst.addressing(), &mem).await;
                    *self.reg8_ref_mut(reg) = self.add(self.reg8(reg), v, true).await;
                },
                Addd => {
                    let v = self.operand16(inst.addressing(), &mem).await;
                    let res = self.add16(self.d(), v, false).await;
                    self.set_d(res);
                },
                Subd => {
                    let v = self.operand16(inst.addressing(), &mem).await;
                    let res = self.sub16(self.d(), v, false).await;
                    self.set_d(res);
                },
                Abx => self.x = self.x.wrapping_add(self.b as u16),
                Dec => {
                    let mut v = self.operand8(inst.addressing(), &mem).await;
                    v = v.wrapping_sub(1);
                    self.update_nzv(v);
                    self.cc.set_overflow(v == 0x7f);
                    self.store8(&mem, inst.addressing(), v).await;
                },
                Inc => {
                    let mut v = self.operand8(inst.addressing(), &mem).await;
                    v = v.wrapping_add(1);
                    self.update_nzv(v);
                    self.cc.set_overflow(v == 0x80);
                    self.store8(&mem, inst.addressing(), v).await;
                },
                Com => {
                    let tmp = self.operand8(inst.addressing(), &mem).await ^ 0xff;
                    self.update_nzv(tmp);
                    self.cc.set_overflow(false);
                    self.cc.set_carry(true);
                    self.store8(&mem, inst.addressing(), tmp).await;
                },
                Neg => {
                    panic!("{:#x?}", self);
                    let mut tmp = self.operand8(inst.addressing(), &mem).await;
                    tmp = (tmp ^ 0xff).wrapping_add(1);
                    self.store8(&mem, inst.addressing(), tmp).await;
                },


                // branches and jumps
                B(cond) => branch = self.branch(&mem, self.has_cond(cond)).await,
                Rts => {
                    jump = Some(self.pull16(&mem, StackRegister::S).await);
                },
                Jsr => {
                    self.push16(&mem, StackRegister::S, self.pc + inst.unprefixed_len() as u16).await;
                    let val = self.calc_addr(&mem, inst.addressing()).await;
                    jump = Some(val);
                },
                Bsr => {
                    self.push16(&mem, StackRegister::S, self.pc + inst.unprefixed_len() as u16).await;
                    branch = (mem.fetch8(self.pc as u32 + 1).await as i8) as i16;
                },
                Jmp => {
                    let val = self.calc_addr(&mem, inst.addressing()).await;
                    //println!("and off we go to {:x}\r", val);
                    jump = Some(val);
                },

                Tfr => {
                    let v = self.operand8(inst.addressing(), &mem).await;
                    let input = match v & 0xf0 {
                        0x00 => self.d(),
                        0x10 => self.x,
                        0x20 => self.y,
                        0x30 => self.u,
                        0x40 => self.s,
                        0x50 => self.pc,
                        0x80 => self.a as u16 | 0xff00,
                        0x90 => self.b as u16 | 0xff00,
                        0xa0 => self.cc.into_bits() as u16 | (self.cc.into_bits() as u16) << 8,
                        0xb0 => self.dp as u16 | (self.dp as u16) << 8,
                        _ => { eprintln!("invalid postbyte for tfr: {:x}", v); 0xff },
                    };
                    match v & 0x0f {
                        0x00 => self.set_d(input),
                        0x01 => self.x = input,
                        0x02 => self.y = input,
                        0x03 => self.u = input,
                        0x04 => self.s = input,
                        0x05 => jump = Some(input),
                        0x0a => self.cc = (input as u8).into(),
                        0x0b => self.dp = (input & 0xff) as u8,
                        _ => eprintln!("invalid postbyte for tfr: {:x}", v),
                    };
                },
                Pul(s) => {
                    let val: StackSet = self.operand8(inst.addressing(), &mem).await.into();
                    self.pullset(&mem, val, s).await;
                },
                Psh(s) => {
                    let val: StackSet = self.operand8(inst.addressing(), &mem).await.into();
                    self.pushset(&mem, val, s).await;
                },
                Swi | Swi2 | Swi3 => {
                    self.cc.set_entire(true);
                    self.pushset(&mem, StackSet::entire(), StackRegister::S).await;
                    let vec = match inst.opcode() {
                        Swi => {
                            self.cc.set_irq_enable(true);
                            self.cc.set_firq_enable(true);
                            Vector::Swi
                        },
                        Swi2 => Vector::Swi2,
                        Swi3 => Vector::Swi3,
                        _ => unreachable!(),
                    };
                    jump = Some(self.vector(&mem, vec).await);
                },
                Bit(reg) => {
                    let val = self.reg8(reg) & self.operand8(inst.addressing(), &mem).await;
                    self.update_nzv(val);
                },
                And(reg) => {
                    let val = self.reg8(reg) & self.operand8(inst.addressing(), &mem).await;
                    *self.reg8_ref_mut(reg) = self.update_nzv(val);
                },
                Or(reg) => {
                    let val = self.reg8(reg) | self.operand8(inst.addressing(), &mem).await;
                    *self.reg8_ref_mut(reg) = self.update_nzv(val);
                },
                Eor(reg) => {
                    let val = self.reg8(reg) ^ self.operand8(inst.addressing(), &mem).await;
                    *self.reg8_ref_mut(reg) = self.update_nzv(val);
                },
                Rol => {
                    let mut val = self.operand8(inst.addressing(), &mem).await;
                    let carry = val & 0x80 != 0;
                    let overflow = (val & 0x80 != 0) ^ (val & 0x40 != 0);
                    val = (val << 1) | self.cc.carry() as u8;
                    self.cc.set_carry(carry);
                    self.update_nzv(val);
                    self.cc.set_overflow(overflow);
                    self.store8(&mem, inst.addressing(), val).await;
                },
                Ror => {
                    let mut val = self.operand8(inst.addressing(), &mem).await;
                    let carry = val & 0x01 != 0;
                    val = (val >> 1) | (self.cc.carry() as u8) * 0x80;
                    self.cc.set_carry(carry);
                    self.update_nzv(val);
                    self.store8(&mem, inst.addressing(), val).await;
                },
                Orcc => {
                    self.cc = (self.cc.into_bits() | self.operand8(inst.addressing(), &mem).await).into();
                },
                Tst => {
                    let val = self.operand8(inst.addressing(), &mem).await;
                    self.update_nzv(val);
                },
                Asl => {
                    let mut val = self.operand8(inst.addressing(), &mem).await;
                    self.cc.set_carry(val & 0x80 != 0);
                    let overflow = (val & 0x80 != 0) ^ (val & 0x40 != 0);
                    val = self.update_nzv(val << 1);
                    self.cc.set_overflow(overflow);
                    self.store8(&mem, inst.addressing(), val).await;
                },
                Asr => {
                    let mut val = self.operand8(inst.addressing(), &mem).await;
                    self.cc.set_carry(val & 0x01 != 0);
                    val = (val >> 1) | val & 0x80;
                    self.cc.set_zero(val == 0);
                    self.cc.set_negative(val & 0x80 != 0);
                    self.store8(&mem, inst.addressing(), val).await;
                },
                Lsr => {
                    let mut val = self.operand8(inst.addressing(), &mem).await;
                    self.cc.set_carry(val & 0x01 != 0);
                    val = val >> 1;
                    self.cc.set_zero(val == 0);
                    self.cc.set_negative(false);
                    self.store8(&mem, inst.addressing(), val).await;
                },
                Sex => {
                    if self.b & 0x80 != 0 {
                        self.a = 0xff;
                        self.cc.set_negative(true);
                    } else {
                        self.a = 0;
                        self.cc.set_negative(false);
                    }
                    self.cc.set_zero(self.b == 0);
                },
                Nop => {},
                _ => todo!(),
            }
            inst_done.store(true, Ordering::SeqCst);
            other_state.lock().clone_from(self);
            pending!();
            self.pc += inst.unprefixed_len() as u16;
            if let Some(0) = jump { return; }
            if let Some(target) = jump {
                //eprintln!("jump to {:x}\r", target);
                self.pc = target;
            } else if branch != 0 {
                self.pc = (self.pc as i32 + branch as i32) as u16;
            }
        }
    }

    async fn store8(&mut self, m: &MemoryWrapper, a: Addressing, v: u8) {
        match a {
            Addressing::AccumulatorA => self.a = v,
            Addressing::AccumulatorB => self.b = v,
            _ => {
                let addr = self.calc_addr(m, a).await;
                m.set8(addr as u32, v).await
            },
        }
    }
    async fn store16(&mut self, m: &MemoryWrapper, a: Addressing, v: u16) {
        let [lo, hi] = v.to_le_bytes();
        m.set8(self.calc_addr(m, a).await as u32, hi).await;
        m.set8(self.calc_addr(m, a).await as u32 + 1, lo).await;
    }
    async fn calc_addr(&mut self, m: &MemoryWrapper, a: Addressing) -> u16 {
        match a {
            Addressing::Immediate16 | Addressing::Immediate8 |
                Addressing::Relative16 | Addressing::Relative8 => self.pc + 1,
            Addressing::Direct => u16::from_be_bytes([self.dp, m.fetch8(self.pc as u32 + 1).await]),
            Addressing::Extended => {
                let hi = m.fetch8(self.pc as u32 + 1).await;
                let lo = m.fetch8(self.pc as u32 + 2).await;
                u16::from_be_bytes([hi, lo])
            },
            Addressing::Indexed(data) => {
                let indirect = data.indirect();
                let addr;
                match data.flavor() {
                    IndexedFlavor::Increment1(reg) => {
                        addr = self.idxreg(reg);
                        self.setidxreg(reg, self.idxreg(reg).wrapping_add(1));
                    },
                    IndexedFlavor::Increment2(reg) => {
                        addr = self.idxreg(reg);
                        self.setidxreg(reg, self.idxreg(reg).wrapping_add(2));
                    },
                    IndexedFlavor::Decrement1(reg) => {
                        addr = self.idxreg(reg);
                        self.setidxreg(reg, self.idxreg(reg).wrapping_sub(1));
                    },
                    IndexedFlavor::Decrement2(reg) => {
                        addr = self.idxreg(reg);
                        self.setidxreg(reg, self.idxreg(reg).wrapping_sub(2));
                    },
                    IndexedFlavor::NoOffset(reg) => addr = self.idxreg(reg),
                    IndexedFlavor::Offset5(reg, off) => {
                        addr = (self.idxreg(reg) as i32 + off as i32) as u16;
                    },
                    IndexedFlavor::Offset8(reg, off) => {
                        addr = (self.idxreg(reg) as i32 + off as i32) as u16;
                    },
                    IndexedFlavor::Offset16(reg, off) => {
                        addr = (self.idxreg(reg) as i32 + off as i32) as u16;
                    },
                    IndexedFlavor::PcOffset8(off) => {
                        addr = (self.pc as i32 + off as i32) as u16;
                    },
                    IndexedFlavor::PcOffset16(off) => {
                        addr = ((self.pc as i32 + off as i32) as u16);
                        println!("pc off16 effective addr {:x}\r", addr);
                    },
                    IndexedFlavor::BOffset(reg) => {
                        addr = (self.b as i32 + self.reg16(reg) as i32) as u16;
                    },
                    f => todo!("Indexed flavor {:x?}", f),
                }
                if indirect {
                    m.fetch16_be(addr as u32).await
                } else {
                    addr
                }
            },
            other => todo!("Addressing {:x?}", other),
        }
    }
}
