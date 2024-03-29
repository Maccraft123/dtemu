use bitfield_struct::bitfield;
use futures::pending;
use crate::MemoryWrapper;
use parking_lot::Mutex;
use std::sync::mpsc;
use std::sync::atomic::{AtomicBool, Ordering};
    
use unasm::i8080::{self, Instruction, Condition, Reg, RegPair};
use crate::cpu::{Cpu, CpuRegs, DisasmFn};

struct Intel8080Stuff<'a> {
    mem: MemoryWrapper,
    cached_state: &'a Mutex<Intel8080State>,
    instruction_done: &'a AtomicBool,
    trace_tx: &'a Mutex<Option<mpsc::Sender<String>>>
}

#[bitfield(u8, order = Msb)]
struct ProgramStatusWord {
    sign: bool,
    zero: bool,
    _unused1: bool,
    aux_carry: bool,
    _unused2: bool,
    parity: bool,
    unused3: bool,
    carry: bool,
}

#[derive(Debug, Clone)]
pub struct Intel8080State {
    a: u8,
    b: u8,
    c: u8,
    d: u8,
    e: u8,
    h: u8,
    l: u8,
    sp: u16,
    pc: u16,
    // not an actual register!
    psw: ProgramStatusWord,
}

impl CpuRegs for Intel8080State {
    fn next_instruction(&self) -> u32 { self.pc as u32 }
    fn stack_bot(&self) -> u32 { self.sp.saturating_sub(0x20)as u32 }
    fn stack_top(&self) -> u32 { self.sp as u32 }
}

pub struct Intel8080 {
    state: Mutex<Intel8080State>,
    cached_state: Mutex<Intel8080State>,
    instruction_done: AtomicBool,
    mem: MemoryWrapper,
    tx: Mutex<Option<mpsc::Sender<String>>>,
}

impl<'me> Cpu<'me> for Intel8080 {
    fn new(mem: MemoryWrapper) -> Box<Self> {
        Box::new(Self {
            state: Mutex::new(Intel8080State::new()),
            cached_state: Mutex::new(Intel8080State::new()),
            instruction_done: AtomicBool::new(false),
            mem,
            tx: Mutex::new(None),
        })
    }
    fn tick(&'me self) -> std::pin::Pin<Box<dyn futures::Future<Output = ()> + 'me>> {
        let stuff = Intel8080Stuff {
            mem: self.mem.clone(),
            cached_state: &self.cached_state,
            instruction_done: &self.instruction_done,
            trace_tx: &self.tx
        };
        Box::pin(async { self.state.lock().run(stuff).await })
    }
    fn disasm_fn(&self) -> &'static DisasmFn {
        &{|bytes, _| format!("{:?}", unasm::i8080::Instruction::decode_from(bytes))}
    }
    fn instruction_done(&self) -> bool {
        self.instruction_done.load(Ordering::SeqCst)
    }
    fn regs(&self) -> &Mutex<dyn CpuRegs + Send + Sync> {
        &self.cached_state
    }
    fn trace_start(&self, tx: mpsc::Sender<String>) {
        *self.tx.lock() = Some(tx);
    }
    fn trace_end(&self) {
        *self.tx.lock() = None;
    }
}

#[derive(Copy, Clone, Eq, PartialEq)]
enum LogicalOp {
    Xor,
    Or,
    And,
}

#[derive(Copy, Clone, Eq, PartialEq)]
enum ArithOp {
    Add(bool),
    Sub(bool),
}

impl Intel8080State {
    fn logical_op(&mut self, op: LogicalOp, val1: u8, val2: u8) -> u8 {
        let ret = match op {
            LogicalOp::Xor => val1 ^ val2,
            LogicalOp::Or => val1 | val2,
            LogicalOp::And => val1 & val2,
        };
        self.update_zsp(ret);
        if op == LogicalOp::And {
            self.psw.set_aux_carry(((val1 | val2) & 0b1000) != 0);
        } else {
            self.psw.set_aux_carry(false);
        }
        self.psw.set_carry(false);

        ret
    }

    fn arith_op(&mut self, op: ArithOp, val1: u8, mut val2: u8) -> u8 {
        let ret = match op {
            ArithOp::Add(use_carry) => {
                let carry = use_carry as u8 & self.psw.carry() as u8;
                let val = val1 as u16 + val2 as u16 + carry as u16;
                self.psw.set_carry(val & 0xff00 != 0);
                self.psw.set_aux_carry(((val1 & 0x0f) + (val2 & 0x0f) + carry) > 0xf);
                (val & 0xff) as u8
            },
            ArithOp::Sub(use_carry) => {
                let carry = use_carry as u8 & self.psw.carry() as u8;
                val2 += carry;
                
                let val = val1.wrapping_sub(val2);
                self.psw.set_carry(val2 > val1);
                self.psw.set_aux_carry((val2 & 0x0f) <= (val1 & 0x0f));
                (val & 0xff) as u8
            },
        };
        self.update_zsp(ret);
        ret
    }

    fn new() -> Self {
        Self {
            a: 0,
            b: 0,
            c: 0,
            d: 0,
            e: 0,
            h: 0,
            l: 0,
            sp: 0x500,
            //pc: 0,
            pc: 0x100,
            psw: ProgramStatusWord::new().with_unused3(true),
        }
    }

    fn reg(&self, mem: &MemoryWrapper, r: Reg) -> u8 {
        use Reg::*;
        match r {
            A => self.a,
            B => self.b,
            C => self.c,
            D => self.d,
            E => self.e,
            H => self.h,
            L => self.l,
            M => mem.fetch8_fast(u16::from_le_bytes([self.l, self.h]) as u32),
        }
    }

    fn set_reg(&mut self, mem: &MemoryWrapper, r: Reg, val: u8) {
        use Reg::*;
        match r {
            A => self.a = val,
            B => self.b = val,
            C => self.c = val,
            D => self.d = val,
            E => self.e = val,
            H => self.h = val,
            L => self.l = val,
            M => mem.set8_fast(u16::from_le_bytes([self.l, self.h]) as u32, val),
        }
    }

    fn regpair(&self, mem: &MemoryWrapper, rp: RegPair) -> u16 {
        if rp == RegPair::Sp {
            self.sp
        } else {
            u16::from_le_bytes([self.reg(mem, rp.low()), self.reg(mem, rp.high())])
        }
    }

    fn set_regpair(&mut self, mem: &MemoryWrapper, rp: RegPair, val: u16) {
        if rp == RegPair::Sp {
            self.sp = val;
        } else {
            let [low, high] = u16::to_le_bytes(val);
            self.set_reg(mem, rp.high(), high);
            self.set_reg(mem, rp.low(), low);
        }
    }

    async fn push8(&mut self, mem: &MemoryWrapper, val: u8) {
        self.sp = self.sp.wrapping_sub(1);
        mem.set8(self.sp as u32, val).await;
    }

    async fn pop8(&mut self, mem: &MemoryWrapper) -> u8 {
        let val = mem.fetch8(self.sp as u32).await;
        self.sp = self.sp.wrapping_add(1);
        val
    }

    async fn pop16(&mut self, mem: &MemoryWrapper) -> u16 {
        let lo = self.pop8(mem).await;
        let hi = self.pop8(mem).await;
        u16::from_le_bytes([lo, hi])
    }

    async fn push16(&mut self, mem: &MemoryWrapper, val: u16) {
        let [lo, hi] = val.to_le_bytes();
        self.push8(mem, hi).await;
        self.push8(mem, lo).await;
    }

    async fn pop_rp(&mut self, mem: &MemoryWrapper, rp: RegPair) {
        if rp == RegPair::Sp {
            self.a = self.pop8(mem).await;
            self.psw = self.pop8(mem).await.into();
        } else {
            let val = self.pop16(mem).await;
            self.set_regpair(mem, rp, val);
        }
    }

    async fn push_rp(&mut self, mem: &MemoryWrapper, rp: RegPair) {
        if rp == RegPair::Sp {
            self.push8(mem, self.psw.into_bits()).await;
            self.push8(mem, self.a).await;
        } else {
            self.push16(mem, self.regpair(mem, rp)).await;
        }
    }

    fn update_zsp(&mut self, val: u8) -> u8 {
        self.psw.set_zero(val == 0);
        self.psw.set_sign(val & 0x80 != 0);
        self.psw.set_parity(val.count_ones() % 2 == 0);
        val
    }

    fn has_cond(&self, cond: Condition) -> bool {
        match cond {
            Condition::NonZero => !self.psw.zero(),
            Condition::Zero => self.psw.zero(),
            Condition::NoCarry => !self.psw.carry(),
            Condition::Carry => self.psw.carry(),
            Condition::Odd => !self.psw.parity(),
            Condition::Even => self.psw.parity(),
            Condition::Plus => !self.psw.sign(),
            Condition::Minus => self.psw.sign(),
        }
    }

    async fn run(&mut self, stuff: Intel8080Stuff<'_>) {
        let Intel8080Stuff {
            mut mem,
            cached_state: other_state,
            instruction_done: inst_done,
            trace_tx,
        } = stuff;
        loop {
            let mut jump: Option<u16> = None;
            use Instruction::*;
            let pc = self.pc as u32;
            let bytes = [mem.fetch8_fast(pc), mem.fetch8_fast(pc + 1),mem.fetch8_fast(pc + 2)];
            let inst = Instruction::decode_from(&bytes);

            use std::io::Write;
            //println!("AF: {:02x}{:02x}, BC: {:04x}, DE: {:04x}, HL: {:04x}, SP: {:04x}\r",
            //    self.a, self.psw.into_bits(), self.regpair(&mem, RegPair::Bc), self.regpair(&mem, RegPair::De),
            //    self.regpair(&mem, RegPair::Hl), self.sp);
            //println!("about to run {:04x}={:x?}\r", pc, inst);
            //if pc == 0x352 { return }
            match inst {
                Jmp(addr) => jump = Some(addr),
                Call(addr) => {
                    if addr != 5 {
                        self.push16(&mut mem, self.pc + inst.len() as u16).await;
                        jump = Some(addr);
                    } else {
                        // i REALLY should think something better to replace this, oh well
                        match self.c {
                            9 => {
                                let mut addr = self.regpair(&mem, RegPair::De);
                                let mut string = String::new();
                                loop {
                                    let ch = mem.fetch8_fast(addr as u32);
                                    if ch == b'$' {
                                        break
                                    } else {
                                        string.push(ch as char);
                                    }
                                    addr += 1;
                                }
                                print!("{}", string);
                                std::io::stdout().flush().unwrap();
                            },
                            2 => {
                                print!("{}", self.e as char);
                                std::io::stdout().flush().unwrap();
                            },
                            other => unimplemented!("CP/M BDOS call {:x}\r", other),
                        }
                    }
                },
                Xchg => {
                    let tmp = self.regpair(&mem, RegPair::De);
                    self.set_regpair(&mem, RegPair::De, self.regpair(&mem, RegPair::Hl));
                    self.set_regpair(&mem, RegPair::Hl, tmp);
                },
                Mov(dst, src) => self.set_reg(&mem, dst, self.reg(&mem, src)),
                Sta(addr) => mem.set8(addr as u32, self.a).await,
                Lda(addr) => self.a = mem.fetch8(addr as u32).await,
                Ldax(rp) => self.a = mem.fetch8(self.regpair(&mem, rp) as u32).await,
                Stax(rp) => mem.set8(self.regpair(&mem, rp) as u32, self.a).await,
                Lhld(addr) => {
                    self.l = mem.fetch8(addr as u32).await;
                    self.h = mem.fetch8((addr + 1) as u32).await;
                },
                Shld(addr) => {
                    mem.set8(addr as u32, self.l).await;
                    mem.set8((addr + 1) as u32, self.h).await;
                },
                Mvi(r, val) => self.set_reg(&mem, r, val),
                Push(rp) => self.push_rp(&mut mem, rp).await,
                Pop(rp) => self.pop_rp(&mem, rp).await,
                Lxi(rp, val) => self.set_regpair(&mem, rp, val),
                Sphl => self.set_regpair(&mem, RegPair::Sp, self.regpair(&mem, RegPair::Hl)),
                Xthl => {
                    let new_l = mem.fetch8(self.sp as u32).await;
                    let new_h = mem.fetch8(self.sp as u32 + 1).await;
                    mem.set8(self.sp as u32, self.l).await;
                    mem.set8(self.sp as u32 + 1, self.h).await;
                    self.l = new_l;
                    self.h = new_h;
                },
                Pchl => jump = Some(self.regpair(&mem, RegPair::Hl)),
                Xri(val) => self.a = self.logical_op(LogicalOp::Xor, self.a, val),
                Xra(reg) => self.a = self.logical_op(LogicalOp::Xor, self.a, self.reg(&mem, reg)),
                Ori(val) => self.a = self.logical_op(LogicalOp::Or, self.a, val),
                Ora(reg) => self.a = self.logical_op(LogicalOp::Or, self.a, self.reg(&mem, reg)),
                Ani(val) => self.a = self.logical_op(LogicalOp::And, self.a, val),
                Ana(reg) => self.a = self.logical_op(LogicalOp::And, self.a, self.reg(&mem, reg)),
                Add(reg) => self.a = self.arith_op(ArithOp::Add(false), self.a, self.reg(&mem, reg)),
                Adc(reg) => self.a = self.arith_op(ArithOp::Add(true), self.a, self.reg(&mem, reg)),
                Sub(reg) => self.a = self.arith_op(ArithOp::Sub(false), self.a, self.reg(&mem, reg)),
                Adi(val) => self.a = self.arith_op(ArithOp::Add(false), self.a, val),
                Aci(val) => self.a = self.arith_op(ArithOp::Add(true), self.a, val),
                Sbb(reg) => self.a = self.arith_op(ArithOp::Sub(true), self.a, self.reg(&mem, reg)),
                Sui(val) => self.a = self.arith_op(ArithOp::Sub(false), self.a, val),
                Sbi(val) => self.a = self.arith_op(ArithOp::Sub(true), self.a, val),
                Dad(rp) => {
                    let (result, carry) = self.regpair(&mem, RegPair::Hl).overflowing_add(self.regpair(&mem, rp));
                    self.psw.set_carry(carry);
                    self.set_regpair(&mem, RegPair::Hl, result);
                },
                Cpi(val) => { self.arith_op(ArithOp::Sub(false), self.a, val); },
                Cmp(reg) => { self.arith_op(ArithOp::Sub(false), self.a, self.reg(&mem, reg)); },
                Ret => jump = Some(self.pop16(&mem).await),
                Stc => self.psw.set_carry(true),
                Cmc => self.psw.set_carry(!self.psw.carry()),
                Cma => self.a = !self.a,
                Daa => {
                    // stolen straight from https://github.com/GunshipPenguin/lib8080/blob/master/src/i8080.c#L405
                    let mut add = 0x00;
                    if self.a & 0x0f > 0x09 || self.psw.aux_carry() {
                        add |= 0x06;
                        self.psw.set_aux_carry(false);
                    }
                    if self.a & 0xf0 > 0x90 || (self.a & 0xf0 >= 0x90 && self.a & 0x0f > 9) || self.psw.carry() {
                        add |= 0x60;
                        self.psw.set_carry(true);
                    }
                    self.a += add;
                    self.update_zsp(self.a);
                },
                R(cond) => {
                    if self.has_cond(cond) {
                        jump = Some(self.pop16(&mem).await);
                    }
                },
                Dcr(reg) => {
                    self.set_reg(&mem, reg, self.reg(&mem, reg).wrapping_sub(1));
                    self.update_zsp(self.reg(&mem, reg));
                    self.psw.set_aux_carry(!((self.a & 0xf) == 0xf)); // whaaaa
                },
                Inr(reg) => {
                    self.psw.set_aux_carry(self.a & 0xf == 0xf);
                    self.set_reg(&mem, reg, self.reg(&mem, reg).wrapping_add(1));
                    self.update_zsp(self.reg(&mem, reg));
                },
                Inx(rp) => self.set_regpair(&mem, rp, self.regpair(&mem, rp).wrapping_add(1)),
                Dcx(rp) => self.set_regpair(&mem, rp, self.regpair(&mem, rp).wrapping_sub(1)),
                Rrc => {
                    let carry = self.a & 0x1 != 0;
                    self.a = (self.a >> 1) | (carry as u8 * 0x80);
                    self.psw.set_carry(carry);
                },
                Rlc => {
                    let carry = self.a & 0x80 != 0;
                    self.a = (self.a << 1) | (carry as u8 * 0x01);
                    self.psw.set_carry(carry);
                },
                Rar => {
                    let carry = self.a & 0x1 != 0;
                    self.a = (self.a >> 1) | (self.psw.carry() as u8 * 0x80);
                    self.psw.set_carry(carry);
                },
                Ral => {
                    let carry = self.a & 0x80 != 0;
                    self.a = (self.a << 1) | (self.psw.carry() as u8 * 0x01);
                    self.psw.set_carry(carry);
                },
                C(cond, addr) => {
                    if self.has_cond(cond) {
                        self.push16(&mut mem, self.pc + inst.len() as u16).await;
                        jump = Some(addr);
                    }
                },
                J(cond, addr) => {
                    if self.has_cond(cond){
                        jump = Some(addr);
                    }
                },
                Nop => (),
                Di => (),
                Ei => (),
                other => unimplemented!("8080 instruction {:x?}\r\n{:#x?}", other, self),
            }
            if let Some(0) = jump { return; }
            self.pc = jump.unwrap_or(self.pc + inst.len() as u16);
        }
    }
}
