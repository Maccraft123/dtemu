use bitfield_struct::bitfield;
use futures::pending;
use crate::MemoryWrapper;
use parking_lot::Mutex;
use std::sync::atomic::{AtomicBool, Ordering};

use un6502::Addressing;
use crate::cpu::{Cpu, DisasmFn};

#[bitfield(u8, order = Msb)]
struct ProcFlags {
    negative: bool,
    overflow: bool,
    unused: bool,
    break_: bool,
    decimal: bool,
    irq_disable: bool,
    zero: bool,
    carry: bool,
}

impl ProcFlags {
    fn into_stack(mut self) -> u8 {
        self.set_break_(true);
        self.into()
    }
    fn from_stack(val: u8) -> ProcFlags {
        let mut this: ProcFlags = val.into();
        this.set_break_(false);
        this
    }
}

#[derive(Debug, Clone)]
pub struct Mos6502State {
    pc: u16,
    sp: u8,
    a: u8,
    x: u8,
    y: u8,
    flags: ProcFlags,
}

pub struct Mos6502 {
    state: Mutex<Mos6502State>,
    cached_state: Mutex<Mos6502State>,
    instruction_done: AtomicBool,
    mem: MemoryWrapper,
}

impl Cpu for Mos6502 {
    type Instruction = un6502::Instruction;
    type Registers = Mos6502State;

    fn new(mem: MemoryWrapper) -> Self {
        Self {
            state: Mutex::new(Mos6502State::new()),
            cached_state: Mutex::new(Mos6502State::new()),
            instruction_done: AtomicBool::new(false),
            mem,
        }
    }
    async fn tick(&self) {
        self.state.lock().run(self.mem.clone(), &self.cached_state, &self.instruction_done).await
    }
    fn disasm_fn(&self) -> &'static DisasmFn {
        &{|b| un6502::disasm(b).to_string()}
    }
    fn next_instruction(&self) -> u32 {
        self.cached_state.lock().pc as u32
    }
    fn instruction_done(&self) -> bool {
        self.instruction_done.load(Ordering::SeqCst)
    }
    fn regs(&self) -> &Mutex<Mos6502State> {
        &self.cached_state
    }
}

impl Mos6502State {
    fn new() -> Self {
        Self {
            pc: 0xfffc,
            sp: 0xf0,
            a: 0xaa,
            x: 0,
            y: 0,
            flags: ProcFlags::new().with_unused(true).with_zero(true).with_irq_disable(true),
        }
    }
    async fn run(&mut self, memory: MemoryWrapper, other_state: &Mutex<Mos6502State>, inst_done: &AtomicBool) {
        // pretend to do the init stuff
        pending!(); pending!();
        pending!(); pending!();
        // woo reset vector
        let (lo, hi) = (memory.fetch8(0xfffc).await, memory.fetch8(0xfffd).await);
        self.pc = u16::from_le_bytes([lo, hi]);

        loop {
            let mut jumpto = None;
            use un6502::Opcode;
            

            let inst = un6502::decode_u8(memory.fetch8(self.pc as u32).await);
            inst_done.store(false, Ordering::SeqCst);

            match inst.opcode() {
                // Transfer instructions
                Opcode::Lda => self.a = self.update_zn(self.load8(&memory, inst.addressing()).await),
                Opcode::Ldx => self.x = self.update_zn(self.load8(&memory, inst.addressing()).await),
                Opcode::Ldy => self.y = self.update_zn(self.load8(&memory, inst.addressing()).await),

                Opcode::Sta => { self.store8(&memory, inst.addressing(), self.a).await; },
                Opcode::Stx => { self.store8(&memory, inst.addressing(), self.x).await; },
                Opcode::Sty => { self.store8(&memory, inst.addressing(), self.y).await; },

                Opcode::Tax => { pending!(); self.x = self.update_zn(self.a); },
                Opcode::Tay => { pending!(); self.y = self.update_zn(self.a); },
                Opcode::Tsx => { pending!(); self.x = self.update_zn(self.sp); },
                Opcode::Txa => { pending!(); self.a = self.update_zn(self.x); },
                Opcode::Txs => { pending!(); self.sp = self.x; },
                Opcode::Tya => { pending!(); self.a = self.update_zn(self.y); }

                // Stack instructions
                Opcode::Pha => self.push8(&memory, self.a).await,
                Opcode::Php => self.push8(&memory, self.flags.into_stack()).await,
                Opcode::Pla => { self.a = self.pull8(&memory).await; self.update_zn(self.a); },
                Opcode::Plp => self.flags = ProcFlags::from_stack(self.pull8(&memory).await),

                // Decrements and increments
                Opcode::Dec => {
                    let val = self.update_zn(self.load8(&memory, inst.addressing()).await.wrapping_sub(1));
                    self.store8(&memory, inst.addressing(), val).await;
                    pending!();
                },
                Opcode::Dex => { pending!(); self.x = self.x.wrapping_sub(1); self.update_zn(self.x); },
                Opcode::Dey => { pending!(); self.y = self.y.wrapping_sub(1); self.update_zn(self.y); },
                Opcode::Inc => {
                    let val = self.update_zn(self.load8(&memory, inst.addressing()).await.wrapping_add(1));
                    self.store8(&memory, inst.addressing(), val).await;
                    pending!();
                },
                Opcode::Inx => { pending!(); self.x = self.x.wrapping_add(1); self.update_zn(self.x); }
                Opcode::Iny => { pending!(); self.y = self.y.wrapping_add(1); self.update_zn(self.y); },

                // Arithmetic operations
                Opcode::Adc => {
                    let val = self.load8(&memory, inst.addressing()).await;
                    self.arith_helper(val);
                },
                Opcode::Sbc => {
                    let val = self.load8(&memory, inst.addressing()).await ^ 0xff;
                    self.arith_helper(val);
                },

                // Logical operations
                Opcode::And => { pending!(); self.a = self.update_zn(self.a & self.load8(&memory, inst.addressing()).await)},
                Opcode::Eor => { pending!(); self.a = self.update_zn(self.a ^ self.load8(&memory, inst.addressing()).await)},
                Opcode::Ora => { pending!(); self.a = self.update_zn(self.a | self.load8(&memory, inst.addressing()).await)},

                // Shift and rotate instructions
                Opcode::Asl => {
                    let val = self.load8(&memory, inst.addressing()).await;
                    self.flags.set_carry(val & 0x80 != 0);
                    self.store8(&memory, inst.addressing(), val << 1).await;
                },
                Opcode::Lsr => {
                    let val = self.load8(&memory, inst.addressing()).await;
                    self.flags.set_carry(val & 0x1 != 0);
                    self.store8(&memory, inst.addressing(), val >> 1).await;
                },
                Opcode::Rol => {
                    let mut val = self.load8(&memory, inst.addressing()).await;
                    let new_carry = val & 0x80 != 0;
                    val = (val << 1) | self.flags.carry() as u8;
                    self.store8(&memory, inst.addressing(), val).await;
                    self.flags.set_carry(new_carry);
                },
                Opcode::Ror => {
                    let mut val = self.load8(&memory, inst.addressing()).await;
                    let new_carry = val & 0x1 != 0;
                    val = (val >> 1) | (self.flags.carry() as u8 * 0x80);
                    self.store8(&memory, inst.addressing(), val).await;
                    self.flags.set_carry(new_carry);
                },

                // Flag instructions
                Opcode::Clc => { pending!(); self.flags.set_carry(false); }
                Opcode::Cld => { pending!(); self.flags.set_decimal(false); }
                Opcode::Cli => { pending!(); self.flags.set_irq_disable(false); }
                Opcode::Clv => { pending!(); self.flags.set_overflow(false); }
                Opcode::Sec => { pending!(); self.flags.set_carry(true); }
                Opcode::Sed => { pending!(); self.flags.set_decimal(true); /*unimplemented!("BCD arithmetics")*/ }
                Opcode::Sei => { pending!(); self.flags.set_irq_disable(true); }

                // Comparisons
                Opcode::Cmp => self.cmp_helper(self.a, self.load8(&memory, inst.addressing()).await),
                Opcode::Cpx => self.cmp_helper(self.x, self.load8(&memory, inst.addressing()).await),
                Opcode::Cpy => self.cmp_helper(self.y, self.load8(&memory, inst.addressing()).await),

                // Conditional branch instructions
                Opcode::Bcc => jumpto = self.branch_on(&memory, !self.flags.carry()).await,
                Opcode::Bcs => jumpto = self.branch_on(&memory, self.flags.carry()).await,
                Opcode::Beq => jumpto = self.branch_on(&memory, self.flags.zero()).await,
                Opcode::Bmi => jumpto = self.branch_on(&memory, self.flags.negative()).await,
                Opcode::Bne => jumpto = self.branch_on(&memory, !self.flags.zero()).await,
                Opcode::Bpl => jumpto = self.branch_on(&memory, !self.flags.negative()).await,
                Opcode::Bvc => jumpto = self.branch_on(&memory, !self.flags.overflow()).await,
                Opcode::Bvs => jumpto = self.branch_on(&memory, self.flags.overflow()).await,

                // Jumps and subroutines
                Opcode::Jmp => {
                    let (lo, hi) = (memory.fetch8(self.pc as u32 + 1).await, memory.fetch8(self.pc as u32 + 2).await);
                    let new_pc;

                    if inst.addressing() == Addressing::Absolute {
                        new_pc = u16::from_le_bytes([lo, hi]);
                    } else if inst.addressing() == Addressing::Indirect {
                        let addr = u16::from_le_bytes([lo, hi]) as u32;

                        let new_lo = memory.fetch8(addr).await;
                        let new_hi = memory.fetch8(addr & 0xff00 | ((addr & 0xff) as u8).wrapping_add(1) as u32).await;
                        new_pc = u16::from_le_bytes([new_lo, new_hi]);
                    } else {
                        unreachable!();
                    }

                    if new_pc & 0xff00 != (self.pc + 2) & 0xff00 {
                        pending!();
                    }
                    if new_pc == self.pc {
                        panic!("refusing to do an infinite loop");
                    }
                    jumpto = Some(new_pc);
                },
                Opcode::Jsr => {
                    self.push8(&memory, ((self.pc + 2) & 0x00ff) as u8).await;
                    self.push8(&memory, (((self.pc + 2) & 0xff00) >> 8) as u8).await;
                    jumpto = Some(u16::from_le_bytes([memory.fetch8(self.pc as u32 + 1).await, memory.fetch8(self.pc as u32 + 2).await]));
                },
                Opcode::Rts => {
                    let hi = self.pull8(&memory).await;
                    let lo = self.pull8(&memory).await;
                    jumpto = Some(((hi as u16) << 8) | lo as u16 + 1);
                }

                // Interrupts
                Opcode::Brk => {
                    //let (lo, hi) = (memory.fetch8(0xfffc).await, memory.fetch8(0xfffd).await);
                    //jumpto = Some(u16::from_le_bytes([lo, hi]));
                    eprintln!("BRK encountered!");
                    return;
                },
                //Opcode::Rti => ,

                // Other
                Opcode::Bit => {
                    pending!();
                    let val = self.load8(&memory, inst.addressing()).await;
                    self.flags.set_negative(val & 0x80 != 0);
                    self.flags.set_overflow(val & 0x40 != 0);
                    self.flags.set_zero(val & self.a == 0);
                },
                Opcode::Nop => pending!(),

                other => todo!("{:?}", other),
            }

            if jumpto == Some(0x00) || jumpto == Some(0x01) {
                panic!("\r\nno i won't do a jump to 0x0 or 0x1 are you stupid\r
State on exit just before it: {:#x?}\r", other_state.lock());
            }
            self.pc = jumpto.unwrap_or(self.pc.wrapping_add(inst.len()));

            inst_done.store(true, Ordering::SeqCst);
            other_state.lock().clone_from(self);
        }
    }
    
    fn update_zn(&mut self, val: u8) -> u8 {
        if val == 0 {
            self.flags.set_zero(true);
        } else {
            self.flags.set_zero(false);
            self.flags.set_negative(val & 0x80 != 0);
        }
        val
    }

    async fn calc_addr(&self, memory: &MemoryWrapper, a: Addressing) -> u16 {
        match a {
            Addressing::Immediate   => self.pc + 1,
            Addressing::Absolute    => memory.fetch16(self.pc as u32 + 1).await,
            Addressing::AbsoluteX   => memory.fetch16(self.pc as u32 + 1).await + self.x as u16,
            Addressing::AbsoluteY   => memory.fetch16(self.pc as u32 + 1).await + self.y as u16,
            Addressing::ZeroPage    => memory.fetch8(self.pc as u32 + 1).await as u16,
            Addressing::ZeroPageX   => {
                pending!(); 
                memory.fetch8(self.pc as u32 + 1).await.wrapping_add(self.x) as u16
            },
            Addressing::ZeroPageY   => {
                pending!();
                memory.fetch8(self.pc as u32 + 1).await.wrapping_add(self.y) as u16
            },
            Addressing::Indirect    => memory.fetch16(memory.fetch16(self.pc as u32 + 1).await as u32).await,
            Addressing::IndirectX   => {
                let addr = memory.fetch8(self.pc as u32 + 1).await.wrapping_add(self.x) as u32;
                memory.fetch16(addr as u32).await
            },
            Addressing::IndirectY   => {
                let addr = memory.fetch8(self.pc as u32 + 1).await;
                memory.fetch16(addr as u32).await + self.y as u16
            },
            Addressing::Accumulator => unreachable!("this is not how it works"),
            Addressing::Implied     => unreachable!("no"),
            Addressing::Relative    => unreachable!("how"),
        }
    }

    async fn load8(&self, memory: &MemoryWrapper, s: Addressing) -> u8 {
        if s == Addressing::Accumulator {
            self.a
        } else {
            memory.fetch8(self.calc_addr(memory, s).await as u32).await
        }
    }

    async fn store8(&mut self, memory: &MemoryWrapper, s: Addressing, val: u8) {
        if s == Addressing::Accumulator {
            self.a = val
        } else {
            memory.set8(self.calc_addr(memory, s).await as u32, val).await
        }
    }

    async fn push8(&mut self, memory: &MemoryWrapper, val: u8) {
        memory.set8(0x0100 | self.sp as u32, val).await;
        self.sp = self.sp.wrapping_sub(1);
        pending!();
    }

    async fn pull8(&mut self, memory: &MemoryWrapper) -> u8 {
        self.sp = self.sp.wrapping_add(1);
        pending!();
        pending!();
        let val = memory.fetch8(0x0100 | self.sp as u32).await;
        val
    }

    async fn branch_on(&mut self, memory: &MemoryWrapper, condition: bool) -> Option<u16> {
        if condition {
            let target = (self.pc as i32 + 2 + ((memory.fetch8(self.pc as u32 + 1).await as i8) as i32)) as u16;
            pending!();
            if target & 0xff00 != (self.pc + 2) & 0xff00 {
                pending!();
            }
            Some(target)
        } else {
            None
        }
    }

    fn cmp_helper(&mut self, reg: u8, val: u8) {
        self.update_zn(reg.wrapping_sub(val));
        self.flags.set_carry(val <= reg);
    }

    fn arith_helper(&mut self, val: u8) {
        let (tmp, overflow1) = self.a.overflowing_add(val);
        let (result, overflow2) = tmp.overflowing_add(self.flags.carry() as u8);
        self.flags.set_carry(overflow1 || overflow2); // it came to me in a dream
        if self.a & 0x80 == val & 0x80 {
            self.flags.set_overflow((val & 0x80) != (result & 0x80));
        } else {
            self.flags.set_overflow(false);
        }

        self.a = self.update_zn(result);
    }
}
