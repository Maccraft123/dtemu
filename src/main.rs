use bitfield_struct::bitfield;
use core::pin::pin;
use futures::{pending, poll};
use std::future;
use std::num::NonZeroU64;

mod un6502;

struct Memory<const SIZE: usize> {
    data: [u8; SIZE],
}

impl<const SIZE: usize> Memory<SIZE> {
    fn new() -> Self {
        Self {
            data: [0u8; SIZE],
        }
    }
    fn copyfrom_fast(&mut self, offset: u32, vals: &[u8]) {
        let mut len = vals.len();
        if len > self.data.len() - offset as usize {
            len = self.data.len() - offset as usize;
        }
        self.data[offset as usize..][..len].copy_from_slice(&vals[..len]);
    }
    fn set8_fast(&mut self, addr: u32, val: u8) {
        self.data[addr as usize] = val
    }
    fn fetch8_fast(&self, addr: u32) -> u8 {
        self.data[addr as usize]
    }
    async fn fetch8(&self, addr: u32) -> u8 {
        pending!();
        let tmp = self.data[addr as usize];
        println!("fetched {:x} from {:x}", tmp, addr);
        tmp
    }
    async fn set8(&mut self, addr: u32, val: u8) {
        pending!();
        println!("setting {:x} = {:x}", addr, val);
        self.data[addr as usize] = val
    }
    async fn fetch16(&self, addr: u32) -> u16 {
        if addr & 0x000f == 0xf {
            pending!();
        }
        let tmp = u16::from_le_bytes([self.fetch8(addr).await, self.fetch8(addr+1).await]);
        //println!("fetched {:x} from {:x}", tmp, addr);
        tmp
    }
}

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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Addressing {
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

#[derive(Debug)]
struct Cpu {
    pc: u16,
    sp: u8,
    a: u8,
    x: u8,
    y: u8,
    flags: ProcFlags,
}

impl Cpu {
    fn new() -> Self {
        Self {
            pc: 0xfffc,
            sp: 0xfd,
            a: 0,
            x: 0,
            y: 0,
            flags: 0x24.into(),
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

    async fn calc_addr(&self, offset: u32, memory: &Memory<65536>, a: Addressing) -> u16 {
        match a {
            Addressing::Immediate   => self.pc + offset as u16,
            Addressing::Absolute    => memory.fetch16(self.pc as u32 + offset).await,
            Addressing::AbsoluteX   => memory.fetch16(self.pc as u32 + offset).await + self.x as u16,
            Addressing::AbsoluteY   => memory.fetch16(self.pc as u32 + offset).await + self.y as u16,
            Addressing::ZeroPage    => memory.fetch8(self.pc as u32 + offset).await as u16,
            Addressing::ZeroPageX   => {
                pending!(); 
                memory.fetch8(self.pc as u32 + offset).await.wrapping_add(self.x) as u16
            },
            Addressing::ZeroPageY   => {
                pending!();
                memory.fetch8(self.pc as u32 + offset).await.wrapping_add(self.y) as u16
            },
            Addressing::Indirect    => memory.fetch16(memory.fetch16(self.pc as u32 + offset).await as u32).await,
            Addressing::IndirectX   => {
                let addr = memory.fetch8(self.pc as u32 + offset).await.wrapping_add(self.x) as u32;
                memory.fetch16(addr as u32).await
            },
            Addressing::IndirectY   => {
                let addr = memory.fetch8(self.pc as u32 + offset).await;
                memory.fetch16(addr as u32).await + self.y as u16
            },
            Addressing::Accumulator => unreachable!("this is not how it works"),
            Addressing::Implied     => unreachable!("no"),
            Addressing::Relative    => unreachable!("how"),
        }
    }

    async fn load8(&self, offset: u32, memory: &Memory<65536>, s: Addressing) -> u8 {
        if s == Addressing::Accumulator {
            self.a
        } else {
            memory.fetch8(self.calc_addr(offset, memory, s).await as u32).await
        }
    }

    async fn store8(&mut self, offset: u32, memory: &mut Memory<65536>, s: Addressing, val: u8) {
        if s == Addressing::Accumulator {
            self.a = val
        } else {
            memory.set8(self.calc_addr(offset, memory, s).await as u32, val).await
        }
    }

    async fn push8(&mut self, memory: &mut Memory<65536>, val: u8) {
        memory.set8(0x0100 | self.sp as u32, val).await;
        self.sp -= 1;
        println!("pushed {:x} onto stack", val);
        pending!();
    }

    async fn pull8(&mut self, memory: &Memory<65536>) -> u8 {
        self.sp += 1;
        pending!();
        pending!();
        let val = memory.fetch8(0x0100 | self.sp as u32).await;
        println!("pulled {:x} from stack", val);
        val
    }

    async fn branch_on(&mut self, memory: &Memory<65536>, condition: bool) -> Option<u16> {
        if condition {
            let target = (self.pc as i32 + 2 + ((memory.fetch8(self.pc as u32 + 1).await as i8) as i32)) as u16;
            //println!("taking a branch to {:x}", target);
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
        println!("cmp {:x} {:x}",  reg, val);
        if reg == val {
            self.flags.set_negative(false);
            self.flags.set_zero(true);
            self.flags.set_carry(true);
        } else if reg > val {
            self.flags.set_negative(reg - val & 0x80 != 0);
            self.flags.set_zero(false);
            self.flags.set_carry(true);
        } else if reg < val {
            self.flags.set_negative(reg.wrapping_sub(val) & 0x80 != 0);
            self.flags.set_zero(false);
            self.flags.set_carry(false);
        } else {
            unreachable!();
        }
    }

    fn arith_helper(&mut self, val: u8) {
        let (tmp, overflow1) = self.a.overflowing_add(val);
        let (result, overflow2) = tmp.overflowing_add(self.flags.carry() as u8);
        self.flags.set_carry(overflow1 || overflow2); // it came to me in a dream
        if self.a & 0x80 == val & 0x80 {
            self.flags.set_overflow(val & 0x80 != result & 0x80);
        }

        self.a = self.update_zn(result);
    }

    async fn run(&mut self, memory: &mut Memory<65536>, instructions: Option<NonZeroU64>) {
        // pretend to do the init stuff
        pending!(); pending!();
        pending!(); pending!();
        // woo reset vector
        //let (lo, hi) = (memory.fetch8(0xfffc).await, memory.fetch8(0xfffd).await);
        //self.pc = u16::from_le_bytes([lo, hi]);
        self.pc = 0x400;

        loop {
            let mut jumpto = None;
            use un6502::Opcode;
            
            let inst = un6502::decode_u8(memory.fetch8(self.pc as u32).await);
            println!("Running {:?}", inst);
            match inst.opcode() {
                // Transfer instructions
                Opcode::Lda => self.a = self.update_zn(self.load8(1, &memory, inst.addressing()).await),
                Opcode::Ldx => self.x = self.update_zn(self.load8(1, &memory, inst.addressing()).await),
                Opcode::Ldy => self.y = self.update_zn(self.load8(1, &memory, inst.addressing()).await),

                Opcode::Sta => { self.store8(1, memory, inst.addressing(), self.a).await; },
                Opcode::Stx => { self.store8(1, memory, inst.addressing(), self.x).await; },
                Opcode::Sty => { self.store8(1, memory, inst.addressing(), self.y).await; },

                Opcode::Tax => { pending!(); self.x = self.update_zn(self.a); },
                Opcode::Tay => { pending!(); self.y = self.update_zn(self.a); },
                Opcode::Tsx => { pending!(); self.x = self.update_zn(self.sp); },
                Opcode::Txa => { pending!(); self.a = self.update_zn(self.x); },
                Opcode::Txs => { pending!(); self.sp = self.x; },
                Opcode::Tya => { pending!(); self.a = self.update_zn(self.y); }

                // Stack instructions
                Opcode::Pha => self.push8(memory, self.a).await,
                Opcode::Php => self.push8(memory, self.flags.into()).await,
                Opcode::Pla => self.a = self.pull8(memory).await,
                Opcode::Plp => self.flags = self.pull8(memory).await.into(),

                // Decrements and increments

                Opcode::Dec => {
                    let val = self.update_zn(self.load8(1, memory, inst.addressing()).await.wrapping_sub(1));
                    self.store8(1, memory, inst.addressing(), val).await;
                    pending!();
                },
                Opcode::Dex => { pending!(); self.x = self.x.wrapping_sub(1); self.update_zn(self.x); },
                Opcode::Dey => { pending!(); self.y = self.y.wrapping_sub(1); self.update_zn(self.y);  println!("{:x}", self.y); },
                Opcode::Inc => {
                    let val = self.update_zn(self.load8(1, memory, inst.addressing()).await.wrapping_add(1));
                    self.store8(1, memory, inst.addressing(), val).await;
                    pending!();
                },
                Opcode::Inx => { pending!(); self.x = self.x.wrapping_add(1); self.update_zn(self.x); }
                Opcode::Iny => { pending!(); self.y = self.y.wrapping_add(1); self.update_zn(self.y); },

                // Arithmetic operations
                Opcode::Adc => {
                    let val = self.load8(1, memory, inst.addressing()).await;
                    self.arith_helper(val);
                },
                Opcode::Sbc => {
                    let val = self.load8(1, memory, inst.addressing()).await ^ 0xff;
                    self.arith_helper(val);
                },

                // Logical operations
                Opcode::And => { pending!(); self.a = self.update_zn(self.a & self.load8(1, memory, inst.addressing()).await)},
                Opcode::Eor => { pending!(); self.a = self.update_zn(self.a ^ self.load8(1, memory, inst.addressing()).await)},
                Opcode::Ora => { pending!(); self.a = self.update_zn(self.a | self.load8(1, memory, inst.addressing()).await)},

                // Shift and rotate instructions
                Opcode::Asl => {
                    let val = self.load8(1, memory, inst.addressing()).await;
                    self.flags.set_carry(val & 0x80 != 0);
                    self.store8(1, memory, inst.addressing(), val << 1).await;
                },
                Opcode::Lsr => {
                    let val = self.load8(1, memory, inst.addressing()).await;
                    self.flags.set_carry(val & 0x1 != 0);
                    self.store8(1, memory, inst.addressing(), val >> 1).await;
                },
                //Opcode::Rol => ,
                //Opcode::Ror => ,

                // Flag instructions
                Opcode::Clc => { pending!(); self.flags.set_carry(false); }
                Opcode::Cld => { pending!(); self.flags.set_decimal(false); }
                Opcode::Cli => { pending!(); self.flags.set_irq_disable(false); }
                Opcode::Clv => { pending!(); self.flags.set_overflow(false); }
                Opcode::Sec => { pending!(); self.flags.set_carry(true); }
                Opcode::Sed => { pending!(); self.flags.set_decimal(true); /*unimplemented!("BCD arithmetics")*/ }
                Opcode::Sei => { pending!(); self.flags.set_irq_disable(true); }

                // Comparisons
                Opcode::Cmp => self.cmp_helper(self.a, self.load8(1, memory, inst.addressing()).await),
                Opcode::Cpx => self.cmp_helper(self.x, self.load8(1, memory, inst.addressing()).await),
                Opcode::Cpy => self.cmp_helper(self.y, self.load8(1, memory, inst.addressing()).await),

                // Conditional branch instructions
                Opcode::Bcc => jumpto = self.branch_on(memory, !self.flags.carry()).await,
                Opcode::Bcs => jumpto = self.branch_on(memory, self.flags.carry()).await,
                Opcode::Beq => jumpto = self.branch_on(memory, self.flags.zero()).await,
                Opcode::Bmi => jumpto = self.branch_on(memory, self.flags.negative()).await,
                Opcode::Bne => jumpto = self.branch_on(memory, !self.flags.zero()).await,
                Opcode::Bpl => jumpto = self.branch_on(memory, !self.flags.negative()).await,
                Opcode::Bvc => jumpto = self.branch_on(memory, !self.flags.overflow()).await,
                Opcode::Bvs => jumpto = self.branch_on(memory, self.flags.negative()).await,

                // Jumps and subroutines
                Opcode::Jmp => {
                    let (lo, hi) = (memory.fetch8(self.pc as u32 + 1).await, memory.fetch8(self.pc as u32 + 2).await);
                    let new_pc = u16::from_le_bytes([lo, hi]);
                    if new_pc & 0xff00 != (self.pc + 2) & 0xff00 {
                        pending!();
                    }
                    if new_pc == self.pc {
                        println!("refusing to do an infinite loop");
                        return;
                    }
                    jumpto = Some(new_pc);
                },
                Opcode::Jsr => {
                    println!("return location: {:x}", self.pc + 3);
                    self.push8(memory, ((self.pc + 3) & 0x00ff) as u8).await;
                    self.push8(memory, (((self.pc + 3) & 0xff00) >> 8) as u8).await;
                    jumpto = Some(u16::from_le_bytes([memory.fetch8(self.pc as u32 + 1).await, memory.fetch8(self.pc as u32 + 2).await]));
                },
                Opcode::Rts => {
                    let lo = self.pull8(memory).await;
                    let hi = self.pull8(memory).await;
                    jumpto = Some(((hi as u16) << 8) | lo as u16);
                }

                // Interrupts
                Opcode::Brk => { println!("hit brk!"); return; },
                //Opcode::Rti => ,

                // Other
                Opcode::Bit => {
                    pending!();
                    let val = self.load8(1, memory, inst.addressing()).await;
                    self.flags.set_negative(val & 0x80 != 0);
                    self.flags.set_overflow(val & 0x40 != 0);
                    self.flags.set_zero(val & self.a == 0);
                },
                Opcode::Nop => pending!(),

                other => todo!("{:?}", other),
            }

            let asdf: u8 = self.flags.into();
            //println!("{:02x} {:04x} {:?} A:{:02x} X:{:02x} Y:{:02x} P:{:02x} SP:{:02x}", memory.fetch8_fast(self.pc as u32), self.pc, inst.opcode(), self.a, self.x, self.y, asdf, self.sp);
            self.pc = jumpto.unwrap_or(self.pc + inst.len());

            if let Some(pc) = jumpto {
                println!("and off we go to {:x}", pc);
            }
        }
    }
}

async fn run() -> u64 {
    let mut cycles = 0;

    let mut memory: Memory<65536> = Memory::new();
    let mut cpu = Cpu::new();

    let rom002 = include_bytes!("002.rom");
    let rom003 = include_bytes!("003.rom");
    let nestest = include_bytes!("nestest.nes");
    let _6502test = include_bytes!("6502_functional_test.bin");

    let kim1 = false;
    let run_nestest = false;
    let run_6502_functest = true;
    if kim1 {
        memory.copyfrom_fast(0x1c00, rom002);
        memory.copyfrom_fast(0x1800, rom003);

        memory.set8_fast(0xfffa, 0x00);
        memory.set8_fast(0xfffb, 0x1c);
        memory.set8_fast(0xfffc, 0x22);
        memory.set8_fast(0xfffd, 0x1c);
        memory.set8_fast(0xfffe, 0x1f);
        memory.set8_fast(0xffff, 0x1c);
    } else if run_nestest {
        memory.copyfrom_fast(0xc000 - 0x10, nestest);

        memory.set8_fast(0xfffc, 0x00);
        memory.set8_fast(0xfffd, 0xc0);
    } else if run_6502_functest {
        memory.copyfrom_fast(0, _6502test);
    } else {
        memory.set8_fast(0xfffc, 0x20);
        memory.set8_fast(0xfffd, 0x20);

        memory.set8_fast(0x2020, 0xa9);
        memory.set8_fast(0x2021, 0x69);
        memory.set8_fast(0x2022, 0x48);
        memory.set8_fast(0x2023, 0xa9);
        memory.set8_fast(0x2024, 0x00);
        memory.set8_fast(0x2025, 0x68);
    }

    {
        let mut cpu_future = pin!(cpu.run(&mut memory, None));

        loop {
            if poll!(&mut cpu_future).is_ready() {
                break;
            }
            cycles += 1;
            //println!("{}", cycles);
        }
        cycles -= 1;
    }

    println!("CPU state on exit: {:#x?}", cpu);
    println!("{} cycles done", cycles - 2);

    println!("nestest result: $02 = {:02x}, $03 = {:03x}", memory.fetch8_fast(0x02), memory.fetch8_fast(0x03));
    cycles
}

fn main() {
    futures::executor::block_on(run());
}
