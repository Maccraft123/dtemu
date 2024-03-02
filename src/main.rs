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
        self.data[offset as usize..][..vals.len()].copy_from_slice(vals);
    }
    fn set8_fast(&mut self, addr: u32, val: u8) {
        self.data[addr as usize] = val
    }
    fn fetch8_fast(&self, addr: u32) -> u8 {
        self.data[addr as usize]
    }
    async fn indirect_fetch8(&self, addr: u32) -> u8 {
        self.fetch8(self.fetch8(addr).await as u32).await
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
        println!("fetched {:x} from {:x}", tmp, addr);
        tmp
    }
}

#[bitfield(u8)]
struct ProcFlags {
    negative: bool,
    overflow: bool,
    __: bool,
    break_: bool,
    decimal: bool,
    irq_disable: bool,
    zero: bool,
    carry: bool,
}

#[derive(Debug, Clone, Copy)]
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
    //Relative,
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
            sp: 0xff,
            a: 0,
            x: 0,
            y: 0,
            flags: ProcFlags::new(),
        }
    }
    fn pc(&self) -> u16 { self.pc }
    fn sp(&self) -> u8 { self.sp }
    fn a(&self) -> u8 { self.a }
    fn x(&self) -> u8 { self.x }
    fn y(&self) -> u8 { self.y }
    fn flags(&self) -> &ProcFlags { &self.flags }
    fn update_zn(&mut self, val: u8) -> u8 {
        self.flags.set_zero(val == 0);
        self.flags.set_negative(val & 0x80 != 0);
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
        }
    }

    async fn load8(&self, offset: u32, memory: &Memory<65536>, s: Addressing) -> u8 {
        memory.fetch8(self.calc_addr(offset, memory, s).await as u32).await
    }

    async fn store8(&self, offset: u32, memory: &mut Memory<65536>, s: Addressing, val: u8) {
        memory.set8(self.calc_addr(offset, memory, s).await as u32, val).await
    }

    async fn push8(&mut self, memory: &mut Memory<65536>, val: u8) {
        memory.set8(0x0100 | self.sp as u32, val).await;
        self.sp -= 1;
        pending!();
    }

    async fn pull8(&mut self, memory: &Memory<65536>) -> u8 {
        self.sp += 1;
        pending!();
        pending!();
        memory.fetch8(0x0100 | self.sp as u32).await
    }

    async fn run(&mut self, memory: &mut Memory<65536>, instructions: Option<NonZeroU64>) {
        // pretend to do the init stuff
        //pending!(); pending!(); pending!(); pending!();
        //pending!();
        // woo reset vector
        let (lo, hi) = (memory.fetch8(0xfffc).await, memory.fetch8(0xfffd).await);
        self.pc = u16::from_le_bytes([lo, hi]);

        let mut instructions: Option<u64> = instructions.map(|v| v.into());
        loop {
            let mut jumpto = None;
            use un6502::DatalessInstruction;
            use un6502::Opcode;
            
            let inst = un6502::decode_u8(memory.fetch8(self.pc as u32).await);
            println!("Running {:?}", inst);
            match inst.opcode() {
                Opcode::Lda => self.a = self.update_zn(self.load8(1, &memory, inst.addressing()).await),
                Opcode::Ldx => self.x = self.update_zn(self.load8(1, &memory, inst.addressing()).await),
                Opcode::Ldy => self.y = self.update_zn(self.load8(1, &memory, inst.addressing()).await),
                Opcode::Stx => { self.store8(1, memory, inst.addressing(), self.x).await; },
                Opcode::Tax => { pending!(); self.x = self.update_zn(self.a); },
                Opcode::Tay => { pending!(); self.y = self.update_zn(self.a); },
                Opcode::Txs => { pending!(); self.sp = self.x; },
                Opcode::Txa => { pending!(); self.a = self.update_zn(self.x); },
                Opcode::Cld => { pending!(); self.flags.set_decimal(false); }
                Opcode::Sei => { pending!(); self.flags.set_irq_disable(true); }
                Opcode::Jmp => {
                    let (lo, hi) = (memory.fetch8(self.pc as u32 + 1).await, memory.fetch8(self.pc as u32 + 2).await);
                    self.pc = u16::from_le_bytes([lo, hi]) - 2;
                },
                Opcode::Pla => self.a = self.pull8(memory).await,
                Opcode::Pha => self.push8(memory, self.a).await,
                Opcode::Jsr => {
                    self.push8(memory, (((self.pc + 2) & 0xff00) >> 8) as u8).await;
                    self.push8(memory, ((self.pc + 2) & 0x00ff) as u8).await;
                    jumpto = Some(u16::from_le_bytes([memory.fetch8(self.pc as u32 + 1).await, memory.fetch8(self.pc as u32 + 2).await]));
                },
                Opcode::Rti => {
                    jumpto = Some(u16::from_le_bytes([self.pull8(memory).await, self.pull8(memory).await]));
                },
                Opcode::Brk => return,
            }

            if let Some(ref mut cnt) = instructions {
                if *cnt == 0 {
                    return;
                }
                *cnt -= 1;
            }
            if let Some(pc) = jumpto {
                self.pc = pc;
                jumpto = None;
            } else {
                self.pc += inst.len();
            }

            println!("CPU State now: {:#x?}", self);
        }
    }
}

async fn run() -> u64 {
    let mut cycles = 0;

    let mut memory: Memory<65536> = Memory::new();
    let mut cpu = Cpu::new();

    let rom002 = include_bytes!("002.rom");
    let rom003 = include_bytes!("003.rom");

    let kim1 = true;
    if kim1 {
        memory.copyfrom_fast(0x1c00, rom002);
        memory.copyfrom_fast(0x1800, rom003);

        memory.set8_fast(0xfffa, 0x00);
        memory.set8_fast(0xfffb, 0x1c);
        memory.set8_fast(0xfffc, 0x22);
        memory.set8_fast(0xfffd, 0x1c);
        memory.set8_fast(0xfffe, 0x1f);
        memory.set8_fast(0xffff, 0x1c);
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
        }
        cycles -= 1;

    }

    println!("CPU state on exit: {:#x?}", cpu);
    println!("{} cycles done", cycles - 2);
    cycles
}

fn main() {
    futures::executor::block_on(run());
}
