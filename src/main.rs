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

    async fn fetch8(&self, offset: u32, memory: &Memory<65536>, a: Addressing) -> u8 {
        match a {
            Addressing::Immediate => memory.fetch8(self.pc as u32 + offset).await,
            Addressing::Absolute => {
                let addr = memory.fetch16(self.pc as u32 + offset).await as u32;
                memory.fetch8(addr).await
            }
            Addressing::AbsoluteX => {
                //pending!();
                let addr = memory.fetch16(self.pc as u32 + offset).await + self.x as u16;
                memory.fetch8(addr as u32).await
            }
            Addressing::AbsoluteY => {
                //pending!();
                let addr = memory.fetch16(self.pc as u32 + offset).await + self.y as u16;
                memory.fetch8(addr as u32).await
            }
            Addressing::ZeroPage => memory.indirect_fetch8(self.pc as u32 + offset).await,
            Addressing::ZeroPageX => {
                pending!();
                memory.fetch8(memory.fetch8(self.pc as u32 + offset).await.wrapping_add(self.x) as u32).await
            },
            Addressing::ZeroPageY => {
                pending!();
                memory.fetch8(memory.fetch8(self.pc as u32 + offset).await.wrapping_add(self.y) as u32).await
            },
            Addressing::Indirect => {
                let addr = memory.fetch16(self.pc as u32 + offset).await as u32;
                let addr2 = memory.fetch16(addr as u32).await as u32;
                memory.fetch8(addr2).await
            },
            Addressing::IndirectX => {
                let addr = memory.fetch8(self.pc as u32 + offset).await.wrapping_add(self.x) as u32;
                let addr2 = memory.fetch16(addr as u32).await as u32;
                memory.fetch8(addr2).await
            },
            Addressing::IndirectY => {
                let addr = memory.fetch8(self.pc as u32 + offset).await;
                let addr2 = memory.fetch16(addr as u32).await as u32 + self.y as u32;
                memory.fetch8(addr2).await
            },
            Addressing::Accumulator => unreachable!("this is not how it works"),
            Addressing::Implied => unreachable!("no"),
        }
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
            use un6502::DatalessInstruction;
            use un6502::Opcode;
            
            let inst = un6502::decode_u8(memory.fetch8(self.pc as u32).await);
            println!("Running {:?}", inst);
            match inst.opcode() {
                Opcode::Lda => self.a = self.update_zn(self.fetch8(1, &memory, inst.addressing()).await),
                Opcode::Ldx => self.x = self.update_zn(self.fetch8(1, &memory, inst.addressing()).await),
                Opcode::Ldy => self.y = self.update_zn(self.fetch8(1, &memory, inst.addressing()).await),
                Opcode::Tax => { pending!(); self.x = self.update_zn(self.a); },
                Opcode::Tay => { pending!(); self.y = self.update_zn(self.a); },
                Opcode::Txs => { pending!(); self.sp = self.x; },
                Opcode::Jmp => {
                    let (lo, hi) = (memory.fetch8(self.pc as u32 + 1).await, memory.fetch8(self.pc as u32 + 2).await);
                    self.pc = u16::from_le_bytes([lo, hi]) - 2;
                },
                Opcode::Brk => return,
            }

            if let Some(ref mut cnt) = instructions {
                if *cnt == 0 {
                    return;
                }
                *cnt -= 1;
            }
            self.pc += inst.len();
        }
    }
}

async fn run() -> u64 {
    let mut cycles = 0;

    let mut memory: Memory<65536> = Memory::new();
    let mut cpu = Cpu::new();

    let rom002 = include_bytes!("002.rom");
    let rom003 = include_bytes!("003.rom");

    memory.copyfrom_fast(0x1c00, rom002);
    memory.copyfrom_fast(0x1800, rom003);

    memory.set8_fast(0xfffa, 0x00);
    memory.set8_fast(0xfffb, 0x1c);
    memory.set8_fast(0xfffc, 0x22);
    memory.set8_fast(0xfffd, 0x1c);
    memory.set8_fast(0xfffe, 0x1f);
    memory.set8_fast(0xffff, 0x1c);

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
