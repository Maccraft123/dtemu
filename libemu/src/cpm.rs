use asane::intel8085::Intel8080;
use asane::NoopBus;
use crate::inner_prelude::*;
use crate::{KeyboardInput, TerminalOutput};

pub struct CpmMachine<T: Backend<Input: KeyboardInput, Output: TerminalOutput>> {
    cpu: Intel8080,
    memory: [u8; 0x10000],
    backend: T,
    cycles: usize,
}

impl<T: Backend<Input: KeyboardInput, Output: TerminalOutput>> Machine<T> for CpmMachine<T> {
    #[inline]
    fn cycles(&self) -> usize {
        self.cycles
    }
    #[inline]
    fn new(mut backend: T) -> CpmMachine<T> {
        let mut memory = [0u8; 0x10000];
        let program = backend.request_firmware("CP/M Program");
        memory[0x100..][..program.len()].copy_from_slice(&program);
        memory[0x0] = 0xc3; // jmp 0xff00, bios entry
        memory[0x1] = 0x00;
        memory[0x2] = 0xff;
        memory[0x4] = 0x01; // A: drive is current
        memory[0x5] = 0xc3; // jmp 0xfe00, bdos entry
        memory[0x6] = 0x00;
        memory[0x7] = 0xfe;
        memory[0xfe00] = 0xc9; // ret
        memory[0xff00] = 0xc5; // push bc, to have 0x0 as return address
        memory[0xff01] = 0xc3; // jmp 0x0100, program entry
        memory[0xff02] = 0x00;
        memory[0xff03] = 0x01;
        Self {
            cpu: Intel8080::new(&mut memory, &mut NoopBus(0)),
            memory,
            backend,
            cycles: 0,
        }
    }
    #[inline]
    fn tick(&mut self) -> Result<bool, Box<dyn Error>> {
        cassette::block_on(async {
            use asane::intel8085::{Reg, RegPair};
            if self.backend.should_exit() {
                return Ok(false)
            }
            // 2x faster on ryzen 6900hs than not doing this
            for _ in 0..0xffff {
                let mut tmp = asane::BusAccess::Read;
                let mut iobus = |_, _| {
                    0
                };
                let (cycles, pc) = self.cpu.step_block(&mut self.memory, &mut iobus).await;
                self.cycles += cycles;
                match pc {
                    0x0 => return Ok(false),
                    0xfe01..=0xfeff | 0xff02..=0xffff=> {
                        panic!("{:x}", self.cpu.pc())
                    },
                    // bdos call
                    0xfe00 => {
                        match self.cpu.reg(&mut self.memory, Reg::C) {
                            0 => return Ok(false),
                            1 => {
                                let ch = self.backend.i().read_key()?;
                                self.backend.o().write_key(ch)?;
                                self.cpu.set_reg(&mut self.memory, Reg::A, ch.try_into().unwrap());
                                self.cpu.set_reg(&mut self.memory, Reg::L, ch.try_into().unwrap());
                            },
                            2 => self.backend.o().write_key(self.cpu.reg(&mut self.memory, Reg::E).into())?,
                            6 => {
                                match self.cpu.reg(&mut self.memory, Reg::E) {
                                    0xff => {
                                        let ch = self.backend.i().read_key()?;
                                        self.cpu.set_reg(&mut self.memory, Reg::A, ch.try_into().unwrap());
                                    },
                                    ch => self.backend.o().write_key(ch.into())?,
                                }
                            },
                            9 => {
                                let addr = self.cpu.rp(RegPair::De) as usize;
                                let string = self.memory[addr..]
                                    .iter()
                                    .map(|v| *v as char)
                                    .take_while(|v| *v != '$')
                                    .map(|v| v.into());
                                self.backend.o().write_iter(string)?;
                            },
                            11 => {
                                let ch = self.backend.i().has_key()? as u8;
                                self.cpu.set_reg(&mut self.memory, Reg::A, ch);
                                self.cpu.set_reg(&mut self.memory, Reg::L, ch);
                            },
                            12 => {
                                self.cpu.set_reg(&mut self.memory, Reg::A, 0x22); // CP/M 2.2
                                self.cpu.set_reg(&mut self.memory, Reg::L, 0x22); // CP/M 2.2
                                self.cpu.set_reg(&mut self.memory, Reg::B, 0x0); // 8080, plain cp/m
                                self.cpu.set_reg(&mut self.memory, Reg::H, 0x0); // 8080, plain cp/m
                            },
                            15 => self.cpu.set_reg(&mut self.memory, Reg::A, 0xff),
                            other => unimplemented!("CP/M BDOS function {}", other),
                        }
                    },
                    _ => (),
                }
            }

            Ok(true)
        })
    }
}
