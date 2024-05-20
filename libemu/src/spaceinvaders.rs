use asane::intel8085::Intel8080;
use asane::{BusRead, BusWrite};
use crate::inner_prelude::*;
use crate::{KeyboardInput, FramebufferOutput};

pub struct SpaceInvaders<T: Backend<Input: KeyboardInput, Output: FramebufferOutput>> {
    cpu: Intel8080,
    memory: SpaceInvadersMemoryBus,
    io: SpaceInvadersIoBus,
    backend: T,
}

impl<T: Backend<Input: KeyboardInput, Output: FramebufferOutput>> Machine<T> for SpaceInvaders<T> {
    #[inline]
    fn cycles(&self) -> usize {
        self.cycles
    }
    #[inline]
    fn new(mut backend: T) -> SpaceInvaders<T> {
        Self {
            cpu: Intel8080::new(),
            memory: SpaceInvadersMemoryBus::new(),
            io: SpaceInvadersIoBus::new(),
            backend,
        }
    }
    #[inline]
    fn tick(&mut self) -> Result<bool, Box<dyn Error>> {
        use asane::intel8085::{Reg, RegPair};
        if self.backend.should_exit() {
            return Ok(false)
        }
        let (pc, cycles) = self.cpu.step_instruction_sync(&mut self.memory, &mut self.io);
        self.cycles += cycles;
        match pc {
        }
        Ok(true)
    }
}

struct SpaceInvadersMemoryBus {
    rom: [u8; 0x2000],
    ram: [u8; 0x400],
    vram: [u8; 0x1c00],
}

impl BusRead<u16> for SpaceInvadersMemoryBus {
    fn read8(&self, addr: u16) -> u8 {
        match addr {
            0x0000..0x2000 => self.rom[addr],
            0x2000..0x2400 => self.ram[addr - 0x2000],
            0x2400..0x4000 => self.vram[addr - 0x2400],
            _ => self.ram[addr & 0x3ff]
        }
    }
}

impl BusWrite<u16> for SpaceInvadersMemoryBus {
    fn write8(&mut self, addr: u16, val: u8) {
        match addr {
            0x0000..0x2000 => self.rom[addr] = val,
            0x2000..0x2400 => self.ram[addr - 0x2000] = val,
            0x2400..0x4000 => self.vram[addr - 0x2400] = val,
            _ => self.ram[addr & 0x3ff] = val,
        }
    }
}

struct SpaceInvadersIoBus {}

impl BusRead<u8> for SpaceInvadersMemoryBus {
    fn read8(&self, addr: u8) -> u8 {
        match addr {
            _ => todo!(),
        }
    }
}

impl BusWrite<u8> for SpaceInvadersMemoryBus {
    fn write8(&mut self, addr: u8, val: u8) {
        match addr {
            _ => todo!(),
        }
    }
}

