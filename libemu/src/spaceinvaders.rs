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
    fn new(mut backend: T) -> SpaceInvaders<T> {
        backend.o().set_resolution((256, 224)).unwrap();
        Self {
            cpu: Intel8080::new(),
            memory: SpaceInvadersMemoryBus::with_rom(backend.request_firmware("Space Invaders ROM")),
            io: SpaceInvadersIoBus,
            backend,
        }
    }
    #[inline]
    fn tick(&mut self) -> Result<bool, Box<dyn Error>> {
        use asane::intel8085::{Reg, RegPair};
        if self.backend.should_exit() {
            return Ok(false)
        }
        self.cpu.step_instructions_sync(1024, &mut self.memory, &mut self.io);

        for row in (0..0x1be0).step_by(32) {
            for addr in 0..32 {
                let px = self.memory.vram[row | addr];
            }
        }

        Ok(true)
    }
}

struct SpaceInvadersMemoryBus {
    rom: [u8; 0x2000],
    ram: [u8; 0x400],
    vram: [u8; 0x1c00],
}

impl SpaceInvadersMemoryBus {
    fn with_rom(r: Vec<u8>) -> Self {
        Self {
            rom: r.try_into().unwrap(),
            ram: [0u8; 0x400],
            vram: [0u8; 0x1c00],
        }
    }
}

impl BusRead<u16> for SpaceInvadersMemoryBus {
    fn read8(&self, addr: u16) -> u8 {
        match addr {
            0x0000..0x2000 => self.rom[addr as usize],
            0x2000..0x2400 => self.ram[addr as usize - 0x2000],
            0x2400..0x4000 => self.vram[addr as usize - 0x2400],
            _ => self.ram[addr as usize & 0x3ff]
        }
    }
}

impl BusWrite<u16> for SpaceInvadersMemoryBus {
    fn write8(&mut self, addr: u16, val: u8) {
        match addr {
            0x0000..0x2000 => self.rom[addr as usize] = val,
            0x2000..0x2400 => self.ram[addr as usize - 0x2000] = val,
            0x2400..0x4000 => self.vram[addr as usize - 0x2400] = val,
            _ => self.ram[addr as usize & 0x3ff] = val,
        }
    }
}

struct SpaceInvadersIoBus;

impl BusRead<u8> for SpaceInvadersIoBus {
    fn read8(&self, addr: u8) -> u8 {
        match addr {
            _ => todo!(),
        }
    }
}

impl BusWrite<u8> for SpaceInvadersIoBus {
    fn write8(&mut self, addr: u8, val: u8) {
        match addr {
            _ => todo!(),
        }
    }
}

