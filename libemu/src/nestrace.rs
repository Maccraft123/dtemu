use asane::mos6502::Mos6502;
use asane::NoopBus;
use cartridge::{INesFile, Mapper};
use crate::inner_prelude::*;
use crate::{KeyboardInput, TerminalOutput};

pub struct Nestrace<T: Backend<Input: KeyboardInput, Output: TerminalOutput>> {
    cpu: Mos6502,
    memory: NesMemory,
    backend: T,
    cycles: usize,
}

impl<T: Backend<Input: KeyboardInput, Output: TerminalOutput>> Machine<T> for Nestrace<T> {
    #[inline]
    fn cycles(&self) -> usize {
        self.cycles
    }
    #[inline]
    fn new(mut backend: T) -> Nestrace<T> {
        let program = backend.request_firmware("NES ROM");
        let mut memory = NesMemory::with_cartridge(&program);
        Self {
            cpu: Mos6502::new(&mut memory, &mut ()),
            memory,
            backend,
            cycles: 7,
        }
    }
    #[inline]
    fn tick(&mut self) -> Result<bool, Box<dyn Error>> {
        cassette::block_on(async {
            self.cpu.set_pc(0xc000);
            if self.backend.should_exit() {
                return Ok(false)
            }
            for _ in 0..=0xffff {
                self.cycles += self.cpu.step_block(&mut self.memory, &mut ()).await.0;
            }

            Ok(true)
        })
    }
}

struct NesPpu {
    chr_rom: Vec<u8>,
    palette_table: [u8; 32],
    vram: [u8; 2048],
    oam_data: [u8; 256],
    mirroring: bool,
}

impl NesPpu {
    fn new(chr_rom: Vec<u8>, mirroring: bool) -> Self {
        Self {
           chr_rom: chr_rom,
           mirroring: mirroring,
           vram: [0; 2048],
           oam_data: [0; 64 * 4],
           palette_table: [0; 32],
        }
    }
}

struct NesMapper {
    file: INesFile,
    m: Mapper,
    //prg_ram: Vec<u8>,
    prg_rom: Vec<u8>,
    //chr_rom: Vec<u8>,
}

impl NesMapper {
    fn new(file: INesFile) -> Self {
        let m = file.mapper();
        let prg_rom = file.prg_rom().to_vec();
        Self {
            file,
            m,
            prg_rom,
        }
    }
    fn read8_cpu(&self, addr: u16) -> u8 {
        match self.m {
            Mapper::Nrom => {
                match addr {
                    0xc000..=0xffff if self.prg_rom.len() < 0x8000 => self.prg_rom[addr as usize - 0xc000],
                    0x8000..=0xffff => self.prg_rom[addr as usize - 0x8000],
                    _ => 0,
                }
            },
            _ => todo!(),
        }
    }
    fn write8_cpu(&mut self, addr: u16, val: u8) {
        match self.m {
            Mapper::Nrom => {
                match addr {
                    0xc000..=0xffff if self.prg_rom.len() < 0x8000 => self.prg_rom[addr as usize - 0xc000] = val,
                    0x8000..=0xffff => self.prg_rom[addr as usize - 0x8000] = val,
                    _ => (),
                }
            },
            _ => todo!(),
        }
    }
}

struct NesMemory {
    ram: [u8; 0x800],
    mapper: NesMapper,
    ppu: NesPpu,
}

impl NesMemory {
    fn with_cartridge(c: &[u8]) -> Self {
        let cart = INesFile::parse(c).unwrap().1;
        let ppu = NesPpu::new(cart.chr_rom().to_vec(), cart.header().nametable_arrangement());
        let mapper = NesMapper::new(cart);
        Self {
            ram: [0u8; 0x800],
            mapper,
            ppu,
        }
    }
}

use asane::{BusWrite, BusRead};

impl BusRead<u16> for NesMemory {
    fn read8(&mut self, addr: u16) -> u8 {
        match addr {
            0x0000..=0x1fff => self.ram[addr as usize & 0x7ff],
            0x2000..=0x3fff => todo!("ppu"),
            0x4000..=0x401f => todo!(),
            _ => self.mapper.read8_cpu(addr),
        }
    }
}

impl BusWrite<u16> for NesMemory {
    fn write8(&mut self, addr: u16, val: u8) {
        match addr {
            0x0000..=0x1fff => self.ram[addr as usize & 0x7ff] = val,
            0x2000..=0x3fff => todo!("ppu"),
            0x4000..=0x401f => todo!(),
            _ => self.mapper.write8_cpu(addr, val),
        }
    }
}
