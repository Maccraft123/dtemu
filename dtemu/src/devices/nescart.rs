use crate::devices::prelude::*;
use cartridge::{INesFile, INesHeader};

trait Mapper: Mmio + Send + Sync {
    fn new(_: INesFile) -> Box<Self> where Self: Sized;
    fn read8_chr(&self, addr: u16) -> u8;
    fn write8_chr(&mut self, addr: u16, val: u8);
}

struct Nrom {
    inner: INesFile,
    prgrom_mask: usize,
    prgram: [u8; 8192],
}

impl Mapper for Nrom {
    fn new(file: INesFile) -> Box<Self> {
        let prgrom_len = file.prg_rom().len();
        if prgrom_len != 16384 && prgrom_len != 32768 {
            panic!("PRGROM size {} is invalid for NROM mapper", prgrom_len);
        };
        Box::new(Self {
            inner: file,
            prgrom_mask: if prgrom_len == 32768 { 0xffff } else { 0x3fff },
            prgram: [0u8; 8192],
        })
    }
    fn read8_chr(&self, addr: u16) -> u8 {
        self.inner.chr_rom()[addr as usize]
    }
    fn write8_chr(&mut self, _: u16, _: u8) {}
}

impl Mmio for Nrom {
    fn write8(&mut self, addr: u32, val: u8) {
        let addr = addr as usize;
        match addr {
            0x6000..=0x7fff => self.prgram[addr - 0x6000] = val,
            0x8000..=0xffff=> eprintln!("Invalid write to PRGROM"),
            _ => unreachable!(),
        }
    }
    fn read8(&mut self, addr: u32) -> u8 {
        let addr = addr as usize;
        match addr {
            0x0000..=0x5fff => 0,
            0x6000..=0x7fff => self.prgram[addr - 0x6000],
            0x8000..=0xffff => self.inner.prg_rom()[(addr - 0x8000) & self.prgrom_mask],
            _ => unreachable!(),
        }
    }
}

pub struct NesCartridge {
    mapper: Option<Box<dyn Mapper>>,
    phandle: Option<usize>,
    name: String,
}

impl NesCartridge {
    pub(crate) fn read8_chr(&mut self, addr: u16) -> u8 {
        if let Some(ref m) = &self.mapper {
            m.read8_chr(addr)
        } else {
            0
        }
    }
    pub(crate) fn write8_chr(&mut self, addr: u16, val: u8) {
        if let Some(ref mut m) = &mut self.mapper {
            m.write8_chr(addr, val)
        }
    }
}

impl Device for NesCartridge {
    fn firmware_name(&mut self) -> Option<&str> { Some("nescart") }
    fn phandle(&self) -> Option<usize> { self.phandle }
    fn load_firmware(&mut self, fw: &[u8]) {
        let (leftover, file) = INesFile::parse(fw).unwrap();
        let mapper = match file.mapper() {
            cartridge::Mapper::Nrom => Nrom::new(file),
            _ => unimplemented!(),
        };
        self.mapper = Some(mapper);
    }
    fn node_name(&self) -> &str { &self.name }
    fn new(node: &FdtNode<'_, '_>) -> Arc<Mutex<Self>> {
        Arc::new(Mutex::new(Self {
            mapper: None,
            name: node.name.to_string(),
            phandle: node.property("phandle").map(|p| p.as_usize()).flatten(),
        }))
    }
    fn as_mmio(&mut self) -> Option<&mut dyn Mmio> { Some(self) }
    fn as_nescart(&mut self) -> Option<&mut NesCartridge> { Some(self) }
}

impl Mmio for NesCartridge {
    fn write8(&mut self, addr: u32, val: u8) {
        if let Some(ref mut m) = self.mapper {
            m.write8(addr + 0x4020, val)
        }
    }
    fn read8(&mut self, addr: u32) -> u8 {
        if let Some(ref mut m) = self.mapper {
            m.read8(addr + 0x4020)
        } else {
            0
        }
    }
}
