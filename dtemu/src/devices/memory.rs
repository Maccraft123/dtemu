use crate::devices::prelude::*;

#[derive(Debug)]
pub struct Rom {
    data: Vec<u8>,
    rom_name: String,
    allow_write: bool,
    name: String,
}

impl Device for Rom {
    fn as_mmio(&mut self) -> Option<&mut dyn Mmio> { Some(self) }
    fn node_name(&self) -> &str { &self.name }
    fn new(_compats: &[&str], reg: Option<(u32, u32)>, node: &DevTreeNode) -> Box<Self> {
        let len = reg.expect("reg has to be supplied with a memory device").1 as usize;
        let name = node.name().unwrap().to_string();
        let mut prop_iter = node.props();
        let mut rom_name = None;
        let mut write = false;
        while let Some(prop) = prop_iter.next().unwrap() {
            if prop.name().unwrap() == "rom,name" {
                rom_name = Some(prop.str().unwrap().to_string());
            } else if prop.name().unwrap() == "rom,allow-write" {
                write = true;
            }
        }
        Box::new(Self {
                data: vec![0; len],
                rom_name: rom_name.expect("rom,name must contain human-readable name of the ROM"),
                allow_write: write,
                name,
            }
        )
    }
}

impl Mmio for Rom {
    fn read8(&mut self, addr: u32) -> u8 { self.data[addr as usize] }
    fn write8(&mut self, addr: u32, val: u8) {
        if self.allow_write {
            self.data[addr as usize] = val
        } else {
            eprintln!("Invalid write to ROM")
        }
    }
    fn firmware_name(&mut self) -> Option<&str> {
        Some(&self.rom_name)
    }
    fn load_firmware(&mut self, fw: &[u8]) {
        if fw.len() > self.data.len() {
            panic!("file too big, my size: {:x}, file size: {:x}", self.data.len(), fw.len());
        }
        for (i, x) in fw.iter().enumerate() {
             self.data[i] = *x;
        }
    }
}

#[derive(Debug)]
pub struct Memory {
    data: Vec<u8>,
    name: String,
}

impl Device for Memory {
    fn as_mmio(&mut self) -> Option<&mut dyn Mmio> { Some(self) }
    fn node_name(&self) -> &str { &self.name }
    fn new(_compats: &[&str], reg: Option<(u32, u32)>, node: &DevTreeNode) -> Box<Self> {
        let len = reg.expect("reg has to be supplied with a memory device").1 as usize;
        let name = node.name().unwrap().to_string();
        Box::new(Self { data: vec![0; len], name } )
    }
}

impl Mmio for Memory {
    fn read8(&mut self, addr: u32) -> u8 { self.data[addr as usize] }
    fn write8(&mut self, addr: u32, val: u8) { self.data[addr as usize] = val }
}
