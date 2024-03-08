use crate::devices::prelude::*;

#[derive(Debug)]
pub struct Rom {
    data: Vec<u8>,
    name: String,
}

impl Device for Rom {
    fn as_mmio(&mut self) -> Option<&mut dyn Mmio> { Some(self) }
}

impl Mmio for Rom {
    fn read8(&mut self, addr: u32) -> u8 { self.data[addr as usize] }
    fn write8(&mut self, _: u32, _: u8) { eprintln!("Invalid write to ROM") }
    fn new(_compats: &[&str], reg: Option<(u32, u32)>, node: &DevTreeNode) -> Box<Self> {
        let len = reg.expect("reg has to be supplied with a memory device").1 as usize;
        let mut prop_iter = node.props();
        let mut name = None;
        while let Some(prop) = prop_iter.next().unwrap() {
            if prop.name().unwrap() == "rom,name" {
                name = Some(prop.str().unwrap().to_string());
            }
        }
        Box::new(Self {
                data: vec![0; len],
                name: name.expect("rom,name must contain human-readable name of the ROM"),
            }
        )
    }
    fn firmware_name(&mut self) -> Option<&str> {
        Some(&self.name)
    }
    fn load_firmware(&mut self, fw: &[u8]) {
        for (i, x) in fw.iter().enumerate() {
             self.data[i] = *x;
        }
    }
}

#[derive(Debug)]
pub struct Memory {
    data: Vec<u8>,
}

impl Device for Memory {
    fn as_mmio(&mut self) -> Option<&mut dyn Mmio> { Some(self) }
}

impl Mmio for Memory {
    fn read8(&mut self, addr: u32) -> u8 { self.data[addr as usize] }
    fn write8(&mut self, addr: u32, val: u8) { self.data[addr as usize] = val }
    fn new(_compats: &[&str], reg: Option<(u32, u32)>, _node: &DevTreeNode) -> Box<Self> {
        let len = reg.expect("reg has to be supplied with a memory device").1 as usize;
        Box::new(Self { data: vec![0; len] } )
    }
}
