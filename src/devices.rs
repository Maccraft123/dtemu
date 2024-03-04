use crate::Mmio;


use fdt_rs::base::DevTreeNode;
use fdt_rs::prelude::*;

mod mc6821 {
    use crate::Mmio;


    use fdt_rs::base::DevTreeNode;
    use fdt_rs::prelude::*;

    #[derive(Debug)]
    pub struct Mc6821 {
    }

    impl Mmio for Mc6821 {
        fn read8(&mut self, addr: u32) -> u8 {
            match addr {
                0 => 0,
                1 => 0,
                2 => 0,
                3 => 0,
                _ => unreachable!(),
            }
        }
        fn write8(&mut self, addr: u32, val: u8) {
            match addr {
                0 => print!("{:x}", val),
                1 => print!("{:x}", val),
                2 => print!("{:x}", val),
                3 => print!("{:x}", val),
                _ => unreachable!(),
            }
        }
        fn new(compats: &[&str], reg: Option<(u32, u32)>, node: &DevTreeNode) -> Box<Self> {
            let len = reg.expect("reg has to be supplied with a memory-mapped device").1 as usize;
            Box::new(Self { } )
        }
    }
}

#[derive(Debug)]
struct Rom {
    data: Vec<u8>,
    name: String,
}

impl Mmio for Rom {
    fn read8(&mut self, addr: u32) -> u8 { self.data[addr as usize] }
    fn write8(&mut self, addr: u32, _: u8) { eprintln!("Invalid write to ROM") }
    fn new(compats: &[&str], reg: Option<(u32, u32)>, node: &DevTreeNode) -> Box<Self> {
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
        self.data = fw.to_vec();
    }
}

#[derive(Debug)]
struct Memory {
    data: Vec<u8>,
}

impl Mmio for Memory {
    fn read8(&mut self, addr: u32) -> u8 { self.data[addr as usize] }
    fn write8(&mut self, addr: u32, val: u8) { self.data[addr as usize] = val }
    fn new(compats: &[&str], reg: Option<(u32, u32)>, node: &DevTreeNode) -> Box<Self> {
        let len = reg.expect("reg has to be supplied with a memory device").1 as usize;
        Box::new(Self { data: vec![0; len] } )
    }
}

pub fn probe(compats: Vec<&str>, node: DevTreeNode) -> Option<Box<dyn Mmio>> {
    println!("probing {:?}", compats);
    let mut prop_iter = node.props();
    let mut reg = None;
    while let Some(prop) = prop_iter.next().unwrap() {
        if prop.name().unwrap() == "reg" {
            reg = Some((prop.u32(0).unwrap(), prop.u32(1).unwrap()));
        }
    }

    let mut dev: Option<Box<dyn Mmio>> = None;
    for compatible in compats.iter() {
        dev = match *compatible {
            "memory" => Some(Memory::new(&compats, reg, &node)),
            "motorola,mc6821" => Some(mc6821::Mc6821::new(&compats, reg, &node)),
            "generic-rom" => Some(Rom::new(&compats, reg, &node)),
            _ => None,
        };
        if dev.is_some() {
            break;
        }
    }

    if dev.is_none() {
        eprintln!("Warning: Failed to look up device for compatible strings {:?}", compats);
    }

    dev
}
