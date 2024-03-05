use crate::MemoryWrapper;

use std::sync::mpsc;

use fdt_rs::base::DevTreeNode;
use fdt_rs::prelude::*;

pub trait Device: core::fmt::Debug {
    fn as_mmio(&mut self) -> Option<&mut dyn Mmio> { None }
    fn as_console_output(&mut self) -> Option<&mut dyn ConsoleOutput> { None }
    fn as_console_input(&mut self) -> Option<&mut dyn ConsoleInput> { None }
    fn as_console(&mut self) -> Option<&mut dyn Console> { None }
}

pub trait Mmio: Device {
    fn read8(&mut self, addr: u32) -> u8;
    fn write8(&mut self, addr: u32, val: u8);
    fn new(compats: &[&str], reg: Option<(u32, u32)>, node: &DevTreeNode) -> Box<Self> where Self: Sized;
    fn tick(&mut self, _: MemoryWrapper) {}

    fn firmware_name(&mut self) -> Option<&str> { None }
    fn load_firmware(&mut self, _fw: &[u8]) {}
}

pub trait Console: ConsoleInput + ConsoleOutput {}

pub trait ConsoleOutput: Device {
    fn attach_outchan(&mut self, channel: mpsc::Sender<char>);
}

pub trait ConsoleInput: Device {
    fn attach_inchan(&mut self, channel: mpsc::Receiver<char>);
}

mod mc6821 {
    use crate::devices::{Device, Mmio, ConsoleOutput, Console, ConsoleInput};

    use std::sync::mpsc;

    use fdt_rs::base::DevTreeNode;

    #[derive(Debug)]
    pub struct Mc6821 {
        output: Option<mpsc::Sender<char>>,
        input: Option<mpsc::Receiver<char>>,
    }

    impl Device for Mc6821 {
        fn as_mmio(&mut self) -> Option<&mut dyn Mmio> { Some(self) }
        fn as_console_output(&mut self) -> Option<&mut dyn ConsoleOutput> { Some(self) }
        fn as_console_input(&mut self) -> Option<&mut dyn ConsoleInput> { Some(self) }
        fn as_console(&mut self) -> Option<&mut dyn Console> { Some(self) }
    }

    impl Mmio for Mc6821 {
        fn read8(&mut self, addr: u32) -> u8 {
            match addr {
                // keyboard input
                0 => 0,
                // keyboard control
                1 => 0,
                // display output
                2 => 0,
                // display control
                3 => 0,
                _ => unreachable!(),
            }
        }
        fn write8(&mut self, addr: u32, mut val: u8) {
            match addr {
                // keyboard input
                0 => (),
                // keyboard control
                1 => (),
                // display output
                2 => {
                    if let Some(ch) = &mut self.output {
                        // magic that i stole from https://github.com/RyuKojiro/apple1/blob/master/src/pia.c
                        val &= !0x80;

                        if val == b'\r' {
                            val = b'\n';
                        }
                        let _ = ch.send(char::from_u32(val as u32).unwrap());
                    }
                },
                // display control
                3 => (),
                _ => unreachable!(),
            }
        }
        fn new(_compats: &[&str], _reg: Option<(u32, u32)>, _node: &DevTreeNode) -> Box<Self> {
            //let len = reg.expect("reg has to be supplied with a memory-mapped device").1 as usize;
            Box::new(Self { output: None, input: None } )
        }
    }

    impl ConsoleOutput for Mc6821 {
        fn attach_outchan(&mut self, channel: mpsc::Sender<char>) {
            self.output = Some(channel);
        }
    }

    impl ConsoleInput for Mc6821 {
        fn attach_inchan(&mut self, channel: mpsc::Receiver<char>) {
            self.input = Some(channel);
        }
    }
    impl Console for Mc6821 {}
}

#[derive(Debug)]
struct Rom {
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
        self.data = fw.to_vec();
    }
}

#[derive(Debug)]
struct Memory {
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
