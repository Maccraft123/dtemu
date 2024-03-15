use crate::MemoryWrapper;

use std::sync::mpsc;

use fdt_rs::base::DevTreeNode;
use fdt_rs::prelude::*;

pub mod mc6821;
pub mod memory;

pub mod prelude {
    pub use crate::MemoryWrapper;
    pub use super::{Device, Mmio, Console, ConsoleOutput, ConsoleInput};
    pub use fdt_rs::prelude::*;
    pub use fdt_rs::base::DevTreeNode;
}

pub mod console_prelude {
    pub use super::prelude::*;
    pub use crossterm::event::Event;
}

mod blargg {
    use super::prelude::*;
    #[derive(Debug)]
    pub struct Dumper();
    impl Device for Dumper {
        fn as_mmio(&mut self) -> Option<&mut dyn Mmio> {
            Some(self)
        }
    }
    impl Mmio for Dumper {
        fn new(compats: &[&str], reg: Option<(u32, u32)>, node: &DevTreeNode) -> Box<Self> {
            Box::new(Self())
        }
        fn read8(&mut self, _: u32) -> u8 { 0 }
        fn write8(&mut self, _: u32, val: u8) {
            if val != 0x80 {
                eprintln!("{:x}", val);
            }
        }
    }
}

pub trait Device: core::fmt::Debug + Send {
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
    fn terminal_size(&self) -> (u8, u8);
}

pub trait ConsoleInput: Device {
    fn attach_inchan(&mut self, channel: mpsc::Receiver<crossterm::event::Event>);
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
            "memory" => Some(memory::Memory::new(&compats, reg, &node)),
            "motorola,mc6821" => Some(mc6821::Mc6821::new(&compats, reg, &node)),
            "generic-rom" => Some(memory::Rom::new(&compats, reg, &node)),

            "blargg,instr_test_v4_output" => Some(blargg::Dumper::new(&compats, reg, &node)),
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
