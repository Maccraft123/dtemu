use crate::MemoryWrapper;

use std::sync::mpsc;
use fdt::node::FdtNode;

pub mod mc6821;
pub mod memory;
pub mod pvconsole;

pub mod prelude {
    pub use crate::MemoryWrapper;
    pub use super::{Device, Mmio, Console, ConsoleOutput, ConsoleInput};
    pub use fdt::node::FdtNode;
}

pub mod console_prelude {
    pub use super::prelude::*;
    pub use crossterm::event::Event;
}

pub trait Device: core::fmt::Debug + Send {
    fn new(node: &FdtNode<'_, '_>) -> Box<Self> where Self: Sized;
    fn node_name(&self) -> &str;
    fn as_mmio(&mut self) -> Option<&mut dyn Mmio> { None }
    fn as_console_output(&mut self) -> Option<&mut dyn ConsoleOutput> { None }
    fn as_console_input(&mut self) -> Option<&mut dyn ConsoleInput> { None }
    fn as_console(&mut self) -> Option<&mut dyn Console> { None }
    //fn as_gpio(&mut self) -> Option<&mut dyn Gpio> { None }
}

pub trait Mmio: Device {
    fn read8(&mut self, addr: u32) -> u8;
    fn write8(&mut self, addr: u32, val: u8);
    fn tick(&mut self, _: MemoryWrapper) {}
    fn firmware_name(&mut self) -> Option<&str> { None }
    fn load_firmware(&mut self, _fw: &[u8]) {}
}

/*pub trait Gpio: Device {
    fn get_gpio_output(&mut self, pin: u32) -> Arc<AtomicBool>;
    fn get_gpio_input(&mut self, pin: u32) -> Arc<AtomicBool>;
    fn get_gpioset_output(&mut self, pins: &[u23]) -> Arc<dyn GpioSetOutput>;
    fn get_gpioset_input(&mut self, pins: &[u23]) -> Arc<dyn GpioSetInput>;
}

pub trait OutputPin {
    fn set(&self, state: bool);
}

pub trait InputPin {
    fn get(&self) -> bool;
}*/

pub trait Console: ConsoleInput + ConsoleOutput {}

pub trait ConsoleOutput: Device {
    fn attach_outchan(&mut self, channel: mpsc::Sender<char>);
    fn terminal_size(&self) -> (u8, u8);
}

pub trait ConsoleInput: Device {
    fn attach_inchan(&mut self, channel: mpsc::Receiver<crossterm::event::Event>);
}

pub fn probe(node: &FdtNode<'_, '_>) -> Option<Box<dyn Mmio>> {
    println!("probing {:?}", node.name);
    let mut dev: Option<Box<dyn Mmio>> = None;
    let compats = node.compatible()?;
    for compatible in compats.all() {
        dev = match compatible {
            "memory" => Some(memory::Memory::new(node)),
            "motorola,mc6821" => Some(mc6821::Mc6821::new(node)),
            "generic-rom" => Some(memory::Rom::new(node)),
            "dtemu,pv-console" => Some(pvconsole::PvConsole::new(node)),
            _ => None,
        };
        if dev.is_some() {
            break;
        }
    }

    if dev.is_none() {
        eprintln!("Warning: Failed to look up device for compatible strings {:?}", compats.all().collect::<Vec<&str>>());
    }

    dev
}
