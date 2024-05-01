use crossbeam_channel::{Sender, Receiver};
use crate::MemoryWrapper;
use parking_lot::Mutex;
use fdt::node::FdtNode;
use std::sync::Arc;
use std::any::Any;

pub mod mc6821;
pub mod mc6850;
pub mod memory;
pub mod pvconsole;
//pub mod nescart;
//pub mod ricoh2c02;

pub mod prelude {
    pub use parking_lot::Mutex;
    pub use std::sync::Arc;
    pub use super::{Device, Mmio, Console, ConsoleOutput, ConsoleInput};
    pub use fdt::node::FdtNode;
}

pub mod console_prelude {
    pub use super::prelude::*;
    pub use crossbeam_channel::{Sender, Receiver};
    pub use crossterm::event::Event;
}

//pub mod graphics_prelude {
//    pub use super::prelude::*;
//    pub use crossbeam_channel::{Sender, Receiver};
//    pub use rgb::RGB8;
//}

pub trait Device: Send + Sync + Any {
    fn new(node: &FdtNode<'_, '_>) -> Arc<Mutex<Self>> where Self: Sized;
    fn init(&mut self, _: MemoryWrapper) {}
    fn node_name(&self) -> &str;
    fn phandle(&self) -> Option<usize>;
    fn as_mmio(&mut self) -> Option<&mut dyn Mmio> { None }
    fn as_console_output(&mut self) -> Option<&mut dyn ConsoleOutput> { None }
    fn as_console_input(&mut self) -> Option<&mut dyn ConsoleInput> { None }
    fn as_console(&mut self) -> Option<&mut dyn Console> { None }
    //fn as_gpio(&mut self) -> Option<&mut dyn Gpio> { None }
    fn firmware_name(&mut self) -> Option<&str> { None }
    fn load_firmware(&mut self, _fw: &[u8]) {}
    fn tick(&mut self) {}

    // that's so ugly
    //fn as_nescart(&mut self) -> Option<&mut NesCartridge> { None }
}

pub trait Mmio: Send + Sync {
    fn read8(&mut self, addr: u32) -> u8;
    fn write8(&mut self, addr: u32, val: u8);
}

pub trait MmioDevice: Mmio + Device {}
impl<T: Mmio + Device + Send> MmioDevice for T {}

//pub trait Graphics: Device {
//    fn attach_pixchan(&mut self, _: Sender<(u32, u32, rgb::RGB8)>);
//}

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
    fn attach_outchan(&mut self, channel: Sender<char>);
    fn terminal_size(&self) -> (u8, u8);
}

pub trait ConsoleInput: Device {
    fn attach_inchan(&mut self, channel: Receiver<crossterm::event::Event>);
}

pub fn probe(node: &FdtNode<'_, '_>) -> Option<Arc<Mutex<dyn MmioDevice>>> {
    println!("probing {:?}", node.name);
    let mut dev: Option<Arc<Mutex<dyn MmioDevice>>> = None;
    let compats = node.compatible()?;
    for compatible in compats.all() {
        dev = match compatible {
            "memory" => Some(memory::Memory::new(node)),
            "motorola,mc6821" => Some(mc6821::Mc6821::new(node)),
            "motorola,mc6850" => Some(mc6850::Mc6850::new(node)),
            "generic-rom" => Some(memory::Rom::new(node)),
            "dtemu,pv-console" => Some(pvconsole::PvConsole::new(node)),
            //"nintendo,nes-cartridge" => Some(nescart::NesCartridge::new(node)),
            //"ricoh,2c02" => Some(ricoh2c02::Ricoh2c02::new(node)),
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
