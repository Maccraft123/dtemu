#![no_std]
#![no_main]

extern crate alloc;

use libemu::{Backend, Machine, Key};
use libemu::CpmMachine;
use core::convert::Infallible;
use alloc::boxed::Box;
use alloc::vec::Vec;

psp::module!("libemu_psp", 1, 1);

struct PspBackend {
    program: Vec<u8>,
    quit: bool,
}

impl Backend for PspBackend {
    type TtyError = Infallible;
    fn should_exit(&mut self) -> bool {
        self.quit
    }
    fn write_key(&mut self, k: Key) -> Result<(), Self::TtyError> {
        match k {
            Key::Character(ch) => psp::dprint!("{}", ch),
            _ => (),
        }
        Ok(())
    }
    fn read_key(&mut self) -> Result<Key, Self::TtyError> {
        todo!("keyboard")
    }
    fn poll_key(&mut self) -> Result<Option<Key>, Self::TtyError> {
        todo!("keyboard")
    }
    fn peek_key(&mut self) -> Result<Option<Key>, Self::TtyError> {
        todo!("keyboard")
    }
    fn request_firmware(&mut self, _: &str) -> Vec<u8> {
        self.program.clone()
    }
}

fn psp_main() {
    psp::enable_home_button();
    
    let backend = PspBackend {
        program: include_bytes!("tst8080.com").to_vec(),
        quit: false,
    };
    let mut machine = CpmMachine::new(backend);
    let mut run = true;

    while machine.tick().unwrap() {}
    psp::dprintln!("\n\n Machine stopped");
}
