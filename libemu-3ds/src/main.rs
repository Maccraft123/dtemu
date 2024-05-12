use ctru::prelude::*;

use libemu::{Backend, Machine, Key};
use libemu::cpm::CpmMachine;
use std::convert::Infallible;

struct CtrBackend {
    program: Vec<u8>,
    quit: bool,
}

impl Backend for CtrBackend {
    type TtyError = Infallible;
    fn should_exit(&mut self) -> bool {
        self.quit
    }
    fn write_key(&mut self, k: Key) -> Result<(), Self::TtyError> {
        match k {
            Key::Character(ch) => print!("{}", ch),
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

fn main() {
    let apt = Apt::new().unwrap();
    let mut hid = Hid::new().unwrap();
    let gfx = Gfx::new().unwrap();
    let _console = Console::new(gfx.top_screen.borrow_mut());

    let backend = CtrBackend {
        program: include_bytes!("tst8080.com").to_vec(),
        quit: false,
    };
    let mut machine = CpmMachine::new(backend);
    let mut run = true;

    while apt.main_loop() && machine.tick().unwrap() {}
    println!("\n\n Machine stopped, press START to exit");
    while apt.main_loop() {
        hid.scan_input();
        if hid.keys_down().contains(KeyPad::START) {
            break;
        }
    }
}
