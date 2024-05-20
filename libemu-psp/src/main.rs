#![no_std]
#![no_main]

extern crate alloc;

use libemu::{Backend, Machine, Key, BrokenKeyboard, TerminalOutput};
use libemu::cpm::CpmMachine;
use core::convert::Infallible;
use alloc::boxed::Box;
use alloc::vec::Vec;

psp::module!("libemu_psp", 1, 1);

struct PspBackend {
    program: Vec<u8>,
    quit: bool,
    out: PspOutput,
}

impl Backend for PspBackend {
    type Input = BrokenKeyboard;
    type Output = PspOutput;
    fn should_exit(&mut self) -> bool {
        self.quit
    }
    fn request_firmware(&mut self, _: &str) -> Vec<u8> {
        self.program.clone()
    }
    fn i(&mut self) -> &mut Self::Input {
        BrokenKeyboard::ref_mut()
    }
    fn o(&mut self) -> &mut PspOutput {
        &mut self.out
    }
}

struct PspOutput;

impl TerminalOutput for PspOutput {
    type Error = Infallible;
    fn write_key(&mut self, key: Key) -> Result<(), Self::Error> {
        match key {
            Key::Character(ch) if ch != '\r' => psp::dprint!("{}", ch),
            _ => (),
        }
        Ok(())
    }
}

fn psp_main() {
    psp::enable_home_button();
    
    let backend = PspBackend {
        program: include_bytes!("tst8080.com").to_vec(),
        quit: false,
        out: PspOutput,
    };
    let mut machine = CpmMachine::new(backend);
    let mut run = true;

    while machine.tick().unwrap() {}
    psp::dprintln!("\n\n Machine stopped");
}
