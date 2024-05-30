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

fn get_cur_tick() -> u64 {
    unsafe {
        let mut ret = 0;
        psp::sys::sceRtcGetCurrentTick(&mut ret as *mut u64);
        ret
    }
}

use core::time::Duration;

fn psp_main() {
    psp::enable_home_button();
    
    let backend = PspBackend {
        program: include_bytes!("tst8080.com").to_vec(),
        quit: false,
        out: PspOutput,
    };
    let mut machine = CpmMachine::new(backend);
    let mut run = true;
    let start = get_cur_tick();
    while machine.tick().unwrap() {}
    let cycles = machine.cycles();
    let end = get_cur_tick();
    let duration = Duration::from_micros(end - start);
    psp::dprintln!("\n\n Machine stopped");
    psp::dprintln!("{:.3}s passed", duration.as_secs_f32());
    let freq_mhz = (cycles as f32/duration.as_secs_f32())/1000000f32;
    psp::dprintln!("{:.3}MHz clock speed", freq_mhz);
}
