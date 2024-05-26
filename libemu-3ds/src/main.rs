use ctru::prelude::*;

use libemu::{Backend, Machine, Key, BrokenKeyboard, TerminalOutput};
use libemu::cpm::CpmMachine;
use std::convert::Infallible;
use std::path::PathBuf;
use std::cell::OnceCell;

static mut APT: OnceCell<Apt> = OnceCell::new();
static mut GFX: OnceCell<Gfx> = OnceCell::new();
static mut HID: OnceCell<Hid> = OnceCell::new();

unsafe fn apt() -> &'static Apt {
    APT.get_or_init(|| Apt::new().unwrap())
}

unsafe fn gfx() -> &'static Gfx {
    GFX.get_or_init(|| Gfx::new().unwrap())
}

unsafe fn hid() -> &'static Hid {
    HID.get_or_init(|| Hid::new().unwrap())
}

unsafe fn hid_mut() -> &'static mut Hid {
    let _ = hid();
    HID.get_mut().unwrap()
}

unsafe fn gfx_mut() -> &'static mut Gfx {
    let _ = gfx();
    GFX.get_mut().unwrap()
}

struct CtrBackend<'screen> {
    program: Vec<u8>,
    quit: bool,
    out: CtrOutput<'screen>,
}

impl<'screen> Backend for CtrBackend<'screen> {
    type Input = BrokenKeyboard;
    type Output = CtrOutput<'screen>;
    fn should_exit(&mut self) -> bool {
        unsafe {
            //hid_mut().scan_input();
            //let keys = hid_mut().keys_held();
            //!(apt().main_loop()) || self.quit || keys.contains(KeyPad::START)
        }
        false
    }
    fn request_firmware(&mut self, _: &str) -> Vec<u8> {
        self.program.clone()
    }
    fn i(&mut self) -> &mut Self::Input {
        BrokenKeyboard::ref_mut()
    }
    fn o<'a>(&'a mut self) -> &'a mut CtrOutput<'screen> {
        &mut self.out
    }
}

struct CtrOutput<'screen> {
    bottom: Console<'screen>,
    top: Console<'screen>,
}

impl<'screen> TerminalOutput for CtrOutput<'screen> {
    type Error = Infallible;
    fn write_key(&mut self, k: Key) -> Result<(), Self::Error> {
        self.top.select();
        match k {
            Key::Character(ch) => print!("{}", ch),
            _ => (),
        }
        Ok(())
    }
    /*fn request_firmware(&mut self, name: &str) -> Vec<u8> {
        self.bottom.select();
        let prompt = format!("Select file for '{}'\r", name);
        println!("{prompt}");
        let mut path = PathBuf::from("/");
        let mut selected = false;
        unsafe {
            while !selected && !self.should_exit() {
                // wait for input
                let mut keys = KeyPad::empty();
                while apt().main_loop() && keys.is_empty() {
                    hid_mut().scan_input();
                    keys = hid().keys_down();
                }
                let mut iter = std::fs::read_dir(&path).unwrap();
                for file in iter {
                    println!("{:?}", file);
                }
            }
        }
        println!("wooo");
        loop {}
    }*/
}

fn main() {
    let (top, bottom) = unsafe {
        let mut topscreen = gfx_mut().top_screen.borrow_mut();
        topscreen.set_wide_mode(true);
        let top = Console::new(topscreen);
        let bottom = Console::new(gfx_mut().bottom_screen.borrow_mut());
        (top, bottom)
    };

    let old_hook = std::panic::take_hook();
    std::panic::set_hook(Box::new(move |panic_info| {
        old_hook(panic_info);
        loop {} // why does the old hook seem to quit
    }));

    // ensure backend is dropped so that a new console can be made
    let backend = CtrBackend {
        program: include_bytes!("tst8080.com").to_vec(),
        quit: false,
        out: CtrOutput {
            top,
            bottom,
        },
    };
    let mut machine = CpmMachine::new(backend);

    while machine.tick().unwrap() {}
    drop(machine);

    unsafe {
        let _top = Console::new(gfx_mut().top_screen.borrow_mut());
        println!("\n\n Machine stopped, press START to exit");
        while apt().main_loop() {
            hid_mut().scan_input();
            if hid().keys_down().contains(KeyPad::START) {
                break;
            }
        }
    }
}
