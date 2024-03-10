// i gotta clean those up later
use futures::{pending, poll};
use std::io::Write;
use std::sync::mpsc;
use std::path::PathBuf;
use clap::Parser;
use std::{io, thread, fs};
use std::sync::Arc;
use std::sync::atomic::{Ordering, AtomicBool};
use parking_lot::Mutex;
use fdt_rs::prelude::*;
use fdt_rs::base::DevTree;
use std::collections::HashMap;
use crossterm::event::{self, Event};
use std::time::Duration;

mod devices;
mod mos6502;
mod cpu;

use cpu::Cpu;

use mos6502::Mos6502;

use devices::{Mmio};

pub struct MemoryWrapper {
    ptr: Arc<Mutex<MemoryMap>>,
}

impl Clone for MemoryWrapper {
    fn clone(&self) -> Self { MemoryWrapper { ptr: Arc::clone(&self.ptr) } }
}

// FIXME: 6502 timings
impl MemoryWrapper {
    fn new(ptr: Arc<Mutex<MemoryMap>>) -> Self {
        Self { ptr }
    }
    //fn set8_fast(&self, addr: u32, val: u8) {
    //    self.ptr.lock().write8(addr, val)
    //}
    fn fetch8_fast(&self, addr: u32) -> u8 {
        self.ptr.lock().read8(addr)
    }
    async fn fetch8(&self, addr: u32) -> u8 {
        pending!();
        self.ptr.lock().read8(addr)
    }
    async fn set8(&self, addr: u32, val: u8) {
        pending!();
        self.ptr.lock().write8(addr, val)
    }
    async fn fetch16(&self, addr: u32) -> u16 {
        if addr & 0x000f == 0xf { // this correct? idk
            pending!();
        }
        u16::from_le_bytes([self.fetch8(addr).await, self.fetch8(addr+1).await])
    }
}

#[derive(Debug)]
struct MemoryMap {
    segments: Vec<Segment>,
}

impl MemoryMap {
    fn new() -> Self {
        Self { segments: Vec::new() }
    }
    fn insert_mmio_device(&mut self, device: Box<dyn Mmio>, start: u32, size: u32) {
        self.segments.push(Segment{device, start, size})
    }
    fn read8(&mut self, addr: u32) -> u8 {
        let mut data = None;
        for seg in self.segments.iter_mut() {
            if addr >= seg.start && addr < seg.start + seg.size {
                data = Some(seg.device.read8(addr - seg.start));
            }
        
        }
        data.unwrap_or(0)
        //data.expect(&format!("Invalid memory read at {:x}", addr))
    }
    fn write8(&mut self, mut addr: u32, val: u8) {
        let mut written = false;
        if addr == 0xd0f2 { addr = 0xd012; };
        for seg in self.segments.iter_mut() {
            if addr >= seg.start && addr < seg.start + seg.size {
                seg.device.write8(addr - seg.start, val);
                written = true;
            }
        }
        if !written {
            panic!("Invalid memory write {:x} = {:x}", addr, val);
        }
    }
}

#[derive(Debug)]
struct Segment {
    start: u32,
    size: u32,
    device: Box<dyn Mmio>,
}

#[derive(Debug)]
struct Machine {
    mem: Arc<Mutex<MemoryMap>>,
    console_in: Option<mpsc::Receiver<char>>,
    console_out: Option<mpsc::Sender<Event>>,
    console_size: (u8, u8),
}

impl Machine {
    fn new_mem_wrapper(&self) -> MemoryWrapper {
        MemoryWrapper::new(Arc::clone(&self.mem))
    }
}

#[derive(Parser, Debug)]
struct Args {
    #[arg(short, long)]
    machine: PathBuf,
    #[arg(long)]
    fw: Vec<String>,
}

#[repr(align(4))]
struct Align4<T>(T);

struct EmuTui<'a> {
    //mem: MemoryWrapper,
    console_recv: mpsc::Receiver<char>,
    console_send: mpsc::Sender<Event>,
    _console_size: (u8, u8),
    run: &'a AtomicBool,
}

impl EmuTui<'_> {
    fn run<'a> (&'a mut self) {
        use crossterm::{execute, terminal};

        let original_hook = std::panic::take_hook();
        std::panic::set_hook(Box::new(move |panic_info| {
            execute!(io::stderr(), terminal::LeaveAlternateScreen).unwrap();
            //execute!(io::stderr(), event::DisableMouseCapture).unwrap();
            terminal::disable_raw_mode().unwrap();
            original_hook(panic_info);
        }));

        let mut stdout = io::stdout();
        terminal::enable_raw_mode()
            .expect("failed to enable raw mode");
        //execute!(stdout, EnableMouseCapture)
        //    .expect("unable to enable mouse capture");
        //execute!(stdout, terminal::EnterAlternateScreen)
        //    .expect("unable to enter alternate screen");

        while self.run.load(Ordering::SeqCst) {
            if let Ok(true) = event::poll(Duration::from_millis(50)) {
                let ev = event::read().unwrap();
                match ev {
                    Event::Key(kev) => {
                        if kev.code == event::KeyCode::Esc {
                            self.run.store(false, Ordering::SeqCst);
                        } else {
                            if self.console_send.send(ev).is_err() {
                                self.run.store(false, Ordering::SeqCst);
                            }
                        }
                    },
                    _ => (),
                }
            }

            if let Ok(ch) = self.console_recv.try_recv() {
                write!(stdout, "{}", ch).unwrap();
                while let Ok(ch2) = self.console_recv.try_recv() {
                    write!(stdout, "{}", ch2).unwrap();
                }
                stdout.flush().unwrap();
            }
        }

        terminal::disable_raw_mode()
            .expect("failed to disable raw mode");
        //execute!(stdout, terminal::LeaveAlternateScreen)
        //    .expect("unable to switch to main screen");
        //execute!(terminal.backend_mut(), event::DisableMouseCapture)
        //    .expect("unable to disable mouse capture");
    }
}

fn run(mut mach: Machine) {
    let mut cycles = 0;
    let run = AtomicBool::new(true);
    let mut tui = EmuTui {
        //mem: mach.new_mem_wrapper(),
        console_recv: mach.console_in.take().unwrap(),
        console_send: mach.console_out.take().unwrap(),
        _console_size: mach.console_size,
        run: &run,
    };

    thread::scope(|s| {
        s.spawn(|| tui.run());

        futures::executor::block_on( async {
            let mem = mach.new_mem_wrapper();
            let mut cpu = Mos6502::new(mach.new_mem_wrapper());
            let mut cpu_fut = std::pin::pin!(cpu.tick());

            while run.load(Ordering::SeqCst)  {
                if poll!(&mut cpu_fut).is_ready() {
                    run.store(false, Ordering::SeqCst);
                }
                let cur = cpu.cur_instruction();
                let bytes = [
                    mem.fetch8_fast(cur),
                    mem.fetch8_fast(cur+1),
                    mem.fetch8_fast(cur+2),
                ];
                println!("{}", cpu.disasm_instruction(&bytes));
                cycles += 1;
            }
            cycles -= 1;
        } );
    });
}

fn main() {
    let args = Args::parse();

    let mut firmwares: HashMap<&str, Vec<u8>> = HashMap::new();
    for firmware in args.fw.iter() {
        if let Some((name, path)) = firmware.split_once('=') {
            let data = fs::read(&path)
                .expect(&format!("Failed to load {:?} file", path));
            firmwares.insert(name, data);
        } else {
            eprintln!("Invalid syntax {:?}, expected \"fw_name=file_path\"", firmware);
        }
    }

    // setup the machine itself
    let dt_data = Align4(fs::read(&args.machine)
        .expect("Failed to load DTB"));

    let devtree = unsafe {
        assert!(dt_data.0.len() >= DevTree::MIN_HEADER_SIZE);
        let size = DevTree::read_totalsize(&dt_data.0)
            .expect("Failed to read out total DTB size");
        let buf = &dt_data.0[..size];
        DevTree::new(buf)
            .expect("Failed creation of DevTree index")
    };

    let mut mem = MemoryMap::new();
    let mut node_iter = devtree.nodes();
    let root = devtree.root().unwrap().unwrap();
    while let Some(node) = node_iter.next().unwrap() {
        if node == root { continue; }

        let mut prop_iter = node.props();

        let mut dev = None;
        let mut reg = None;
        while let Some(prop) = prop_iter.next().unwrap() {
            match prop.name().unwrap() {
                "compatible" => {
                    let compat_strings: Vec<&str> = prop.iter_str().collect().unwrap();
                    dev = devices::probe(compat_strings, prop.node());
                },
                "reg" => {
                    reg = Some((prop.u32(0).unwrap(), prop.u32(1).unwrap()));
                },
                _ => (),
            }
        }

        if let Some(device) = dev {
            if let Some((start, size)) = reg {
                mem.insert_mmio_device(device, start, size);
            } else {
                eprintln!("Missing reg property for {:?}", node.name());
            }
        }
    }

    // wire up some devices
    let (mut device_sender, emu_receiver) = Some(mpsc::channel()).unzip();
    let (emu_sender, mut device_receiver) = Some(mpsc::channel()).unzip();
    let mut size = (0, 0);
    for device in mem.segments.iter_mut().map(|s| &mut s.device) {
        if let Some(name) = device.firmware_name() {
            if let Some(data) = firmwares.get(name) {
                device.load_firmware(&data);
            } else {
                eprintln!("Missing firmware for {:?}", name);
            }
        }

        if let Some(condev) = device.as_console_output() {
            if let Some(out) = device_sender.take() {
                condev.attach_outchan(out);
                size = condev.terminal_size();
            } else {
                eprintln!("Only one console output device may be active at any given moment");
            }
        }

        if let Some(condev) = device.as_console_input() {
            if let Some(inp) = device_receiver.take() {
                condev.attach_inchan(inp);
            } else {
                eprintln!("Only one console input device may be active at any given moment");
            }
        }
    }

    let mach = Machine {
        mem: Arc::new(Mutex::new(mem)),
        console_in: emu_receiver,
        console_out: emu_sender,
        console_size: size,
    };

    run(mach);
}
