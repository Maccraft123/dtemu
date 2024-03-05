use core::pin::pin;
use futures::{pending, poll};
use std::io::{Write, self};
use std::sync::mpsc;
use std::path::PathBuf;
use clap::Parser;
use std::fs;
use std::sync::Arc;
use parking_lot::Mutex;
use fdt_rs::prelude::*;
use fdt_rs::base::DevTree;
use std::collections::HashMap;

mod devices;
mod mos6502;

use mos6502::Cpu;

use devices::{Mmio};

pub struct MemoryWrapper {
    ptr: Arc<Mutex<MemoryMap>>,
}

impl MemoryWrapper {
    fn new(ptr: Arc<Mutex<MemoryMap>>) -> Self {
        Self { ptr }
    }
    //fn set8_fast(&self, addr: u32, val: u8) {
    //    self.ptr.lock().write8(addr, val)
    //}
    //fn fetch8_fast(&self, addr: u32) -> u8 {
    //    self.ptr.lock().read8(addr)
    //}
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
            if addr > seg.start && addr <= seg.start + seg.size {
                data = Some(seg.device.read8(addr - seg.start));
            }
        }
        data.expect(&format!("Invalid memory read at {:x}", addr))
    }
    fn write8(&mut self, addr: u32, val: u8) {
        let mut written = false;
        for seg in self.segments.iter_mut() {
            if addr > seg.start && addr <= seg.start + seg.size {
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
    cpu: Cpu,
    console_in: mpsc::Receiver<char>,
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

async fn run(mut mach: Machine) -> u64 {
    let mut cycles = 0;

    {
        let mut cpu_future = pin!(mach.cpu.run(mach.new_mem_wrapper()));
        let mut stdout = io::stdout().lock();

        loop {
            if poll!(&mut cpu_future).is_ready() {
                break;
            }
            if let Ok(data) = mach.console_in.try_recv() {
                let _ = write!(stdout, "{}", data);
                let _ = stdout.flush();
            }
            cycles += 1;
        }
        cycles -= 1;
    }

    println!("");
    println!("CPU state on exit: {:#x?}", mach.cpu);
    println!("{} cycles done", cycles - 2);
    cycles
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
            //println!("{:?}", prop.name());

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

    // TODO: look up the cpu
    let cpu = Cpu::new();

    // wire up the console
    let (mut sender, receiver) = Some(mpsc::channel()).unzip();
    for device in mem.segments.iter_mut().map(|s| &mut s.device) {
        if let Some(name) = device.firmware_name() {
            if let Some(data) = firmwares.get(name) {
                device.load_firmware(&data);
            } else {
                eprintln!("Missing firmware for {:?}", name);
            }
        }

        if let Some(condev) = device.as_console_output() {
            if let Some(out) = sender.take() {
                condev.attach_outchan(out);
            } else {
                eprintln!("Only one console device may be active at any given moment");
            }
        }
    }

    let mach = Machine {
        cpu,
        mem: Arc::new(Mutex::new(mem)),
        console_in: receiver.unwrap(),
    };

    futures::executor::block_on(run(mach));
}
