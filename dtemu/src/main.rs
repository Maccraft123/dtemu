// i gotta clean those up later
use futures::{pending, poll};
use std::io::Write;
use std::sync::mpsc;
use std::path::PathBuf;
use clap::Parser;
use std::{io, thread, fs};
use std::sync::Arc;
use std::sync::atomic::{Ordering, AtomicBool};
use parking_lot::{Condvar, Mutex};
use std::collections::HashMap;
use crossterm::event::{self, Event};
use std::time::Duration;
use fdt::Fdt;

mod devices;
mod mos6502;
mod i8080;
mod cpu;

use cpu::{Cpu, CpuRegs};

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
    fn set8_fast(&self, addr: u32, val: u8) {
        self.ptr.lock().write8(addr, val)
    }
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
    fn insert_mmio_device(&mut self, device: Box<dyn Mmio>, start: u32, size: u32, mirror_size: u32) {
        self.segments.push(Segment{device, start, size, mirror_size})
    }
    fn read8(&mut self, addr: u32) -> u8 {
        for seg in self.segments.iter_mut() {
            if addr >= seg.start && addr < (seg.start + seg.size) {
                return seg.device.read8((addr - seg.start) % seg.mirror_size);
                //data = Some(seg.device.read8((addr - seg.start)));
            }
        }
        0
    }
    fn write8(&mut self, addr: u32, val: u8) {
        let mut written = false;
        for seg in self.segments.iter_mut() {
            if addr >= seg.start && addr < (seg.start + seg.size) {
                seg.device.write8((addr - seg.start) % seg.mirror_size, val);
                written = true;
            }
        }
        //if !written {
        //    eprintln!("Invalid memory write: {:x} = {:x}\r", addr, val);
        //}
    }
}

#[derive(Debug)]
struct Segment {
    start: u32,
    size: u32,
    mirror_size: u32,
    device: Box<dyn Mmio>,
}

struct Machine {
    mem: MemoryWrapper,
    console_in: Option<mpsc::Receiver<char>>,
    console_out: Option<mpsc::Sender<Event>>,
    console_size: (u8, u8),
    cpu: Box<dyn for<'a> Cpu<'a>>,
}

impl Machine {
    fn new_mem_wrapper(&self) -> MemoryWrapper {
        self.mem.clone()
    }
}

#[derive(Parser, Debug)]
struct Args {
    #[arg(short, long)]
    machine: PathBuf,
    #[arg(long)]
    fw: Vec<String>,
    #[arg(short, long)]
    cycles: Option<u64>,
    #[arg(long)]
    dump_cpu_state: bool,
}

struct EmuTui {
    console_recv: mpsc::Receiver<char>,
    console_send: mpsc::Sender<Event>,
    _console_size: (u8, u8),
}

impl EmuTui {
    fn run(&mut self, mut info: DebuggerInfo) {
        use crossterm::{execute, terminal};
        use crossterm::event::KeyCode;
        use ratatui::prelude::*;
        use ratatui::widgets::{Block, Borders, Paragraph, List};

        let original_hook = std::panic::take_hook();
        std::panic::set_hook(Box::new(move |panic_info| {
            execute!(io::stderr(), terminal::LeaveAlternateScreen).unwrap();
            terminal::disable_raw_mode().unwrap();
            original_hook(panic_info);
        }));

        let mut debugger = false;
        let mut stdout = io::stdout();
        let mut terminal = None;
        terminal::enable_raw_mode()
            .expect("failed to enable raw mode");

        while info.run.load(Ordering::SeqCst) {
            if let Ok(true) = event::poll(Duration::from_millis(50)) {
                let ev = event::read().unwrap();
                if debugger {
                    match ev {
                        Event::Key(kev) => {
                            match kev.code {
                                KeyCode::Esc => info.run.store(false, Ordering::SeqCst),
                                KeyCode::Char(c) => match c {
                                    // toggle debugger view
                                    '`' => {
                                        debugger = false;
                                        execute!(stdout, terminal::LeaveAlternateScreen)
                                            .expect("unable to leave alternate screen");
                                    },
                                    // do a step
                                    ' ' => {
                                        if info.singlestep.load(Ordering::SeqCst) {
                                            let mut step = info.do_a_step.lock();
                                            *step = true;
                                            info.do_a_step_condvar.notify_all();
                                        }
                                    },
                                    // toggle singlestep mode
                                    's' => {
                                        if info.singlestep.load(Ordering::SeqCst) {
                                            info.singlestep.store(false, Ordering::SeqCst);
                                            let mut step = info.do_a_step.lock();
                                            *step = true;
                                            info.do_a_step_condvar.notify_all();
                                        } else {
                                            info.singlestep.store(true, Ordering::SeqCst);
                                        }
                                    },
                                    _ => (),
                                },
                                _ => (),
                            }
                        },
                        _ => (),
                    }
                } else {
                    match ev {
                        Event::Key(kev) => {
                            match kev.code {
                                KeyCode::Esc => info.run.store(false, Ordering::SeqCst),
                                KeyCode::Char(c) => match c {
                                    // toggle debugger view
                                    '`' => {
                                        debugger = true;
                                        execute!(stdout, terminal::EnterAlternateScreen)
                                            .expect("unable to enter alternate screen");
                                    },
                                    // do a step
                                    ' ' if info.singlestep.load(Ordering::SeqCst) => {
                                        let mut step = info.do_a_step.lock();
                                        *step = true;
                                        info.do_a_step_condvar.notify_all();
                                    },
                                    _ => self.console_send.send(ev).unwrap(),
                                },
                                _ => self.console_send.send(ev).unwrap(),
                            }
                        },
                        _ => (),
                    }
                }
            }

            if !debugger {
                if let Some(ref mut ch) = info.cpu_dump {
                    if let Ok(string) = ch.try_recv() {
                        write!(stdout, "{}\r\n", string).unwrap();
                        stdout.flush().unwrap();
                    }
                } else {
                    if let Ok(ch) = self.console_recv.try_recv() {
                        write!(stdout, "{}", ch).unwrap();
                        while let Ok(ch2) = self.console_recv.try_recv() {
                            write!(stdout, "{}", ch2).unwrap();
                        }
                        stdout.flush().unwrap();
                    }
                }
            }

            if debugger {
                if terminal.is_none() {
                    terminal = Some(Terminal::new(CrosstermBackend::new(io::stdout())).unwrap());
                }
                terminal.as_mut().unwrap().draw(|frame| {
                    let cpu_regs = info.cpu_regs.lock();
                    let layout = Layout::horizontal([
                        Constraint::Min(1),
                        Constraint::Length(7),
                        Constraint::Length(60),
                    ]);
                    let [regs, stack, memory] = layout.areas(frame.size());

                    let mut mem_hex = String::new();
                    for x in 0x00..0x10 {
                        mem_hex += &format!("{:x}0: ", x);
                        for y in 0x0..0x10 {
                            mem_hex += &format!("{:02x} ", info.mem.fetch8_fast(x << 4 | y));
                        }
                        mem_hex += "\n";
                    }

                    frame.render_widget(
                        Paragraph::new(mem_hex)
                            .block(Block::default().title("Memory").borders(Borders::ALL)),
                        memory
                    );

                    let mut stack_contents = Vec::new();
                    let bot = cpu_regs.stack_bot();
                    let top = cpu_regs.stack_top();
                    if bot > top {
                        for addr in top..bot {
                            stack_contents.push(format!("{:02x?}\n", info.mem.fetch8_fast(addr)));
                        }
                    }
                    frame.render_widget(
                        List::new(stack_contents)
                            .block(Block::default().title("Stack").borders(Borders::ALL)),
                        stack
                    );

                    let pc = cpu_regs.next_instruction();
                    let bytes: Vec<u8> = (pc..pc+4).map(|v| info.mem.fetch8_fast(v)).collect();
                    let instruction = (info.disasm_fn)(&bytes, Some(pc as u32));

                    frame.render_widget(
                        Paragraph::new(format!("{:#x?}\n{}", cpu_regs, instruction))
                            .block(Block::default().title("CPU Registers").borders(Borders::ALL)),
                        regs
                    );
                }).unwrap();
            }

            if !debugger && terminal.is_some() {
                terminal = None;
            }
        }

        terminal::disable_raw_mode()
            .expect("failed to disable raw mode");
        if debugger {
            execute!(stdout, terminal::LeaveAlternateScreen)
                .expect("unable to switch to main screen");
        }
    }
}

struct DebuggerInfo<'mach, 'cpu> {
    singlestep: &'mach AtomicBool,
    do_a_step: &'mach Mutex<bool>,
    do_a_step_condvar: &'mach Condvar,
    cpu_regs: &'cpu Mutex<(dyn CpuRegs + Send + Sync)>,
    mem: MemoryWrapper,
    disasm_fn: &'static cpu::DisasmFn,
    run: &'mach AtomicBool,
    cpu_dump: Option<mpsc::Receiver<String>>, // FIXME
}

fn run(mut mach: Machine, mut do_cycles: Option<u64>, do_dump_cpu: bool) {
    let mut cycles = 0;
    let run = AtomicBool::new(true);
    let singlestep = AtomicBool::new(false);
    let do_a_step = Mutex::new(false);
    let do_a_step_condvar = Condvar::new();

    let mut tui = EmuTui {
        console_recv: mach.console_in.take().unwrap(),
        console_send: mach.console_out.take().unwrap(),
        _console_size: mach.console_size,
    };

    let cpu_dump;
    if do_dump_cpu {
        let (tx, rx) = mpsc::channel();
        mach.cpu.trace_start(tx);
        cpu_dump = Some(rx);
    } else {
        cpu_dump = None;
    }

    let debugger_info = DebuggerInfo {
        run: &run,
        singlestep: &singlestep,
        do_a_step: &do_a_step,
        do_a_step_condvar: &do_a_step_condvar,
        cpu_regs: mach.cpu.regs(),
        mem: mach.new_mem_wrapper(),
        disasm_fn: mach.cpu.disasm_fn(),
        cpu_dump,
    };

    thread::scope(|s| {
        futures::executor::block_on( async {
            let mut cpu_fut = std::pin::pin!(mach.cpu.tick());
            s.spawn(|| tui.run(debugger_info));

            // Iterates once for every instruction
            'outer: loop {
                // Iterates once for every clock cycle
                'cycles: loop {
                    if poll!(&mut cpu_fut).is_ready() {
                        run.store(false, Ordering::SeqCst);
                    }

                    cycles += 1;

                    if !run.load(Ordering::SeqCst) {
                        break 'outer;
                    }

                    if mach.cpu.instruction_done() {
                        break 'cycles;
                    }
                    if let Some(ref mut c) = do_cycles {
                        *c -= 1;
                        if *c == 0 {
                            run.store(false, Ordering::SeqCst);
                            break 'outer;
                        }
                    }
                }

                // TODO: put into debugger 
                //if cpu.regs().lock().next_instruction() == 0x1000 {
                //    singlestep.store(true, Ordering::SeqCst);
                //}

                if singlestep.load(Ordering::SeqCst) {
                    let mut step = do_a_step.lock();
                    if !*step {
                        do_a_step_condvar.wait(&mut step);
                    }
                    *step = false;
                }
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
    let dt_data = fs::read(&args.machine)
        .expect("Failed to load DTB");

    let devtree = Fdt::new(&dt_data)
        .expect("Failed to parse DTB");

    let mut mem = MemoryMap::new();
    for node in devtree.all_nodes() {
        let Some(mut reg) = node.reg() else {
            eprintln!("Missing reg property for {}", node.name);
            continue;
        };
        let reg = reg.next().unwrap();
        let (start, size) = (reg.starting_address as usize as u32, reg.size.unwrap_or(0) as u32);
        let mirror_size = node.property("mirror-size")
            .map(|prop| prop.as_usize())
            .flatten()
            .unwrap_or(size as usize) as u32;
        let dev = devices::probe(&node);

        if let Some(device) = dev {
            mem.insert_mmio_device(device, start, size, mirror_size);
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
                println!("Wired input to {}", device.node_name());
            } else {
                eprintln!("Only one console input device may be active at any given moment");
            }
        }
    }

    let mem = MemoryWrapper::new(Arc::new(Mutex::new(mem)));
    let mut cpu: Option<Box<dyn for<'a> Cpu<'a>>> = None;

    if let Some(c) = devtree.cpus().next() {
        let compatibles = c.property("compatible").unwrap();
        for compat in compatibles.as_str().unwrap().split('\0') {
            match compat {
                "mos,6502" => cpu = Some(mos6502::Mos6502::new(mem.clone())),
                "intel,8080" => cpu = Some(i8080::Intel8080::new(mem.clone())),
                _ => ()
            }
            if cpu.is_some() {
                break;
            }
        }
    }
    if cpu.is_none() {
        panic!("Unsupported CPU");
    }

    let mach = Machine {
        mem: mem.clone(),
        console_in: emu_receiver,
        console_out: emu_sender,
        console_size: size,
        cpu: cpu.unwrap(),
    };

    run(mach, args.cycles, args.dump_cpu_state);
}
