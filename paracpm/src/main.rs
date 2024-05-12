use asane::intel8085::{Intel8080, Reg, RegPair};
use asane::Cpu;
use clap::{ValueEnum, Parser};

use std::io::{self, Write, Read};
use std::path::PathBuf;
use std::fs;
use std::collections::VecDeque;
use std::sync::Mutex;

fn bdos_call(mut m: &mut [u8], cpu: &mut Intel8080, v: CpmVersion) -> bool {
    let function = cpu.reg(&m, Reg::C);
    let mut retval8 = None;
    static kbd_queue: Mutex<VecDeque<u8>> = Mutex::new(VecDeque::new());
    match function {
        0 => return true,
        1 => {
            let ch = if let Some(ch) = kbd_queue.lock().unwrap().pop_front() {
                ch
            } else {
                let mut buf = [0u8; 0x10];
                let num = io::stdin().lock().read(&mut buf)
                    .unwrap();
                kbd_queue.lock().unwrap().extend(&buf[1..num]);
                if buf[..num].iter().any(|v| *v == 3) { // ^C
                    return true;
                }
                buf[0]
            };
            print!("{}", ch as char);
            io::stdout().flush().unwrap();
            retval8 = Some(ch);
        },
        2 => {
            print!("{}", cpu.reg(&m, Reg::E) as char);
            io::stdout().flush().unwrap();
        },
        9 => {
            let mut addr = cpu.rp(RegPair::De) as usize;
            let mut string = String::new();
            loop {
                let ch = m[addr] as char;
                if ch == '$' {
                    break;
                } else {
                    string.push(ch);
                }
                addr += 1;
            }
            print!("{}", string);
            io::stdout().flush().unwrap();
        },
        11 => {
            let mut q = kbd_queue.lock().unwrap();
            if q.len() > 0 {
                retval8 = Some(1);
            } else {
                let mut buf = [0u8; 0x10];
                let num = io::stdin().lock().read(&mut buf)
                    .unwrap();
                if buf[..num].iter().any(|v| *v == 3) { // ^C
                    return true;
                }
                q.extend(&buf[..num]);
                retval8 = Some((num > 0) as u8);
            }
        },
        12 => {
            retval8 = Some(v as u8);
            cpu.set_reg(&mut m, Reg::H, 0); // TODO
            cpu.set_reg(&mut m, Reg::B, 0); // TODO
        },
        _ => todo!("cpm bdos function {}", function),
    }
    if let Some(v) = retval8 {
        cpu.set_reg(&mut m, Reg::A, v);
        cpu.set_reg(&mut m, Reg::L, v);
    }

    false
}

#[derive(ValueEnum, Copy, Clone, Debug, Eq, PartialEq)]
#[repr(u8)]
enum CpmVersion {
    #[clap(name = "1")]
    V1 = 0x00,
    #[clap(name = "2.0")]
    V2 = 0x20,
    #[clap(name = "2.1")]
    V21 = 0x21,
    #[clap(name = "2.2")]
    V22 = 0x22,
}

#[derive(Parser, Debug)]
struct Args {
    #[arg(short, long)]
    program: PathBuf,
    #[arg(short, long)]
    debug: bool,
    #[arg(short, long, value_enum)]
    cpm_version: CpmVersion,
}

fn main() {
    let args = Args::parse();
    let prog = fs::read(&args.program)
        .unwrap();
    let v = args.cpm_version;
    crossterm::terminal::enable_raw_mode().unwrap();

    let mut memory = vec![0u8; 0x10000];
    memory[0x0] = 0xc3; // jmp 0xff00, bios entry
    memory[0x1] = 0x00;
    memory[0x2] = 0xff;
    memory[0x4] = 0x01;
    memory[0x5] = 0xc3; // jmp 0xfe00, bdos entry
    memory[0x6] = 0x00;
    memory[0x7] = 0xfe;
    memory[0xfe00] = 0xc9; // ret
    memory[0xff00] = 0xc5; // push bc, to have 0x0 as return address
    memory[0xff01] = 0xc3; // jmp 0x0100, program entry
    memory[0xff02] = 0x00;
    memory[0xff03] = 0x01;

    memory[0x100..][..prog.len()].copy_from_slice(&prog);

    let mut cpu = Intel8080::new();

    futures_lite::future::block_on(async {
        loop {
            if args.debug {
                println!("{:x?}\r", cpu.next_instruction(&memory));
            }
            let pc = cpu.step_instruction(&mut memory).await;
            if args.debug {
                println!("{:#x?}\r", cpu);
            }
            match pc {
                0 => {
                    println!("\r");
                    break;
                },
                0xfe00 => {
                    if bdos_call(&mut memory, &mut cpu, v) {
                        break;
                    }
                },
                _ => (),
            }
        }
    });

    crossterm::terminal::disable_raw_mode().unwrap();
}
