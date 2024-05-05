use asane::intel8085::{Intel8085, Reg, RegPair};
use asane::Cpu;
use clap::Parser;

use std::io::{self, Write};
use std::path::PathBuf;
use std::fs;

fn bdos_call(m: &[u8], cpu: &Intel8085) {
    let function = cpu.reg(&m, Reg::C);
    match function {
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
        _ => todo!("cpm bdos function {}", function),
    }
}

#[derive(Parser, Debug)]
struct Args {
    #[arg(short, long)]
    program: PathBuf,
}

fn main() {
    let args = Args::parse();
    let prog = fs::read(&args.program)
        .unwrap();

    let mut memory = vec![0u8; 0x10000];
    memory[0x5] = 0xc9;

    memory[0x100..][..prog.len()].copy_from_slice(&prog);
    let mut cpu = Intel8085::new();

    futures_lite::future::block_on(async {
        loop {
            let pc = cpu.step_instruction(&mut memory).await;
            //println!("{:#x?}", cpu);
            match pc {
                0 => {
                    println!("\nMachine reset, exiting");
                    break;
                },
                0x5 => bdos_call(&memory, &cpu),
                _ => (),
            }
        }
    })
}
