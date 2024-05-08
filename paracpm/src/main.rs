use asane::intel8085::{Intel8080, Reg, RegPair};
use asane::Cpu;
use clap::Parser;

use std::io::{self, Write, Read};
use std::path::PathBuf;
use std::fs;
//use std::collections::VecDeque;

fn bdos_call(mut m: &mut [u8], cpu: &mut Intel8080) {
    let function = cpu.reg(&m, Reg::C);
    match function {
        1 => {
            let mut tmp = [0u8];
            io::stdin().lock().read_exact(&mut tmp)
                .unwrap();

            cpu.set_reg(&mut m, Reg::A, tmp[0]);
            cpu.set_reg(&mut m, Reg::L, tmp[0]);
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
        12 => {
            cpu.set_rp(RegPair::Hl, 0);
        },
        _ => todo!("cpm bdos function {}", function),
    }
}

#[derive(Parser, Debug)]
struct Args {
    #[arg(short, long)]
    program: PathBuf,
    #[arg(short, long)]
    debug: bool,
}

fn main() {
    let args = Args::parse();
    let prog = fs::read(&args.program)
        .unwrap();

    let mut memory = vec![0u8; 0x10000];
    memory[0x0] = 0xc3; // jmp 0x100
    memory[0x1] = 0x00;
    memory[0x2] = 0x01;
    memory[0x5] = 0xc9; // ret
    memory[0x6] = 0xff;
    memory[0x7] = 0x00;

    memory[0x100..][..prog.len()].copy_from_slice(&prog);
    let mut cpu = Intel8080::new();

    futures_lite::future::block_on(async {
        loop {
            if args.debug {
                println!("{:x?}", cpu.next_instruction(&memory));
            }
            let pc = cpu.step_instruction(&mut memory).await;
            if args.debug {
                println!("{:#x?}", cpu);
            }
            match pc {
                0 => {
                    println!("\n");
                    break;
                },
                0x5 => bdos_call(&mut memory, &mut cpu),
                _ => (),
            }
        }
    })
}
