use std::fmt;
use std::sync::mpsc;
use parking_lot::Mutex;
use crate::MemoryWrapper;

pub type DisasmFn = fn(&[u8], Option<u32>) -> String;

pub trait Cpu {
    type Instruction: fmt::Display;
    type Registers: CpuRegs;
    type TraceFormat: fmt::Display;

    /// Makes a new instance of a struct implementing Cpu
    fn new(_: MemoryWrapper) -> Self where Self: Sized;
    /// Runs the cpu execution loop, with each clock cycle being represented
    /// by an async pending!()
    async fn tick(&self);
    fn regs(&self) -> &Mutex<Self::Registers>;
    /// If the Cpu just finished executing an instruction
    fn instruction_done(&self) -> bool;
    /// Function pointer to the function to turn bytes into a disassembled
    /// instruction
    fn disasm_fn(&self) -> &'static DisasmFn;
    fn trace_start(&self, _: mpsc::Sender<Self::TraceFormat>);
    fn trace_end(&self);
}

pub trait CpuRegs: fmt::Debug {
    /// address of next instruction to be executed
    fn next_instruction(&self) -> u32;
    /// the address where current stack starts at
    fn stack_bot(&self) -> u32;
    /// the address where stack pointer points to
    fn stack_top(&self) -> u32;
}
