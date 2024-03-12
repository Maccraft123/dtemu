use std::fmt;
use parking_lot::Mutex;
use crate::MemoryWrapper;

pub type DisasmFn = fn(&[u8]) -> String;

pub trait Cpu {
    type Instruction: fmt::Display;
    type Registers: CpuRegs;

    /// Makes a new instance of a struct implementing Cpu
    fn new(_: MemoryWrapper) -> Self where Self: Sized;
    /// Runs the cpu execution loop, with each clock cycle being represented
    /// by an async pending!()
    async fn tick(&self);
    fn regs(&self) -> &Mutex<Self::Registers>;
    /// If the Cpu just finished executing an instruction
    fn instruction_done(&self) -> bool;
    /// Function pointer to the function to turn bytes into an instruction
    /// thingy idk im tired
    fn disasm_fn(&self) -> &'static DisasmFn;
}

pub trait CpuRegs: fmt::Debug {
    fn next_instruction(&self) -> u32;
    fn stack_bot(&self) -> u32;
    fn stack_top(&self) -> u32;
}
