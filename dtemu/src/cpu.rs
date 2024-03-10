use std::fmt;
use parking_lot::Mutex;
use crate::MemoryWrapper;

pub type DisasmFn = fn(&[u8]) -> String;

pub trait Cpu {
    type Instruction: fmt::Display;
    type Registers: fmt::Debug;

    fn new(_: MemoryWrapper) -> Self where Self: Sized;
    async fn tick(&self);
    fn next_instruction(&self) -> u32;
    fn regs(&self) -> &Mutex<Self::Registers>;
    fn disasm_fn(&self) -> &'static DisasmFn;
}
