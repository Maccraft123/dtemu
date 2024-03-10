use std::fmt;
use crate::MemoryWrapper;
use std::pin::Pin;
use futures::Future;

pub trait Cpu {
    type Instruction: fmt::Display;

    fn new(_: MemoryWrapper) -> Self where Self: Sized;
    async fn tick(&self);
    fn disasm_instruction(&self, _: &[u8]) -> Self::Instruction;
    fn cur_instruction(&self) -> u32;
}
