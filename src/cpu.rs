use std::fmt;
use crate::MemoryWrapper;
use std::pin::Pin;
use futures::Future;

pub trait Cpu {
    type State: fmt::Debug;

    fn new() -> Self where Self: Sized;
    fn tick_future(&mut self, _: MemoryWrapper) -> Pin<Box<dyn Future<Output = ()> + '_>>;
    fn disasm_instruction(&self, _: &[u8]) -> String;
    fn cur_instruction(&self) -> u32;
    fn state(&self) -> &Self::State;
}
