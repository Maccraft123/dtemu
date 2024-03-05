use crate::devices::prelude::*;
use std::sync::mpsc;

#[derive(Debug)]
pub struct Mc6821 {
    output: Option<mpsc::Sender<char>>,
    input: Option<mpsc::Receiver<char>>,
}

impl Device for Mc6821 {
    fn as_mmio(&mut self) -> Option<&mut dyn Mmio> { Some(self) }
    fn as_console_output(&mut self) -> Option<&mut dyn ConsoleOutput> { Some(self) }
    fn as_console_input(&mut self) -> Option<&mut dyn ConsoleInput> { Some(self) }
    fn as_console(&mut self) -> Option<&mut dyn Console> { Some(self) }
}

impl Mmio for Mc6821 {
    fn read8(&mut self, addr: u32) -> u8 {
        match addr {
            // keyboard input
            0 => 0,
            // keyboard control
            1 => 0,
            // display output
            2 => 0,
            // display control
            3 => 0,
            _ => unreachable!(),
        }
    }
    fn write8(&mut self, addr: u32, mut val: u8) {
        match addr {
            // keyboard input
            0 => (),
            // keyboard control
            1 => (),
            // display output
            2 => {
                if let Some(ch) = &mut self.output {
                    // magic that i stole from https://github.com/RyuKojiro/apple1/blob/master/src/pia.c
                    val &= !0x80;

            if val == b'\r' {
                val = b'\n';
            }
            let _ = ch.send(char::from_u32(val as u32).unwrap());
                }
            },
            // display control
            3 => (),
            _ => unreachable!(),
        }
    }
    fn new(_compats: &[&str], _reg: Option<(u32, u32)>, _node: &DevTreeNode) -> Box<Self> {
        //let len = reg.expect("reg has to be supplied with a memory-mapped device").1 as usize;
        Box::new(Self { output: None, input: None } )
    }
}

impl ConsoleOutput for Mc6821 {
    fn attach_outchan(&mut self, channel: mpsc::Sender<char>) {
        self.output = Some(channel);
    }
}

impl ConsoleInput for Mc6821 {
    fn attach_inchan(&mut self, channel: mpsc::Receiver<char>) {
        self.input = Some(channel);
    }
}
impl Console for Mc6821 {}
