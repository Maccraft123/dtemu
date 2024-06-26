use crate::devices::console_prelude::*;
use crossterm::event::KeyCode;

// this entire file is a giant fucking hack

#[derive(Debug)]
pub struct Mc6821 {
    output: Option<Sender<char>>,
    input: Option<Receiver<Event>>,
    next_char: Option<u8>,
    name: String,
    phandle: Option<usize>,
}

impl Device for Mc6821 {
    fn node_name(&self) -> &str { &self.name }
    fn phandle(&self) -> Option<usize> { self.phandle }
    fn new(node: &FdtNode<'_, '_>) -> Arc<Mutex<Self>> {
        Arc::new(Mutex::new(Self {
            output: None,
            input: None,
            next_char: None,
            name: node.name.to_string(),
            phandle: node.property("phandle").map(|p| p.as_usize()).flatten(),
        } ))
    }
    fn as_mmio(&mut self) -> Option<&mut dyn Mmio> { Some(self) }
    fn as_console_output(&mut self) -> Option<&mut dyn ConsoleOutput> { Some(self) }
    fn as_console_input(&mut self) -> Option<&mut dyn ConsoleInput> { Some(self) }
    fn as_console(&mut self) -> Option<&mut dyn Console> { Some(self) }
}

impl Mmio for Mc6821 {
    fn read8(&mut self, addr: u32) -> u8 {
        match addr % 4 {
            // keyboard data
            0 => self.next_char.take().unwrap_or(0) | 0x80,
            // keyboard control
            1 => {
                if let Some(ref mut chan) = self.input {
                    if let Ok(ev) = chan.try_recv() {
                        if let Event::Key(k) = ev {
                            self.next_char = match k.code {
                                KeyCode::Backspace => Some(0xdf),
                                KeyCode::Enter => Some(b'\r'),
                                KeyCode::Char(ch) => Some(ch.to_ascii_uppercase() as u8),
                                _ => None,
                            }
                        }
                    }
                }

                if self.next_char.is_some() {
                    0xff
                } else {
                    0x00
                }
            },
            // display output
            2 => self.next_char.unwrap_or(0),
            // display control
            3 => 0,
            _ => unreachable!(),
        }
    }
    fn write8(&mut self, addr: u32, mut val: u8) {
        match addr % 4 {
            // keyboard input
            0 => (),
            // keyboard control
            1 => (),
            // display output
            2 => {
                if let Some(ch) = &mut self.output {
                    // magic that i stole from https://github.com/RyuKojiro/apple1/blob/master/src/pia.c
                    val &= !0x80;

                    let _ = ch.send(char::from_u32(val as u32).unwrap());
                    if val == b'\r' {
                        let _ = ch.send('\n');
                    }

                }
            },
            // display control
            3 => (),
            _ => unreachable!(),
        }
    }
}

impl ConsoleOutput for Mc6821 {
    fn attach_outchan(&mut self, channel: Sender<char>) {
        self.output = Some(channel);
    }
    fn terminal_size(&self) -> (u8, u8) { (40, 24) }
}

impl ConsoleInput for Mc6821 {
    fn attach_inchan(&mut self, channel: Receiver<Event>) {
        self.input = Some(channel);
    }
}
impl Console for Mc6821 {}
