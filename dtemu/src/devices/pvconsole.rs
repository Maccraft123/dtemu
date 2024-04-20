use crate::devices::console_prelude::*;
use crossterm::event::KeyCode;

#[derive(Debug)]
pub struct PvConsole {
    output: Option<Sender<char>>,
    input: Option<Receiver<Event>>,
    next_char: Option<u8>,
    phandle: Option<usize>,
    name: String,
}

impl PvConsole {
    fn has_char(&mut self) -> bool {
        if self.next_char.is_some() {
            return true;
        }
        if let Some(ref mut chan) = self.input {
            if let Ok(ev) = chan.try_recv() {
                if let Event::Key(k) = ev {
                    self.next_char = match k.code {
                        KeyCode::Char(ch) => Some(ch as u8),
                        KeyCode::Enter => Some(b'\n'),
                        KeyCode::Backspace => Some(8), // b'\b'
                        _ => None,
                    }
                }
            }
        }
        self.next_char.is_some()
    }
}


impl Device for PvConsole {
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

impl Mmio for PvConsole {
    fn read8(&mut self, addr: u32) -> u8 {
        match addr % 0x10 {
            // keyboard input char
            0 => {
                if self.has_char() {
                    self.next_char.take().unwrap_or(0x00)
                } else {
                    0x00
                }
            },
            // keyboard input available
            1 => {
                if self.has_char() {
                    0xff
                } else {
                    0x00
                }
            },
            2 => 0,
            2..=0x10 => 0,
            _ => unreachable!(),
        }
    }
    fn write8(&mut self, addr: u32, val: u8) {
        match addr % 0x10 {
            // keyboard input char
            0 => (),
            // keyboard input available
            1 => (),
            // display output
            2 => {
                if let Some(ch) = &mut self.output {
                    let _ = ch.send(char::from_u32(val as u32).unwrap());
                }
            },
            3..=0x10 => (),
            _ => unreachable!(),
        }
    }
}

impl ConsoleOutput for PvConsole {
    fn attach_outchan(&mut self, channel: Sender<char>) {
        self.output = Some(channel);
    }
    fn terminal_size(&self) -> (u8, u8) { (40, 24) }
}

impl ConsoleInput for PvConsole {
    fn attach_inchan(&mut self, channel: Receiver<Event>) {
        self.input = Some(channel);
    }
}
impl Console for PvConsole {}
