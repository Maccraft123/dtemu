use crate::devices::console_prelude::*;
use crossterm::event::KeyCode;

// this entire file is a giant fucking hack

#[derive(Debug)]
pub struct Mc6850 {
    output: Option<Sender<char>>,
    input: Option<Receiver<Event>>,
    next_char: Option<u8>,
    name: String,
    phandle: Option<usize>,
}

impl Device for Mc6850 {
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

impl Mmio for Mc6850 {
    fn read8(&mut self, addr: u32) -> u8 {
        match addr % 2 {
            // status/control
            0 => {
                if let Some(ref mut chan) = self.input {
                    if let Ok(ev) = chan.try_recv() {
                        if let Event::Key(k) = ev {
                            self.next_char = match k.code {
                                KeyCode::Backspace => Some(0x8),
                                KeyCode::Enter => Some(b'\r'),
                                KeyCode::Char(ch) => Some(ch.to_ascii_uppercase() as u8),
                                _ => None,
                            }
                        }
                    }
                }

                //println!("acia ret {:x}", self.next_char.is_some() as u8 | 0x2);
                self.next_char.is_some() as u8 | 0x2
            },
            // rx/tx
            1 => {
                self.next_char.take().unwrap_or(0)
            },
            _ => unreachable!(),
        }
    }
    fn write8(&mut self, addr: u32, val: u8) {
        match addr % 2 {
            // status/control
            0 => {

                //if val & 0x80 != 0 {
                    //eprintln!("IRQs are not supported");
                //}
            },
            // rx/tx
            1 => {
                if val != 0 {
                    if let Some(ch) = &mut self.output {
                        //val &= !0x80;

                        ch.send(val as char).unwrap();
                        //println!("send {:x}({:?})\r", val, val as char);
                        //if val == b'\r' {
                            //let _ = ch.send('\n');
                        //}
                    }
                }
            },
            _ => unreachable!(),
        }
    }
}

impl ConsoleOutput for Mc6850 {
    fn attach_outchan(&mut self, channel: Sender<char>) {
        self.output = Some(channel);
    }
    fn terminal_size(&self) -> (u8, u8) { (40, 24) }
}

impl ConsoleInput for Mc6850 {
    fn attach_inchan(&mut self, channel: Receiver<Event>) {
        self.input = Some(channel);
    }
}
impl Console for Mc6850 {}
