use clap::Parser;

use std::time::Duration;
use std::io::{self, Write, StdoutLock};
use std::path::PathBuf;
use std::fs;
use std::collections::VecDeque;

use crossterm::event::{self, KeyCode, Event, KeyModifiers};
use libemu::{Backend, Machine, Key};
use libemu::cpm::CpmMachine;

#[derive(Parser, Debug)]
struct Args {
    #[arg(short, long)]
    program: PathBuf,
    #[arg(short, long)]
    debug: bool,
}

struct CrosstermBackend {
    program: Vec<u8>,
    out: StdoutLock<'static>,
    queue: VecDeque<Key>,
    seen_ctrlc: bool,
}

impl Drop for CrosstermBackend {
    fn drop(&mut self) {
        crossterm::terminal::disable_raw_mode().unwrap();
    }
}

impl CrosstermBackend {
    #[inline]
    fn new(program: Vec<u8>) -> Self {
        crossterm::terminal::enable_raw_mode().unwrap();
        Self {
            program,
            out: io::stdout().lock(),
            queue: VecDeque::new(),
            seen_ctrlc: false,
        }
    }
    #[inline]
    fn read_key(&mut self) -> Result<Key, io::Error>  {
        loop {
            match event::read()? {
                Event::Key(kev) => {
                    if kev.code == KeyCode::Char('c') && kev.modifiers.contains(KeyModifiers::CONTROL) {
                        self.seen_ctrlc = true;
                    }
                    match kev.code {
                        KeyCode::Enter => return Ok(Key::Character('\r')),
                        KeyCode::Char(ch) => return Ok(Key::Character(ch)),
                        _ => continue,
                    }
                },
                _ => (),
            }
        }
    }
    #[inline]
    fn update_queue(&mut self) -> Result<(), io::Error> {
        if event::poll(Duration::from_secs(0))? {
            let key = self.read_key()?;
            self.queue.push_back(key);
        }
        Ok(())
    }
}

impl Backend for CrosstermBackend {
    type TtyError = io::Error;
    #[inline]
    fn should_exit(&mut self) -> bool {
        self.seen_ctrlc
    }
    #[inline]
    fn write_key(&mut self, key: Key) -> Result<(), io::Error>{
        match key {
            Key::Character(ch) => write!(self.out, "{}", ch)?,
            _ => todo!(),
        }
        self.out.flush()
    }
    #[inline]
    fn read_key(&mut self) -> Result<Key, io::Error> {
        self.update_queue()?;
        if let Some(k) = self.queue.pop_front() {
            Ok(k)
        } else {
            self.read_key()
        }
    }
    #[inline]
    fn poll_key(&mut self) -> Result<Option<Key>, io::Error> {
        self.update_queue()?;
        Ok(self.queue.pop_front())
    }
    #[inline]
    fn peek_key(&mut self) -> Result<Option<Key>, io::Error> {
        self.update_queue()?;
        Ok(self.queue.front().copied())
    }
    #[inline]
    fn request_firmware(&mut self, name: &str) -> Vec<u8> {
        self.program.clone()
    }
}

fn main() {
    let args = Args::parse();
    let prog = fs::read(&args.program)
        .unwrap();
    let backend = CrosstermBackend::new(prog);
    let mut machine = CpmMachine::new(backend);
    while machine.tick().unwrap() {}
}
