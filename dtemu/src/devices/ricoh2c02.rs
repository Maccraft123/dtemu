use crate::devices::graphics_prelude::*;
use crate::devices::nescart::NesCartridge;

static COLOR_PALETTE: [(u8,u8,u8); 64] = [
   (0x80, 0x80, 0x80), (0x00, 0x3D, 0xA6), (0x00, 0x12, 0xB0), (0x44, 0x00, 0x96), (0xA1, 0x00, 0x5E),
   (0xC7, 0x00, 0x28), (0xBA, 0x06, 0x00), (0x8C, 0x17, 0x00), (0x5C, 0x2F, 0x00), (0x10, 0x45, 0x00),
   (0x05, 0x4A, 0x00), (0x00, 0x47, 0x2E), (0x00, 0x41, 0x66), (0x00, 0x00, 0x00), (0x05, 0x05, 0x05),
   (0x05, 0x05, 0x05), (0xC7, 0xC7, 0xC7), (0x00, 0x77, 0xFF), (0x21, 0x55, 0xFF), (0x82, 0x37, 0xFA),
   (0xEB, 0x2F, 0xB5), (0xFF, 0x29, 0x50), (0xFF, 0x22, 0x00), (0xD6, 0x32, 0x00), (0xC4, 0x62, 0x00),
   (0x35, 0x80, 0x00), (0x05, 0x8F, 0x00), (0x00, 0x8A, 0x55), (0x00, 0x99, 0xCC), (0x21, 0x21, 0x21),
   (0x09, 0x09, 0x09), (0x09, 0x09, 0x09), (0xFF, 0xFF, 0xFF), (0x0F, 0xD7, 0xFF), (0x69, 0xA2, 0xFF),
   (0xD4, 0x80, 0xFF), (0xFF, 0x45, 0xF3), (0xFF, 0x61, 0x8B), (0xFF, 0x88, 0x33), (0xFF, 0x9C, 0x12),
   (0xFA, 0xBC, 0x20), (0x9F, 0xE3, 0x0E), (0x2B, 0xF0, 0x35), (0x0C, 0xF0, 0xA4), (0x05, 0xFB, 0xFF),
   (0x5E, 0x5E, 0x5E), (0x0D, 0x0D, 0x0D), (0x0D, 0x0D, 0x0D), (0xFF, 0xFF, 0xFF), (0xA6, 0xFC, 0xFF),
   (0xB3, 0xEC, 0xFF), (0xDA, 0xAB, 0xEB), (0xFF, 0xA8, 0xF9), (0xFF, 0xAB, 0xB3), (0xFF, 0xD2, 0xB0),
   (0xFF, 0xEF, 0xA6), (0xFF, 0xF7, 0x9C), (0xD7, 0xE8, 0x95), (0xA6, 0xED, 0xAF), (0xA2, 0xF2, 0xDA),
   (0x99, 0xFF, 0xFC), (0xDD, 0xDD, 0xDD), (0x11, 0x11, 0x11), (0x11, 0x11, 0x11)
];

pub struct Ricoh2c02 {
    name: String,
    chan: Option<Sender<(u32, u32, RGB8)>>,
    phandle: Option<usize>,
    odd_frame: bool,
    vblank: bool,
    scanline: u32,
    pixel: u32,
    chr: Option<Arc<Mutex<dyn MmioDevice>>>,
    cart_phandle: usize,
    nametable: u8,
    attribute: u8,
    pattern_table: u16,
    nametable_base: u16,
    vram_add: u8,
    sprite_pat_table: u16,
    bg_pat_table: u16,
    big_sprites: bool,
    nmi: bool,
}

impl Ricoh2c02 {
    fn chrbyte(&self, addr: u16) -> u8 {
        self.chr.as_ref()
            .unwrap()
            .lock()
            .as_nescart()
            .unwrap()
            .read8_chr(addr)
    }
}

impl Device for Ricoh2c02 {
    fn tick(&mut self) {
        let skip = self.odd_frame && self.pixel == 339 && self.scanline == 261;
        match self.scanline {
            _ if skip => (),
            // Visible scanlines
            0..=239 => {
                self.vblank = false;
                match self.pixel & 0b1110 {
                    0 => self.nametable = self.chrbyte(nametable_base + self.pixel + self.scanline * 341),
                    1 => self.attribute_table = self.chrbyte(0),
                    2 => self.pattern_table = self.chrbyte(pattern_table_base),
                    3 => self.pattern_table = self.chrbyte(),
                    _ => (),
                }
                if let Some(ref mut ch) = self.chan {
                    ch.send(RGB8::from());
                }
            },
            // Post-render scanline
            240 => {
            },
            241..=261 => {
                if self.pixel == 1 {
                    self.vblank = true;
                    // nmi firing goes here
                }
            },
            _ => unreachable!(),
        }
        self.pixel += 1;
        if self.pixel > 341 {
            self.pixel = 0;
            self.scanline += 1;
        }
        if self.scanline > 261 {
            self.scanline = 0;
            self.odd_frame = !self.odd_frame;
        }
    }
    fn phandle(&self) -> Option<usize> { self.phandle }
    fn node_name(&self) -> &str { &self.name }
    fn new(node: &FdtNode<'_, '_>) -> Arc<Mutex<Self>> {
        Arc::new(Mutex::new(Self {
            chr: None,
            name: node.name.to_string(),
            chan: None,
            odd_frame: false,
            pixel: 0,
            scanline: 0,
            vblank: false,
            nametable: 0,
            attribute: 0,
            pattern_table: 0,
            bg_pat_table: 0,
            big_sprites: false,
            nametable_base: 0,
            nmi: false,
            sprite_pat_table: 0,
            vram_add: 1,
            phandle: node.property("phandle").map(|p| p.as_usize()).flatten(),
            cart_phandle: node.property("nes-cartridge")
                .expect("nes-cartridge has to be specified in a NES PPU device(ricoh,2c02)")
                .as_usize()
                .expect("nes-cartridge prop of ricoh,2c02 has to be a phandle"),
        }))
    }
    fn init(&mut self, mem: MemoryWrapper) {
        self.chr = Some(mem.device_by_phandle(self.cart_phandle)
            .expect("Failed to lookup nes-cartridge device"));
    }
    fn as_mmio(&mut self) -> Option<&mut dyn Mmio> { Some(self) }
}

impl Graphics for Ricoh2c02 {
    fn attach_pixchan(&mut self, chan: Sender<(u32, u32, rgb::RGB8)>) {
        self.chan = Some(chan);
    }
}

impl Mmio for Ricoh2c02 {
    fn read8(&mut self, addr: u32) -> u8 {
        match addr {
            // Status
            2 => {
                let val = self.vblank as u8 * 0x80;
                self.vblank = false;
                val
            },
            // OAM Data
            4 => 0,
            _ => 0,
        }
    }
    fn write8(&mut self, addr: u32, val: u8) {
        match addr {
            // ppuctrl
            0 => {
                self.nametable_base = match val & 0x03 {
                    0 => 0x2000,
                    1 => 0x2400,
                    2 => 0x2800,
                    3 => 0x2c00,
                    _ => unreachable!(),
                };
                self.vram_add = if val & 0x04 == 0 { 1 } else { 32};
                self.sprite_pat_table = (val as u16 & 0x08) << 9;
                self.bg_pat_table = (val as u16 & 0x10) << 8;
                self.big_sprites = val & 0x20 != 0;
                self.nmi = val & 0x80 != 0;
            },
            // ppumask
            1 => {
            },
            // oamaddr
            3 => {
            },
            // oamdata
            4 => {
            },
            // ppuscroll
            5 => {
            },
            // ppuaddr
            6 => {
            },
            // ppudata
            7 => {
            },
            _ => (),
        }
    }
}
