use crate::devices::prelude::*;

#[derive(Debug)]
pub struct Rom {
    data: Vec<u8>,
    rom_name: String,
    allow_write: bool,
    name: String,
    phandle: Option<usize>,
    //fw_offset: u32,
}

impl Device for Rom {
    fn phandle(&self) -> Option<usize> { self.phandle }
    fn firmware_name(&mut self) -> Option<&str> {
        Some(&self.rom_name)
    }
    fn load_firmware(&mut self, fw: &[u8]) {
        if fw.len() > self.data.len() {
            panic!("file too big, my size: {:x}, file size: {:x}", self.data.len(), fw.len());
        }
        for (i, x) in fw.iter().enumerate() {
             self.data[i] = *x;
        }
    }
    fn as_mmio(&mut self) -> Option<&mut dyn Mmio> { Some(self) }
    fn node_name(&self) -> &str { &self.name }
    fn new(node: &FdtNode<'_, '_>) -> Arc<Mutex<Self>> {
        let len = node.reg()
            .expect("reg has to be supplied with a memory device")
            .next().unwrap()
            .size.unwrap();
        let name = node.name.to_string();
        let rom_name;
        if let Some(romname) = node.property("rom,name") {
            rom_name = romname.as_str().unwrap().to_string();
        } else {
            panic!("rom,name must exist and contain human-readable name of the ROM");
        }
        let write = node.property("rom,allow-write").is_some();
        //let fw_offset = node.property("rom,fw-offset")
            //.and_then(|p| p.as_usize())
            //.unwrap_or(0) as u32;

        Arc::new(Mutex::new(Self {
                data: vec![0; len],
                rom_name,
                allow_write: write,
                name,
                phandle: node.property("phandle").map(|p| p.as_usize()).flatten(),
                //fw_offset,
            }
        ))
    }    
}

impl Mmio for Rom {
    fn read8(&mut self, addr: u32) -> u8 { self.data[addr as usize] }
    fn write8(&mut self, addr: u32, val: u8) {
        if self.allow_write {
            self.data[addr as usize] = val
        } else {
            //eprintln!("Invalid write to ROM")
        }
    }
}

#[derive(Debug)]
pub struct Memory {
    data: Vec<u8>,
    name: String,
}

impl Device for Memory {
    fn as_mmio(&mut self) -> Option<&mut dyn Mmio> { Some(self) }
    fn node_name(&self) -> &str { &self.name }
    fn phandle(&self) -> Option<usize> { None } // no
    fn new(node: &FdtNode<'_, '_>) -> Arc<Mutex<Self>> {
        let len = node.reg()
            .expect("reg has to be supplied with a memory device")
            .next().unwrap()
            .size.unwrap();
        let name = node.name.to_string();
        Arc::new(Mutex::new(Self { data: vec![0; len], name } ))
    }
}

impl Mmio for Memory {
    fn read8(&mut self, addr: u32) -> u8 { self.data[addr as usize] }
    fn write8(&mut self, addr: u32, val: u8) { self.data[addr as usize] = val }
}
