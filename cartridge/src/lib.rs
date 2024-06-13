#![no_std]
extern crate alloc;
use alloc::vec::Vec;

use nom::{
    sequence::tuple,
    combinator::map,
    bytes::complete::{
        tag,
        take,
    },
    number,
};
use bitfield_struct::bitfield;

#[bitfield(u8, order = Lsb)]
struct Flags6 {
    horizontal_nametable: bool,
    nvram: bool,
    trainer: bool,
    alt_nametable: bool,
    #[bits(4)]
    mapper_num_lo: u8,
}

fn flags10(f: &[u8]) -> nom::IResult<&[u8], Flags10> {
    map(
        number::complete::u8,
        Flags10::from_bits,
    )(f)
}

fn flags7(f: &[u8]) -> nom::IResult<&[u8], Flags7> {
    map(
        number::complete::u8,
        Flags7::from_bits,
    )(f)
}

fn flags6(f: &[u8]) -> nom::IResult<&[u8], Flags6> {
    map(
        number::complete::u8,
        Flags6::from_bits,
    )(f)
}

fn tvsystem(f: &[u8]) -> nom::IResult<&[u8], TvSystemRaw> {
    map(
        number::complete::u8,
        TvSystemRaw::from_bits,
    )(f)
}

#[bitfield(u8, order = Lsb)]
struct TvSystemRaw {
    pal: bool,
    #[bits(7)]
    pad: u8,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum TvSystem {
    Pal,
    Ntsc,
}

#[bitfield(u8, order = Lsb)]
struct Flags7 {
    vs_unisystem: bool,
    playchoice_10: bool,
    #[bits(2)]
    nes20: u8,
    #[bits(4)]
    mapper_num_hi: u8,
}

#[bitfield(u8, order = Lsb)]
struct Flags10 {
    ntsc: bool,
    pal: bool,
    #[bits(2)]
    __: u8,
    prg_ram: bool,
    bus_conflicts: bool,
    #[bits(2)]
    __: u8,
}

#[derive(Debug, Copy, Clone)]
pub enum Mapper {
    Nrom = 0,
    Mmc1,
    Uxrom,
    Cnrom,
    Mmc3,
    Mmc5,
    Mapper006,
    Axrom,
    Mapper008,
    Mmc2,
    Mmc4,
    ColorDreams,
    Mapper012,
}

impl Mapper {
    fn from_ines_idx(num: u8) -> Self {
        match num {
            0 => Self::Nrom,
            1 => Self::Mmc1,
            2 => Self::Uxrom,
            3 => Self::Cnrom,
            4 => Self::Mmc3,
            5 => Self::Mmc5,
            6 => Self::Mapper006,
            7 => Self::Axrom,
            8 => Self::Mapper008,
            9 => Self::Mmc2,
            10 => Self::Mmc4,
            11 => Self::ColorDreams,
            12 => Self::Mapper012,
            idk => todo!("mapper {}", idk),
        }
    }
}

pub struct INesFile {
    header: INesHeader,
    mapper: Mapper,
    trainer: Vec<u8>,
    prg_rom: Vec<u8>,
    chr_rom: Vec<u8>,
    playchoice_inst_rom: Vec<u8>,
    playchoice_prom: Vec<u8>,
}

impl INesFile {
    pub fn header(&self) -> &INesHeader { &self.header }
    pub fn mapper(&self) -> Mapper { self.mapper }
    pub fn prg_rom(&self) -> &[u8] { &self.prg_rom }
    pub fn prg_ram_size(&self) -> usize { self.header.prg_ram_size as usize }
    pub fn chr_rom(&self) -> &[u8] { &self.chr_rom }
    pub fn trainer(&self) -> Option<&[u8]> {
        if self.trainer.is_empty() {
            None
        } else {
            Some(&self.trainer)
        }
    }
    pub fn parse(f: &[u8]) -> nom::IResult<&[u8], INesFile> {
        let (f2, hdr) = INesHeader::parse(f)?;
        let ret = map(
            tuple((
                take::<usize, &[u8], _>(0x200 * hdr.trainer as usize),
                take(0x4000 * hdr.prg_rom_size as usize),
                take(0x2000 * hdr.chr_rom_size as usize),
                take(0x2000 * hdr.playchoice10 as usize),
                take(0x2 * hdr.playchoice10 as usize),
            )),
            |v| {
                let ret = INesFile {
                    header: hdr.clone(),
                    mapper: Mapper::from_ines_idx(hdr.mapper),
                    trainer: v.0.to_vec(),
                    prg_rom: v.1.to_vec(),
                    chr_rom: v.2.to_vec(),
                    playchoice_inst_rom: v.3.to_vec(),
                    playchoice_prom: v.4.to_vec(),
                };
                assert!(ret.trainer.is_empty());
                assert_eq!(ret.prg_rom.len(), 0x4000);
                assert_eq!(ret.chr_rom.len(), 0x2000);
                assert!(ret.playchoice_inst_rom.is_empty());
                assert!(ret.playchoice_prom.is_empty());
                ret
            }
        )(f2);
        ret
    }
}

#[derive(Debug, Clone)]
pub struct INesHeader {
    prg_rom_size: u8,
    chr_rom_size: u8,
    nametable_arrangement: bool,
    nvram: bool,
    trainer: bool,
    alt_nametable: bool,
    mapper: u8,
    vs_unisystem: bool,
    playchoice10: bool,
    nes20: bool,
    prg_ram_size: u8,
    tv_system: TvSystem,
}

impl INesHeader {
    pub fn nametable_arrangement(&self) -> bool { self.nametable_arrangement }
    pub fn nvram(&self) -> bool { self.nvram }
    pub fn alt_nametable(&self) -> bool { self.alt_nametable }
    pub fn vs_unisystem(&self) -> bool { self.vs_unisystem }
    pub fn playchoice10(&self) -> bool { self.playchoice10 }
    pub fn is_nes20(&self) -> bool { self.nes20 }
    pub fn tvsystem(&self) -> TvSystem { self.tv_system }
    pub fn parse(f: &[u8]) -> nom::IResult<&[u8], INesHeader> {
        let oldf = f;
        map(
            tuple((
                    tag(b"NES\x1a"),
                    number::complete::u8,
                    number::complete::u8,
                    flags6,
                    flags7,
                    number::complete::u8,
                    tvsystem,
                    flags10,
            )),
            |v| {
                INesHeader {
                    prg_rom_size: v.1,
                    chr_rom_size: v.2,
                    nametable_arrangement: v.3.horizontal_nametable(),
                    nvram: v.3.nvram(),
                    trainer: v.3.trainer(),
                    alt_nametable: v.3.alt_nametable(),
                    mapper: v.3.mapper_num_lo() | v.4.mapper_num_hi() << 4,
                    vs_unisystem: v.4.vs_unisystem(),
                    playchoice10: v.4.playchoice_10(),
                    nes20: v.4.nes20() == 0x02,
                    prg_ram_size: v.5,
                    tv_system: if v.6.pal() { TvSystem::Pal } else { TvSystem::Ntsc },
                }
            },
            )(f).map(|(_, hdr)| (&oldf[0x10..], hdr))
    }
}
