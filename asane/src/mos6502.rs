use crate::cpu_prelude::*;
use asm_playground::mos6502::{DatalessInstruction, Addressing};
use core::convert::Infallible;

define_cycles!();

#[derive(Clone, Debug)]
pub struct Flags {
    negative: bool,
    overflow: bool,
    brk: bool,
    decimal: bool,
    irq: bool,
    zero: bool,
    carry: bool,
}

impl Flags {
    fn to_stack(&self) -> u8 {
        self.negative as u8 * 0x80 |
            self.overflow as u8 * 0x40 |
            self.brk as u8 * 0x10 |
            self.decimal as u8 * 0x08 |
            self.irq as u8 * 0x04 |
            self.zero as u8 * 0x02 |
            self.carry as u8 * 0x01
    }
    pub fn to_u8(&self) -> u8 { self.to_stack() }
    pub fn negative(&self) -> bool { self.negative }
    pub fn overflow(&self) -> bool { self.overflow }
    pub fn brk(&self) -> bool { self.brk }
    pub fn decimal(&self) -> bool { self.decimal }
    pub fn irq(&self) -> bool { self.irq }
    pub fn zero(&self) -> bool { self.zero }
    pub fn carry(&self) -> bool { self.carry }
}

impl From<u8> for Flags {
    #[inline]
    fn from(v: u8) -> Flags {
        Self {
            negative: v & 0x80 != 0,
            overflow: v & 0x40 != 0,
            brk: v & 0x10 != 0,
            decimal: v & 0x08 != 0,
            irq: v & 0x04 != 0,
            zero: v & 0x02 != 0,
            carry: v & 0x01 != 0,
        }
    }
}

trait UpdateZn {
    fn update_zn(self, f: &mut Flags) -> Self;
}

impl UpdateZn for u8 {
    fn update_zn(self, f: &mut Flags) -> u8 {
        f.zero = self == 0;
        f.negative = self & 0x80 != 0;
        self
    }
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum Interrupt {
    Nmi,
    Irq,
}

#[derive(Clone, Debug)]
pub struct Mos6502 {
    pc: u16,
    s: u8,
    a: u8,
    x: u8,
    y: u8,
    flags: Flags,
}

impl Cpu for Mos6502 {
    type AddressWidth = u16;
    type IoAddressWidth = Infallible;
    type Interrupt = Interrupt;
    type Instruction = asm_playground::mos6502::Instruction;
    #[inline(always)]
    fn set_pc(&mut self, new: u16) {
        self.pc = new;
    }
    #[inline(always)]
    fn pc(&self) -> u16 {
        self.pc
    }
    #[inline]
    fn new(m: &mut impl Bus<u16>, _: &mut impl Bus<Infallible>) -> Self {
        Self {
            pc: m.read16le(0xfffc),
            s: 0xfd,
            a: 0,
            x: 0,
            y: 0,
            flags: 0x24.into(),
        }
    }
    #[inline]
    fn next_instruction(&self, _: &mut impl Bus<u16>) -> Self::Instruction {
        todo!()
    }
    #[inline]
    async fn irq(&mut self, _: Self::Interrupt, _: &mut impl Bus<u16>, _: &mut impl Bus<Infallible>) -> usize {
        todo!()
    }
    #[inline]
    async fn step_block(&mut self, m: &mut impl Bus<u16>, _: &mut impl Bus<Infallible>) -> (usize, u16) {
            use asm_playground::mos6502::Opcode::*;
            cycles!(1);
            let i = DatalessInstruction::from_u8(m.read8(self.pc)).expect("Failed to decode 6502 instruction");
            self.pc += 1;
            if i.len() == 1 {
                let _ = self.rd8(m, self.pc).await;
            }
            match i.opcode() {
                // Load/Store/Transfer
                Lda => self.a = self.load8(m, i.addressing()).await.update_zn(&mut self.flags),
                Ldx => self.x = self.load8(m, i.addressing()).await.update_zn(&mut self.flags),
                Ldy => self.y = self.load8(m, i.addressing()).await.update_zn(&mut self.flags),
                Sta => self.store8(m, i.addressing(), self.a).await,
                Stx => self.store8(m, i.addressing(), self.x).await,
                Sty => self.store8(m, i.addressing(), self.y).await,
                Tax => self.x = self.a.update_zn(&mut self.flags),
                Tay => self.y = self.a.update_zn(&mut self.flags),
                Tsx => self.x = self.s.update_zn(&mut self.flags),
                Txa => self.a = self.x.update_zn(&mut self.flags),
                Txs => self.s = self.x,
                Tya => self.a = self.y.update_zn(&mut self.flags),
                // Stack instructions
                Pha => self.push8(m, self.a).await,
                Php => self.push8(m, self.flags.to_stack()).await,
                Pla => self.a = self.pull8(m).await.update_zn(&mut self.flags),
                Plp => self.flags = self.pull8(m).await.into(),
                // Decrements and increments
                Dec => self.rmw(m, i.addressing(), |_, v| v.wrapping_sub(1)).await,
                Dex => self.x = self.x.wrapping_sub(1).update_zn(&mut self.flags),
                Dey => self.y = self.y.wrapping_sub(1).update_zn(&mut self.flags),
                Inc => self.rmw(m, i.addressing(), |_, v| v.wrapping_add(1)).await,
                Inx => self.x = self.x.wrapping_add(1).update_zn(&mut self.flags),
                Iny => self.y = self.y.wrapping_add(1).update_zn(&mut self.flags),
                // Arithmentic operations
                Adc => self.rma(m, i.addressing(), Self::adc).await,
                Sbc => self.rma(m, i.addressing(), Self::sbc).await,
                Cmp => {
                    let v = self.load8(m, i.addressing()).await;
                    self.flags.carry = true;
                    Self::sbc(&mut self.flags, self.a, v).update_zn(&mut self.flags);
                },
                Cpx => {
                    let v = self.load8(m, i.addressing()).await;
                    self.flags.carry = true;
                    Self::sbc(&mut self.flags, self.x, v).update_zn(&mut self.flags);
                },
                Cpy => {
                    let v = self.load8(m, i.addressing()).await;
                    self.flags.carry = true;
                    Self::sbc(&mut self.flags, self.y, v).update_zn(&mut self.flags);
                },
                // Logical operations
                And => self.rma(m, i.addressing(), |_, a, v| a & v).await,
                Eor => self.rma(m, i.addressing(), |_, a, v| a ^ v).await,
                Ora => self.rma(m, i.addressing(), |_, a, v| a | v).await,
                // Shift and rotate instructions
                Asl => self.rmw(m, i.addressing(), |f, v| { f.carry = v & 0x80 != 0; v << 1 }).await,
                Lsr => self.rmw(m, i.addressing(), |f, v| { f.carry = v & 0x01 != 0; v >> 1 }).await,
                Rol => self.rmw(m, i.addressing(), |f, v| {
                    let carry = v & 0x80 != 0;
                    let ret = v << 1 | f.carry as u8;
                    f.carry = carry;
                    ret
                }).await,
                Ror => self.rmw(m, i.addressing(), |f, v| {
                    let carry = v & 0x1 != 0;
                    let ret = v >> 1 | f.carry as u8 * 0x80;
                    f.carry = carry;
                    ret
                }).await,
                // Flag instructions
                Clc => self.flags.carry = false,
                Cld => self.flags.decimal = false,
                Cli => self.flags.irq = false,
                Clv => self.flags.overflow = false,
                Sec => self.flags.carry = true,
                Sed => self.flags.decimal = true,
                Sei => self.flags.irq = true,
                // Jumps, subroutines and branches
                Bcc => self.branch_on(m, !self.flags.carry).await,
                Bcs => self.branch_on(m, self.flags.carry).await,
                Beq => self.branch_on(m, self.flags.zero).await,
                Bmi => self.branch_on(m, self.flags.negative).await,
                Bne => self.branch_on(m, !self.flags.zero).await,
                Bpl => self.branch_on(m, !self.flags.negative).await,
                Bvc => self.branch_on(m, !self.flags.overflow).await,
                Bvs => self.branch_on(m, self.flags.overflow).await,
                Jmp => self.pc = self.calc_addr(m, i.addressing()).await,
                Jsr => {
                    let dst = self.calc_addr(m, i.addressing()).await;
                    self.pc += 2;
                    let pc = self.pc.to_le_bytes();
                    self.push8(m, pc[1]).await;
                    self.push8(m, pc[0]).await;
                    self.pc = dst;
                },
                Rts => {
                    let lo = self.pull8(m).await;
                    let hi = self.pull8(m).await;
                    self.pc = u16::from_le_bytes([lo, hi]);
                },
                Brk => {
                    let pc = self.pc.to_le_bytes();
                    self.push8(m, pc[1]).await;
                    self.push8(m, pc[0]).await;
                    self.push8(m, self.flags.to_stack()).await;
                    self.pc = m.read16le(0xfffe);
                },
                Rti => {
                    self.flags = self.pull8(m).await.into();
                    let lo = self.pull8(m).await;
                    let hi = self.pull8(m).await;
                    self.pc = u16::from_le_bytes([lo, hi]);
                },
                // Other
                Bit => {
                    cycles!(1);
                    let v = self.load8(m, i.addressing()).await;
                    self.flags.negative = v & 0x80 != 0;
                    self.flags.overflow = v & 0x40 != 0;
                    self.flags.zero = v & self.a == 0;
                },
                Nop => (),
                Jam => panic!(),
            }
            (take_cycles!(), self.pc)
    }
}

impl Mos6502 {
    pub fn a(&self) -> u8 { self.a }
    pub fn x(&self) -> u8 { self.x }
    pub fn y(&self) -> u8 { self.y }
    pub fn sp(&self) -> u8 { self.s }
    pub fn flags(&self) -> Flags { self.flags.clone() }
    async fn branch_on(&mut self, m: &mut impl Bus<u16>, cond: bool) {
        if cond {
            let target = self.calc_addr(m, Addressing::Relative).await;
            if target & 0xff00 != self.pc & 0xff00 {
                cycles!(1);
            }
            self.pc = target;
            cycles!(1);
        } else {
            self.pc += 1;
        }
    }
    fn adc(f: &mut Flags, a: u8, v: u8) -> u8 {
        let result = a as usize + v as usize + f.carry as usize;
        f.overflow = (!(a ^ v) & (a ^ result as u8) & 0x80) != 0; // stolen from https://github.com/superzazu/6502
        f.carry = result > 0xff;
        result as u8
    }
    fn sbc(f: &mut Flags, a: u8, v: u8) -> u8 {
        Self::adc(f, a, v ^ 0xff)
    }
    /*fn sbc(f: &mut Flags, a: u8, v: u8) -> u8 {
        let result = (a as usize)
            .wrapping_sub(v as usize)
            .wrapping_sub((!f.carry) as usize);
        f.overflow = (!(a ^ v) & (a ^ result as u8) & 0x80) != 0; // stolen from https://github.com/superzazu/6502
        f.carry = !(result > 0xff);
        //f.carry = result > a as usize;
        result as u8
    }*/
    async fn rma(&mut self, m: &mut impl Bus<u16>, a: Addressing, modify: impl Fn(&mut Flags, u8, u8) -> u8) {
        let input = self.load8(m, a).await;
        self.a = modify(&mut self.flags, self.a, input).update_zn(&mut self.flags);
    }
    async fn rmw(&mut self, m: &mut impl Bus<u16>, a: Addressing, modify: impl Fn(&mut Flags, u8) -> u8) {
        let input = self.load8_nopc(m, a).await;
        self.store8_nopc(m, a, input).await;
        let output = modify(&mut self.flags, input).update_zn(&mut self.flags);
        self.store8(m, a, output).await;
    }
    async fn push8(&mut self, m: &mut impl Bus<u16>, v: u8) {
        self.w8(m, 0x0100 | self.s as u16, v).await;
        self.s = self.s.wrapping_sub(1);
        cycles!(1);
    }
    async fn pull8(&mut self, m: &mut impl Bus<u16>) -> u8{
        self.s = self.s.wrapping_add(1);
        cycles!(2);
        self.rd8(m, 0x0100 | self.s as u16).await
    }
    async fn w8(&self, m: &mut impl Bus<u16>, addr: u16, value: u8) {
        cycles!(1);
        m.write8(addr, value);
    }
    async fn store8_nopc(&mut self, m: &mut impl Bus<u16>, a: Addressing, value: u8) {
        if a == Addressing::Accumulator {
            self.a = value;
        } else {
            let addr = self.calc_addr(m, a).await;
            self.w8(m, addr, value).await
        }
    }
    async fn store8(&mut self, m: &mut impl Bus<u16>, a: Addressing, value: u8) {
        if a == Addressing::Accumulator {
            self.a = value;
        } else {
            let addr = self.calc_addr(m, a).await;
            self.pc += a.len();
            self.w8(m, addr, value).await
        }
    }
    async fn rd8(&self, m: &mut impl Bus<u16>, addr: u16) -> u8 {
        cycles!(1);
        m.read8(addr)
    }
    async fn rd16(&self, m: &mut impl Bus<u16>, addr: u16) -> u16 {
        cycles!(2);
        m.read16le(addr)
    }
    async fn load8_nopc(&mut self, m: &mut impl Bus<u16>, a: Addressing) -> u8 {
        if a == Addressing::Accumulator {
            self.a
        } else {
            let addr = self.calc_addr(m, a).await;
            self.rd8(m, addr).await
        }
    }
    async fn load8(&mut self, m: &mut impl Bus<u16>, a: Addressing) -> u8 {
        if a == Addressing::Accumulator {
            self.a
        } else {
            let addr = self.calc_addr(m, a).await;
            self.pc += a.len();
            self.rd8(m, addr).await
        }
    }
    async fn calc_addr(&self, m: &mut impl Bus<u16>, a: Addressing) -> u16 {
        match a {
            Addressing::Immediate   => self.pc,
            Addressing::Absolute    => self.rd16(m, self.pc).await,
            Addressing::AbsoluteX   => {
                cycles!(1);
                self.rd16(m, self.pc).await + self.x as u16
            },
            Addressing::AbsoluteY   => {
                cycles!(1);
                self.rd16(m, self.pc).await + self.y as u16
            },
            Addressing::ZeroPage    => self.rd8(m, self.pc).await as u16,
            Addressing::ZeroPageX   => {
                cycles!(1);
                self.rd8(m, self.pc).await.wrapping_add(self.x) as u16
            },
            Addressing::ZeroPageY   => {
                cycles!(1);
                self.rd8(m, self.pc).await.wrapping_add(self.y) as u16
            },
            Addressing::Indirect    => {
                let addr = self.rd16(m, self.pc).await;
                self.rd16(m, addr).await
            }
            Addressing::IndirectX   => {
                let addr = self.rd8(m, self.pc).await.wrapping_add(self.x);
                self.rd16(m, addr as u16).await
            },
            Addressing::IndirectY   => {
                let addr = self.rd8(m, self.pc).await;
                self.rd16(m, addr as u16).await + self.y as u16
            },
            Addressing::Accumulator => panic!("accumulator does not have an address"),
            Addressing::Implied     => unreachable!("instructions with implied addressing don't have any data"),
            Addressing::Relative    => {
                let off = self.rd8(m, self.pc).await as i8;
                (self.pc as isize + off as isize + 1) as u16
            },
        }
    }
}
#[cfg(test)]
mod tests {
    extern crate std;
    
    use super::Mos6502;
    use crate::Cpu;
    /*macro_rules! decode_hex {
        ($path: literal) => {
            {
                let string = include_str!($path);
                let mut string_no_comments = String::new();
                for line in string.lines() {
                    if !line.starts_with("//") {
                        string_no_comments += line;
                    }
                }
                hex::decode(&string_no_comments).unwrap()
            }
        }
    }*/
    fn runhmc(test: &[u8], count: usize, location: u16) -> (usize, Mos6502, [u8; 0x10000]){
        let mut memory = [0u8; 0x10000];
        let [lo, hi] = location.to_le_bytes();
        memory[location as usize..][..test.len()].copy_from_slice(test);
        memory[0xfffc] = lo;
        memory[0xfffd] = hi;
        let mut cpu = Mos6502::new(&mut memory, &mut ());
        let cycles = cpu.step_blocks_sync(count, &mut memory, &mut ()).0;

        (cycles, cpu, memory)
    }
    #[test]
    fn test00_loadstore() {
        let rom = include_bytes!("../tests/neskell/src/tests/hmc-6502/load_store_test.bin");
        let (_, cpu, memory) = runhmc(rom, 47, 0xe000);
        assert_eq!(memory[0x022a], 0x55);
        assert_eq!(cpu.a(), 0x55);
        assert_eq!(cpu.x(), 0x2a);
        assert_eq!(cpu.y(), 0x73);
    }
    #[test]
    fn test01_andorxor() {
        let rom = include_bytes!("../tests/neskell/src/tests/hmc-6502/and_or_xor_test.bin");
        let (_, _, memory) = runhmc(rom, 109, 0xe000);
        assert_eq!(memory[0xa9], 0xaa);
    }
    #[test]
    fn test02_incdec() {
        let rom = include_bytes!("../tests/neskell/src/tests/hmc-6502/inc_dec_test.bin");
        let (_, _, memory) = runhmc(rom, 34, 0xe000);
        assert_eq!(memory[0x71], 0xff);
    }
    #[test]
    fn test03_bitshifts() {
        let rom = include_bytes!("../tests/neskell/src/tests/hmc-6502/bitshift_test.bin");
        let (_, _, memory) = runhmc(rom, 58, 0xe000);
        assert_eq!(memory[0x1dd], 0x6e);
    }
    #[test]
    fn test04_jumpsret() {
        let rom = include_bytes!("../tests/neskell/src/tests/hmc-6502/jump_ret_test.bin");
        let (_, _, memory) = runhmc(rom, 35, 0x0600);
        assert_eq!(memory[0x40], 0x42);
    }
    #[test]
    fn test05_reginstrs() {
        let rom = include_bytes!("../tests/neskell/src/tests/hmc-6502/reg_transf_test.bin");
        let (_, _, memory) = runhmc(rom, 18, 0xe000);
        assert_eq!(memory[0x40], 0x33);
    }
    #[test]
    fn test06_addsub() {
        let rom = include_bytes!("../tests/neskell/src/tests/hmc-6502/add_sub_test.bin");
        let (_, _, memory) = runhmc(rom, 69, 0xe000);
        assert_eq!(memory[0x30], 0xaa);
    }
    #[test]
    fn test07_cmpbeqbne() {
        let rom = include_bytes!("../tests/neskell/src/tests/hmc-6502/cmp_beq_bne_test.bin");
        let (_, _, memory) = runhmc(rom, 54,  0xe000);
        assert_eq!(memory[0x15], 0x7f);
    }
    #[test]
    fn test08_cpxybit() {
        let rom = include_bytes!("../tests/neskell/src/tests/hmc-6502/cpx_cpy_bit_test.bin");
        let (_, _, memory) = runhmc(rom, 29, 0xe000);
        assert_eq!(memory[0x42], 0xa5);
    }
    #[test]
    fn test09_otherbranches() {
        let rom = include_bytes!("../tests/neskell/src/tests/hmc-6502/misc_branch_test.bin");
        let (_, _, memory) = runhmc(rom, 39, 0xe000);
        assert_eq!(memory[0x80], 0x1f);
    }
    #[test]
    fn test10_flaginstrs() {
        let rom = include_bytes!("../tests/neskell/src/tests/hmc-6502/flag_test.bin");
        let (_, _, memory) = runhmc(rom, 13, 0xe000);
        assert_eq!(memory[0x30], 0xce);
    }
    #[test]
    fn test11_stackinstrs() {
        let rom = include_bytes!("../tests/neskell/src/tests/hmc-6502/stack_test.bin");
        let (_, _, memory) = runhmc(rom, 11, 0xe000);
        assert_eq!(memory[0x30], 0x29);
    }
    #[test]
    fn test12_rti() {
        let rom = include_bytes!("../tests/neskell/src/tests/hmc-6502/rti_test.bin");
        let (_, _, memory) = runhmc(rom, 15, 0x0600);
        assert_eq!(memory[0x33], 0x42);
    }
    #[test]
    fn two_plus_two() {
        let rom = &[0xa9, 0x02, 0x69, 0x02];
        let (_, cpu, _) = runhmc(rom, 2, 0xe000);
        assert_eq!(cpu.a(), 0x4);
    }
    #[test]
    fn zero_minus_zero() {
        let rom = &[0x38, 0xa9, 0x00, 0xe9, 0x00];
        let (_, cpu, _) = runhmc(rom, 3, 0xe000);
        assert_eq!(cpu.a(), 0x0);
        assert_eq!(cpu.flags.carry(), true);
        assert_eq!(cpu.flags.zero(), true);
    }
    #[test]
    fn simple_cmp() {
        let rom = &[0xa9, 0x18, 0xc9, 0x18];
        let (_, cpu, _) = runhmc(rom, 2, 0xe000);
        assert_eq!(cpu.a(), 0x18);
        assert_eq!(cpu.flags.zero(), true);
    }
    #[test]
    fn branch_pagecross_test() {
        let rom = include_bytes!("../tests/neskell/src/tests/unit/branch_pagecross_test.bin");
        let (cycles, cpu, _memory) = runhmc(rom, 5, 0x02f9);
        assert_eq!(cpu.a(), 0xff);
        assert_eq!(cycles, 14);
    }
}
