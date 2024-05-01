pub mod i8080;
pub mod mos6502;
pub mod mc6809;

use unasm_derive::Operand;
pub trait Operand: Sized {
    fn parse(_: &HashSet<String>, idx: u32, patches: &OperandPatches) -> impl FnMut(&str) -> nom::IResult<&str, Self>;
    fn set(&mut self, _: f64);
}

use std::sync::Mutex;
#[derive(Debug)]
pub struct OperandPatches {
    vec: Mutex<Vec<(u32, Patch)>>,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Patch {
    /// Sets the operand in .0 to label .1 
    LabelAbsolute(String),
    /// Sets the operand in .0 to (label-instruction address)
    LabelRelative(String),
    /// Sets the operand in .0 to (self.1-instruction address)
    ConstRelative(u32),
}

impl Patch {
    /*fn label(&self) -> Option<&str> {
        match &self {
            Patch::LabelAbsolute(s) => Some(s),
            Patch::LabelRelative(s) => Some(s),
            Patch::ConstRelative(_) => None,
        }
    }*/
    fn into_relative(self) -> Self {
        match self {
            Self::LabelAbsolute(s) => Self::LabelRelative(s),
            _ => self,
        }
    }
}

impl OperandPatches {
    pub fn add_patch(&self, op: u32, patch: Patch) {
        let mut vec = self.vec.lock().unwrap();
        if !vec.iter().any(|(num, _)| *num == op) {
            vec.push((op, patch));
        }
    }
    pub fn add_or_replace_patch(&self, op: u32, patch: Patch) {
        let mut vec = self.vec.lock().unwrap();
        if let Some(ref mut p) = vec.iter_mut().find(|(num, _)| *num == op) {
            p.1 = patch;
        } else {
            vec.push((op, patch));
        }
    }
    pub fn get_patch(&self, op: u32) -> Option<Patch> {
        let mut vec = self.vec.lock().unwrap();
        vec.iter_mut()
            .find(|(num, _)| *num == op)
            .map(|v| v.1.clone())
    }
    pub fn new() -> Self {
        Self { vec: Mutex::new(Vec::new()) }
    }
    pub fn inner(&self) -> &Mutex<Vec<(u32, Patch)>> {
        &self.vec
    }
    pub fn into_inner(self) -> Vec<(u32, Patch)> {
        self.vec.into_inner().unwrap()
    }
}

use nom::character::complete::hex_digit1;
use nom::bytes::complete::{take_while1, tag_no_case, tag};
use nom::sequence::{terminated, preceded};
use nom::branch::alt;
use nom::combinator::{map_res, map, verify};
use std::collections::HashSet;

macro_rules! impl_operand {
    ($name: ty) => {
        impl Operand for $name {
            fn set(&mut self, v: f64) {
                *self = v as $name;
            }
            fn parse(labels: &HashSet<String>, i: u32, p: &OperandPatches) -> impl FnMut(&str) -> nom::IResult<&str, $name> {
                move |s| {
                    alt((
                            map_res(
                                alt((
                                    preceded(
                                        tag("$"),
                                        hex_digit1,
                                    ),
                                    preceded(
                                        tag_no_case("0x"),
                                        hex_digit1,
                                    ),
                                    terminated(
                                        hex_digit1,
                                        tag_no_case("h"),
                                    ),
                                )),
                                |num| <$name>::from_str_radix(num, 16)
                            ),
                            map(
                                verify(
                                    take_while1(|c: char| c.is_ascii_alphabetic() || c == '_'),
                                    |res: &str| labels.contains(res),
                                ),
                                |label: &str| {
                                    p.add_patch(i, Patch::LabelAbsolute(label.to_string()));
                                    0 as $name
                                },
                            ),
                    ))(s)
                }
            }
        }
    };
}

impl_operand!(u8);
impl_operand!(u16);
impl_operand!(u32);

pub trait ParsableEncodableInstruction: ParsableInstruction + EncodableInstruction {}

impl<T: ParsableInstruction + EncodableInstruction> ParsableEncodableInstruction for T {}

use unasm_derive::ParsableInstruction;
pub trait ParsableInstruction: core::fmt::Debug {
    /// Returns the parser, first argument is the hashset of known labels
    fn parse(_: &HashSet<String>) -> impl FnMut(&str) -> nom::IResult<&str, (Self, OperandPatches)> where Self: Sized;
    /// Sets value of an operand, used mainly for patching label addresses and expressions
    fn set_operand(&mut self, operand: usize, data: f64);
}

pub trait ValidateInstruction: ParsableInstruction {
    /// Ensure that this instruction is correct, on 8080 instruction "mov m, m" is decoded as "hlt"
    /// and should not be accepted
    fn validate(&self);
}

pub trait FixupInstruction: ParsableInstruction {
    /// A hack to allow branching to labels for 6502
    fn fixup(&mut self, _: &OperandPatches);
}

use unasm_derive::ParsableOpcode;
pub trait ParsableOpcode {
    fn parse(_: &str) -> nom::IResult<&str, Self> where Self: Sized;
}

pub trait EncodableInstruction: core::fmt::Debug {
    /// Encodes this instruction, returning a Vec with the binary form
    fn encode(&self) -> Vec<u8>;
    /// Returns length of this instruction
    fn len(&self) -> usize;
}

pub trait DecodableInstruction {
    type Error;
    /// Decodes a string of bytes into an instruction
    fn decode(_: &[u8]) -> Result<Self, Self::Error> where Self: Sized;
    /// Returns length of this instruction
    fn len(&self) -> usize;
}
