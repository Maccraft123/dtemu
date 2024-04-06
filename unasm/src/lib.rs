pub mod i8080;
pub mod mos6502;

use unasm_derive::Operand;
pub trait Operand: Sized {
    fn parse(_: &HashSet<String>, idx: u32, patches: &LabelPatches) -> impl FnMut(&str) -> nom::IResult<&str, Self>;
}

use std::sync::Mutex;
#[derive(Debug)]
pub struct LabelPatches {
    vec: Mutex<Vec<(u32, String)>>,
}

impl LabelPatches {
    pub fn add_patch(&self, op: u32, label: String) {
        self.vec.lock().unwrap().push((op, label));
    }
    pub fn new() -> Self {
        Self { vec: Mutex::new(Vec::new()) }
    }
    pub fn into_inner(self) -> Vec<(u32, String)> {
        self.vec.into_inner().unwrap()
    }
}

use nom::character::complete::hex_digit1;
use nom::bytes::complete::{take_while1, tag_no_case};
use nom::sequence::{terminated, preceded};
use nom::branch::alt;
use nom::combinator::{map_res, map, verify};
use std::collections::HashSet;

macro_rules! impl_operand {
    ($name: ty) => {
        impl Operand for $name {
            fn parse(labels: &HashSet<String>, i: u32, p: &LabelPatches) -> impl FnMut(&str) -> nom::IResult<&str, $name> {
                move |s| {
                    alt((
                            map_res(
                                alt((
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
                                    p.add_patch(i, label.to_string());
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

use unasm_derive::ParsableInstruction;
pub trait ParsableInstruction: Sized + EncodableInstruction {
    fn parse(_: &HashSet<String>) -> impl FnMut(&str) -> nom::IResult<&str, (Self, LabelPatches)>;
    fn set_operand(&mut self, operand: usize, data: f64);
}

pub trait EncodableInstruction: Sized {
    fn encode(&self) -> Vec<u8>;
}

pub trait DecodableInstruction: Sized {
    type Error;
    fn decode(&self, _: &str) -> Result<Self, Self::Error>;
}
