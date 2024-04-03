pub mod i8080;
pub mod mos6502;

use unasm_derive::Operand;
pub trait Operand {
    fn parse<'a>(_: &'a str) -> nom::IResult<&'a str, Self> where Self: Sized;
}

pub trait ParsableOpcode {
    fn parse<'a>(_: &'a str) -> nom::IResult<&'a str, Self> where Self: Sized;
}
