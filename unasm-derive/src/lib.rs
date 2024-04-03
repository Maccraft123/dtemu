use proc_macro::TokenStream;

use syn::{parse_macro_input, Ident, DeriveInput, Data, Fields, Type, Meta};
use darling::{ast, util, FromVariant, FromDeriveInput};
use quote::quote;

#[derive(Debug, FromVariant)]
#[darling(attributes(operand))]
struct VariantField {
    ident: Ident,
    alias: Option<String>,
    rename: Option<String>,
}

#[derive(Debug, FromDeriveInput)]
#[darling(attributes(operand), supports(enum_any))]
struct OperandImpl {
    ident: Ident,
    data: ast::Data<VariantField, util::Ignored>,
}

#[proc_macro_derive(Operand, attributes(operand))]
pub fn operand(input: TokenStream) -> TokenStream {
    let input = OperandImpl::from_derive_input(&parse_macro_input!(input as DeriveInput)).unwrap();

    eprintln!("{:#?}", input);

    let variants = input.data.take_enum().unwrap();
    let mut parsers = Vec::new();
    let mut matchers = Vec::new();
    for f in variants.iter() {
        eprintln!("{:?}", f);
        let ident = f.ident.clone();
        let lowercase;
        if let Some(name) = &f.rename {
            lowercase = name.to_lowercase();
        } else {
            lowercase = f.ident.to_string().to_lowercase();
        }
        let alias = f.alias.clone().map(|s| s.to_lowercase());
        parsers.push(
            quote! { ::nom::bytes::complete::tag(#lowercase) }
        );
        matchers.push(
            quote! { #lowercase => { Self::#ident } }
        );
        if let Some(alias) = alias {
            matchers.push(
                quote! { #alias => { Self::#ident } }
            );
        }
    }

    let name = input.ident;
    quote! {
        impl Operand for #name {
            fn parse(s: &str) -> nom::IResult<&str, #name> {
                ::nom::combinator::map(
                    ::nom::branch::alt((
                        #(#parsers),*
                    )),
                    |r| match r {
                        #(#matchers),*
                        _ => unreachable!(),
                    }
                )(s)
            }
        }
    }.into()
}

fn parse_u8_hex(s: &str) -> nom::IResult<&str, u8> {
    todo!("parsing u8")
}

fn lookup_parsefn(ty: Type) -> Option<&'static str> {
    match ty {
        Type::Path(p) => {
            let name = p.path
                .get_ident()?
                .to_string();
            match name.as_str() {
                "u8" => Some("::unasm_derive::parse_u8_hex"),
                _ => None,
            }
        },
        _ => None,
    }
}
