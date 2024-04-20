use proc_macro::TokenStream;

use syn::{parse_macro_input, Ident, DeriveInput, Type};
use darling::{ast, util, FromField, FromVariant, FromDeriveInput};
use quote::quote;

#[derive(Debug, FromVariant)]
#[darling(attributes(operand))]
struct OperandField {
    ident: Ident,
    alias: Option<String>,
    rename: Option<String>,
}

#[derive(Debug, FromDeriveInput)]
#[darling(attributes(operand), supports(enum_any))]
struct OperandInput {
    ident: Ident,
    data: ast::Data<OperandField, util::Ignored>,
}

#[proc_macro_derive(Operand, attributes(operand))]
pub fn operand(input: TokenStream) -> TokenStream {
    let input = OperandInput::from_derive_input(&parse_macro_input!(input as DeriveInput)).unwrap();
    let variants = input.data.take_enum().unwrap();
    let mut parsers = Vec::new();
    let mut matchers = Vec::new();
    for f in variants.iter() {
        let ident = f.ident.clone();
        let lowercase;
        if let Some(name) = &f.rename {
            lowercase = name.to_lowercase();
        } else {
            lowercase = f.ident.to_string().to_lowercase();
        }
        let alias = f.alias.clone().map(|s| s.to_lowercase());
        parsers.push(
            quote! { ::nom::bytes::complete::tag_no_case(#lowercase) }
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
            fn parse(_: &::std::collections::HashSet<String>, _: u32, _: &crate::OperandPatches) -> impl FnMut(&str) -> nom::IResult<&str, #name> {
                |s| { ::nom::combinator::map(
                    ::nom::branch::alt((
                        #(#parsers),*
                    )),
                    |r| match r {
                        #(#matchers),*
                        _ => unreachable!(),
                    }
                )(s) }
            }
            fn set(&mut self, _: f64) {}
        }
    }.into()
}

#[derive(Debug, FromVariant)]
#[darling(attributes(opcode))]
struct OpcodeField {
    ident: Ident,
    alias: Option<String>,
    rename: Option<String>,
}

#[derive(Debug, FromDeriveInput)]
#[darling(attributes(opcode), supports(enum_unit))]
struct OpcodeInput {
    ident: Ident,
    data: ast::Data<OpcodeField, util::Ignored>,
}

#[proc_macro_derive(ParsableOpcode, attributes(opcode))]
pub fn opcode(input: TokenStream) -> TokenStream {
    let input = OpcodeInput::from_derive_input(&parse_macro_input!(input as DeriveInput)).unwrap();

    let variants = input.data.take_enum().unwrap();
    let mut parsers = Vec::new();
    let mut matchers = Vec::new();
    for f in variants.iter() {
        let ident = f.ident.clone();
        let lowercase;
        if let Some(name) = &f.rename {
            lowercase = name.to_lowercase();
        } else {
            lowercase = f.ident.to_string().to_lowercase();
        }
        let alias = f.alias.clone().map(|s| s.to_lowercase());
        parsers.push(
            quote! { ::nom::bytes::complete::tag_no_case(#lowercase) }
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

    let mut alt_chunks = Vec::new();
    for chunk in parsers.chunks(21) {
        alt_chunks.push(quote!(
            ::nom::branch::alt((
                #(#chunk),*
            ))
        ));
    }

    let name = input.ident;
    quote! {
        impl ParsableOpcode for #name {
            fn parse(s: &str) -> nom::IResult<&str, #name> {
                 ::nom::combinator::map(
                    ::nom::branch::alt((
                        #(#alt_chunks),*
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

#[derive(Debug, FromField)]
#[darling(attributes(instruction))]
struct InstructionField {
    ty: Type,
    #[darling(default)]
    comma: bool,
    #[darling(default)]
    nospace: bool,
}

#[derive(Debug, FromVariant)]
#[darling(attributes(instruction))]
struct InstructionVariant {
    ident: Ident,
    fields: ast::Fields<InstructionField>,
}

#[derive(Debug, FromDeriveInput)]
#[darling(attributes(instruction), supports(enum_tuple, enum_unit, struct_tuple))]
struct InstructionInput {
    ident: Ident,
    data: ast::Data<InstructionVariant, Type>,
    #[darling(default)]
    fixup: bool,
}

#[proc_macro_derive(ParsableInstruction, attributes(instruction))]
pub fn instruction(input: TokenStream) -> TokenStream {
    let input = InstructionInput::from_derive_input(&parse_macro_input!(input as DeriveInput))
        .expect("Failed to parse input");

    let ty = &input.ident;
    let mut parsers = Vec::new();
    let mut set_operand_arms = Vec::new();

    let fixup = if input.fixup {
        quote!(<#ty as FixupInstruction>::fixup(&mut this, &patches))
    } else {
        quote!(())
    };

    match input.data {
        ast::Data::Enum(variants) => {
            for v in variants {
                let lowercase = v.ident.to_string().to_lowercase();
                let mut parser = Vec::new();
                let mut bikeshed = Vec::new();
                let mut pattern = Vec::new();
                let mut body = Vec::new();
                let ident = v.ident.clone();
                pattern.push(quote!{ Self::#ident });
                parser.push(quote!(::nom::bytes::complete::tag_no_case(#lowercase)));
                match v.fields.style {
                    ast::Style::Tuple => {
                        let mut pat_body = Vec::new();
                        for (i, f) in v.fields.fields.iter().enumerate() {
                            let path = f.ty.clone();
                            let idx = syn::Index::from(i+1);
                            let first;
                            let idx_ident = syn::Ident::new(&format!("_{}", i), v.ident.span());
                            pat_body.push(quote!(ref mut #idx_ident));
                            if let Type::Path(p) = &path {
                                if let Some(id) = p.path.get_ident() {
                                    if match id.to_string().as_str() {
                                        "u8" | "u16" | "u32" |
                                            "i8" | "i16" | "i32" => true,
                                        _ => false,
                                    } {
                                        body.push(quote!( if idx == #i { *#idx_ident = data as #id } ));
                                    }
                                }
                            }
                            if f.nospace {
                                first = quote!(::nom::combinator::success(1))
                            } else if f.comma {
                                first = quote!({
                                    ::nom::sequence::preceded(
                                        ::nom::bytes::complete::tag_no_case(","),
                                        ::nom::character::complete::space1,
                                    )
                                });
                            } else {
                                first = quote!({::nom::character::complete::space1})
                            }
                            parser.push(quote! {
                                ::nom::sequence::preceded(
                                    #first,
                                    <#path as Operand>::parse(labels, #i as u32, &patches),
                                )
                            });
                            bikeshed.push(quote!( t.#idx ));
                        }
                        pattern.push(quote!{ (#(#pat_body),*) });
                    },
                    _ => (),
                }

                set_operand_arms.push(quote!( #(#pattern)* => { #(#body)* } ));

                let ident = &v.ident;
                let final_parser;
                if bikeshed.len() != 0 {
                    final_parser = quote!({    
                        ::nom::combinator::map(
                            ::nom::sequence::tuple((#(#parser),*)),
                            |t| { #ty::#ident(#(#bikeshed),*) }
                        )
                    });
                } else {
                    final_parser = quote!({
                        ::nom::combinator::map(
                            #(#parser),*,
                            |_| { #ty::#ident }
                        )
                    });
                }

                parsers.push(final_parser);
            }

            let mut parse_chunks = Vec::new();
            for chunk in parsers.chunks(21) {
                parse_chunks.push(quote!(
                        ::nom::branch::alt((
                                #(#chunk),*
                        ))
                ));
            }

            quote! {
                impl ParsableInstruction for #ty {
                    fn parse(labels: &::std::collections::HashSet<String>) -> impl FnMut(&str) -> ::nom::IResult<&str, (Self, crate::OperandPatches)> {
                        |s| {
                            let patches = crate::OperandPatches::new();
                            let ret = ::nom::combinator::map(
                                ::nom::branch::alt((
                                    #(#parse_chunks),*
                                )),
                                |mut this| {
                                    #fixup;
                                    this
                                },
                            )(s);
                            ret.map(|v| (v.0, (v.1, patches)))
                        }
                    }
                    fn set_operand(&mut self, idx: usize, data: f64) {
                        match self {
                            #(#set_operand_arms),*
                        }
                    }
                }
            }.into()
        },
        ast::Data::Struct(types) => {
            let mut iter = types.into_iter();
            let opcode_ty = iter.next().unwrap();
            let operand_ty = iter.next().unwrap();
            if iter.next().is_some() {
                unimplemented!("tuple structs with more than 2 fields");
            }
            quote! {
                impl ParsableInstruction for #ty {
                    fn parse(labels: &::std::collections::HashSet<String>) -> impl FnMut(&str) -> ::nom::IResult<&str, (Self, crate::OperandPatches)> + '_ {
                        |s| {
                            let patches = crate::OperandPatches::new();
                            let ret = ::nom::combinator::map(
                                ::nom::sequence::pair(
                                    <#opcode_ty as ParsableOpcode>::parse,
                                    ::nom::sequence::preceded(
                                        ::nom::character::complete::space0,
                                        <#operand_ty as Operand>::parse(labels, 0, &patches),
                                    ),
                                ),
                                |(opcode, operand)| {
                                    let mut this = Self(opcode, operand);
                                    #fixup;
                                    this
                                },
                            )(s);
                            ret.map(|v| (v.0, (v.1, patches)))
                        }
                    }
                    fn set_operand(&mut self, op: usize, data: f64) {
                        if op == 0 {
                            self.1.set(data.into());
                        }
                    }
                }
            }.into()
        },
    }
}
