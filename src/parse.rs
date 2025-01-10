use crate::{
    bytecode::ByteCode,
    lex::{Lex, Token},
    value::Value,
};
use std::fs::File;

#[derive(Debug)]
pub struct ParseProto {
    pub constants: Vec<Value>,
    pub byte_codes: Vec<ByteCode>,
}

pub fn load(input: File) -> anyhow::Result<ParseProto> {
    let mut constants = Vec::new();
    let mut byte_codes = Vec::new();
    let mut lex = Lex::new(input);

    loop {
        match lex.next()? {
            Token::Name(name) => {
                constants.push(Value::String(name));
                byte_codes.push(ByteCode::GetGlobal(0, (constants.len() - 1) as u8));

                if let Token::String(s) = lex.next()? {
                    constants.push(Value::String(s));
                    byte_codes.push(ByteCode::LoadConst(1, (constants.len() - 1) as u8));
                    byte_codes.push(ByteCode::Call(0, 1));
                } else {
                    return Err(anyhow::anyhow!("expected string"));
                }
            }
            Token::Eos => break,
            t => {
                return Err(anyhow::anyhow!("unexpected token: {:?}", t));
            }
        }
    }

    println!("{:?}", constants);
    println!("{:?}", byte_codes);

    Ok(ParseProto {
        constants,
        byte_codes,
    })
}
