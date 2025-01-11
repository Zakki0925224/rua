use crate::{
    bytecode::ByteCode,
    lex::{Lex, Token},
    value::Value,
};
use std::io::Read;

#[derive(Debug)]
pub struct ParseProto<R: Read> {
    pub constants: Vec<Value>,
    pub byte_codes: Vec<ByteCode>,

    locals: Vec<String>,
    lex: Lex<R>,
}

impl<R: Read> ParseProto<R> {
    pub fn load(input: R) -> anyhow::Result<Self> {
        let mut proto = Self {
            constants: Vec::new(),
            byte_codes: Vec::new(),
            locals: Vec::new(),
            lex: Lex::new(input),
        };

        proto.chunk()?;

        println!("parsed constants:");
        for (i, c) in proto.constants.iter().enumerate() {
            println!("  [{}]{:?}", i, c);
        }

        println!("parsed byte codes:");
        for c in proto.byte_codes.iter() {
            println!("  {:?}", c);
        }

        println!();

        Ok(proto)
    }

    fn chunk(&mut self) -> anyhow::Result<()> {
        loop {
            match self.lex.next()? {
                Token::Name(name) => {
                    if self.lex.peek()? == &Token::Assign {
                        self.assignment(name)?;
                    } else {
                        self.function_call(name)?;
                    }
                }
                Token::Local => self.local()?,
                Token::Eos => break,
                t => return Err(anyhow::anyhow!("unexpected token: {:?}", t)),
            }
        }

        Ok(())
    }

    fn function_call(&mut self, name: String) -> anyhow::Result<()> {
        let ifunc = self.locals.len();
        let iarg = ifunc + 1;

        let code = self.load_var(ifunc, name);
        self.byte_codes.push(code);

        match self.lex.next()? {
            Token::ParL => {
                // '('
                self.load_exp(iarg)?;

                if self.lex.next()? != Token::ParR {
                    // ')'
                    return Err(anyhow::anyhow!("expected `)`"));
                }
            }
            Token::String(s) => {
                let code = self.load_const(iarg, s);
                self.byte_codes.push(code);
            }
            _ => return Err(anyhow::anyhow!("expected string")),
        }

        self.byte_codes.push(ByteCode::Call(ifunc as u8, 1));
        Ok(())
    }

    fn local(&mut self) -> anyhow::Result<()> {
        let var = if let Token::Name(var) = self.lex.next()? {
            var
        } else {
            return Err(anyhow::anyhow!("expected variable name"));
        };

        if self.lex.next()? != Token::Assign {
            return Err(anyhow::anyhow!("expected `=`"));
        }

        self.load_exp(self.locals.len())?;
        self.locals.push(var);

        Ok(())
    }

    fn assignment(&mut self, var: String) -> anyhow::Result<()> {
        self.lex.next()?; // '='

        if let Some(i) = self.get_local(&var) {
            self.load_exp(i)?;
        } else {
            let dst = self.add_const(var) as u8;

            let code = match self.lex.next()? {
                Token::Nil => ByteCode::SetGlobalConst(dst, self.add_const(()) as u8),
                Token::True => ByteCode::SetGlobalConst(dst, self.add_const(true) as u8),
                Token::False => ByteCode::SetGlobalConst(dst, self.add_const(false) as u8),
                Token::Integer(i) => ByteCode::SetGlobalConst(dst, self.add_const(i) as u8),
                Token::Float(f) => ByteCode::SetGlobalConst(dst, self.add_const(f) as u8),
                Token::String(s) => ByteCode::SetGlobalConst(dst, self.add_const(s) as u8),

                Token::Name(var) => {
                    if let Some(i) = self.get_local(&var) {
                        ByteCode::SetGlobal(dst, i as u8)
                    } else {
                        ByteCode::SetGlobalGlobal(dst, self.add_const(var) as u8)
                    }
                }

                _ => return Err(anyhow::anyhow!("invalid argument")),
            };
            self.byte_codes.push(code);
        }

        Ok(())
    }

    fn add_const(&mut self, c: impl Into<Value>) -> usize {
        let c = c.into();

        let constants = &mut self.constants;
        constants.iter().position(|v| v == &c).unwrap_or_else(|| {
            constants.push(c);
            constants.len() - 1
        })
    }

    fn load_const(&mut self, dst: usize, c: impl Into<Value>) -> ByteCode {
        ByteCode::LoadConst(dst as u8, self.add_const(c) as u16)
    }

    fn load_var(&mut self, dst: usize, name: String) -> ByteCode {
        if let Some(i) = self.get_local(&name) {
            ByteCode::Move(dst as u8, i as u8)
        } else {
            let ic = self.add_const(name);
            ByteCode::GetGlobal(dst as u8, ic as u8)
        }
    }

    fn get_local(&self, name: &str) -> Option<usize> {
        self.locals.iter().position(|v| v == name)
    }

    fn load_exp(&mut self, dst: usize) -> anyhow::Result<()> {
        let code = match self.lex.next()? {
            Token::Nil => ByteCode::LoadNil(dst as u8),
            Token::True => ByteCode::LoadBool(dst as u8, true),
            Token::False => ByteCode::LoadBool(dst as u8, false),
            Token::Integer(i) => {
                if let Ok(ii) = i16::try_from(i) {
                    ByteCode::LoadInt(dst as u8, ii)
                } else {
                    self.load_const(dst, i)
                }
            }
            Token::Float(f) => self.load_const(dst, f),
            Token::String(s) => self.load_const(dst, s),
            Token::Name(var) => self.load_var(dst, var),
            _ => return Err(anyhow::anyhow!("invalid argument")),
        };
        self.byte_codes.push(code);
        Ok(())
    }
}
