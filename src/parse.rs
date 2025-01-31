use crate::{
    bytecode::ByteCode,
    lex::{Lex, Token},
    value::Value,
};
use std::{cmp::Ordering, io::Read};

#[derive(Debug, PartialEq)]
enum ExpDesc {
    Nil,
    Boolean(bool),
    Integer(i64),
    Float(f64),
    String(Vec<u8>),
    Local(usize),
    Global(usize),
    Index(usize, usize),
    IndexField(usize, usize),
    IndexInt(usize, u8),
    Call,
}

enum ConstStack {
    Const(usize),
    Stack(usize),
}

#[derive(Debug)]
pub struct ParseProto<R: Read> {
    pub constants: Vec<Value>,
    pub byte_codes: Vec<ByteCode>,

    sp: usize,
    locals: Vec<String>,
    lex: Lex<R>,
}

impl<R: Read> ParseProto<R> {
    pub fn load(input: R) -> anyhow::Result<Self> {
        let mut proto = Self {
            constants: Vec::new(),
            byte_codes: Vec::new(),
            sp: 0,
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

    fn block(&mut self) -> anyhow::Result<()> {
        loop {
            self.sp = self.locals.len();

            match self.lex.next()? {
                Token::SemiColon => (),
                t @ Token::Name(_) | t @ Token::ParL => {
                    let desc = self.prefixexp(t)?;
                    if desc == ExpDesc::Call {
                        // nothing to do
                    } else {
                        self.assignment(desc)?;
                    }
                }
                Token::Local => self.local()?,
                Token::Eos => break,
                t => return Err(anyhow::anyhow!("unexpected token: {:?}", t)),
            }
        }

        Ok(())
    }

    fn chunk(&mut self) -> anyhow::Result<()> {
        self.block()
    }

    fn local(&mut self) -> anyhow::Result<()> {
        let mut vars = Vec::new();
        let nexp = loop {
            vars.push(self.read_name()?);

            match self.lex.peek()? {
                Token::Comma => {
                    self.lex.next()?;
                }
                Token::Assign => {
                    self.lex.next()?;
                    break self.explist()?;
                }
                _ => break 0,
            }
        };

        if nexp < vars.len() {
            let ivar = self.locals.len() + nexp;
            let nnil = vars.len() - nexp;
            self.byte_codes
                .push(ByteCode::LoadNil(ivar as u8, nnil as u8));
        }

        self.locals.append(&mut vars);
        Ok(())
    }

    fn assign_var(&mut self, var: ExpDesc, value: ExpDesc) -> anyhow::Result<()> {
        if let ExpDesc::Local(i) = var {
            self.discharge(i, value)?;
        } else {
            match self.discharge_const(value)? {
                ConstStack::Const(i) => self.assign_from_const(var, i)?,
                ConstStack::Stack(i) => self.assign_from_stack(var, i)?,
            }
        }

        Ok(())
    }

    fn assign_from_stack(&mut self, var: ExpDesc, value: usize) -> anyhow::Result<()> {
        let code = match var {
            ExpDesc::Local(i) => ByteCode::Move(i as u8, value as u8),
            ExpDesc::Global(name) => ByteCode::SetGlobal(name as u8, value as u8),
            ExpDesc::Index(t, key) => ByteCode::SetTable(t as u8, key as u8, value as u8),
            ExpDesc::IndexField(t, key) => ByteCode::SetField(t as u8, key as u8, value as u8),
            ExpDesc::IndexInt(t, key) => ByteCode::SetInt(t as u8, key, value as u8),
            _ => return Err(anyhow::anyhow!("assign from stack")),
        };
        self.byte_codes.push(code);

        Ok(())
    }

    fn assign_from_const(&mut self, var: ExpDesc, value: usize) -> anyhow::Result<()> {
        let code = match var {
            ExpDesc::Global(name) => ByteCode::SetGlobalConst(name as u8, value as u8),
            ExpDesc::Index(t, key) => ByteCode::SetTableConst(t as u8, key as u8, value as u8),
            ExpDesc::IndexField(t, key) => ByteCode::SetFieldConst(t as u8, key as u8, value as u8),
            ExpDesc::IndexInt(t, key) => ByteCode::SetIntConst(t as u8, key, value as u8),
            _ => return Err(anyhow::anyhow!("assign from const")),
        };
        self.byte_codes.push(code);

        Ok(())
    }

    fn assignment(&mut self, first_var: ExpDesc) -> anyhow::Result<()> {
        let mut vars = vec![first_var];
        loop {
            match self.lex.next()? {
                Token::Comma => {
                    let token = self.lex.next()?;
                    vars.push(self.prefixexp(token)?);
                }
                Token::Assign => break,
                t => return Err(anyhow::anyhow!("invalid assign: {:?}", t)),
            }
        }

        let exp_sp0 = self.sp;
        let mut nfexp = 0;
        let last_exp = loop {
            let desc = self.exp()?;

            if self.lex.peek()? == &Token::Comma {
                self.lex.next()?;
                self.discharge(exp_sp0 + nfexp, desc)?;
                nfexp += 1;
            } else {
                break desc;
            }
        };

        match (nfexp + 1).cmp(&vars.len()) {
            Ordering::Equal => {
                let last_var = vars.pop().ok_or(anyhow::anyhow!("empty vars"))?;
                self.assign_var(last_var, last_exp)?;
            }
            Ordering::Less => {
                todo!("expand last exps");
            }
            Ordering::Greater => {
                nfexp = vars.len();
            }
        }

        while let Some(var) = vars.pop() {
            nfexp -= 1;
            self.assign_from_stack(var, exp_sp0 + nfexp)?;
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

    fn explist(&mut self) -> anyhow::Result<usize> {
        let mut n = 0;
        let sp0 = self.sp;

        loop {
            let desc = self.exp()?;
            self.discharge(sp0 + n, desc)?;

            n += 1;
            if self.lex.peek()? != &Token::Comma {
                return Ok(n);
            }
            self.lex.next()?;
        }
    }

    fn args(&mut self) -> anyhow::Result<ExpDesc> {
        let ifunc = self.sp - 1;
        let argn = match self.lex.next()? {
            Token::ParL => {
                if self.lex.peek()? != &Token::ParR {
                    let argn = self.explist()?;
                    self.lex.expect(Token::ParR)?;
                    argn
                } else {
                    self.lex.next()?;
                    0
                }
            }
            Token::CurlyL => {
                self.table_constructor()?;
                1
            }
            Token::String(s) => {
                self.discharge(ifunc + 1, ExpDesc::String(s))?;
                1
            }
            t => return Err(anyhow::anyhow!("invalid args: {:?}", t)),
        };

        self.byte_codes
            .push(ByteCode::Call(ifunc as u8, argn as u8));
        Ok(ExpDesc::Call)
    }

    fn simple_name(&mut self, name: String) -> ExpDesc {
        if let Some(ilocal) = self.locals.iter().rposition(|v| v == &name) {
            ExpDesc::Local(ilocal)
        } else {
            ExpDesc::Global(self.add_const(name))
        }
    }

    fn read_name(&mut self) -> anyhow::Result<String> {
        if let Token::Name(name) = self.lex.next()? {
            Ok(name)
        } else {
            Err(anyhow::anyhow!("expected name"))
        }
    }

    fn discharge(&mut self, dst: usize, desc: ExpDesc) -> anyhow::Result<()> {
        let code = match desc {
            ExpDesc::Nil => ByteCode::LoadNil(dst as u8, 1),
            ExpDesc::Boolean(b) => ByteCode::LoadBool(dst as u8, b),
            ExpDesc::Integer(i) => {
                if let Ok(i) = i16::try_from(i) {
                    ByteCode::LoadInt(dst as u8, i)
                } else {
                    ByteCode::LoadConst(dst as u8, self.add_const(i) as u16)
                }
            }
            ExpDesc::Float(f) => ByteCode::LoadConst(dst as u8, self.add_const(f) as u16),
            ExpDesc::String(s) => ByteCode::LoadConst(dst as u8, self.add_const(s) as u16),
            ExpDesc::Local(src) => {
                if dst != src {
                    ByteCode::Move(dst as u8, src as u8)
                } else {
                    return Ok(());
                }
            }
            ExpDesc::Global(iname) => ByteCode::GetGlobal(dst as u8, iname as u8),
            ExpDesc::Index(itable, ikey) => ByteCode::GetTable(dst as u8, itable as u8, ikey as u8),
            ExpDesc::IndexField(itable, ikey) => {
                ByteCode::GetField(dst as u8, itable as u8, ikey as u8)
            }
            ExpDesc::IndexInt(itable, ikey) => ByteCode::GetInt(dst as u8, itable as u8, ikey),
            ExpDesc::Call => return Err(anyhow::anyhow!("discharge Call")),
        };
        self.byte_codes.push(code);
        self.sp = dst + 1;
        Ok(())
    }

    fn discharge_top(&mut self, desc: ExpDesc) -> anyhow::Result<usize> {
        self.discharge_if_need(self.sp, desc)
    }

    fn discharge_if_need(&mut self, dst: usize, desc: ExpDesc) -> anyhow::Result<usize> {
        if let ExpDesc::Local(i) = desc {
            return Ok(i);
        }

        self.discharge(dst, desc)?;
        Ok(dst)
    }

    fn discharge_const(&mut self, desc: ExpDesc) -> anyhow::Result<ConstStack> {
        match desc {
            ExpDesc::Nil => Ok(ConstStack::Const(self.add_const(()))),
            ExpDesc::Boolean(b) => Ok(ConstStack::Const(self.add_const(b))),
            ExpDesc::Integer(i) => Ok(ConstStack::Const(self.add_const(i))),
            ExpDesc::Float(f) => Ok(ConstStack::Const(self.add_const(f))),
            ExpDesc::String(s) => Ok(ConstStack::Const(self.add_const(s))),
            _ => Ok(ConstStack::Stack(self.discharge_top(desc)?)),
        }
    }

    fn prefixexp(&mut self, ahead: Token) -> anyhow::Result<ExpDesc> {
        let sp0 = self.sp;

        let mut desc = match ahead {
            Token::Name(name) => self.simple_name(name),
            Token::ParL => {
                let desc = self.exp()?;
                self.lex.expect(Token::ParR)?;
                desc
            }
            t => {
                return Err(anyhow::anyhow!("invalid prefixexp {:?}", t));
            }
        };

        loop {
            match self.lex.peek()? {
                Token::SqurL => {
                    self.lex.next()?;
                    let itable = self.discharge_if_need(sp0, desc)?;
                    desc = match self.exp()? {
                        ExpDesc::String(s) => ExpDesc::IndexField(itable, self.add_const(s)),
                        ExpDesc::Integer(i) if u8::try_from(i).is_ok() => {
                            ExpDesc::IndexInt(itable, u8::try_from(i)?)
                        }
                        key => ExpDesc::Index(itable, self.discharge_top(key)?),
                    };

                    self.lex.expect(Token::SqurR)?;
                }
                Token::Dot => {
                    self.lex.next()?;
                    let name = self.read_name()?;
                    let itable = self.discharge_if_need(sp0, desc)?;
                    desc = ExpDesc::IndexField(itable, self.add_const(name));
                }
                Token::Colon => return Err(anyhow::anyhow!("args")),
                Token::ParL | Token::CurlyL | Token::String(_) => {
                    self.discharge(sp0, desc)?;
                    desc = self.args()?;
                }
                _ => return Ok(desc),
            }
        }
    }

    fn exp_with_ahead(&mut self, ahead: Token) -> anyhow::Result<ExpDesc> {
        match ahead {
            Token::Nil => Ok(ExpDesc::Nil),
            Token::True => Ok(ExpDesc::Boolean(true)),
            Token::False => Ok(ExpDesc::Boolean(false)),
            Token::Integer(i) => Ok(ExpDesc::Integer(i)),
            Token::Float(f) => Ok(ExpDesc::Float(f)),
            Token::String(s) => Ok(ExpDesc::String(s)),
            Token::Function => todo!("Function"),
            Token::CurlyL => self.table_constructor(),
            Token::Sub | Token::Not | Token::BitXor | Token::Len => todo!("unop"),
            Token::Dots => todo!("dots"),
            t => self.prefixexp(t),
        }
    }

    fn exp(&mut self) -> anyhow::Result<ExpDesc> {
        let ahead = self.lex.next()?;
        self.exp_with_ahead(ahead)
    }

    fn table_constructor(&mut self) -> anyhow::Result<ExpDesc> {
        let table = self.sp;
        self.sp += 1;

        let inew = self.byte_codes.len();
        self.byte_codes.push(ByteCode::NewTable(table as u8, 0, 0));

        enum TableEntry {
            Map(
                (
                    fn(u8, u8, u8) -> ByteCode,
                    fn(u8, u8, u8) -> ByteCode,
                    usize,
                ),
            ),
            Array(ExpDesc),
        }

        let mut narray = 0;
        let mut nmap = 0;
        loop {
            let sp0 = self.sp;

            let entry = match self.lex.peek()? {
                Token::CurlyR => {
                    self.lex.next()?;
                    break;
                }
                Token::SqurL => {
                    self.lex.next()?;

                    let key = self.exp()?;
                    self.lex.expect(Token::SqurR)?;
                    self.lex.expect(Token::Assign)?;

                    TableEntry::Map(match key {
                        ExpDesc::Local(i) => (ByteCode::SetTable, ByteCode::SetFieldConst, i),
                        ExpDesc::String(s) => (
                            ByteCode::SetField,
                            ByteCode::SetFieldConst,
                            self.add_const(s),
                        ),
                        ExpDesc::Integer(i) if u8::try_from(i).is_ok() => {
                            (ByteCode::SetInt, ByteCode::SetIntConst, i as usize)
                        }
                        ExpDesc::Nil => return Err(anyhow::anyhow!("nil can not be table key")),
                        ExpDesc::Float(f) if f.is_nan() => {
                            return Err(anyhow::anyhow!("NaN can not be table key"))
                        }
                        _ => (
                            ByteCode::SetTable,
                            ByteCode::SetTableConst,
                            self.discharge_top(key)?,
                        ),
                    })
                }
                Token::Name(_) => {
                    let name = self.read_name()?;
                    if self.lex.peek()? == &Token::Assign {
                        self.lex.next()?;
                        TableEntry::Map((
                            ByteCode::SetField,
                            ByteCode::SetFieldConst,
                            self.add_const(name),
                        ))
                    } else {
                        TableEntry::Array(self.exp_with_ahead(Token::Name(name))?)
                    }
                }
                _ => TableEntry::Array(self.exp()?),
            };

            match entry {
                TableEntry::Map((op, opk, key)) => {
                    let value = self.exp()?;
                    let code = match self.discharge_const(value)? {
                        ConstStack::Const(i) => opk(table as u8, key as u8, i as u8),
                        ConstStack::Stack(i) => op(table as u8, key as u8, i as u8),
                    };
                    self.byte_codes.push(code);

                    nmap += 1;
                    self.sp = sp0;
                }
                TableEntry::Array(desc) => {
                    self.discharge(sp0, desc)?;

                    narray += 1;
                    if narray % 2 == 50 {
                        self.byte_codes.push(ByteCode::SetList(table as u8, 50));
                        self.sp = table + 1;
                    }
                }
            }

            match self.lex.next()? {
                Token::SemiColon | Token::Comma => (),
                Token::CurlyR => break,
                t => return Err(anyhow::anyhow!("invalid table constructor: {:?}", t)),
            }
        }

        if self.sp > table + 1 {
            self.byte_codes.push(ByteCode::SetList(
                table as u8,
                (self.sp - (table + 1)) as u8,
            ));
        }

        self.byte_codes[inew] = ByteCode::NewTable(table as u8, narray, nmap);

        self.sp = table + 1;
        Ok(ExpDesc::Local(table))
    }
}
