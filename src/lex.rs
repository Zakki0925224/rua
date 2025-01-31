use std::{
    io::{Bytes, Read},
    iter::Peekable,
    mem,
};

#[derive(Debug, PartialEq)]
pub enum Token {
    // keywords
    And,
    Break,
    Do,
    Else,
    Elseif,
    End,
    False,
    For,
    Function,
    Goto,
    If,
    In,
    Local,
    Nil,
    Not,
    Or,
    Repeat,
    Return,
    Then,
    True,
    Until,
    While,

    Add,       // +
    Sub,       // -
    Mul,       // *
    Div,       // /
    Mod,       // %
    Pow,       // ^
    Len,       // #
    BitAnd,    // &
    BitXor,    // ~
    BitOr,     // |
    ShiftL,    // <<
    ShiftR,    // >>
    Idiv,      // //
    Equal,     // ==
    NotEq,     // ~=
    LesEq,     // <=
    GreEq,     // >=
    Less,      // <
    Greater,   // >
    Assign,    // =
    ParL,      // (
    ParR,      // )
    CurlyL,    // {
    CurlyR,    // }
    SqurL,     // [
    SqurR,     // ]
    DoubColon, // ::
    SemiColon, // ;
    Colon,     // :
    Comma,     // ,
    Dot,       // .
    Concat,    // ..
    Dots,      // ...

    // constant values
    Integer(i64),
    Float(f64),
    String(Vec<u8>),

    // name of variables or table keys
    Name(String),

    // end
    Eos,
}

#[derive(Debug)]
pub struct Lex<R: Read> {
    input: Peekable<Bytes<R>>,
    ahead: Token,
}

impl<R: Read> Lex<R> {
    pub fn new(input: R) -> Self {
        Self {
            input: input.bytes().peekable(),
            ahead: Token::Eos,
        }
    }

    pub fn next(&mut self) -> anyhow::Result<Token> {
        if self.ahead == Token::Eos {
            self.do_next()
        } else {
            Ok(mem::replace(&mut self.ahead, Token::Eos))
        }
    }

    pub fn peek(&mut self) -> anyhow::Result<&Token> {
        if self.ahead == Token::Eos {
            self.ahead = self.do_next()?;
        }

        Ok(&self.ahead)
    }

    pub fn expect(&mut self, t: Token) -> anyhow::Result<()> {
        let next = self.next()?;

        if next != t {
            return Err(anyhow::anyhow!("expect {:?}, got {:?}", t, next));
        }

        Ok(())
    }

    fn do_next(&mut self) -> anyhow::Result<Token> {
        if let Some(byte) = self.next_byte() {
            match byte {
                b'\n' | b'\r' | b'\t' | b' ' => self.do_next(),
                b'+' => Ok(Token::Add),
                b'*' => Ok(Token::Mul),
                b'%' => Ok(Token::Mod),
                b'^' => Ok(Token::Pow),
                b'#' => Ok(Token::Len),
                b'&' => Ok(Token::BitAnd),
                b'|' => Ok(Token::BitOr),
                b'(' => Ok(Token::ParL),
                b')' => Ok(Token::ParR),
                b'{' => Ok(Token::CurlyL),
                b'}' => Ok(Token::CurlyR),
                b'[' => Ok(Token::SqurL),
                b']' => Ok(Token::SqurR),
                b';' => Ok(Token::SemiColon),
                b',' => Ok(Token::Comma),
                b'/' => self.check_ahead(b'/', Token::Idiv, Token::Div),
                b'=' => self.check_ahead(b'=', Token::Equal, Token::Assign),
                b'~' => self.check_ahead(b'=', Token::NotEq, Token::BitXor),
                b':' => self.check_ahead(b':', Token::DoubColon, Token::Colon),
                b'<' => self.check_ahead2(b'=', Token::LesEq, b'<', Token::ShiftL, Token::Less),
                b'>' => self.check_ahead2(b'=', Token::GreEq, b'>', Token::ShiftR, Token::Greater),
                b'\'' | b'"' => self.read_string(byte),
                b'.' => match self.peek_byte()? {
                    b'.' => {
                        self.next_byte();
                        if self.peek_byte()? == b'.' {
                            self.next_byte();
                            Ok(Token::Dots)
                        } else {
                            Ok(Token::Concat)
                        }
                    }
                    b'0'..=b'9' => self.read_number_fraction(0),
                    _ => Ok(Token::Dot),
                },
                b'-' => {
                    if self.peek_byte()? == b'-' {
                        self.next_byte();
                        self.read_comment()?;
                        self.do_next()
                    } else {
                        Ok(Token::Sub)
                    }
                }
                b'0'..=b'9' => self.read_number(byte),
                b'A'..=b'Z' | b'a'..=b'z' | b'_' => self.read_name(byte),
                _ => Err(anyhow::anyhow!("invalid character: {}", byte)),
            }
        } else {
            Ok(Token::Eos)
        }
    }

    fn peek_byte(&mut self) -> anyhow::Result<u8> {
        match self.input.peek() {
            Some(Ok(byte)) => Ok(*byte),
            Some(_) => Err(anyhow::anyhow!("lex peek error")),
            None => Ok(b'\0'),
        }
    }

    fn next_byte(&mut self) -> Option<u8> {
        self.input.next().and_then(|r| Some(r.unwrap()))
    }

    fn check_ahead(&mut self, ahead: u8, long: Token, short: Token) -> anyhow::Result<Token> {
        if self.peek_byte()? == ahead {
            self.next_byte();
            Ok(long)
        } else {
            Ok(short)
        }
    }

    fn check_ahead2(
        &mut self,
        ahead1: u8,
        long1: Token,
        ahead2: u8,
        long2: Token,
        short: Token,
    ) -> anyhow::Result<Token> {
        let byte = self.peek_byte()?;
        if byte == ahead1 {
            self.next_byte();
            Ok(long1)
        } else if byte == ahead2 {
            self.next_byte();
            Ok(long2)
        } else {
            Ok(short)
        }
    }

    fn read_number(&mut self, first: u8) -> anyhow::Result<Token> {
        if first == b'0' {
            let second = self.peek_byte()?;
            if second == b'x' || second == b'X' {
                return self.read_heximal();
            }
        }

        let mut n = (first - b'0') as i64;
        loop {
            let byte = self.peek_byte()?;
            if let Some(d) = (byte as char).to_digit(10) {
                self.next_byte();
                n = n * 10 + d as i64;
            } else if byte == b'.' {
                return self.read_number_fraction(n);
            } else if byte == b'e' || byte == b'E' {
                return self.read_number_exp(n as f64);
            } else {
                break;
            }
        }

        let fch = self.peek_byte()?;
        if (fch as char).is_alphabetic() || fch == b'.' {
            return Err(anyhow::anyhow!("malformat number"));
        }

        Ok(Token::Integer(n))
    }

    fn read_number_fraction(&mut self, i: i64) -> anyhow::Result<Token> {
        self.next_byte();

        let mut n: i64 = 0;
        let mut x: f64 = 1.0;

        loop {
            let byte = self.peek_byte()?;
            if let Some(d) = (byte as char).to_digit(10) {
                self.next_byte();
                n = n * 10 + d as i64;
                x *= 10.0;
            } else {
                break;
            }
        }

        Ok(Token::Float(i as f64 + n as f64 / x))
    }

    fn read_number_exp(&mut self, _: f64) -> anyhow::Result<Token> {
        self.next_byte();
        todo!("lex number exp")
    }

    fn read_heximal(&mut self) -> anyhow::Result<Token> {
        self.next_byte();
        todo!("lex heximal")
    }

    fn read_string(&mut self, quote: u8) -> anyhow::Result<Token> {
        let mut s = Vec::new();

        loop {
            match self
                .next_byte()
                .ok_or(anyhow::anyhow!("unfinished string"))?
            {
                b'\n' => return Err(anyhow::anyhow!("unfinished string")),
                b'\\' => s.push(self.read_escape()?),
                byte if byte == quote => break,
                byte => s.push(byte),
            }
        }

        Ok(Token::String(s))
    }

    fn read_escape(&mut self) -> anyhow::Result<u8> {
        match self.next_byte().ok_or(anyhow::anyhow!("string escape"))? {
            b'a' => Ok(0x07),
            b'b' => Ok(0x08),
            b'f' => Ok(0x0c),
            b'v' => Ok(0x0b),
            b'n' => Ok(b'\n'),
            b'r' => Ok(b'\r'),
            b't' => Ok(b'\t'),
            b'\\' => Ok(b'\\'),
            b'"' => Ok(b'"'),
            b'\'' => Ok(b'\''),
            b'x' => {
                let n1 = (self.next_byte().unwrap() as char).to_digit(16).unwrap();
                let n2 = (self.next_byte().unwrap() as char).to_digit(16).unwrap();
                Ok((n1 * 16 + n2) as u8)
            }
            ch @ b'0'..b'9' => {
                let mut n = (ch as char).to_digit(10).unwrap();
                if let Some(d) = (self.peek_byte()? as char).to_digit(10) {
                    self.next_byte();
                    n = n * 10 + d;
                    if let Some(d) = (self.peek_byte()? as char).to_digit(10) {
                        self.next_byte();
                        n = n * 10 + d;
                    }
                }
                Ok(u8::try_from(n)?)
            }
            _ => Err(anyhow::anyhow!("invalid string escape")),
        }
    }

    fn read_name(&mut self, first: u8) -> anyhow::Result<Token> {
        let mut s = String::new();
        s.push(first as char);

        loop {
            let ch = self.peek_byte()? as char;
            if ch.is_alphanumeric() || ch == '_' {
                self.next_byte();
                s.push(ch);
            } else {
                break;
            }
        }

        match &s as &str {
            "and" => Ok(Token::And),
            "break" => Ok(Token::Break),
            "do" => Ok(Token::Do),
            "else" => Ok(Token::Else),
            "elseif" => Ok(Token::Elseif),
            "end" => Ok(Token::End),
            "false" => Ok(Token::False),
            "for" => Ok(Token::For),
            "function" => Ok(Token::Function),
            "goto" => Ok(Token::Goto),
            "if" => Ok(Token::If),
            "in" => Ok(Token::In),
            "local" => Ok(Token::Local),
            "nil" => Ok(Token::Nil),
            "not" => Ok(Token::Not),
            "or" => Ok(Token::Or),
            "repeat" => Ok(Token::Repeat),
            "return" => Ok(Token::Return),
            "then" => Ok(Token::Then),
            "true" => Ok(Token::True),
            "until" => Ok(Token::Until),
            "while" => Ok(Token::While),
            _ => Ok(Token::Name(s)),
        }
    }

    fn read_comment(&mut self) -> anyhow::Result<()> {
        match self.next_byte() {
            None => Ok(()),
            Some(b'[') => todo!("long comment"),
            Some(_) => {
                while let Some(byte) = self.next_byte() {
                    if byte == b'\n' {
                        break;
                    }
                }

                Ok(())
            }
        }
    }
}
