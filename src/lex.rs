use std::{
    fs::File,
    io::{Read, Seek, SeekFrom},
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
    String(String),

    // name of variables or table keys
    Name(String),

    // end
    Eos,
}

#[derive(Debug)]
pub struct Lex {
    input: File,
    ahead: Token,
}

impl Lex {
    pub fn new(input: File) -> Self {
        Self {
            input,
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

    fn do_next(&mut self) -> anyhow::Result<Token> {
        let ch = self.read_char()?;
        match ch {
            '\n' | '\r' | '\t' | ' ' => self.do_next(),
            '+' => Ok(Token::Add),
            '*' => Ok(Token::Mul),
            '%' => Ok(Token::Mod),
            '^' => Ok(Token::Pow),
            '#' => Ok(Token::Len),
            '&' => Ok(Token::BitAnd),
            '|' => Ok(Token::BitOr),
            '(' => Ok(Token::ParL),
            ')' => Ok(Token::ParR),
            '{' => Ok(Token::CurlyL),
            '}' => Ok(Token::CurlyR),
            '[' => Ok(Token::SqurL),
            ']' => Ok(Token::SqurR),
            ';' => Ok(Token::SemiColon),
            ',' => Ok(Token::Comma),
            '/' => self.check_ahead('/', Token::Idiv, Token::Div),
            '=' => self.check_ahead('=', Token::Equal, Token::Assign),
            '~' => self.check_ahead('=', Token::NotEq, Token::BitXor),
            ':' => self.check_ahead(':', Token::DoubColon, Token::Colon),
            '<' => self.check_ahead2('=', Token::LesEq, '<', Token::ShiftL, Token::Less),
            '>' => self.check_ahead2('=', Token::GreEq, '>', Token::ShiftR, Token::Greater),
            '\'' | '"' => self.read_string(ch),
            '.' => match self.read_char()? {
                '.' => {
                    if self.read_char()? == '.' {
                        Ok(Token::Dots)
                    } else {
                        self.putback_char()?;
                        Ok(Token::Concat)
                    }
                }
                '0'..='9' => {
                    self.putback_char()?;
                    self.read_number_fraction(0)
                }
                _ => {
                    self.putback_char()?;
                    Ok(Token::Dot)
                }
            },
            '-' => {
                if self.read_char()? == '-' {
                    self.read_comment()?;
                    self.do_next()
                } else {
                    self.putback_char()?;
                    Ok(Token::Sub)
                }
            }
            '0'..='9' => self.read_number(ch),
            'A'..='Z' | 'a'..='z' | '_' => self.read_name(ch),
            '\0' => Ok(Token::Eos),
            _ => Err(anyhow::anyhow!("invalid character: {}", ch)),
        }
    }

    fn read_char(&mut self) -> anyhow::Result<char> {
        let mut buf = [0; 1];
        self.input.read(&mut buf)?;
        Ok(buf[0] as char)
    }

    fn putback_char(&mut self) -> anyhow::Result<()> {
        self.input.seek(SeekFrom::Current(-1))?;
        Ok(())
    }

    fn check_ahead(&mut self, ahead: char, long: Token, short: Token) -> anyhow::Result<Token> {
        if self.read_char()? == ahead {
            Ok(long)
        } else {
            self.putback_char()?;
            Ok(short)
        }
    }

    fn check_ahead2(
        &mut self,
        ahead1: char,
        long1: Token,
        ahead2: char,
        long2: Token,
        short: Token,
    ) -> anyhow::Result<Token> {
        let ch = self.read_char()?;
        if ch == ahead1 {
            Ok(long1)
        } else if ch == ahead2 {
            Ok(long2)
        } else {
            self.putback_char()?;
            Ok(short)
        }
    }

    fn read_number(&mut self, first: char) -> anyhow::Result<Token> {
        if first == '0' {
            let second = self.read_char()?;
            if second == 'x' || second == 'X' {
                return self.read_heximal();
            }
            self.putback_char()?;
        }

        let mut n = first.to_digit(10).unwrap() as i64;
        loop {
            let ch = self.read_char()?;
            if let Some(d) = ch.to_digit(10) {
                n = n * 10 + d as i64;
            } else if ch == '.' {
                return self.read_number_fraction(n);
            } else if ch == 'e' || ch == 'E' {
                return self.read_number_exp(n as f64);
            } else {
                self.putback_char()?;
                break;
            }
        }

        let fch = self.read_char()?;
        if fch.is_alphabetic() || fch == '.' {
            return Err(anyhow::anyhow!("malformat number"));
        } else {
            self.putback_char()?;
        }

        Ok(Token::Integer(n))
    }

    fn read_number_fraction(&mut self, i: i64) -> anyhow::Result<Token> {
        let mut n: i64 = 0;
        let mut x: f64 = 1.0;

        loop {
            let ch = self.read_char()?;
            if let Some(d) = ch.to_digit(10) {
                n = n * 10 + d as i64;
                x *= 10.0;
            } else {
                self.putback_char()?;
                break;
            }
        }

        Ok(Token::Float(i as f64 + n as f64 / x))
    }

    fn read_number_exp(&mut self, _: f64) -> anyhow::Result<Token> {
        todo!()
    }

    fn read_heximal(&mut self) -> anyhow::Result<Token> {
        todo!()
    }

    fn read_string(&mut self, quote: char) -> anyhow::Result<Token> {
        let mut s = String::new();

        loop {
            match self.read_char()? {
                '\n' | '\0' => return Err(anyhow::anyhow!("unfinished string")),
                '\\' => todo!("escape"),
                ch if ch == quote => break,
                ch => s.push(ch),
            }
        }

        Ok(Token::String(s))
    }

    fn read_name(&mut self, first: char) -> anyhow::Result<Token> {
        let mut s = first.to_string();

        loop {
            let ch = self.read_char()?;
            if ch.is_alphanumeric() || ch == '_' {
                s.push(ch);
            } else {
                self.putback_char()?;
                break;
            }
        }

        match s.as_str() {
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
        match self.read_char()? {
            '[' => todo!("long comment"),
            _ => loop {
                let ch = self.read_char()?;
                if ch == '\n' || ch == '\0' {
                    break;
                }
            },
        }

        Ok(())
    }
}
