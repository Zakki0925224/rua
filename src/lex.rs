use std::{
    fs::File,
    io::{Read, Seek, SeekFrom},
};

#[derive(Debug)]
pub enum Token {
    Name(String),
    String(String),
    Eos,
}

#[derive(Debug)]
pub struct Lex {
    input: File,
}

impl Lex {
    pub fn new(input: File) -> Self {
        Self { input }
    }

    pub fn next(&mut self) -> anyhow::Result<Token> {
        let ch = self.read_char()?;

        match ch {
            ' ' | '\r' | '\n' | '\t' => self.next(),
            '\0' => Ok(Token::Eos),

            '"' => {
                let mut s = String::new();
                loop {
                    match self.read_char()? {
                        '\0' => return Err(anyhow::anyhow!("unfinished literal string")),
                        '"' => break,
                        ch => s.push(ch),
                    }
                }
                Ok(Token::String(s))
            }

            'A'..='Z' | 'a'..='z' | '_' => {
                let mut name = String::new();
                name.push(ch);

                loop {
                    match self.read_char()? {
                        '\0' => break,
                        '_' => name.push('_'),
                        ch if ch.is_alphanumeric() => name.push(ch),
                        _ => {
                            self.input.seek(SeekFrom::Current(-1))?;
                            break;
                        }
                    }
                }

                Ok(Token::Name(name))
            }

            _ => Err(anyhow::anyhow!("unexpected char: {}", ch)),
        }
    }

    fn read_char(&mut self) -> anyhow::Result<char> {
        let mut buf: [u8; 1] = [0];

        if self.input.read(&mut buf)? == 1 {
            Ok(buf[0] as char)
        } else {
            Ok('\0')
        }
    }
}
