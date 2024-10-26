use std::io::Read;

use collections::Vector;
use mem::Arena;
use util::Range;

const LET: [u8; 3] = [b'l', b'e', b't'];
const TYPE: [u8; 4] = [b't', b'y', b'p', b'e'];
const PROCEDURE: [u8; 4] = [b'p', b'r', b'o', b'c'];
const TRUE: [u8; 4] = [b't', b'r', b'u', b'e'];
const FALSE: [u8; 5] = [b'f', b'a', b'l', b's', b'e'];
const MUT: [u8; 3] = [b'm', b'u', b't'];
const CASE: [u8; 4] = [b'c', b'a', b's', b'e'];
const OF: [u8; 2] = [b'o', b'f'];

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Keyword {
    Let,
    Type,
    Procedure,
    True,
    False,
    Mut,
    Of,
    Case,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Symbol {
    Equal,
    ParentesisLeft,
    ParentesisRight,
    Semicolon,
    DoubleColon,
    CurlyBraceLeft,
    CurlyBraceRight,
    Comma,
    Arrow,
    Dot,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Operator {
    Plus,
    Minus,
    Star,
    Slash,

    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Bang,
    BangEqual,
    EqualEqual,
}

#[derive(Debug, Copy, Clone)]
pub enum Token {
    Identifier(Range),
    String(Range),
    Number(Range),
    Keyword(Keyword),
    Operator(Operator),
    Symbol(Symbol),
    Eof,
}

pub struct Lexer {
    pub words: Vector<u8>,
    content: Vector<u8>,
    offset: u32,
    start: u32,

    file: std::fs::File,
}

impl Keyword {
    fn get(string: &[u8]) -> Option<Self> {
        if string.len() == 0 {
            panic!("unreachable");
        }

        match string[0] {
            b't' => {
                if mem::eql(string, &TYPE) {
                    Some(Self::Type)
                } else if mem::eql(string, &TRUE) {
                    Some(Self::True)
                } else {
                    None
                }
            }
            b'l' => {
                if mem::eql(string, &LET) {
                    Some(Self::Let)
                } else {
                    None
                }
            }
            b'o' => {
                if mem::eql(string, &OF) {
                    Some(Self::Of)
                } else {
                    None
                }
            }
            b'c' => {
                if mem::eql(string, &CASE) {
                    Some(Self::Case)
                } else {
                    None
                }
            }
            b'p' => {
                if mem::eql(string, &PROCEDURE) {
                    Some(Self::Procedure)
                } else {
                    None
                }
            }
            b'm' => {
                if mem::eql(string, &MUT) {
                    Some(Self::Mut)
                } else {
                    None
                }
            }
            b'f' => {
                if mem::eql(string, &FALSE) {
                    Some(Self::False)
                } else {
                    None
                }
            }
            _ => None,
        }
    }
}

impl PartialEq for Token {
    fn eq(&self, other: &Token) -> bool {
        match (self, other) {
            (Token::Identifier(_), Token::Identifier(_)) => true,
            (Token::Number(_), Token::Number(_)) => true,
            (Token::Keyword(a), Token::Keyword(b)) => a == b,
            (Token::Operator(a), Token::Operator(b)) => a == b,
            (Token::Symbol(a), Token::Symbol(b)) => a == b,
            (Token::String(_), Token::String(_)) => true,
            (Token::Eof, Token::Eof) => true,
            _ => false,
        }
    }
}

impl Token {
    pub fn iden(&self) -> Range {
        if let Token::Identifier(r) = self {
            *r
        } else {
            panic!("should not happen");
        }
    }

    pub fn keyword(&self) -> Keyword {
        if let Token::Keyword(k) = self {
            *k
        } else {
            panic!("should not happen");
        }
    }

    pub fn operator(&self) -> Operator {
        if let Token::Operator(op) = self {
            *op
        } else {
            panic!("should not happen");
        }
    }

    pub fn number(&self) -> Range {
        if let Token::Number(r) = self {
            *r
        } else {
            panic!("should not happen");
        }
    }

    pub const IDEN: Token = Token::Identifier(Range { start: 0, end: 0 });
    pub const PARENTESISLEFT: Token = Token::Symbol(Symbol::ParentesisLeft);
    pub const PARENTESISRIGHT: Token = Token::Symbol(Symbol::ParentesisRight);
    pub const COMMA: Token = Token::Symbol(Symbol::Comma);
    pub const DOT: Token = Token::Symbol(Symbol::Dot);
    pub const DOUBLECOLON: Token = Token::Symbol(Symbol::DoubleColon);
    pub const BRACELEFT: Token = Token::Symbol(Symbol::CurlyBraceLeft);
    pub const BRACERIGHT: Token = Token::Symbol(Symbol::CurlyBraceRight);
    pub const EQUAL: Token = Token::Symbol(Symbol::Equal);
    pub const ARROW: Token = Token::Symbol(Symbol::Arrow);
    pub const SEMICOLON: Token = Token::Symbol(Symbol::Semicolon);
    pub const EOF: Token = Token::Eof;
    pub const MUT: Token = Token::Keyword(Keyword::Mut);
    pub const OF: Token = Token::Keyword(Keyword::Of);
    pub const TRUE: Token = Token::Keyword(Keyword::True);
    pub const FALSE: Token = Token::Keyword(Keyword::False);
}

impl Lexer {
    pub fn new(path: String, arena: &mut Arena) -> Lexer {
        Lexer {
            words: Vector::new(1024, arena),
            content: Vector::new(1024, arena),
            file: std::fs::File::open(path.as_str()).unwrap(),
            offset: 0,
            start: 0,
        }
    }

    fn assert(&mut self, expected: u8) -> bool {
        if self.content.value(self.offset) == expected {
            self.offset += 1;
            true
        } else {
            false
        }
    }

    fn at_end(&mut self) -> bool {
        if self.offset >= self.content.len() {
            let len = self.offset - self.start;

            self.content.self_extend(0, self.offset, len);
            self.content.set_len(len);

            self.start = 0;
            self.offset = len;

            let read = self.file.read(self.content.buffer()).unwrap() as u32;

            if read == 0 {
                return true;
            }

            self.content.set_len(len + read);
        }

        self.offset >= self.content.len()
    }

    pub fn next(&mut self) -> Token {
        while !self.at_end() {
            match self.content.value(self.offset) {
                b' ' | b'\r' | b'\n' | b'\t' => self.offset += 1,
                _ => break,
            }
        }

        self.start = self.offset;

        if self.at_end() {
            return Token::Eof;
        }

        let c = self.content.value(self.offset);

        self.offset += 1;

        if util::is_digit(c) {
            while !self.at_end() && util::is_digit(self.content.value(self.offset)) {
                self.offset += 1;
            }

            let string = self.content.range(Range::new(self.start, self.offset));

            Token::Number(self.words.extend_range(string))
        } else if util::is_alpha(c) {
            while !self.at_end() && util::is_ascci(self.content.value(self.offset)) {
                self.offset += 1;
            }

            let string = self.content.range(Range::new(self.start, self.offset));

            if let Some(keyword) = Keyword::get(string) {
                Token::Keyword(keyword)
            } else {
                Token::Identifier(self.words.extend_range(string))
            }
        } else {
            match c {
                b'(' => Token::Symbol(Symbol::ParentesisLeft),
                b')' => Token::Symbol(Symbol::ParentesisRight),
                b'{' => Token::Symbol(Symbol::CurlyBraceLeft),
                b'}' => Token::Symbol(Symbol::CurlyBraceRight),
                b';' => Token::Symbol(Symbol::Semicolon),
                b':' => Token::Symbol(Symbol::DoubleColon),
                b',' => Token::Symbol(Symbol::Comma),
                b'.' => Token::Symbol(Symbol::Dot),

                b'-' => Token::Operator(Operator::Minus),
                b'+' => Token::Operator(Operator::Plus),
                b'*' => Token::Operator(Operator::Star),
                b'/' => Token::Operator(Operator::Slash),

                b'!' => {
                    if self.assert(b'=') {
                        Token::Operator(Operator::BangEqual)
                    } else {
                        Token::Operator(Operator::Bang)
                    }
                }

                b'=' => {
                    if self.assert(b'=') {
                        Token::Operator(Operator::EqualEqual)
                    } else if self.assert(b'>') {
                        Token::Symbol(Symbol::Arrow)
                    } else {
                        Token::Symbol(Symbol::Equal)
                    }
                }

                b'>' => {
                    if self.assert(b'=') {
                        Token::Operator(Operator::GreaterEqual)
                    } else {
                        Token::Operator(Operator::Greater)
                    }
                }

                b'<' => {
                    if self.assert(b'=') {
                        Token::Operator(Operator::LessEqual)
                    } else {
                        Token::Operator(Operator::Less)
                    }
                }

                b'"' => {
                    self.offset += 1;

                    while !self.at_end() && self.content.value(self.offset) != b'"' {
                        self.offset += 1;
                    }

                    if self.at_end() {
                        return Token::Eof;
                    }

                    self.offset += 1;

                    let string = self
                        .content
                        .range(Range::new(self.start + 1, self.offset - 1));

                    Token::String(self.words.extend_range(string))
                }
                _ => todo!(),
            }
        }
    }
}
