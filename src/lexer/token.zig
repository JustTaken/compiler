const util = @import("util");
const mem = @import("mem");

pub const Keyword = enum {
    Let,
    Type,
    Procedure,
    Mut,

    pub fn print(self: Keyword, formater: util.Formater) void {
        switch (self) {
            .Let => formater("Keyword::Let", .{}),
            .Type => formater("Keyword::Type", .{}),
            .Procedure => formater("Keyword::Procedure", .{}),
            .Mut => formater("Keyword::Mut", .{}),
        }
    }

    pub fn from_string(string: []const u8) ?Keyword {
        if (string.len <= 1) return null;

        switch (string[0]) {
            't' => {
                if (mem.equal(u8, string[1..], "ype")) return .Type else return null;
            },
            'p' => {
                if (mem.equal(u8, string[1..], "roc")) return .Procedure else return null;
            },
            'm' => {
                if (mem.equal(u8, string[1..], "ut")) return .Mut else return null;
            },
            'l' => {
                if (mem.equal(u8, string[1..], "et")) return .Let else return null;
            },
            else => return null,
        }
    }
};

pub const Symbol = enum {
    Equal,
    ParentesisLeft,
    ParentesisRight,
    Semicolon,
    DoubleColon,
    CurlyBracketLeft,
    CurlyBracketRight,
    Comma,
    Arrow,
    Dot,

    pub fn print(self: Symbol, formater: util.Formater) void {
        switch (self) {
            .Equal => formater("Symbol::Equal", .{}),
            .ParentesisLeft => formater("Symbol::ParentesisLeft", .{}),
            .ParentesisRight => formater("Symbol::ParentesisRight", .{}),
            .Semicolon => formater("Symbol::Semicolon", .{}),
            .DoubleColon => formater("Symbol::DoubleColon", .{}),
            .CurlyBracketLeft => formater("Symbol::CurlyBracketLeft", .{}),
            .CurlyBracketRight => formater("Symbol::CurlyBracketRight", .{}),
            .Comma => formater("Symbol::Comma", .{}),
            .Arrow => formater("Symbol::Arrow", .{}),
            .Dot => formater("Symbol::Dot", .{}),
        }
    }
};

pub const Operator = enum {
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

    pub fn print(self: Operator, formater: util.Formater) void {
        switch (self) {
            .Plus => formater("Operator::Plus", .{}),
            .Minus => formater("Operator::Minus", .{}),
            .Star => formater("Operator::Star", .{}),
            .Slash => formater("Operator::Slash", .{}),
            .Greater => formater("Operator::Greater", .{}),
            .GreaterEqual => formater("Operator::GreaterEqual", .{}),
            .Less => formater("Operator::Less", .{}),
            .LessEqual => formater("Operator::LessEqual", .{}),
            .Bang => formater("Operator::Bang", .{}),
            .BangEqual => formater("Operator::BangEqual", .{}),
            .EqualEqual => formater("Operator::EqualEqual", .{}),
        }
    }
};

const TokenKind = enum {
    Identifier,
    String,
    Number,
    Keyword,
    Operator,
    Symbol,
    Eof,
};

pub const Token = union(TokenKind) {
    Identifier: []const u8,
    String: []const u8,
    Number: usize,
    Keyword: Keyword,
    Operator: Operator,
    Symbol: Symbol,

    Eof,

    pub const IDEN: Token = Token{ .Identifier = "" };
    pub const NUMBER: Token = Token{ .Number = 0 };
    pub const PARENTESISLEFT: Token = Token{ .Symbol = Symbol.ParentesisLeft };
    pub const PARENTESISRIGHT: Token = Token{ .Symbol = Symbol.ParentesisRight };
    pub const COMMA: Token = Token{ .Symbol = Symbol.Comma };
    pub const DOT: Token = Token{ .Symbol = Symbol.Dot };
    pub const DOUBLECOLON: Token = Token{ .Symbol = Symbol.DoubleColon };
    pub const BRACELEFT: Token = Token{ .Symbol = Symbol.CurlyBracketLeft };
    pub const BRACERIGHT: Token = Token{ .Symbol = Symbol.CurlyBracketRight };
    pub const EQUAL: Token = Token{ .Symbol = Symbol.Equal };
    pub const ARROW: Token = Token{ .Symbol = Symbol.Arrow };
    pub const PLUS: Token = Token{ .Operator = Operator.Plus };
    pub const SEMICOLON: Token = Token{ .Symbol = Symbol.Semicolon };
    pub const TYPE: Token = Token{ .Keyword = Keyword.Type };
    pub const PROC: Token = Token{ .Keyword = Keyword.Procedure };
    pub const LET: Token = Token{ .Keyword = Keyword.Let };
    pub const MUT: Token = Token{ .Keyword = Keyword.Mut };
    pub const OF: Token = Token{ .Keyword = Keyword.Of };
    pub const TRUE: Token = Token{ .Keyword = Keyword.True };
    pub const FALSE: Token = Token{ .Keyword = Keyword.False };
    pub const EOF: Token = Token.Eof;

    pub fn print(self: Token, formater: util.Formater) void {
        switch (self) {
            .Eof => formater("Token::Eof", .{}),
            .Identifier => formater("Token::Identifier", .{}),
            .String => formater("Token::String", .{}),
            .Number => formater("Token::Number", .{}),
            .Keyword => |keyword| formater("Token::Keyword({})", .{keyword}),
            .Operator => |operator| formater("Token::Operator({})", .{operator}),
            .Symbol => |symbol| formater("Token::Symbol({})", .{symbol}),
        }
    }

    pub fn eql(self: Token, other: Token) bool {
        switch (self) {
            .Eof => return @as(TokenKind, other) == .Eof,
            .Identifier => return @as(TokenKind, other) == .Identifier,
            .String => return @as(TokenKind, other) == .String,
            .Number => return @as(TokenKind, other) == .Number,
            .Keyword => {
                if (@as(TokenKind, other) != .Keyword) {
                    return false;
                }

                return self.Keyword == other.Keyword;
            },
            .Operator => {
                if (@as(TokenKind, other) != .Operator) {
                    return false;
                }

                return self.Operator == other.Operator;
            },
            .Symbol => {
                if (@as(TokenKind, other) != .Symbol) {
                    return false;
                }

                return self.Symbol == other.Symbol;
            },
        }
    }
};

