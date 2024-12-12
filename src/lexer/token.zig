const util = @import("util");
const mem = @import("mem");

pub const Keyword = enum(u8) {
    let,
    @"type",
    proc,
    mut,

    pub fn print(self: Keyword, formater: util.Formater) void {
        const zone = util.tracy.initZone(@src(), .{.name = "Keyword::print"});
        defer zone.deinit();

        switch (self) {
            .let => formater("Keyword::Let", .{}),
            .@"type" => formater("Keyword::Type", .{}),
            .proc => formater("Keyword::Procedure", .{}),
            .mut => formater("Keyword::Mut", .{}),
        }
    }

    pub fn from_string(string: []const u8) ?Keyword {
        const zone = util.tracy.initZone(@src(), .{.name = "Keyword::from_string"});
        defer zone.deinit();

        inline for (@typeInfo(Keyword).@"enum".fields) |field| {
            if (mem.equal(u8, string, field.name)) {
                return @enumFromInt(field.value);
            }
        }

        return null;
    }
};

pub const Symbol = enum(u8) {
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
        const zone = util.tracy.initZone(@src(), .{.name = "Symbol::print"});
        defer zone.deinit();

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

pub const Operator = enum(u8) {
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
        const zone = util.tracy.initZone(@src(), .{.name = "Operator::print"});
        defer zone.deinit();

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

pub const TokenKind = enum(u8) {
    Eof,
    Identifier,
    String,
    Number,
    Keyword,
    Symbol,
    Operator,
};

pub const Token = union(TokenKind) {
    Eof,
    Identifier: util.Index,
    String: util.Index,
    Number: util.Index,
    Keyword: Keyword,
    Symbol: Symbol,
    Operator: Operator,

    pub const IDEN: Token = Token{ .Identifier = 0 };
    pub const NUMBER: Token = Token{ .Number = 0 };
    pub const STRING: Token = Token{ .String = 0 };

    pub const PARENTESISLEFT: Token = Token{ .Symbol = Symbol.ParentesisLeft };
    pub const PARENTESISRIGHT: Token = Token{ .Symbol = Symbol.ParentesisRight };
    pub const COMMA: Token = Token{ .Symbol = Symbol.Comma };
    pub const DOT: Token = Token{ .Symbol = Symbol.Dot };
    pub const DOUBLECOLON: Token = Token{ .Symbol = Symbol.DoubleColon };
    pub const BRACELEFT: Token = Token{ .Symbol = Symbol.CurlyBracketLeft };
    pub const BRACERIGHT: Token = Token{ .Symbol = Symbol.CurlyBracketRight };
    pub const EQUAL: Token = Token{ .Symbol = Symbol.Equal };
    pub const ARROW: Token = Token{ .Symbol = Symbol.Arrow };
    pub const SEMICOLON: Token = Token{ .Symbol = Symbol.Semicolon };

    pub const BANG: Token = Token{ .Operator = Operator.Bang };
    pub const BANGEQUAL: Token = Token{ .Operator = Operator.BangEqual };
    pub const EQUALEQUAL: Token = Token{ .Operator = Operator.EqualEqual };
    pub const PLUS: Token = Token{ .Operator = Operator.Plus };
    pub const MINUS: Token = Token{ .Operator = Operator.Minus };
    pub const STAR: Token = Token{ .Operator = Operator.Star };
    pub const SLASH: Token = Token{ .Operator = Operator.Slash };
    pub const GREATER: Token = Token{ .Operator = Operator.Greater };
    pub const GREATEREQUAL: Token = Token{ .Operator = Operator.GreaterEqual };
    pub const LESS: Token = Token{ .Operator = Operator.Less };
    pub const LESSEQUAL: Token = Token{ .Operator = Operator.LessEqual };

    pub const TYPE: Token = Token{ .Keyword = Keyword.@"type" };
    pub const PROC: Token = Token{ .Keyword = Keyword.procedure };
    pub const LET: Token = Token{ .Keyword = Keyword.let };
    pub const MUT: Token = Token{ .Keyword = Keyword.mut };

    pub const EOF: Token = Token.Eof;

    pub fn print(self: Token, formater: util.Formater) void {
        const zone = util.tracy.initZone(@src(), .{.name = "Token::print"});
        defer zone.deinit();

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
        const zone = util.tracy.initZone(@src(), .{.name = "Token::eql"});
        defer zone.deinit();

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

fn variant_offset(comptime t: TokenKind) usize {
    const zone = util.tracy.initZone(@src(), .{.name = "variant_offset"});
    defer zone.deinit();

    comptime var len: usize = 0;
    const p = @intFromEnum(t);
    const fields = @typeInfo(Token).@"union".fields;

    inline for (0..p) |index| {
        const field = @typeInfo(fields[index].type);

        if (field == .@"enum") {
            const l = field.@"enum".fields.len;

            if (l > 0) {
                len += l - 1;
            }
        }
    }

    return len;
}

pub fn identity_int(_: Token, i: usize) usize {
    const zone = util.tracy.initZone(@src(), .{.name = "identity_int"});
    defer zone.deinit();

    return i;
}

pub fn keyword_int(t: Token, i: usize) usize {
    const zone = util.tracy.initZone(@src(), .{.name = "keyword_int"});
    defer zone.deinit();

    const v: usize = @intFromEnum(t.Keyword) + variant_offset(TokenKind.Keyword);
    return i + v;
}

pub fn operator_int(t: Token, i: usize) usize {
    const zone = util.tracy.initZone(@src(), .{.name = "operator_int"});
    defer zone.deinit();

    const v: usize = @intFromEnum(t.Operator) + variant_offset(TokenKind.Operator);
    return i + v;
}

pub fn symbol_int(t: Token, i: usize) usize {
    const zone = util.tracy.initZone(@src(), .{.name = "symbol_int"});
    defer zone.deinit();

    const v: usize = @intFromEnum(t.Symbol) + variant_offset(TokenKind.Symbol);
    return i + v;
}

