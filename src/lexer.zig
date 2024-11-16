const collections = @import("collections");
const util = @import("util");
const mem = @import("mem");

const Arena = mem.Arena;
const File = util.File;
const Range = util.Range;
const String = collections.String;
const Stream = collections.Stream;

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
    fn from_string(string: []const u8) ?Keyword {
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
    Number: []const u8,
    Keyword: Keyword,
    Operator: Operator,
    Symbol: Symbol,

    Eof,

    pub const IDEN: Token = Token{ .Identifier = "" };
    pub const NUMBER: Token = Token{ .Number = "" };
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

pub const Lexer = struct {
    words: String,
    content: String,

    previous: Token,
    current: Token,

    offset: u32,
    start: u32,

    general_offset: u32,

    stream: Stream,
    arena: *Arena,

    pub fn new(stream: Stream, allocator: *Arena) error{OutOfMemory}!Lexer {
        var self = Lexer{
            .stream = stream,
            .offset = 0,
            .start = 0,
            .general_offset = 0,
            .previous = Token.EOF,
            .current = Token.EOF,
            .arena = try allocator.child("Lexer", @sizeOf(Arena) + (mem.PAGE_SIZE >> 1)),
            .words = undefined,
            .content = undefined,
        };

        errdefer self.arena.deinit();

        self.words = try String.new(1024, self.arena);
        errdefer self.words.deinit(self.arena);

        self.content = try String.new(1024, self.arena);
        errdefer self.content.deinit(self.arena);

        self.advance();

        return self;
    }

    fn assert(self: *Lexer, expected: u8) bool {
        if (self.content.value(self.offset) catch unreachable == expected) {
            self.offset += 1;

            return true;
        }

        return false;
    }

    fn at_end(self: *Lexer) bool {
        if (self.offset >= self.content.len) {
            const len = self.offset - self.start;
            const buffer = self.content.items;

            mem.copy(u8, buffer[self.start..self.offset], buffer[0..len]);

            self.start = 0;
            self.general_offset += self.offset;
            self.offset = len;
            self.content.set_len(len) catch unreachable;
            self.stream.read(&self.content) catch |e| switch (e) {
                error.AtEnd => return true,
                error.OutOfBounds => @panic("TODO"),
            };
        }

        return self.offset >= self.content.len;
    }

    pub fn advance(self: *Lexer) void {
        self.previous = self.current;

        while (!self.at_end()) {
            switch (self.content.value(self.offset) catch unreachable) {
                ' ', '\r', '\n', '\t' => self.offset += 1,
                else => break,
            }
        }

        self.start = self.offset;

        if (self.at_end()) {
            self.current = Token.EOF;
        } else {
            const c = self.content.value(self.offset) catch unreachable;
            self.offset += 1;

            if (util.is_digit(c)) {
                while (!self.at_end() and util.is_digit(self.content.value(self.offset) catch unreachable)) {
                    self.offset += 1;
                }

                const string = self.content.range(Range.new(self.start, self.offset)) catch unreachable;
                const range = self.words.extend_range(string) catch @panic("TODO");

                self.current = Token{ .Number = self.words.range(range) catch unreachable };
            } else if (util.is_alpha(c)) {
                while (!self.at_end() and util.is_ascci(self.content.value(self.offset) catch unreachable)) {
                    self.offset += 1;
                }

                const string = self.content.range(Range.new(self.start, self.offset)) catch unreachable;

                if (Keyword.from_string(string)) |keyword| {
                    self.current = Token{ .Keyword = keyword };
                } else {
                    const range = self.words.extend_range(string) catch @panic("TODO");
                    self.current = Token{ .Identifier = self.words.range(range) catch unreachable };
                }
            } else {
                switch (c) {
                    '(' => self.current = Token{ .Symbol = Symbol.ParentesisLeft },
                    ')' => self.current = Token{ .Symbol = Symbol.ParentesisRight },
                    '{' => self.current = Token{ .Symbol = Symbol.CurlyBracketLeft },
                    '}' => self.current = Token{ .Symbol = Symbol.CurlyBracketRight },
                    ';' => self.current = Token{ .Symbol = Symbol.Semicolon },
                    ':' => self.current = Token{ .Symbol = Symbol.DoubleColon },
                    ',' => self.current = Token{ .Symbol = Symbol.Comma },
                    '.' => self.current = Token{ .Symbol = Symbol.Dot },
                    '-' => self.current = Token{ .Operator = Operator.Minus },
                    '+' => self.current = Token{ .Operator = Operator.Plus },
                    '*' => self.current = Token{ .Operator = Operator.Star },
                    '/' => self.current = Token{ .Operator = Operator.Slash },
                    '!' => {
                        if (self.assert('=')) {
                            self.current = Token{ .Operator = Operator.BangEqual };
                        } else {
                            self.current = Token{ .Operator = Operator.Bang };
                        }
                    },
                    '=' => {
                        if (self.assert('=')) {
                            self.current = Token{ .Operator = Operator.EqualEqual };
                        } else if (self.assert('>')) {
                            self.current = Token{ .Symbol = Symbol.Arrow };
                        } else {
                            self.current = Token{ .Symbol = Symbol.Equal };
                        }
                    },
                    '>' => {
                        if (self.assert('=')) {
                            self.current = Token{ .Operator = Operator.GreaterEqual };
                        } else {
                            self.current = Token{ .Operator = Operator.Greater };
                        }
                    },
                    '<' => {
                        if (self.assert('=')) {
                            self.current = Token{ .Operator = Operator.LessEqual };
                        } else {
                            self.current = Token{ .Operator = Operator.Less };
                        }
                    },
                    '"' => {
                        self.offset += 1;

                        while (!self.at_end() and self.content.value(self.offset) catch unreachable != '"') {
                            self.offset += 1;
                        }

                        if (self.at_end()) {
                            self.current = Token.EOF;
                        }

                        self.offset += 1;

                        const range = self.words.extend_range(
                            self.content.range(Range.new(self.start + 1, self.offset - 1)) catch unreachable,
                        ) catch @panic("TODO");

                        self.current = Token{ .String = self.words.range(range) catch unreachable };
                    },
                    else => @panic("TODO"),
                }
            }
        }
    }

    pub fn match(self: *Lexer, token: Token) bool {
        if (self.current.eql(token)) {
            self.advance();

            return true;
        }

        return self.current.eql(Token.EOF);
    }

    pub fn consume(self: *Lexer, token: Token) void {
        if (self.current.eql(token)) {
            self.advance();
        } else {
            util.print(.Info, "offset: {}", .{self.general_offset + self.offset});
            @panic("TODO");
        }
    }

    pub fn deinit(self: *Lexer) void {
        self.content.deinit(self.arena);
        self.words.deinit(self.arena);
        self.arena.deinit();
    }
};

test "basic" {
    var arena = try Arena.new("Testing", 1);
    defer arena.deinit();

    var file = try collections.File.open("zig-out/basic.lang");
    const stream = file.stream();

    const tokens: []const Token = &.{ Token.TYPE, Token.IDEN, Token.EQUAL, Token.NUMBER, Token.SEMICOLON, Token.TYPE, Token.IDEN, Token.EQUAL, Token.NUMBER, Token.SEMICOLON, Token.PROC, Token.IDEN, Token.PARENTESISLEFT, Token.PARENTESISRIGHT, Token.DOUBLECOLON, Token.IDEN, Token.BRACELEFT, Token.LET, Token.IDEN, Token.DOUBLECOLON, Token.IDEN, Token.EQUAL, Token.NUMBER, Token.PLUS, Token.NUMBER, Token.SEMICOLON, Token.IDEN, Token.BRACERIGHT, Token.EOF };

    var lexer = try Lexer.new(stream, &arena);
    defer lexer.deinit();

    for (tokens) |token| {
        try util.assert(lexer.current.eql(token));

        lexer.advance();
    }
}

test "function call" {
    var arena = try Arena.new("Testing", 1);
    defer arena.deinit();

    var file = try collections.File.open("zig-out/call.lang");

    const stream = file.stream();
    const tokens: []const Token = &.{ Token.TYPE, Token.IDEN, Token.EQUAL, Token.NUMBER, Token.SEMICOLON, Token.TYPE, Token.IDEN, Token.EQUAL, Token.NUMBER, Token.SEMICOLON, Token.PROC, Token.IDEN, Token.PARENTESISLEFT, Token.IDEN, Token.DOUBLECOLON, Token.IDEN, Token.PARENTESISRIGHT, Token.DOUBLECOLON, Token.IDEN, Token.BRACELEFT, Token.IDEN, Token.BRACERIGHT, Token.PROC, Token.IDEN, Token.PARENTESISLEFT, Token.PARENTESISRIGHT, Token.DOUBLECOLON, Token.IDEN, Token.BRACELEFT, Token.LET, Token.IDEN, Token.DOUBLECOLON, Token.IDEN, Token.EQUAL, Token.IDEN, Token.PARENTESISLEFT, Token.NUMBER, Token.PARENTESISRIGHT, Token.SEMICOLON, Token.IDEN, Token.BRACERIGHT };

    var lexer = try Lexer.new(stream, &arena);
    defer lexer.deinit();

    for (tokens) |token| {
        try util.assert(lexer.current.eql(token));

        lexer.advance();
    }
}
