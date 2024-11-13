const collections = @import("collections");
const util = @import("util");
const mem = @import("mem");

const Arena = mem.Arena;
const File = util.File;
const Range = collections.Range;
const String = collections.String;
const Stream = collections.Stream;

pub const Keyword = enum {
    Let,
    Type,
    Procedure,
    Mut,

    fn from_string(string: []const u8) ?Keyword {
        if (string.len == 0) return null;

        switch (string[0]) {
            't' => {
                if (mem.equal(u8, string, "type")) return .Type else return null;
            },
            'p' => {
                if (mem.equal(u8, string, "proc")) return .Procedure else return null;
            },
            'm' => {
                if (mem.equal(u8, string, "mut")) return .Mut else return null;
            },
            'l' => {
                if (mem.equal(u8, string, "let")) return .Let else return null;
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
    CurlyBraceLeft,
    CurlyBraceRight,
    Comma,
    Arrow,
    Dot,
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
    Identifier: Range,
    String: Range,
    Number: Range,
    Keyword: Keyword,
    Operator: Operator,
    Symbol: Symbol,

    Eof,

    pub const IDEN: Token = Token{ .Identifier = Range.new(0, 0) };
    pub const NUMBER: Token = Token{ .Number = Range.new(0, 0) };
    pub const PARENTESISLEFT: Token = Token{ .Symbol = Symbol.ParentesisLeft };
    pub const PARENTESISRIGHT: Token = Token{ .Symbol = Symbol.ParentesisRight };
    pub const COMMA: Token = Token{ .Symbol = Symbol.Comma };
    pub const DOT: Token = Token{ .Symbol = Symbol.Dot };
    pub const DOUBLECOLON: Token = Token{ .Symbol = Symbol.DoubleColon };
    pub const BRACELEFT: Token = Token{ .Symbol = Symbol.CurlyBraceLeft };
    pub const BRACERIGHT: Token = Token{ .Symbol = Symbol.CurlyBraceRight };
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

    stream: Stream,
    arena: *Arena,

    pub fn new(stream: Stream, allocator: *Arena) Lexer {
        const arena = allocator.child("Lexer", mem.PAGE_SIZE >> 1);

        var self = Lexer{
            .words = String.new(1024, arena),
            .content = String.new(1024, arena),
            .stream = stream,
            .previous = Token.EOF,
            .current = Token.EOF,
            .offset = 0,
            .start = 0,
            .arena = arena,
        };

        self.advance();

        return self;
    }

    fn assert(self: *Lexer, expected: u8) bool {
        if (self.content.value(self.offset) == expected) {
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
            self.offset = len;
            self.content.set_len(len);
            self.stream.read(&self.content) catch return true;
        }

        return self.offset >= self.content.len;
    }

    pub fn advance(self: *Lexer) void {
        self.previous = self.current;

        while (!self.at_end()) {
            switch (self.content.value(self.offset)) {
                ' ',
                '\r',
                '\n',
                '\t',
                => self.offset += 1,
                else => break,
            }
        }

        self.start = self.offset;

        if (self.at_end()) {
            self.current = Token.EOF;
        } else {
            const c = self.content.value(self.offset);
            self.offset += 1;

            if (util.is_digit(c)) {
                while (!self.at_end() and util.is_digit(self.content.value(self.offset))) {
                    self.offset += 1;
                }

                const string = self.content.range(Range.new(self.start, self.offset));
                self.current = Token{ .Number = self.words.extend_range(string) };
            } else if (util.is_alpha(c)) {
                while (!self.at_end() and util.is_ascci(self.content.value(self.offset))) {
                    self.offset += 1;
                }

                const string = self.content.range(Range.new(self.start, self.offset));

                if (Keyword.from_string(string)) |keyword| {
                    self.current = Token{ .Keyword = keyword };
                } else {
                    self.current = Token{ .Identifier = self.words.extend_range(string) };
                }
            } else {
                switch (c) {
                    '(' => self.current = Token{ .Symbol = Symbol.ParentesisLeft },
                    ')' => self.current = Token{ .Symbol = Symbol.ParentesisRight },
                    '{' => self.current = Token{ .Symbol = Symbol.CurlyBraceLeft },
                    '}' => self.current = Token{ .Symbol = Symbol.CurlyBraceRight },
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

                        while (!self.at_end() and self.content.value(self.offset) != '"') {
                            self.offset += 1;
                        }

                        if (self.at_end()) {
                            self.current = Token.EOF;
                        }

                        self.offset += 1;

                        const string = self.content.range(Range.new(self.start + 1, self.offset - 1));
                        self.current = Token{ .String = self.words.extend_range(string) };
                    },
                    else => @panic("Should not happen"),
                }
            }
        }
    }

    pub fn match(self: *Lexer, token: Token) bool {
        if (self.current.eql(token)) {
            self.advance();

            return true;
        }

        return false;
    }

    pub fn consume(self: *Lexer, token: Token) void {
        if (self.current.eql(token)) {
            self.advance();
        } else {
            @panic("Should not happen");
        }
    }

    pub fn deinit(self: *Lexer) void {
        self.stream.close();
        self.content.deinit(self.arena);
        self.words.deinit(self.arena);
        self.arena.deinit();
    }
};

test "basic" {
    const buffer = mem.malloc(1);
    var arena = Arena.new(buffer);

    var file = try collections.File.open("zig-out/basic.lang");
    const stream = file.stream();

    const tokens: []const Token = &.{ Token.TYPE, Token.IDEN, Token.EQUAL, Token.NUMBER, Token.SEMICOLON, Token.TYPE, Token.IDEN, Token.EQUAL, Token.NUMBER, Token.SEMICOLON, Token.PROC, Token.IDEN, Token.PARENTESISLEFT, Token.PARENTESISRIGHT, Token.DOUBLECOLON, Token.IDEN, Token.BRACELEFT, Token.LET, Token.IDEN, Token.DOUBLECOLON, Token.IDEN, Token.EQUAL, Token.NUMBER, Token.PLUS, Token.NUMBER, Token.SEMICOLON, Token.IDEN, Token.BRACERIGHT, Token.EOF };

    var lexer = Lexer.new(stream, &arena);

    for (tokens) |token| {
        try util.assert(lexer.current.eql(token));

        lexer.advance();
    }
}

test "function call" {
    const buffer = mem.malloc(1);
    var arena = Arena.new(buffer);

    var file = try collections.File.open("zig-out/call.lang");

    const stream = file.stream();
    const tokens: []const Token = &.{ Token.TYPE, Token.IDEN, Token.EQUAL, Token.NUMBER, Token.SEMICOLON, Token.TYPE, Token.IDEN, Token.EQUAL, Token.NUMBER, Token.SEMICOLON, Token.PROC, Token.IDEN, Token.PARENTESISLEFT, Token.IDEN, Token.DOUBLECOLON, Token.IDEN, Token.PARENTESISRIGHT, Token.DOUBLECOLON, Token.IDEN, Token.BRACELEFT, Token.IDEN, Token.BRACERIGHT, Token.PROC, Token.IDEN, Token.PARENTESISLEFT, Token.PARENTESISRIGHT, Token.DOUBLECOLON, Token.IDEN, Token.BRACELEFT, Token.LET, Token.IDEN, Token.DOUBLECOLON, Token.IDEN, Token.EQUAL, Token.IDEN, Token.PARENTESISLEFT, Token.NUMBER, Token.PARENTESISRIGHT, Token.SEMICOLON, Token.IDEN, Token.BRACERIGHT };

    var lexer = Lexer.new(stream, &arena);

    for (tokens) |token| {
        try util.assert(lexer.current.eql(token));

        lexer.advance();
    }
}
