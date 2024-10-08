const std = @import("std");
const util = @import("util.zig");
const allocator = @import("allocator.zig");

const Vec = @import("collections.zig").Vec;
const Arena = allocator.Arena;
const Range = util.Range;
const Index = util.Index;

pub const Keyword = enum(u8) {
    Let,
    Procedure,
    Mut,
    Return,
    Match,
    Type,
    True,
    False,
    Nil,

    fn from_string(string: []const u8) ?Keyword {
        switch (string[0]) {
            'p' => if (util.equal(u8, "proc", string)) return Keyword.Procedure,
            'r' => if (util.equal(u8, "return", string)) return Keyword.Return,
            't' => if (util.equal(u8, "type", string))
                return Keyword.Type
            else if (util.equal(u8, "true", string)) return Keyword.True,
            'f' => if (util.equal(u8, "false", string)) return Keyword.False,
            'l' => if (util.equal(u8, "let", string)) return Keyword.Let,
            'n' => if (util.equal(u8, "nil", string)) return Keyword.Nil,
            'm' => if (util.equal(u8, "mut", string))
                return Keyword.Mut
            else if (util.equal(u8, "match", string)) return Keyword.Match,
            else => {},
        }

        return null;
    }

    pub fn to_string(self: Keyword) []const u8 {
        return switch (self) {
            .Let => "let",
            .Procedure => "procedure",
            .Mut => "mut",
            .Return => "return",
            .Match => "match",
            .Type => "type",
            .True => "true",
            .False => "false",
            .Nil => "nil",
        };
    }
};

pub const Operator = enum(u8) {
    Star,
    Slash,
    Plus,
    Dash,
    Bang,
    BangEqual,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    pub fn to_string(self: Operator) []const u8 {
        return switch (self) {
            .Star => "star",
            .Slash => "slash",
            .Plus => "plus",
            .Dash => "dash",
            .Bang => "bang",
            .BangEqual => "bangequal",
            .EqualEqual => "equalequal",
            .Greater => "greater",
            .GreaterEqual => "greaterequal",
            .Less => "less",
            .LessEqual => "lessequal",
        };
    }

};

pub const Symbol = enum(u8) {
    Equal,
    Dot,
    DoubleColon,
    BraceLeft,
    BraceRight,
    SquareBracketLeft,
    SquareBracketRight,
    ParentesisLeft,
    ParentesisRight,
    Semicolon,
    Comma,

    pub fn to_string(self: Symbol) []const u8 {
        return switch (self) {
            .Equal => "equal",
            .Dot => "dot",
            .DoubleColon => "doublecolon",
            .BraceLeft => "braceleft",
            .BraceRight => "braceright",
            .SquareBracketLeft => "squarebracketleft",
            .SquareBracketRight => "squarebracketright",
            .ParentesisLeft => "parentesisleft",
            .ParentesisRight => "parentesisright",
            .Semicolon => "semicolon",
            .Comma => "comma",
        };
    }
};

pub const TokenKind = enum(u8) {
    identifier,
    number,
    string,
    operator,
    keyword,
    symbol,
    eof,
};

pub const TokenValue = union {
    identifier: Range,
    number: Range,
    string: Range,
    operator: Operator,
    keyword: Keyword,
    symbol: Symbol,
    eof: void,
};

pub const Token = struct {
    kind: TokenKind,
    value: TokenValue,

    pub fn new(kind: TokenKind, value: TokenValue) Token {
        return Token{
            .kind = kind,
            .value = value,
        };
    }

    pub fn eql(self: *const Token, other: Token) bool {
        if (self.kind != other.kind) return false;
        return switch (self.kind) {
            .identifier, .number, .string, .eof => true,
            .operator => self.value.operator == other.value.operator,
            .keyword => self.value.keyword == other.value.keyword,
            .symbol => self.value.symbol == other.value.symbol,
        };
    }

    pub fn to_string(self: *const Token) []const u8 {
        return switch (self.kind) {
            .identifier => "identifier",
            .number => "number",
            .string => "string",
            .operator => self.value.operator.to_string(),
            .keyword => self.value.keyword.to_string(),
            .symbol => self.value.symbol.to_string(),
            .eof => "eof",
        };
    }
};

pub const Scanner = struct {
    content: Vec(u8),
    words: Vec(u8),
    offset: Index,
    start: Index,
    file: std.fs.File,

    pub fn new(path: []const u8, arena: *Arena) Scanner {
        return Scanner{
            .content = Vec(u8).new(1024, arena),
            .words = Vec(u8).new(1024, arena),
            .offset = 0,
            .start = 0,
            .file = std.fs.cwd().openFile(path, .{}) catch unreachable,
        };
    }

    fn inc(self: *Scanner) void {
        self.offset += 1;
    }

    fn char(self: *Scanner) u8 {
        return self.content.items[self.offset];
    }

    fn at_end(self: *Scanner) bool {
        if (self.offset >= self.content.len) {
            const final = self.content.range(Range.new(self.start, self.offset));

            self.content.clear();
            self.content.extend(final);

            self.start = 0;
            self.offset = @intCast(self.content.len);

            const len = self.file.read(self.content.buffer()) catch return true;

            if (len == 0) {
                return true;
            }

            self.content.len += @intCast(len);
        }

        return self.offset >= self.content.len;
    }

    fn skip_whitespace(self: *Scanner) void {
        while (!self.at_end()) {
            switch (self.char()) {
                ' ', '\n', '\t', '\r' => self.inc(),
                else => break,
            }
        }

        self.start = self.offset;
    }

    fn match(self: *Scanner, c: u8) bool {
        if (self.char() == c) {
            self.inc();
            return true;
        }

        return false;
    }

    pub fn next(self: *Scanner) Token {
        self.start = self.offset;
        self.skip_whitespace();

        if (self.at_end()) {
            return Token.new(.eof, .{ .eof = {} });
        }

        const c = self.char();
        self.inc();

        if (util.is_alpha(c)) {
            while (!self.at_end() and util.is_ascci(self.char())) {
                self.inc();
            }

            const word = self.content.range(Range.new(self.start, self.offset));

            if (Keyword.from_string(word)) |keyword| {
                return Token.new(.keyword, .{ .keyword = keyword });
            }

            return Token.new(
                .identifier,
                .{ .identifier = self.words.extend_range(word) },
            );
        }

        if (util.is_digit(c)) {
            while (!self.at_end() and util.is_digit(self.char())) {
                self.inc();
            }

            const number = self.content.range(Range.new(self.start, self.offset));
            return Token.new(.number, .{ .number = self.words.extend_range(number) });
        }

        return switch (c) {
            '(' => Token.new(.symbol, .{ .symbol = .ParentesisLeft }),
            ')' => Token.new(.symbol, .{ .symbol = .ParentesisRight }),
            '{' => Token.new(.symbol, .{ .symbol = .BraceLeft }),
            '}' => Token.new(.symbol, .{ .symbol = .BraceRight }),
            ':' => Token.new(.symbol, .{ .symbol = .DoubleColon }),
            ';' => Token.new(.symbol, .{ .symbol = .Semicolon }),
            ',' => Token.new(.symbol, .{ .symbol = .Comma }),
            '.' => Token.new(.symbol, .{ .symbol = .Dot }),

            '+' => Token.new(.operator, .{ .operator = .Plus }),
            '-' => Token.new(.operator, .{ .operator = .Dash }),
            '*' => Token.new(.operator, .{ .operator = .Star }),
            '/' => Token.new(.operator, .{ .operator = .Slash }),

            '=' => if (self.match('=')) Token.new(.operator, .{ .operator = .EqualEqual }) else Token.new(.symbol, .{ .symbol = .Equal }),
            '!' => if (self.match('=')) Token.new(.operator, .{ .operator = .BangEqual }) else Token.new(.operator, .{ .operator = .Bang }),
            '>' => if (self.match('=')) Token.new(.operator, .{ .operator = .GreaterEqual }) else Token.new(.operator, .{ .operator = .Greater }),
            '<' => if (self.match('=')) Token.new(.operator, .{ .operator = .LessEqual }) else Token.new(.operator, .{ .operator = .Less }),
            '"' => blk: {
                while (!self.at_end() and self.char() != '"') {
                    self.inc();
                }

                if (self.at_end()) {
                    break :blk Token.new(.eof, .{ .eof = {} });
                }

                self.inc();

                const cont = self.content.range(
                    Range.new(self.start + 1, self.offset - 1),
                );

                break :blk Token.new(
                    .string,
                    .{ .string = self.words.extend_range(cont) },
                );
            },
            else => Token.new(.eof, .{ .eof = {} }),
        };
    }

    pub fn deinit(self: *const Scanner) void {
        self.file.close();
    }
};

test "Tokenizing one function" {
    var arena = Arena.new(allocator.malloc(1));
    defer arena.deinit();

    var scanner = Scanner.new("zig-out/function.lang", &arena);
    defer scanner.deinit();

    // TODO: Test for variants as well, this vector type may be "[]const Token"
    const expect = [_]TokenKind{
        .keyword,    .identifier, .symbol,     .identifier, .symbol,     .identifier, .symbol,  .identifier,
        .symbol,     .identifier, .symbol,     .symbol,     .identifier, .symbol,     .keyword, .keyword,
        .identifier, .symbol,     .identifier, .symbol,     .number,     .operator,   .symbol,  .number,
        .operator,   .number,     .operator,   .number,     .symbol,     .symbol,  
        .keyword, .identifier, .symbol, .identifier, .symbol, .identifier, .operator, .number,
        .symbol, .identifier, .symbol, .eof,
    };

    for (expect) |kind| {
        const next = scanner.next();

        try util.assert(kind == next.kind);
    }
}
