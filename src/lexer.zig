const std = @import("std");
const util = @import("util.zig");
const collections = @import("collections.zig");

const Allocator = std.mem.Allocator;
const Arena = collections.Arena;
const Vec = collections.Vec;
const FixedVec = @import("collections.zig").FixedVec;
const Parser = @import("parser.zig").Parser;

pub const Keyword = enum(u8) {
    Let,
    Function,
    Mut,
    Return,
    Match,
    Type,

    fn from_string(string: []const u8) ?Keyword {
        switch (string[0]) {
            'f' => if (util.eql("fn", string)) return Keyword.Function,
            'r' => if (util.eql("return", string)) return Keyword.Return,
            't' => if (util.eql("type", string)) return Keyword.Type,
            'l' => if (util.eql("let", string)) return Keyword.Let,
            'm' => if (util.eql("mut", string))
                return Keyword.Mut
            else if (util.eql("match", string)) return Keyword.Match,
            else => {},
        }

        return null;
    }

    fn get(string: []const u8) Keyword {
        switch (string[0]) {
            'f' => return Keyword.Function,
            'r' => return Keyword.Return,
            't' => return Keyword.Type,
            'l' => return Keyword.Let,
            'm' => {
                if (string[1] == 'a') return Keyword.Match;
                return Keyword.Mut;
            },
            else => unreachable,
        }
    }
};

pub const Operator = enum(u8) {
    Star,
    Bar,
    Plus,
    Dash,

    pub fn get(string: []const u8) ?Operator {
        return switch (string[0]) {
            '*' => .Star,
            '/' => .Bar,
            '+' => .Plus,
            '-' => .Dash,
            else => return null,
        };
    }

    pub fn precedence(self: *const Operator) u32 {
        return switch (self.*) {
            .Star => 2,
            .Bar => 2,
            .Plus => 1,
            .Dash => 1,
        };
    }
};

pub const Symbol = enum(u8) {
    Equal,
    DoubleColon,
    CurlyBracketLeft,
    CurlyBracketRight,
    SquareBracketLeft,
    SquareBracketRight,
    ParentesisLeft,
    ParentesisRight,
    SemiColon,
    Colon,

    fn get(string: []const u8) ?Symbol {
        return switch (string[0]) {
            '=' => .Equal,
            ':' => .DoubleColon,
            '{' => .CurlyBracketLeft,
            '}' => .CurlyBracketRight,
            '[' => .SquareBracketLeft,
            ']' => .SquareBracketRight,
            '(' => .ParentesisLeft,
            ')' => .ParentesisRight,
            ';' => .SemiColon,
            ',' => .Colon,
            else => return null,
        };
    }
};

pub const TokenId = enum(u8) {
    Identifier,
    Operator,
    Literal,
    Keyword,
    Symbol,

    pub fn symbol(string: []const u8) Symbol {
        return Symbol.get(string) orelse @panic("Should not happen");
    }

    pub fn keyword(string: []const u8) Keyword {
        return Keyword.get(string);
    }

    pub fn operator(string: []const u8) Operator {
        return Operator.get(string) orelse @panic("Should not happen");
    }

    pub fn identifier(string: []const u8) []const u8 {
        var len: u32 = 0;
        for (0..string.len) |i| {
            if (!is_ascci(string[i])) break;
            len += 1;
        }

        return string[0..len];
    }

    fn from_string(string: [*]const u8, range: *const Range) ?TokenId {
        if (range.end == range.start) return null;
        var self: TokenId = undefined;

        switch (string[range.start]) {
            '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' => {
                if (util.is_number(string[range.start..range.end])) {
                    self = TokenId.Literal;
                } else {
                    @panic("Identifier cannot start with a number");
                }
            },
            else => {
                if (Keyword.from_string(string[range.start..range.end])) |_| {
                    self = TokenId.Keyword;
                } else {
                    self = TokenId.Identifier;
                }
            },
        }

        return self;
    }
};

fn is_ascci(char: u8) bool {
    return (char >= 'A' and char <= 'Z') or
        (char >= 'a' and char <= 'z') or
        (char >= '0' and char <= '9');
}

const Range = struct {
    start: usize,
    end: usize,
};

const TokenizerState = struct {
    range: Range,
    concatenating: bool,
    flag: bool,

    fn init() TokenizerState {
        return .{
            .concatenating = false,
            .flag = false,
            .range = Range{
                .start = 0,
                .end = 0,
            },
        };
    }

    fn reset(self: *TokenizerState, i: usize) void {
        if (!self.flag) {
            self.concatenating = false;
            self.range.start = i;
        }

        self.range.end = i;
        self.flag = false;
    }

    fn concat(self: *TokenizerState, i: usize) void {
        self.flag = true;

        if (!self.concatenating) {
            self.concatenating = true;
            self.range.start = i;
            self.range.end = i;
        }
    }
};

pub const Lexer = struct {
    arena: Arena,

    token_id: Vec(TokenId),
    token_start: Vec(u16),
    token_next: u16,

    content: Vec(u8),

    state: TokenizerState,
    file_open: bool,
    file: std.fs.File,

    pub fn init(arena: *Arena) Lexer {
        var self: Lexer = undefined;

        self.arena = Arena.init(arena.alloc(u8, 1024 * 7)[0 .. 1024 * 7]);
        self.content = Vec(u8).init(4096, &self.arena);

        self.token_id = Vec(TokenId).init(1024, &self.arena);
        self.token_start = Vec(u16).init(1024, &self.arena);
        self.token_next = 0;

        self.state = TokenizerState.init();
        self.file_open = false;

        return self;
    }

    pub fn set_path(self: *Lexer, path: []const u8) void {
        if (self.file_open) self.file.close();

        self.token_id.clear();
        self.token_start.clear();
        self.content.clear();

        self.file_open = true;
        self.file = std.fs.cwd().openFile(path, .{}) catch
            @panic("file does not exist");
    }

    fn push(self: *Lexer) void {
        if (TokenId.from_string(self.content.items, &self.state.range)) |id| {
            self.token_id.push(id);
            self.token_start.push(@intCast(self.state.range.start));
        }
    }

    pub fn next_id(self: *const Lexer) TokenId {
        return self.token_id.items[self.token_next];
    }

    pub fn next_start(self: *const Lexer) u16 {
        return self.token_start.items[self.token_next];
    }

    pub fn consume(self: *Lexer) void {
        self.token_next += 1;
    }

    pub fn has_next(self: *Lexer, parser: *Parser) bool {
        _ = parser;
        return self.token_next < self.token_id.len;
    }

    pub fn tokenize(self: *Lexer) void {
        if (!self.file_open) return;

        {
            const start = self.state.range.start;
            const end = self.state.range.end;

            util.copy(
                u8,
                self.content.items[start..end],
                self.content.items[0..end],
            );

            self.state.range.end -= start;
            self.state.range.start = 0;
            self.content.len = @intCast(self.state.range.end);

            const buffer =
                self.content.items[self.content.len..self.content.capacity];
            self.content.len += @intCast(self.file.read(buffer) catch
                @panic("could not read from file"));
        }

        for (0..self.content.len) |i| {
            self.state.reset(i);

            switch (self.content.items[i]) {
                ' ', '\n' => self.push(),
                '+', '-', '*', '/' => {
                    self.push();
                    self.token_id.push(TokenId.Operator);
                    self.token_start.push(@intCast(i));
                },
                ';', '=', ':', ',', '{', '}', '(', ')', '[', ']' => {
                    self.push();
                    self.token_id.push(TokenId.Symbol);
                    self.token_start.push(@intCast(i));
                },
                else => self.state.concat(i),
            }
        }
    }

    pub fn deinit(self: *const Lexer) void {
        if (self.file_open) {
            self.file.close();
        }
    }
};
