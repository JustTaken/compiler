const std = @import("std");
const util = @import("util.zig");
const collections = @import("collections.zig");

const Allocator = std.mem.Allocator;
const Arena = collections.Arena;
const Vec = collections.Vec;
const FixedVec = @import("collections.zig").FixedVec;

pub const Keyword = enum(u8) {
    Let,
    Function,
    Mut,
    Return,
    Match,
    Struct,

    fn from_string(string: []const u8) ?Keyword {
        switch (string[0]) {
            'f' => if (util.eql("fn", string)) return Keyword.Function,
            'r' => if (util.eql("return", string)) return Keyword.Return,
            's' => if (util.eql("struct", string)) return Keyword.Struct,
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
            's' => return Keyword.Struct,
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

    pub fn get(string: []const u8) Operator {
        return switch (string[0]) {
            '*' => .Star,
            '/' => .Bar,
            '+' => .Plus,
            '-' => .Dash,
            else => @panic("Should not be here"),
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

    fn get(string: []const u8) Symbol {
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
            else => @panic("Should not be here"),
        };
    }
};

pub const TokenId = enum(u16) {
    Keyword,
    Operator,
    Literal,
    Symbol,
    Identifier,

    pub fn symbol(string: []const u8) Symbol {
        return Symbol.get(string);
    }

    pub fn keyword(string: []const u8) Keyword {
        return Keyword.get(string);
    }

    pub fn operator(string: []const u8) Operator {
        return Operator.get(string);
    }
};

pub const Token = struct {
    id: TokenId,
    start: u16,

    fn init(id: TokenId, start: usize) Token {
        return .{
            .start = @intCast(start),
            .id = id,
        };
    }

    fn from_string(string: [*]const u8, range: *const Range) ?Token {
        if (range.end == range.start) return null;
        var id: TokenId = undefined;

        switch (string[range.start]) {
            '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' => {
                if (util.is_number(string[range.start..range.end])) {
                    id = TokenId.Literal;
                } else {
                    @panic("Identifier cannot start with a number");
                }
            },
            else => {
                if (Keyword.from_string(string[range.start..range.end])) |_| {
                    id = TokenId.Keyword;
                } else {
                    id = TokenId.Identifier;
                }
            },
        }

        return .{
            .start = @intCast(range.start),
            .id = id,
        };
    }
};

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
    tokens: Vec(Token),
    content: Vec(u8),

    state: TokenizerState,
    file_open: bool,
    file: std.fs.File,

    pub fn init(arena: *Arena) Lexer {
        var self: Lexer = undefined;

        self.arena = Arena.init(arena.alloc(u8, 1024 * 8)[0 .. 1024 * 8]);
        self.content = Vec(u8).init(4096, &self.arena);
        self.tokens = Vec(Token).init(4096 / @sizeOf(Token), &self.arena);
        self.state = TokenizerState.init();
        self.file_open = false;

        return self;
    }

    pub fn set_path(self: *Lexer, path: []const u8) void {
        if (self.file_open) self.file.close();

        self.tokens.clear();
        self.content.clear();

        self.file_open = true;
        self.file = std.fs.cwd().openFile(path, .{}) catch
            @panic("file does not exist");
    }

    fn push(self: *Lexer, range: *const Range, token: ?Token) void {
        if (Token.from_string(self.content.items, range)) |t| {
            self.tokens.push(t);
        }

        if (token) |t| {
            self.tokens.push(t);
        }
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

            const buffer = self.content.items[self.content.len..self.content.capacity];
            self.content.len += @intCast(self.file.read(buffer) catch
                @panic("could not read from file"));
        }

        for (0..self.content.len) |i| {
            self.state.reset(i);

            switch (self.content.items[i]) {
                ' ', '\n' => self.push(&self.state.range, null),
                '+', '-', '*', '/' => self.push(
                    &self.state.range,
                    Token.init(TokenId.Operator, i),
                ),
                ';', '=', ':', ',', '{', '}', '(', ')', '[', ']' => self.push(
                    &self.state.range,
                    Token.init(TokenId.Symbol, i),
                ),
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
