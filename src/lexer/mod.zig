const mem = @import("mem");
const util = @import("util");
const collections = @import("collections");

pub const token = @import("token.zig");

pub const Lexer = struct {
    words: collections.SliceManager(u8),
    content: collections.String,

    previous: token.Token,
    current: token.Token,

    offset: u32,
    start: u32,

    general_offset: u32,

    stream: collections.Stream(u8),
    arena: *mem.Arena,

    pub fn new(stream: collections.Stream(u8), allocator: *mem.Arena) error{OutOfMemory}!Lexer {
        const zone = util.tracy.initZone(@src(), .{.name = "Lexer::new"});
        defer zone.deinit();

        var self = Lexer{
            .stream = stream,
            .offset = 0,
            .start = 0,
            .general_offset = 0,
            .previous = token.Token.EOF,
            .current = token.Token.EOF,
            .arena = try allocator.child("Lexer", mem.PAGE_SIZE),
            .words = undefined,
            .content = undefined,
        };

        errdefer self.arena.deinit();

        const free_space = self.arena.capacity - self.arena.usage;

        self.words = try collections.SliceManager(u8).new(free_space / 3 - 2, self.arena);
        errdefer self.words.deinit(self.arena);

        self.content = try collections.String.new(free_space >> 1, self.arena);
        errdefer self.content.deinit(self.arena);

        self.advance();

        return self;
    }

    fn assert(self: *Lexer, expected: u8) bool {
        const zone = util.tracy.initZone(@src(), .{.name = "Lexer::assert"});
        defer zone.deinit();

        if (self.content.value(self.offset) catch unreachable == expected) {
            self.offset += 1;

            return true;
        }

        return false;
    }

    fn at_end(self: *Lexer) bool {
        const zone = util.tracy.initZone(@src(), .{.name = "Lexer::at_end"});
        defer zone.deinit();

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
        const zone = util.tracy.initZone(@src(), .{.name = "Lexer::advance"});
        defer zone.deinit();

        self.previous = self.current;

        while (!self.at_end()) {
            switch (self.content.value(self.offset) catch unreachable) {
                ' ', '\r', '\n', '\t' => self.offset += 1,
                else => break,
            }
        }

        self.start = self.offset;

        if (self.at_end()) {
            self.current = token.Token.EOF;
        } else {
            const c = self.content.value(self.offset) catch unreachable;
            self.offset += 1;

            if (util.is_digit(c)) {
                while (!self.at_end() and util.is_digit(self.content.value(self.offset) catch unreachable)) {
                    self.offset += 1;
                }

                const string = self.content.slice(collections.Slice.new(@intCast(self.start), @intCast(self.offset - self.start))) catch unreachable;

                self.current = token.Token{ .Number = self.words.push(string) catch @panic("TODO") };
            } else if (util.is_alpha(c)) {
                while (!self.at_end() and util.is_ascci(self.content.value(self.offset) catch unreachable)) {
                    self.offset += 1;
                }

                const string = self.content.slice(collections.Slice.new(@intCast(self.start), @intCast(self.offset - self.start))) catch unreachable;

                if (token.Keyword.from_string(string)) |keyword| {
                    self.current = token.Token{ .Keyword = keyword };
                } else {
                    self.current = token.Token{ .Identifier = self.words.push(string) catch @panic("TODO") };
                }
            } else {
                switch (c) {
                    '(' => self.current = token.Token{ .Symbol = token.Symbol.ParentesisLeft },
                    ')' => self.current = token.Token{ .Symbol = token.Symbol.ParentesisRight },
                    '{' => self.current = token.Token{ .Symbol = token.Symbol.CurlyBracketLeft },
                    '}' => self.current = token.Token{ .Symbol = token.Symbol.CurlyBracketRight },
                    ';' => self.current = token.Token{ .Symbol = token.Symbol.Semicolon },
                    ',' => self.current = token.Token{ .Symbol = token.Symbol.Comma },
                    '-' => self.current = token.Token{ .Operator = token.Operator.Minus },
                    '+' => self.current = token.Token{ .Operator = token.Operator.Plus },
                    '*' => self.current = token.Token{ .Operator = token.Operator.Star },
                    ':' => {
                        if (self.assert(':')) {
                            self.current = token.Token{ .Symbol = token.Symbol.DoubleDoubleColon };
                        } else {
                            self.current = token.Token{ .Symbol = token.Symbol.DoubleColon };
                        }
                    },
                    '/' => {
                        if (self.assert('/')) {
                            while (!self.at_end() and self.content.value(self.offset) catch unreachable != '\n') {
                                self.offset += 1;
                            }

                            self.advance();
                        } else self.current = token.Token{ .Operator = token.Operator.Slash };
                    },
                    '!' => {
                        if (self.assert('=')) {
                            self.current = token.Token{ .Operator = token.Operator.BangEqual };
                        } else {
                            self.current = token.Token{ .Operator = token.Operator.Bang };
                        }
                    },
                    '=' => {
                        if (self.assert('=')) {
                            self.current = token.Token{ .Operator = token.Operator.EqualEqual };
                        } else if (self.assert('>')) {
                            self.current = token.Token{ .Symbol = token.Symbol.Arrow };
                        } else {
                            self.current = token.Token{ .Symbol = token.Symbol.Equal };
                        }
                    },
                    '>' => {
                        if (self.assert('=')) {
                            self.current = token.Token{ .Operator = token.Operator.GreaterEqual };
                        } else {
                            self.current = token.Token{ .Operator = token.Operator.Greater };
                        }
                    },
                    '<' => {
                        if (self.assert('=')) {
                            self.current = token.Token{ .Operator = token.Operator.LessEqual };
                        } else {
                            self.current = token.Token{ .Operator = token.Operator.Less };
                        }
                    },
                    '"' => {
                        self.offset += 1;

                        while (!self.at_end() and self.content.value(self.offset) catch unreachable != '"') {
                            self.offset += 1;
                        }

                        if (self.at_end()) {
                            self.current = token.Token.EOF;
                        }

                        self.offset += 1;

                        const content = self.content.slice(collections.Slice.new(@intCast(self.start + 1), @intCast(self.offset - self.start))) catch unreachable;
                        self.current = token.Token{ .String = self.words.push(content) catch unreachable };
                    },
                    else => @panic("TODO"),
                }
            }
        }
    }

    pub fn next(self: *Lexer) ?token.Token {
        const zone = util.tracy.initZone(@src(), .{.name = "Lexer::next"});
        defer zone.deinit();

        self.advance();

        if (self.current.eql(token.Token.Eof)) {
            return null;
        }

        return self.current;
    }

    pub fn match(self: *Lexer, t: token.Token) bool {
        const zone = util.tracy.initZone(@src(), .{.name = "Lexer::match"});
        defer zone.deinit();

        if (self.current.eql(t)) {
            self.advance();

            return true;
        }

        return false;
    }

    pub fn deinit(self: *Lexer) void {
        const zone = util.tracy.initZone(@src(), .{.name = "Lexer::deinit"});
        defer zone.deinit();

        self.content.deinit(self.arena);
        self.words.deinit(self.arena);
        self.arena.deinit();
    }
};

test "simple" {
    var arena = try mem.Arena.new("Testing", 1);
    defer arena.deinit();

    var file = try collections.File.open("test/simple.lang");
    const stream = file.stream();

    const tokens: []const token.Token = &.{token.Token.TYPE, token.Token.IDEN, token.Token.EQUAL, token.Token.NUMBER, token.Token.SEMICOLON, token.Token.PROC, token.Token.IDEN, token.Token.PARENTESISLEFT, token.Token.PARENTESISRIGHT, token.Token.DOUBLECOLON, token.Token.IDEN, token.Token.BRACELEFT, token.Token.NUMBER, token.Token.BRACERIGHT, token.Token.Eof};

    var lexer = try Lexer.new(stream, &arena);
    defer lexer.deinit();

    for (tokens) |t| {
        try util.assert(lexer.current.eql(t));

        lexer.advance();
    }
}

test "function call" {
    var arena = try mem.Arena.new("Testing", 1);
    defer arena.deinit();

    var file = try collections.File.open("test/function_call.lang");

    const stream = file.stream();
    const tokens: []const token.Token = &.{token.Token.TYPE, token.Token.IDEN, token.Token.EQUAL, token.Token.NUMBER, token.Token.SEMICOLON, token.Token.PROC, token.Token.IDEN, token.Token.PARENTESISLEFT, token.Token.PARENTESISRIGHT, token.Token.DOUBLECOLON, token.Token.IDEN, token.Token.BRACELEFT, token.Token.NUMBER, token.Token.BRACERIGHT, token.Token.PROC, token.Token.IDEN, token.Token.PARENTESISLEFT, token.Token.PARENTESISRIGHT, token.Token.DOUBLECOLON, token.Token.IDEN, token.Token.BRACELEFT, token.Token.LET, token.Token.IDEN, token.Token.DOUBLECOLON, token.Token.IDEN, token.Token.EQUAL, token.Token.IDEN, token.Token.PARENTESISLEFT, token.Token.PARENTESISRIGHT, token.Token.SEMICOLON, token.Token.IDEN, token.Token.BRACERIGHT, token.Token.Eof};

    var lexer = try Lexer.new(stream, &arena);
    defer lexer.deinit();

    for (tokens) |t| {
        try util.assert(lexer.current.eql(t));

        lexer.advance();
    }
}


