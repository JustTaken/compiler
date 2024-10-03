const util = @import("util.zig");
const allocator = @import("allocator.zig");

const Arena = allocator.Arena;
const Range = util.Range;
const Vec = @import("collections.zig").Vec;
const Constant = @import("value.zig").Constant;
const Scanner = @import("scanner.zig").Scanner;
const Token = @import("scanner.zig").Token;

const Instruction = enum(u8) {
    Not, Equal, Negate, Greater, Less, Add, Subtract, Multiply, Divide, False,
    True, Nil, Constant, Let, Mut, Procedure, Argument, Parameter, Ret, Call,
};

const Precedence = enum(u8) {
    None, Assignment, Or, And, Equality, Comparison, Term, Factor, Unary, Call, Primary,
};

const Fn = *const fn (parser: *Parser) void;
const Rule = struct {
    prefix: ?Fn,
    infix: ?Fn,
    precedence: Precedence,

    fn new(prefix: ?Fn, infix: ?Fn, precedence: Precedence) Rule {
        return Rule {
            .prefix = prefix,
            .infix = infix,
            .precedence = precedence,
        };
    }

    fn from_token(token: Token) Rule {
        switch (token.kind) {
            .symbol => {
                switch (token.value.symbol) {
                    .ParentesisLeft => return Rule.new(grouping, call, .Assignment),
                    else => return Rule.new(null, null, .None),
                }
            },
            .operator => {
                switch (token.value.operator) {
                    .Dash => return new(unary, binary, .Term),
                    .Plus => return new(null, binary, .Term),
                    .Slash, .Star => return new(null, binary, .Factor),
                    .EqualEqual, .BangEqual => return new(null, binary, .Factor),
                    .Bang => return new(null, binary, .Equality),
                    .Greater, .GreaterEqual,
                    .Less, .LessEqual => return new(null, binary, .Comparison),
                }
            },
            .keyword => {
                switch (token.value.keyword) {
                    .True, .False, .Nil => return new(literal, null, .None),
                    else => return new(null, null, .None),
                }
            },
            .number => return new(number, null, .None),
            .identifier => return new(identifier, null, .None),
            .string => return new(string, null, .None),
            else => return new(null, null, .None),
        }
    }
};

pub const Parser = struct {
    scanner: Scanner,
    current: Token,
    previous: Token,
    instructions: Vec(Instruction),
    constants: Vec(Constant),
    ranges: Vec(Range),

    pub fn new(path: []const u8, arena: *Arena) Parser {
        var self = Parser {
            .scanner = Scanner.new(path, arena),
            .current = Token.new(.eof, .{ .eof = {} }),
            .previous = Token.new(.eof, .{ .eof = {} }),
            .instructions = Vec(Instruction).new(128, arena),
            .constants = Vec(Constant).new(10, arena),
            .ranges = Vec(Range).new(10, arena),
        };

        _ = self.advance();

        return self;
    }

    fn advance(self: *Parser) void {
        self.previous = self.current;
        self.current = self.scanner.next();
    }

    fn consume(self: *Parser, token: Token) void {
        if (self.current.eql(token)) {
            self.advance();
        } else {
            @panic("Consume wrong token");
        }
    }

    fn match(self: *Parser, token: Token) bool {
        const flag = self.current.eql(token);

        if (flag) {
            self.advance();
        }

        return flag;
    }

    fn parse(self: *Parser, precedence: Precedence) void {
        self.advance();

        var rule = Rule.from_token(self.previous);

        if (rule.prefix) |f| {
            f(self);
        } else @panic("Should have this function");

        rule = Rule.from_token(self.current);
        while (@intFromEnum(precedence) <= @intFromEnum(rule.precedence)) {
            if (rule.infix) |f| {
                self.advance();
                f(self);
            } else break;

            rule = Rule.from_token(self.current);
        }
    }

    fn clear(self: *Parser) void {
        self.instructions.clear();
        self.ranges.clear();
        self.constants.clear();
    }

    pub fn next(self: *Parser) bool {
        self.clear();

        const has_next = !self.match(Token.new(.eof, undefined));

        if (has_next) {
            statement(self);
        }

        return has_next;
    }

    pub fn deinit(self: *const Parser) void {
        self.scanner.deinit();
    }
};

fn expression(parser: *Parser) void {
    parser.parse(.Assignment);
}

fn grouping(parser: *Parser) void {
    expression(parser);
    parser.consume(Token.new(.symbol, .{ .symbol = .ParentesisRight }));
}

fn unary(parser: *Parser) void {
    const operator = parser.previous.value.operator;

    parser.parse(.Unary);

    switch (operator) {
        .Bang => parser.instructions.push(.Not),
        .Dash => parser.instructions.push(.Negate),
        else => @panic("Not handled yet"),
    }
}

fn binary(parser: *Parser) void {
    const operator = parser.previous.value.operator;
    const rule = Rule.from_token(parser.previous);

    parser.parse(@enumFromInt(@intFromEnum(rule.precedence) + 1));

    switch (operator) {
        .BangEqual => parser.instructions.extend(&.{ .Equal, .Not }),
        .EqualEqual => parser.instructions.push(.Equal),
        .Greater => parser.instructions.push(.Greater),
        .GreaterEqual => parser.instructions.extend(&.{ .Less, .Not }),
        .Less => parser.instructions.push(.Less),
        .LessEqual => parser.instructions.extend(&.{ .Greater, .Not }),
        .Plus => parser.instructions.push(.Add),
        .Dash => parser.instructions.push(.Subtract),
        .Star => parser.instructions.push(.Multiply),
        .Slash => parser.instructions.push(.Divide),
        else => @panic("Should not be here"),
    }
}

fn literal(parser: *Parser) void {
    const keyword = parser.previous.value.keyword;

    switch (keyword) {
        .False => parser.instructions.push(.False),
        .True => parser.instructions.push(.True),
        .Nil => parser.instructions.push(.Nil),
        else => @panic("Should not be here"),
    }
}

fn identifier(parser: *Parser) void {
    const range = parser.previous.value.identifier;

    if (parser.match(Token.new(.symbol, .{ .symbol = .ParentesisLeft }))) {
        call(parser);
        parser.ranges.push(range);
    } else {
        parser.instructions.push(.Constant);
        parser.constants.push(Constant.new(range, .Identifier));
    }
}

fn number(parser: *Parser) void {
    parser.instructions.push(.Constant);
    parser.constants.push(Constant.new(parser.previous.value.number, .Number));
}

fn procedure(parser: *Parser) void {
    const iden = parser.current;

    parser.consume(Token.new(.identifier, undefined));
    parser.consume(Token.new(.symbol, .{ .symbol = .ParentesisLeft }));

    while (!parser.match(Token.new(.symbol, .{ .symbol = .ParentesisRight }))) {
        _ = parser.match(Token.new(.symbol, .{ .symbol = .Comma }));

        const param = parser.current;

        parser.consume(Token.new(.identifier, undefined));
        parser.consume(Token.new(.symbol, .{ .symbol = .DoubleColon }));

        const kind = parser.current;

        parser.consume(Token.new(.identifier, undefined));

        parser.instructions.push(.Parameter);
        parser.ranges.extend(&.{param.value.identifier, kind.value.identifier});
    }

    parser.consume(Token.new(.symbol, .{ .symbol = .DoubleColon }));

    const ret_iden = parser.current;

    parser.consume(Token.new(.identifier, undefined));
    parser.consume(Token.new(.symbol, .{ .symbol = .BraceLeft }));

    while (!parser.match(Token.new(.symbol, .{ .symbol = .BraceRight }))) {
        statement(parser);
    }

    parser.instructions.push(.Procedure);
    parser.ranges.extend(&.{iden.value.identifier, ret_iden.value.identifier});
}

fn statement(parser: *Parser) void {
    const token = parser.current;

    switch (token.kind) {
        .keyword => {
            parser.advance();

            switch (token.value.keyword) {
                .Let => variable(parser),
                .Procedure => procedure(parser),
                .Type => typ(parser),
                else => @panic("should not be here"),
            }
        },
        else => expression(parser)
    }
}

fn call(parser: *Parser) void {
    while (!parser.match(Token.new(.symbol, .{ .symbol = .ParentesisRight }))) {
        _ = parser.match(Token.new(.symbol, .{ .symbol = .Comma} ));

        expression(parser);
    }

    parser.instructions.push(.Call);
}

fn variable(parser: *Parser) void {
    const mut = parser.match(Token.new(.keyword, .{ .keyword = .Mut }));
    const iden = parser.current;

    parser.consume(Token.new(.identifier, undefined));
    parser.consume(Token.new(.symbol, .{ .symbol = .DoubleColon }));

    const kind = parser.current;

    parser.consume(Token.new(.identifier, undefined));
    parser.consume(Token.new(.symbol, .{ .symbol = .Equal }));

    expression(parser);

    parser.consume(Token.new(.symbol, .{ .symbol = .Semicolon }));
    parser.instructions.push(.Let);

    if (mut) {
        parser.instructions.push(.Mut);
    }

    parser.ranges.extend(&.{iden.value.identifier, kind.value.identifier});
}

fn typ(parser: *Parser) void {
    _ = parser;
}

fn string(parser: *Parser) void {
    _ = parser;
}

test "Parsing one function" {
    var arena = Arena.new(allocator.malloc(1));
    var parser = Parser.new("zig-out/function.lang", &arena);
    defer parser.deinit();

    try util.assert(parser.next());

    var array = [_]Instruction {
        .Parameter, .Parameter, .Constant, .Constant, .Constant, .Add,
        .Constant, .Add, .Multiply, .Let, .Mut, .Procedure,
    };

    const expect = Vec(Instruction).from_array(&array);

    try util.assert(expect.eql(parser.instructions));
    try util.assert(!parser.next());
}

