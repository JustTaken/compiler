const util = @import("util.zig");
const allocator = @import("allocator.zig");

const Arena = allocator.Arena;
const Range = util.Range;
const Vec = @import("collections.zig").Vec;
const Constant = @import("value.zig").Constant;
const Scanner = @import("scanner.zig").Scanner;
const Token = @import("scanner.zig").Token;
const Generator = @import("generator.zig").Generator;
const TypeChecker = @import("checker.zig").TypeChecker;
const Instruction = @import("checker.zig").Instruction;

const Precedence = enum(u8) {
    None,
    Assignment,
    Or,
    And,
    Equality,
    Comparison,
    Term,
    Factor,
    Unary,
    Call,
    Primary,
};

const Fn = *const fn (parser: *Parser) void;
const Rule = struct {
    prefix: ?Fn,
    infix: ?Fn,
    precedence: Precedence,

    fn new(prefix: ?Fn, infix: ?Fn, precedence: Precedence) Rule {
        return Rule{
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
                    .Greater, .GreaterEqual, .Less, .LessEqual => return new(null, binary, .Comparison),
                }
            },
            .keyword => {
                switch (token.value.keyword) {
                    .True,
                    .False,
                    => return new(literal, null, .None),
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
    current: Token,
    previous: Token,
    scanner: Scanner,
    generator: Generator,
    checker: TypeChecker,
    error_buffer: Vec(u8),

    pub fn new(input: []const u8, output: []const u8, arena: *Arena) Parser {
        var scanner = Scanner.new(input, arena);

        return Parser{
            .previous = Token.new(.eof, .{ .eof = {} }),
            .current = scanner.next(),
            .scanner = scanner,
            .generator = Generator.new(output, arena),
            .checker = TypeChecker.new(arena),
            .error_buffer = Vec(u8).new(512, arena),
        };
    }

    fn advance(self: *Parser) void {
        self.previous = self.current;
        self.current = self.scanner.next();
    }

    fn consume(self: *Parser, token: Token) void {
        if (self.current.eql(token)) {
            self.advance();
        } else {
            self.error_buffer.extend("Expected token \"");
            self.error_buffer.extend(token.to_string());
            self.error_buffer.extend("\", found \"");
            self.error_buffer.extend(self.current.to_string());
            self.error_buffer.extend("\"\n");
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

        if (Rule.from_token(self.previous).prefix) |f| {
            f(self);
        }
        {
            self.error_buffer.extend("token \"");
            self.error_buffer.extend(self.previous.to_string());
            self.error_buffer.extend("\" do not have prefix function\n");
        }

        var rule = Rule.from_token(self.current);
        while (@intFromEnum(precedence) <= @intFromEnum(rule.precedence)) {
            if (rule.infix) |f| {
                self.advance();
                f(self);
            } else break;

            rule = Rule.from_token(self.current);
        }
    }

    pub fn next(self: *Parser) bool {
        if (!self.match(Token.EOF)) {
            statement(self);
            return true;
        } else return false;
    }

    pub fn deinit(self: *const Parser) void {
        self.scanner.deinit();
        self.generator.deinit();
    }
};

fn expression(parser: *Parser) void {
    parser.parse(.Assignment);
}

fn grouping(parser: *Parser) void {
    expression(parser);
    parser.consume(Token.PARENTESISRIGHT);
}

fn unary(parser: *Parser) void {
    const operator = parser.previous.value.operator;

    parser.parse(.Unary);

    switch (operator) {
        .Bang => parser.checker.push_instruction(.Not, &parser.scanner.words),
        .Dash => parser.checker.push_instruction(.Negate, &parser.scanner.words),
        else => {
            parser.error_buffer.extend("operator \"");
            parser.error_buffer.extend(operator.to_string());
            parser.error_buffer.extend("\", cannot be unary\n");
        },
    }
}

fn binary(parser: *Parser) void {
    const operator = parser.previous.value.operator;
    const rule = Rule.from_token(parser.previous);

    parser.parse(@enumFromInt(@intFromEnum(rule.precedence) + 1));

    switch (operator) {
        .BangEqual => {
            parser.checker.push_instruction(.Equal, &parser.scanner.words);
            parser.checker.push_instruction(.Not, &parser.scanner.words);
        },
        .GreaterEqual => {
            parser.checker.push_instruction(.Less, &parser.scanner.words);
            parser.checker.push_instruction(.Not, &parser.scanner.words);
        },
        .LessEqual => {
            parser.checker.push_instruction(.Greater, &parser.scanner.words);
            parser.checker.push_instruction(.Not, &parser.scanner.words);
        },
        .EqualEqual => parser.checker.push_instruction(.Equal, &parser.scanner.words),
        .Greater => parser.checker.push_instruction(.Greater, &parser.scanner.words),
        .Less => parser.checker.push_instruction(.Less, &parser.scanner.words),
        .Plus => parser.checker.push_instruction(.Add, &parser.scanner.words),
        .Dash => parser.checker.push_instruction(.Subtract, &parser.scanner.words),
        .Star => parser.checker.push_instruction(.Multiply, &parser.scanner.words),
        .Slash => parser.checker.push_instruction(.Divide, &parser.scanner.words),
        else => {
            parser.error_buffer.extend("operator \"");
            parser.error_buffer.extend(operator.to_string());
            parser.error_buffer.extend("\", cannot be binary\n");
        },
    }
}

fn literal(parser: *Parser) void {
    const keyword = parser.previous.value.keyword;

    switch (keyword) {
        .False => parser.checker.push_boolean(false),
        .True => parser.checker.push_boolean(true),
        else => {
            parser.error_buffer.extend("keyword \"");
            parser.error_buffer.extend(keyword.to_string());
            parser.error_buffer.extend("\", cannot be value\n");
        },
    }
}

fn identifier(parser: *Parser) void {
    const range = parser.previous.value.identifier;

    if (parser.match(Token.PARENTESISLEFT)) {
        call(parser);
        parser.checker.push_range(range);
        parser.checker.push_instruction(.Call, &parser.scanner.words);
    } else {
        parser.checker.push_identifier(range);
    }
}

fn number(parser: *Parser) void {
    parser.checker.push_number(parser.previous.value.number);
}

fn procedure(parser: *Parser) void {
    const iden = parser.current;

    parser.consume(Token.IDEN);
    parser.consume(Token.PARENTESISLEFT);

    while (!parser.match(Token.PARENTESISRIGHT)) {
        _ = parser.match(Token.COMMA);

        const param = parser.current;

        parser.consume(Token.IDEN);
        parser.consume(Token.DOUBLECOLON);

        const kind = parser.current;

        parser.consume(Token.IDEN);

        parser.checker.push_range(param.value.identifier);
        parser.checker.push_range(kind.value.identifier);
        parser.checker.push_instruction(.Parameter, &parser.scanner.words);
    }

    parser.consume(Token.DOUBLECOLON);

    const ret_iden = parser.current;

    parser.consume(Token.IDEN);
    parser.consume(Token.BRACELEFT);

    while (!parser.match(Token.BRACERIGHT)) {
        statement(parser);
    }

    parser.checker.push_range(iden.value.identifier);
    parser.checker.push_range(ret_iden.value.identifier);
    parser.checker.push_instruction(.Procedure, &parser.scanner.words);

    parser.generator.write_procedure(
        iden.value.identifier,
        parser.checker.procedures.last(),
        parser.checker.types.content(),
        null,
        &parser.scanner.words,
    );
}
// self: *Generator,
// name_range: Range,
// procedure: Procedure,
// types: []const Range,
// constant: ?Constant,
// words: *const Vec(u8),

fn statement(parser: *Parser) void {
    const token = parser.current;

    switch (token.kind) {
        .keyword => {
            parser.advance();

            switch (token.value.keyword) {
                .Let => variable(parser),
                .Procedure => procedure(parser),
                .Type => typ(parser),
                else => {
                    parser.error_buffer.extend("keyword \"");
                    parser.error_buffer.extend(token.value.keyword.to_string());
                    parser.error_buffer.extend("\" cannot be statement predecesor\n");
                },
            }
        },
        else => expression(parser),
    }
}

fn call(parser: *Parser) void {
    while (!parser.match(Token.PARENTESISRIGHT)) {
        _ = parser.match(Token.COMMA);

        expression(parser);
    }
}

fn variable(parser: *Parser) void {
    const mut = parser.match(Token.MUT);
    const iden = parser.current;

    parser.consume(Token.IDEN);
    parser.consume(Token.DOUBLECOLON);

    const kind = parser.current;

    parser.consume(Token.IDEN);
    parser.consume(Token.EQUAL);

    expression(parser);

    parser.consume(Token.SEMICOLON);

    if (mut) {}

    parser.checker.push_range(iden.value.identifier);
    parser.checker.push_range(kind.value.identifier);
    parser.checker.push_instruction(.Let, &parser.scanner.words);

    util.print("writing let\n", .{});

    parser.generator.write_let(
        parser.scanner.words.range(iden.value.identifier),
        parser.scanner.words.range(kind.value.identifier),
        &parser.checker.constants.pop(),
        &parser.scanner.words,
    );
}

fn typ(parser: *Parser) void {
    _ = parser;
}

fn string(parser: *Parser) void {
    _ = parser;
}

// test "Parsing one function" {
//     var arena = Arena.new(allocator.malloc(2));
//     defer arena.deinit();

//     var parser = Parser.new("zig-out/function.lang", &arena);
//     defer parser.deinit();

//     try util.assert(parser.next());

//     var array = [_]Instruction{
//         .Parameter, .Parameter, .Constant, .Constant, .Constant,  .Add,
//         .Constant,  .Add,       .Multiply, .Let,      .Mut,       .Constant,
//         .Constant,  .Add,       .Let,      .Constant, .Procedure,
//     };

//     const expect = Vec(Instruction).from_array(&array);

//     try util.assert(expect.eql(parser.instructions));
//     try util.assert(!parser.next());
// }
