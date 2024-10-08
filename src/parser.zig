const util = @import("util/mod.zig");
const allocator = @import("allocator/mod.zig");
const checker = @import("checker.zig");
const collections = @import("collections/mod.zig");

const Arena = allocator.Arena;
const Range = util.Range;
const Vec = collections.Vec;
const Constant = checker.Constant;
const TypeChecker = checker.TypeChecker;
const Instruction = checker.Instruction;
const Scanner = @import("scanner.zig").Scanner;
const Token = @import("scanner.zig").Token;
const Generator = @import("generator.zig").Generator;

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
                    .Greater, .GreaterEqual, .Less, .LessEqual => return new(
                        null,
                        binary,
                        .Comparison,
                    ),
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
    has_error: bool,
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
            .has_error = false,
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
            self.error_buffer.mult_extend(&.{
                "Expected \"",
                token.to_string(),
                "\", found \"",
                self.current.to_string(),
                "\"\n",
            });
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
        } else return;

        var rule = Rule.from_token(self.current);
        while (@intFromEnum(precedence) <= @intFromEnum(rule.precedence)) {
            if (rule.infix) |f| {
                self.advance();
                f(self);
            } else break;

            rule = Rule.from_token(self.current);
        }
    }

    fn show_error(self: *Parser) void {
        if (self.error_buffer.len > 0) {
            util.print("{s}", .{self.error_buffer.offset(0)});
            self.error_buffer.clear();
        }
        self.has_error = true;
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
            parser.error_buffer.mult_extend(&.{
                "operator \"",
                operator.to_string(),
                "\" cannot be interpreted as unary\n",
            });
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
            parser.error_buffer.mult_extend(&.{
                "operator \"",
                operator.to_string(),
                "\", cannot be binary\n",
            });
        },
    }
}

fn literal(parser: *Parser) void {
    const keyword = parser.previous.value.keyword;

    switch (keyword) {
        .False => parser.checker.push_boolean(false),
        .True => parser.checker.push_boolean(true),
        else => {
            parser.error_buffer.mult_extend(&.{
                "keyword \"",
                keyword.to_string(),
                "\", cannot be value\n",
            });
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
        parser.checker.push_identifier(range, &parser.scanner.words);
    }
}

fn number(parser: *Parser) void {
    parser.checker.push_number(parser.previous.value.number);
}

fn procedure(parser: *Parser) void {
    const iden = parser.current;

    parser.checker.push_scope();

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

    if (parser.error_buffer.len == 0 and !parser.has_error) {
        const last_constant: ?Constant = if (parser.checker.constants.len > 0)
            parser.checker.constants.pop()
        else
            null;

        parser.generator.write_procedure(
            iden.value.identifier,
            parser.checker.procedures.last(),
            parser.checker.types.content(),
            last_constant,
            &parser.scanner.words,
        );
    } else {
        parser.show_error();
    }

    parser.checker.pop_scope();
    parser.checker.clear();
    parser.generator.clear();
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
                .Match => case(parser),
                else => {
                    parser.error_buffer.mult_extend(&.{
                        "keyword \"",
                        token.value.keyword.to_string(),
                        "\" cannot be statement predecesor\n",
                    });
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

    if (parser.error_buffer.len == 0 and !parser.has_error) {
        const last_constant = parser.checker.constants.pop();

        if (!last_constant.is_raw()) {
            parser.generator.write_let(
                parser.scanner.words.range(iden.value.identifier),
                parser.scanner.words.range(kind.value.identifier),
                parser.checker.types.offset(0),
                &last_constant,
                &parser.scanner.words,
            );
        }
    } else {
        parser.show_error();
    }
}

const CaseMatch = enum {
    Boolean,
    Identifier,
    Number,
};

fn case(parser: *Parser) void {
    expression(parser);
    parser.consume(Token.BRACELEFT);

    while (!parser.match(Token.BRACERIGHT)) {
        parser.checker.push_scope();
        parser.consume(Token.OF);

        const iden = parser.current;

        const match_on: CaseMatch = switch (iden.kind) {
            .keyword => .Boolean,
            .identifier => .Identifier,
            .number => .Number,
            else => unreachable,
        };

        parser.advance();
        parser.consume(Token.ARROW);

        expression(parser);

        parser.consume(Token.COMMA);

        switch (match_on) {
            .Boolean => parser.checker.push_boolean(iden.eql(Token.TRUE)),
            .Number => parser.checker.push_number(iden.value.number),
            .Identifier => parser.checker.push_identifier(
                iden.value.identifier,
                &parser.scanner.words,
            ),
        }

        parser.checker.push_instruction(.Case, &parser.scanner.words);
        parser.checker.pop_scope();
    }
}

fn typ(parser: *Parser) void {
    _ = parser;
}

fn string(parser: *Parser) void {
    _ = parser;
}
