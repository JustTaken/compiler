const lexer = @import("lexer.zig");
const checker = @import("checker.zig");
const mem = @import("mem");
const util = @import("util");
const collections = @import("collections");

const Lexer = lexer.Lexer;
const TypeChecker = checker.TypeChecker;
const Arena = mem.Arena;
const Stream = collections.Stream;
const String = collections.String;
const File = collections.File;

const Token = lexer.Token;
const Keyword = lexer.Keyword;
const Symbol = lexer.Symbol;
const Operator = lexer.Operator;

const Precedence = enum(usize) {
    Nothing,
    Assignment,
    Equality,
    Comparison,
    Term,
    Factor,
    Unary,
    Call,
    Declaration,
    Scope,
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
        return switch (token) {
            .Symbol => |symbol| switch (symbol) {
                .ParentesisLeft => new(Parser.grouping, Parser.call, .Call),
                .CurlyBracketLeft => new(Parser.scope, Parser.construct, .Scope),
                .Dot => new(null, Parser.property, .Call),
                else => new(null, null, .Nothing),
            },
            .Operator => |operator| switch (operator) {
                .Minus => new(Parser.unary, Parser.binary, .Term),
                .Plus => new(null, Parser.binary, .Term),
                .Bang => new(Parser.unary, null, .Equality),
                .BangEqual, .EqualEqual => new(null, Parser.binary, .Equality),
                .Slash, .Star => new(null, Parser.binary, .Factor),
                else => new(null, Parser.binary, .Comparison),
            },
            .Keyword => |keyword| switch (keyword) {
                .Let => new(Parser.let, null, .Declaration),
                else => new(null, null, .Nothing),
            },
            .Number => new(Parser.number, null, .Nothing),
            .Identifier => new(Parser.identifier, null, .Nothing),
            .String => new(Parser.string, null, .Nothing),
            .Eof => new(null, null, .Nothing),
        };
    }
};

pub const Parser = struct {
    lexer: Lexer,
    checker: TypeChecker,
    arena: *Arena,

    pub fn new(input: Stream, allocator: *Arena) error{OutOfMemory}!Parser {
        var self: Parser = undefined;

        self.arena = try allocator.child("Parser", mem.PAGE_SIZE * 3);
        errdefer self.arena.deinit();

        self.lexer = try Lexer.new(input, self.arena);
        errdefer self.lexer.deinit();

        self.checker = try TypeChecker.new(self.arena);
        errdefer self.checker.deinit();

        return self;
    }

    pub fn next(self: *Parser) bool {
        if (!self.lexer.match(Token.EOF)) {
            self.lexer.advance();

            switch (self.lexer.previous) {
                .Keyword => |keyword| switch (keyword) {
                    .Procedure => self.procedure(),
                    .Type => self.typ(),
                    else => @panic("TODO: No other statement kind is accepted for now"),
                },
                else => @panic("TODO: do not accept expressions here for now"),
            }

            return true;
        }

        return false;
    }

    fn grouping(self: *Parser) void {
        self.parse(.Assignment);
        self.lexer.consume(Token.PARENTESISRIGHT);
    }

    fn unary(self: *Parser) void {
        const operator = self.lexer.previous.Operator;
        self.parse(.Unary);

        switch (operator) {
            .Bang => self.checker.push_unary(.Bang),
            .Minus => self.checker.push_unary(.Minus),
            else => @panic("TODO"),
        }
    }

    fn binary(self: *Parser) void {
        const rule = Rule.from_token(self.lexer.previous);
        const operator = self.lexer.previous.Operator;

        self.parse(@enumFromInt(@intFromEnum(rule.precedence) + 1));

        switch (operator) {
            .BangEqual => {
                self.checker.push_binary(.Eq);
                self.checker.push_unary(.Bang);
            },
            .GreaterEqual => {
                self.checker.push_binary(.Lt);
                self.checker.push_unary(.Bang);
            },
            .LessEqual => {
                self.checker.push_binary(.Gt);
                self.checker.push_unary(.Bang);
            },
            .EqualEqual => self.checker.push_binary(.Eq),
            .Greater => self.checker.push_binary(.Gt),
            .Less => self.checker.push_binary(.Lt),
            .Plus => self.checker.push_binary(.Add),
            .Minus => self.checker.push_binary(.Sub),
            .Star => self.checker.push_binary(.Mul),
            .Slash => self.checker.push_binary(.Div),
            else => @panic("TODO"),
        }
    }

    fn identifier(self: *Parser) void {
        const name = self.lexer.previous.Identifier;
        self.checker.names.push(name) catch @panic("TODO");

        if (self.lexer.current.eql(Token.PARENTESISLEFT) or self.lexer.current.eql(Token.BRACELEFT)) {} else {
            self.checker.push_identifier();
        }
    }

    fn number(self: *Parser) void {
        self.checker.push_number(self.lexer.previous.Number);
    }

    fn construct(self: *Parser) void {
        var field_count: u32 = 0;

        while (!self.lexer.match(Token.BRACERIGHT)) {
            const name = self.lexer.current;

            self.lexer.consume(Token.IDEN);
            self.lexer.consume(Token.DOUBLECOLON);

            self.parse(.Assignment);
            self.lexer.consume(Token.COMMA);

            self.checker.names.push(name.Identifier) catch @panic("TODO");
            field_count += 1;
        }

        self.checker.push_construct(field_count);
    }

    fn call(self: *Parser) void {
        var argument_count: u32 = 0;

        while (!self.lexer.match(Token.PARENTESISRIGHT)) {
            if (!self.lexer.match(Token.COMMA)) {
                if (!self.lexer.previous.eql(Token.PARENTESISLEFT)) {
                    @panic("TODO");
                }
            }

            self.parse(.Assignment);
            argument_count += 1;
        }

        self.checker.push_call(argument_count);
    }

    fn typ(self: *Parser) void {
        const name = self.lexer.current;
        self.lexer.consume(Token.IDEN);

        var field_count: u32 = 0;
        var size: u32 = 0;

        if (self.lexer.match(Token.BRACELEFT)) {
            while (!self.lexer.match(Token.BRACERIGHT)) {
                const field_name = self.lexer.current;

                self.lexer.consume(Token.IDEN);
                self.lexer.consume(Token.DOUBLECOLON);

                const field_type = self.lexer.current;

                self.lexer.consume(Token.IDEN);
                self.lexer.consume(Token.COMMA);
                self.checker.names.extend(&.{ field_name.Identifier, field_type.Identifier }) catch @panic("TODO");

                field_count += 1;
            }
        } else {
            self.lexer.consume(Token.EQUAL);
            const type_size = self.lexer.current;

            self.lexer.consume(Token.NUMBER);
            self.lexer.consume(Token.SEMICOLON);

            size += @intCast(util.parse(type_size.Number));
        }

        self.checker.names.push(name.Identifier) catch @panic("TODO");
        self.checker.push_type(field_count, size);
    }

    fn property(self: *Parser) void {
        const name = self.lexer.current;
        self.lexer.consume(Token.IDEN);

        self.checker.push_property(name.Identifier);
    }

    fn procedure(self: *Parser) void {
        const name = self.lexer.current;

        self.lexer.consume(Token.IDEN);
        self.lexer.consume(Token.PARENTESISLEFT);

        var parameters_size: u32 = 0;
        var parameter_count: u32 = 0;
        const variable_count: u32 = self.checker.variable_constants.len;

        while (!self.lexer.match(Token.PARENTESISRIGHT)) {
            if (!self.lexer.match(Token.COMMA)) {
                if (!self.lexer.previous.eql(Token.PARENTESISLEFT)) {
                    @panic("TODO");
                }
            }

            const param = self.lexer.current;

            self.lexer.consume(Token.IDEN);
            self.lexer.consume(Token.DOUBLECOLON);

            const kind = self.lexer.current;

            self.lexer.consume(Token.IDEN);
            self.checker.names.extend(&.{ param.Identifier, kind.Identifier }) catch @panic("TODO");

            parameters_size += self.checker.push_parameter(parameters_size);
            parameter_count += 1;
        }

        self.lexer.consume(Token.DOUBLECOLON);

        const return_type = self.lexer.current;

        self.lexer.consume(Token.IDEN);
        self.lexer.consume(Token.BRACELEFT);
        self.scope();

        self.checker.names.extend(&.{ name.Identifier, return_type.Identifier }) catch @panic("TODO");
        self.checker.push_procedure(parameter_count, variable_count);
    }

    fn let(self: *Parser) void {
        const mutable = self.lexer.match(Token.MUT);
        const iden = self.lexer.current;

        _ = mutable;

        self.lexer.consume(Token.IDEN);
        self.lexer.consume(Token.DOUBLECOLON);

        const kind = self.lexer.current;

        self.lexer.consume(Token.IDEN);
        self.lexer.consume(Token.EQUAL);

        self.parse(.Assignment);

        self.lexer.consume(Token.SEMICOLON);
        self.checker.names.extend(&.{ iden.Identifier, kind.Identifier }) catch @panic("TODO");
        self.checker.push_let();
    }

    fn string(self: *Parser) void {
        self.lexer.advance();
        @panic("TODO");
    }

    fn scope(self: *Parser) void {
        var expression_count: u32 = 0;
        var declaration_count: u32 = 0;
        var has_return = false;

        while (!self.lexer.match(Token.BRACERIGHT)) {
            switch (self.lexer.current) {
                Token.Keyword => |keyword| switch (keyword) {
                    Keyword.Let => {
                        self.lexer.advance();
                        self.let();
                        declaration_count += 1;
                    },
                    else => @panic("TODO"),
                },

                else => {
                    self.parse(.Assignment);

                    if (!self.lexer.previous.eql(Token.SEMICOLON)) {
                        if (self.lexer.match(Token.BRACERIGHT)) {
                            has_return = true;

                            break;
                        }
                    }

                    expression_count += 1;
                },
            }
        }

        self.checker.push_scope(expression_count, declaration_count, has_return);
    }

    fn parse(self: *Parser, precedence: Precedence) void {
        self.lexer.advance();

        if (Rule.from_token(self.lexer.previous).prefix) |prefix| {
            prefix(self);
        } else {
            @panic("TODO");
        }

        var rule = Rule.from_token(self.lexer.current);

        while (@intFromEnum(precedence) <= @intFromEnum(rule.precedence)) {
            if (rule.infix) |infix| {
                self.lexer.advance();
                infix(self);
            } else {
                break;
            }

            rule = Rule.from_token(self.lexer.current);
        }
    }

    pub fn compile(self: *Parser, stream: Stream) void {
        self.checker.generate(stream);
    }

    pub fn deinit(self: *Parser) void {
        self.lexer.deinit();
        self.checker.deinit();
        self.arena.deinit();
    }
};

test "basic" {
    var arena = try mem.Arena.new("Testing", 3);
    defer arena.deinit();

    var input_file = try collections.File.open("zig-out/basic.lang");
    var output = try String.new(512, &arena);
    defer output.deinit(&arena);

    const input_stream = input_file.stream();
    const output_stream = collections.string_stream(&output);

    var parser = try Parser.new(input_stream, &arena);
    defer parser.deinit();

    while (parser.next()) {}

    parser.compile(output_stream);
    try util.assert(mem.equal(u8, try output.offset(0), &.{ 127, 69, 76, 70, 2, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 62, 0, 1, 0, 0, 0, 129, 0, 64, 0, 0, 0, 0, 0, 64, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 64, 0, 56, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 5, 0, 0, 0, 120, 0, 0, 0, 0, 0, 0, 0, 120, 0, 64, 0, 0, 0, 0, 0, 120, 0, 64, 0, 0, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 0, 16, 0, 0, 0, 0, 0, 0, 184, 10, 0, 0, 0, 131, 192, 10, 195, 232, 242, 255, 255, 255, 72, 137, 199, 184, 60, 0, 0, 0, 15, 5 }));
}

test "binary operation" {
    var arena = try mem.Arena.new("Testing", 3);
    defer arena.deinit();

    var input_file = try collections.File.open("zig-out/binary.lang");
    var output = try String.new(512, &arena);
    defer output.deinit(&arena);

    const input_stream = input_file.stream();
    const output_stream = collections.string_stream(&output);

    var parser = try Parser.new(input_stream, &arena);
    defer parser.deinit();

    while (parser.next()) {}

    parser.compile(output_stream);
    try util.assert(mem.equal(u8, try output.offset(0), &.{ 127, 69, 76, 70, 2, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 62, 0, 1, 0, 0, 0, 134, 0, 64, 0, 0, 0, 0, 0, 64, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 64, 0, 56, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 5, 0, 0, 0, 120, 0, 0, 0, 0, 0, 0, 0, 120, 0, 64, 0, 0, 0, 0, 0, 120, 0, 64, 0, 0, 0, 0, 0, 14, 0, 0, 0, 0, 0, 0, 0, 14, 0, 0, 0, 0, 0, 0, 0, 0, 16, 0, 0, 0, 0, 0, 0, 191, 10, 0, 0, 0, 131, 199, 10, 72, 137, 248, 1, 248, 195, 232, 237, 255, 255, 255, 72, 137, 199, 184, 60, 0, 0, 0, 15, 5 }));
}

test "function call " {
    var arena = try mem.Arena.new("Testing", 3);
    defer arena.deinit();

    var input_file = try collections.File.open("zig-out/call.lang");
    var output = try String.new(512, &arena);
    defer output.deinit(&arena);

    const input_stream = input_file.stream();
    const output_stream = collections.string_stream(&output);

    var parser = try Parser.new(input_stream, &arena);
    defer parser.deinit();

    while (parser.next()) {}

    parser.compile(output_stream);
    try util.assert(mem.equal(u8, try output.offset(0), &.{ 127, 69, 76, 70, 2, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 62, 0, 1, 0, 0, 0, 135, 0, 64, 0, 0, 0, 0, 0, 64, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 64, 0, 56, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 5, 0, 0, 0, 120, 0, 0, 0, 0, 0, 0, 0, 120, 0, 64, 0, 0, 0, 0, 0, 120, 0, 64, 0, 0, 0, 0, 0, 15, 0, 0, 0, 0, 0, 0, 0, 15, 0, 0, 0, 0, 0, 0, 0, 0, 16, 0, 0, 0, 0, 0, 0, 139, 69, 0, 195, 104, 10, 0, 0, 0, 232, 242, 255, 255, 255, 195, 232, 240, 255, 255, 255, 72, 137, 199, 184, 60, 0, 0, 0, 15, 5 }));
}

test "type return" {
    var arena = try mem.Arena.new("Testing", 3);
    defer arena.deinit();

    var input_file = try collections.File.open("zig-out/type_return.lang");
    var output = try String.new(512, &arena);
    defer output.deinit(&arena);

    var parser = try Parser.new(input_file.stream(), &arena);
    defer parser.deinit();

    while (parser.next()) {}

    parser.compile(collections.string_stream(&output));
    try util.assert(true);
}
