const lexer = @import("lexer.zig");
const checker = @import("checker.zig");
const mem = @import("mem");
const util = @import("util");
const collections = @import("collections");

const Lexer = lexer.Lexer;
const TypeChecker = checker.TypeChecker;
const Arena = mem.Arena;
const Stream = collections.Stream;
const File = collections.File;

const Token = lexer.Token;
const Keyword = lexer.Keyword;
const Symbol = lexer.Symbol;
const Operator = lexer.Operator;

const Precedence = enum(usize) {
    Nothing,
    Assignment,
    Or,
    And,
    Equality,
    Comparison,
    Term,
    Factor,
    Unary,
    Property,
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
        return switch (token) {
            .Symbol => |symbol| switch (symbol) {
                .ParentesisLeft => new(Parser.grouping, null, .Assignment),
                .CurlyBraceLeft => new(Parser.scope, null, .Assignment),
                .Dot => new(null, Parser.property, .Property),
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
                else => new(null, null, .Nothing),
            },
            .Number => new(Parser.number, null, .Nothing),
            .Identifier => new(Parser.identifier, null, .Nothing),
            .String => new(Parser.string, null, .Nothing),
            .Eof => @panic("Should not happen"),
        };
    }
};

pub const Parser = struct {
    lexer: Lexer,
    checker: TypeChecker,
    arena: *Arena,

    pub fn new(input: []const u8, output: []const u8, allocator: *Arena) Parser {
        var arena = allocator.child("Parser", mem.PAGE_SIZE * 2);

        var input_file = arena.create(File, File.open(input) catch @panic("File not found"));
        var output_file = arena.create(File, File.create(output) catch @panic("File not found"));

        const input_stream = input_file.stream();
        const output_stream = output_file.stream();

        return Parser{
            .lexer = Lexer.new(input_stream, arena),
            .checker = TypeChecker.new(output_stream, arena),
            .arena = arena,
        };
    }

    pub fn next(self: *Parser) bool {
        if (!self.lexer.match(Token.EOF)) {
            self.statement();
            return true;
        }

        return false;
    }

    fn grouping(self: *Parser) void {
        self.expression();
        self.lexer.consume(Token.PARENTESISRIGHT);
    }

    fn unary(self: *Parser) void {
        const operator = self.lexer.previous.Operator;
        self.parse(.Unary);

        switch (operator) {
            .Bang => self.checker.push_unary(.Bang),
            .Minus => self.checker.push_unary(.Minus),
            else => @panic("Should not happe"),
        }
    }

    fn binary(self: *Parser) void {
        const rule = Rule.from_token(self.lexer.previous);
        const operator = self.lexer.previous.Operator;

        self.parse(@enumFromInt(@intFromEnum(rule.precedence) + 1));
        const words = &self.lexer.words;

        switch (operator) {
            .BangEqual => {
                self.checker.push_binary(.Eq, words);
                self.checker.push_unary(.Bang);
            },
            .GreaterEqual => {
                self.checker.push_binary(.Lt, words);
                self.checker.push_unary(.Bang);
            },
            .LessEqual => {
                self.checker.push_binary(.Gt, words);
                self.checker.push_unary(.Bang);
            },
            .EqualEqual => self.checker.push_binary(.Eq, words),
            .Greater => self.checker.push_binary(.Gt, words),
            .Less => self.checker.push_binary(.Lt, words),
            .Plus => self.checker.push_binary(.Add, words),
            .Minus => self.checker.push_binary(.Sub, words),
            .Star => self.checker.push_binary(.Mul, words),
            .Slash => self.checker.push_binary(.Div, words),
            else => @panic("Should not happen"),
        }
    }

    fn identifier(self: *Parser) void {
        const range = self.lexer.previous.Identifier;
        self.checker.ranges.push(range);

        if (self.lexer.match(Token.PARENTESISLEFT)) {
            self.call();
        } else if (self.lexer.match(Token.BRACELEFT)) {
            self.construct();
        } else {
            self.checker.push_identifier(&self.lexer.words);
        }
    }

    fn number(self: *Parser) void {
        const range = self.lexer.previous.Number;

        self.checker.push_number(range, &self.lexer.words);
    }

    fn construct(self: *Parser) void {
        var field_count: u32 = 0;

        while (!self.lexer.match(Token.BRACERIGHT)) {
            const name = self.lexer.current;

            self.lexer.consume(Token.IDEN);
            self.lexer.consume(Token.DOUBLECOLON);

            self.expression();
            self.lexer.consume(Token.COMMA);

            self.checker.ranges.push(name.Identifier);
            field_count += 1;
        }

        self.checker.push_construct(field_count, &self.lexer.words);
    }

    fn call(self: *Parser) void {
        var argument_count: u32 = 0;

        while (!self.lexer.match(Token.PARENTESISRIGHT)) {
            if (!self.lexer.match(Token.COMMA)) {
                if (!self.lexer.previous.eql(Token.PARENTESISLEFT)) {
                    @panic("Should not happen");
                }
            }

            self.expression();
            argument_count += 1;
        }

        self.checker.push_call(argument_count, &self.lexer.words);
    }

    fn typ(self: *Parser) void {
        self.lexer.advance();

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
                self.checker.ranges.extend(&.{ field_name.Identifier, field_type.Identifier });

                field_count += 1;
            }
        } else {
            self.lexer.consume(Token.EQUAL);
            const type_size = self.lexer.current;

            self.lexer.consume(Token.NUMBER);
            self.lexer.consume(Token.SEMICOLON);

            size += @intCast(util.parse(self.lexer.words.range(type_size.Number)));
        }

        self.checker.ranges.push(name.Identifier);
        self.checker.push_type(field_count, size, &self.lexer.words);
    }

    fn property(self: *Parser) void {
        const name = self.lexer.current;
        self.lexer.consume(Token.IDEN);

        self.checker.push_property(name.Identifier, &self.lexer.words);
    }

    fn procedure(self: *Parser) void {
        self.lexer.advance();

        const name = self.lexer.current;

        self.lexer.consume(Token.IDEN);
        self.lexer.consume(Token.PARENTESISLEFT);

        var parameters_size: u32 = 0;
        var parameter_count: u32 = 0;
        const variable_count: u32 = self.checker.variable_constants.len;

        while (!self.lexer.match(Token.PARENTESISRIGHT)) {
            if (!self.lexer.match(Token.COMMA)) {
                if (!self.lexer.previous.eql(Token.PARENTESISLEFT)) {
                    @panic("Should not happen");
                }
            }

            const param = self.lexer.current;

            self.lexer.consume(Token.IDEN);
            self.lexer.consume(Token.DOUBLECOLON);

            const kind = self.lexer.current;

            self.lexer.consume(Token.IDEN);
            self.checker.ranges.extend(&.{ param.Identifier, kind.Identifier });

            parameters_size += self.checker.push_parameter(parameters_size, &self.lexer.words);
            parameter_count += 1;
        }

        self.lexer.consume(Token.DOUBLECOLON);

        const return_type = self.lexer.current;

        self.lexer.consume(Token.IDEN);
        self.lexer.consume(Token.BRACELEFT);
        self.scope();

        self.checker.ranges.extend(&.{ name.Identifier, return_type.Identifier });
        self.checker.push_procedure(parameter_count, variable_count, &self.lexer.words);
    }

    fn let(self: *Parser) void {
        self.lexer.advance();

        const mutable = self.lexer.match(Token.MUT);
        const iden = self.lexer.current;

        _ = mutable;

        self.lexer.consume(Token.IDEN);
        self.lexer.consume(Token.DOUBLECOLON);

        const kind = self.lexer.current;

        self.lexer.consume(Token.IDEN);
        self.lexer.consume(Token.EQUAL);

        self.expression();

        self.lexer.consume(Token.SEMICOLON);
        self.checker.ranges.extend(&.{ iden.Identifier, kind.Identifier });
        self.checker.push_let(&self.lexer.words);
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
                        self.let();
                        declaration_count += 1;
                    },
                    else => @panic("Here we do not accept this kind of statement"),
                },

                else => {
                    self.expression();

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
            @panic("Should not happen");
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

    fn expression(self: *Parser) void {
        self.parse(.Assignment);
    }

    fn statement(self: *Parser) void {
        switch (self.lexer.current) {
            Token.Keyword => |keyword| switch (keyword) {
                Keyword.Let => self.let(),
                Keyword.Procedure => self.procedure(),
                Keyword.Type => self.typ(),
                else => @panic("TODO"),
            },
            else => self.expression(),
        }
    }

    pub fn compile(self: *Parser) void {
        self.checker.generate(&self.lexer.words);
    }

    pub fn deinit(self: *Parser) void {
        self.lexer.deinit();
        self.checker.deinit();
        self.arena.destroy(File, 1);
        self.arena.destroy(File, 1);
        self.arena.deinit();
    }
};

test "basic" {
    const generator = @import("generator.zig");
    const buffer = mem.malloc(2);
    defer mem.free(buffer);

    var arena = mem.Arena.new(buffer);
    var parser = Parser.new("zig-out/basic.lang", "zig-out/out", &arena);

    const operations: []const []const generator.Operation = &.{ &.{}, &.{}, &.{
        .{ .Binary = .{
            .kind = .Mov,
            .destination = .{ .Register = .Rax },
            .source = .{ .Immediate = 10 },
        } },
        .{ .Binary = .{
            .kind = .Add,
            .destination = .{ .Register = .Rax },
            .source = .{ .Immediate = 10 },
        } },
    } };

    var node: usize = 0;

    while (parser.next()) {
        try util.assert(operations[node].len == parser.checker.generator.operations.len);

        for (parser.checker.generator.operations.offset(0), 0..) |operation, i| {
            try util.assert(operation.equal(operations[node][i]));
        }

        node += 1;
    }

    parser.deinit();
}

test "binary operation" {
    const generator = @import("generator.zig");
    const buffer = mem.malloc(2);
    defer mem.free(buffer);

    var arena = mem.Arena.new(buffer);
    var parser = Parser.new("zig-out/binary.lang", "zig-out/out", &arena);

    const operations: []const []const generator.Operation = &.{ &.{}, &.{}, &.{
        .{ .Binary = .{
            .kind = .Mov,
            .destination = .{ .Register = .Rdi },
            .source = .{ .Immediate = 10 },
        } },
        .{ .Binary = .{
            .kind = .Add,
            .destination = .{ .Register = .Rdi },
            .source = .{ .Immediate = 10 },
        } },
        .{ .Binary = .{
            .kind = .Mov,
            .destination = .{ .Register = .Rax },
            .source = .{ .Register = .Rdi },
        } },
        .{ .Binary = .{
            .kind = .Add,
            .destination = .{ .Register = .Rax },
            .source = .{ .Register = .Rdi },
        } },
    } };

    var node: usize = 0;

    while (parser.next()) {
        try util.assert(operations[node].len == parser.checker.generator.operations.len);

        for (parser.checker.generator.operations.offset(0), 0..) |operation, i| {
            try util.assert(operation.equal(operations[node][i]));
        }

        node += 1;
    }

    parser.deinit();
}

test "function call " {
    const generator = @import("generator.zig");
    const buffer = mem.malloc(2);
    defer mem.free(buffer);

    var arena = mem.Arena.new(buffer);
    var parser = Parser.new("zig-out/call.lang", "zig-out/out", &arena);

    const operations: []const []const generator.Operation = &.{ &.{}, &.{}, &.{
        .{ .Binary = .{
            .kind = .Mov,
            .destination = .{ .Register = .Rax },
            .source = .{ .Memory = .{ .register = .Rbp, .offset = 0 } },
        } },
    }, &.{
        .{ .Binary = .{
            .kind = .Mov,
            .destination = .{ .Stack = {} },
            .source = .{ .Immediate = 10 },
        } },
        .{ .Call = 0 },
    } };

    var node: usize = 0;

    while (parser.next()) {
        try util.assert(operations[node].len == parser.checker.generator.operations.len);

        for (parser.checker.generator.operations.offset(0), 0..) |operation, i| {
            try util.assert(operation.equal(operations[node][i]));
        }

        node += 1;
    }

    parser.deinit();
}
