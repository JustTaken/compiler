const lexer = @import("lexer/mod.zig");
const checker = @import("checker/mod.zig");
const mem = @import("mem");
const util = @import("util");
const collections = @import("collections");

const token = lexer.token;

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

    fn from_token(t: token.Token) Rule {
        return switch (t) {
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
            .Number => new(Parser.number, null, .Nothing),
            .Identifier => new(Parser.identifier, null, .Nothing),
            .String => new(Parser.string, null, .Nothing),
            .Keyword => new(null, null, .Nothing),
            .Eof => new(null, null, .Nothing),
        };
    }
};

pub const Parser = struct {
    lexer: lexer.Lexer,
    checker: checker.TypeChecker,
    arena: *mem.Arena,

    pub fn new(input: collections.Stream, allocator: *mem.Arena) error{OutOfMemory}!Parser {
        var self: Parser = undefined;

        self.arena = try allocator.child("Parser", mem.PAGE_SIZE * 3);
        errdefer self.arena.deinit();

        self.lexer = try lexer.Lexer.new(input, self.arena);
        errdefer self.lexer.deinit();

        self.checker = try checker.TypeChecker.new(self.arena);
        errdefer self.checker.deinit();

        return self;
    }

    pub fn next(self: *Parser) bool {
        if (self.lexer.match(token.Token.EOF)) {
            return false;
        }

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

    fn grouping(self: *Parser) void {
        self.parse(.Assignment);
        self.lexer.consume(token.Token.PARENTESISRIGHT);
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

        if (self.lexer.current.eql(token.Token.PARENTESISLEFT) or self.lexer.current.eql(token.Token.BRACELEFT)) {} else {
            self.checker.push_identifier();
        }
    }

    fn number(self: *Parser) void {
        self.checker.push_number(self.lexer.previous.Number);
    }

    fn construct(self: *Parser) void {
        var field_count: u32 = 0;

        while (!self.lexer.match(token.Token.BRACERIGHT)) {
            const name = self.lexer.current;

            self.lexer.consume(token.Token.IDEN);
            self.lexer.consume(token.Token.DOUBLECOLON);

            self.parse(.Assignment);
            self.lexer.consume(token.Token.COMMA);

            self.checker.names.push(name.Identifier) catch @panic("TODO");
            field_count += 1;
        }

        self.checker.push_construct(field_count);
    }

    fn call(self: *Parser) void {
        var argument_count: u32 = 0;

        while (!self.lexer.match(token.Token.PARENTESISRIGHT)) {
            if (!self.lexer.match(token.Token.COMMA) and !self.lexer.previous.eql(token.Token.PARENTESISLEFT)) {
                @panic("TODO: Show that user cannot place a comma at the start of the type constructor");
            }

            self.parse(.Assignment);
            argument_count += 1;
        }

        self.checker.push_call(argument_count);
    }

    fn typ(self: *Parser) void {
        const name = self.lexer.current;
        self.lexer.consume(token.Token.IDEN);

        var field_count: u32 = 0;
        var size: u32 = 0;

        if (self.lexer.match(token.Token.BRACELEFT)) {
            while (!self.lexer.match(token.Token.BRACERIGHT)) {
                const field_name = self.lexer.current;

                self.lexer.consume(token.Token.IDEN);
                self.lexer.consume(token.Token.DOUBLECOLON);

                const field_type = self.lexer.current;

                self.lexer.consume(token.Token.IDEN);
                self.lexer.consume(token.Token.COMMA);
                self.checker.names.extend(&.{ field_name.Identifier, field_type.Identifier }) catch @panic("TODO");

                field_count += 1;
            }
        } else {
            self.lexer.consume(token.Token.EQUAL);
            const type_size = self.lexer.current;

            self.lexer.consume(token.Token.NUMBER);
            self.lexer.consume(token.Token.SEMICOLON);

            size += @intCast(util.parse(type_size.Number));
        }

        self.checker.names.push(name.Identifier) catch @panic("TODO");
        self.checker.push_type(field_count, size);
    }

    fn property(self: *Parser) void {
        const name = self.lexer.current;

        self.lexer.consume(token.Token.IDEN);
        self.checker.push_property(name.Identifier);
    }

    fn procedure(self: *Parser) void {
        const name = self.lexer.current;

        self.lexer.consume(token.Token.IDEN);
        self.lexer.consume(token.Token.PARENTESISLEFT);

        var parameters_size: u32 = 0;
        var parameter_count: u32 = 0;
        const variable_count: u32 = self.checker.variable_constants.len;

        while (!self.lexer.match(token.Token.PARENTESISRIGHT)) {
            if (!self.lexer.match(token.Token.COMMA) and !self.lexer.previous.eql(token.Token.PARENTESISLEFT)) {
                @panic("TODO: Parameters should start with parentesis and the first element is a identifier");
            }

            const param = self.lexer.current;

            self.lexer.consume(token.Token.IDEN);
            self.lexer.consume(token.Token.DOUBLECOLON);

            const kind = self.lexer.current;

            self.lexer.consume(token.Token.IDEN);
            self.checker.names.extend(&.{ param.Identifier, kind.Identifier }) catch @panic("TODO");

            parameters_size += self.checker.push_parameter(parameters_size);
            parameter_count += 1;
        }

        self.lexer.consume(token.Token.DOUBLECOLON);

        const return_type = self.lexer.current;

        self.lexer.consume(token.Token.IDEN);
        self.lexer.consume(token.Token.BRACELEFT);
        self.scope();

        self.checker.names.extend(&.{ name.Identifier, return_type.Identifier }) catch @panic("TODO");
        self.checker.push_procedure(parameter_count, variable_count);
    }

    fn let(self: *Parser) void {
        const mutable = self.lexer.match(token.Token.MUT);
        const iden = self.lexer.current;

        _ = mutable;

        self.lexer.consume(token.Token.IDEN);
        self.lexer.consume(token.Token.DOUBLECOLON);

        const kind = self.lexer.current;

        self.lexer.consume(token.Token.IDEN);
        self.lexer.consume(token.Token.EQUAL);

        self.parse(.Assignment);

        self.lexer.consume(token.Token.SEMICOLON);
        self.checker.names.extend(&.{ iden.Identifier, kind.Identifier }) catch @panic("TODO");
        self.checker.push_let();
    }

    fn string(self: *Parser) void {
        _ = self;
        @panic("TODO");
    }

    fn scope(self: *Parser) void {
        var expression_count: u32 = 0;
        var declaration_count: u32 = 0;
        var has_return = false;

        while (!self.lexer.match(token.Token.BRACERIGHT)) {
            switch (self.lexer.current) {
                .Keyword => |keyword| switch (keyword) {
                    .Let => {
                        self.lexer.advance();
                        self.let();
                        declaration_count += 1;
                    },
                    else => @panic("TODO"),
                },

                else => {
                    self.parse(.Assignment);

                    if (!self.lexer.previous.eql(token.Token.SEMICOLON)) {
                        if (self.lexer.match(token.Token.BRACERIGHT)) {
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
            @panic("TODO: Show error");
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

    pub fn compile(self: *Parser, stream: collections.Stream) void {
        self.checker.generate(stream);
    }

    pub fn deinit(self: *Parser) void {
        self.lexer.deinit();
        self.checker.deinit();
        self.arena.deinit();
    }
};

test "Lexer" {
    _ = lexer;
}

// test "basic" {
//     var arena = try mem.Arena.new("Testing", 3);
//     defer arena.deinit();

//     var input_file = try collections.File.open("zig-out/basic.lang");
//     var output = try String.new(512, &arena);
//     defer output.deinit(&arena);

//     const input_stream = input_file.stream();
//     const output_stream = collections.string_stream(&output);

//     var parser = try Parser.new(input_stream, &arena);
//     defer parser.deinit();

//     while (parser.next()) {}

//     parser.compile(output_stream);
//     try util.assert(mem.equal(u8, try output.offset(0), &.{ 127, 69, 76, 70, 2, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 62, 0, 1, 0, 0, 0, 129, 0, 64, 0, 0, 0, 0, 0, 64, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 64, 0, 56, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 5, 0, 0, 0, 120, 0, 0, 0, 0, 0, 0, 0, 120, 0, 64, 0, 0, 0, 0, 0, 120, 0, 64, 0, 0, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 0, 16, 0, 0, 0, 0, 0, 0, 184, 10, 0, 0, 0, 131, 192, 10, 195, 232, 242, 255, 255, 255, 72, 137, 199, 184, 60, 0, 0, 0, 15, 5 }));
// }

// test "binary operation" {
//     var arena = try mem.Arena.new("Testing", 3);
//     defer arena.deinit();

//     var input_file = try collections.File.open("zig-out/binary.lang");
//     var output = try String.new(512, &arena);
//     defer output.deinit(&arena);

//     const input_stream = input_file.stream();
//     const output_stream = collections.string_stream(&output);

//     var parser = try Parser.new(input_stream, &arena);
//     defer parser.deinit();

//     while (parser.next()) {}

//     parser.compile(output_stream);
//     try util.assert(mem.equal(u8, try output.offset(0), &.{ 127, 69, 76, 70, 2, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 62, 0, 1, 0, 0, 0, 134, 0, 64, 0, 0, 0, 0, 0, 64, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 64, 0, 56, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 5, 0, 0, 0, 120, 0, 0, 0, 0, 0, 0, 0, 120, 0, 64, 0, 0, 0, 0, 0, 120, 0, 64, 0, 0, 0, 0, 0, 14, 0, 0, 0, 0, 0, 0, 0, 14, 0, 0, 0, 0, 0, 0, 0, 0, 16, 0, 0, 0, 0, 0, 0, 191, 10, 0, 0, 0, 131, 199, 10, 72, 137, 248, 1, 248, 195, 232, 237, 255, 255, 255, 72, 137, 199, 184, 60, 0, 0, 0, 15, 5 }));
// }

// test "function call " {
//     var arena = try mem.Arena.new("Testing", 3);
//     defer arena.deinit();

//     var input_file = try collections.File.open("zig-out/call.lang");
//     var output = try String.new(512, &arena);
//     defer output.deinit(&arena);

//     const input_stream = input_file.stream();
//     const output_stream = collections.string_stream(&output);

//     var parser = try Parser.new(input_stream, &arena);
//     defer parser.deinit();

//     while (parser.next()) {}

//     parser.compile(output_stream);
//     try util.assert(mem.equal(u8, try output.offset(0), &.{ 127, 69, 76, 70, 2, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 62, 0, 1, 0, 0, 0, 135, 0, 64, 0, 0, 0, 0, 0, 64, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 64, 0, 56, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 5, 0, 0, 0, 120, 0, 0, 0, 0, 0, 0, 0, 120, 0, 64, 0, 0, 0, 0, 0, 120, 0, 64, 0, 0, 0, 0, 0, 15, 0, 0, 0, 0, 0, 0, 0, 15, 0, 0, 0, 0, 0, 0, 0, 0, 16, 0, 0, 0, 0, 0, 0, 139, 69, 0, 195, 104, 10, 0, 0, 0, 232, 242, 255, 255, 255, 195, 232, 240, 255, 255, 255, 72, 137, 199, 184, 60, 0, 0, 0, 15, 5 }));
// }

// test "type return" {
//     var arena = try mem.Arena.new("Testing", 3);
//     defer arena.deinit();

//     var input_file = try collections.File.open("zig-out/type_return.lang");
//     var output = try String.new(512, &arena);
//     defer output.deinit(&arena);

//     var parser = try Parser.new(input_file.stream(), &arena);
//     defer parser.deinit();

//     while (parser.next()) {}

//     parser.compile(collections.string_stream(&output));
//     try util.assert(true);
// }
