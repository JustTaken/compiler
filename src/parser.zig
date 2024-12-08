const mem = @import("mem");
const util = @import("util");
const collections = @import("collections");

const generator = @import("generator/mod.zig");
const checker = @import("checker/mod.zig");
const node = @import("node/mod.zig");
const lexer = @import("lexer/mod.zig");
const token = lexer.token;

const ARGUMENT_MAX: u32 = 20;

pub const Precedence = enum(usize) {
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

const RuleKind = enum {
    ParentesisLeft,
    CurlyBracketLeft,
    Dot,
    Minus, Plus, Bang, Equality,
    Factor,
    Comparison,
    Identifier,
    String,
    Number,

    fn init_buffer(buffer: *collections.Vec(Rule)) error{OutOfBounds}!void {
        const data = [_] Rule {
            Rule.new(Parser.group, Parser.call, .Call),
            Rule.new(Parser.scope, Parser.construct, .Scope),
            Rule.new(null, Parser.property, .Call),

            Rule.new(Parser.unary, Parser.binary, .Term),
            Rule.new(null, Parser.binary, .Term),
            Rule.new(Parser.unary, null, .Equality),
            Rule.new(null, Parser.binary, .Equality),
            Rule.new(null, Parser.binary, .Factor),
            Rule.new(null, Parser.binary, .Comparison),

            Rule.new(Parser.identifier, null, .Nothing),
            Rule.new(Parser.string, null, .Nothing),
            Rule.new(Parser.number, null, .Nothing),
        };

        for (data) |f| {
            try buffer.push(f);
        }
    }

    fn from_token(t: token.Token) usize {
        const kind: RuleKind = switch (t) {
            .Symbol => |symbol| switch (symbol) {
                .ParentesisLeft => .ParentesisLeft,
                .CurlyBracketLeft => .CurlyBracketLeft,
                .Dot => .Dot,
                else => @panic("TODO"),
            },
            .Operator => |operator| switch (operator) {
                .Minus => .Minus,
                .Plus => .Plus,
                .Bang => .Bang,
                .BangEqual, .EqualEqual => .Equality,
                .Slash, .Star => .Factor,
                else => @panic("TODO"),
            },
            .Identifier => .Identifier,
            .Number => .Number,
            .String => .String,
            .Keyword => @panic("TODO"),
            .Eof => @panic("TODO"),
        };

        return @intFromEnum(kind);
    }
};

const Fn = *const fn (parser: *Parser, lexer: *lexer.Lexer) void;

pub const Rule = struct {
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
};

pub const Parser = struct {
    rules: collections.Vec(Rule),
    nodes: collections.Vec(node.Node),
    arena: *mem.Arena,

    pub fn new(allocator: *mem.Arena) error{OutOfBounds, OutOfMemory}!Parser {
        var self = Parser {
            .rules = undefined,
            .nodes = undefined,
            .arena = try allocator.child("Parser", mem.PAGE_SIZE * 2),
        };

        errdefer self.arena.deinit();

        self.nodes = try collections.Vec(node.Node).new(5, self.arena);
        errdefer self.nodes.deinit(self.arena);

        self.rules = try collections.Vec(Rule).new(@typeInfo(RuleKind).@"enum".fields.len, self.arena);
        errdefer self.rules.deinit(self.arena);

        try RuleKind.init_buffer(&self.rules);

        return self;
    }

    pub fn next(self: *Parser, tokenizer: *lexer.Lexer) ?node.Node {
        if (self.nodes.len > 0) {
            const n = self.nodes.pop() catch @panic("TODO");
            n.deinit(self.arena);
        }

        const tok = tokenizer.next() orelse return null;

        switch (tok) {
            .Keyword => |keyword| switch (keyword) {
                .Procedure => self.procedure(),
                .Type => self.typ(),
                else => @panic("TODO: No other statement kind is accepted for now"),
            },
            else => @panic("TODO: do not accept expressions here for now"),
        }

        util.assert(self.nodes.len == 1);
        return self.nodes.items[0];
    }

    fn group(self: *Parser, tokenizer: *lexer.Lexer) void {
        self.parse(.Assignment, tokenizer);
        if (!tokenizer.match(token.Token.PARENTESISRIGHT)) @panic("TODO");
    }

    fn unary(self: *Parser, tokenizer: *lexer.Lexer) void {
        const operator = tokenizer.previous.Operator;
        self.parse(.Unary, tokenizer);

        switch (operator) {
            .Bang => self.nodes.push(node.Node { .Unary = self.arena.create(node.Unary, node.Unary.new(self.nodes.pop() catch @panic("TODO"), .Bang)) catch @panic("TODO")}) catch @panic("TODO"),
            .Minus => self.nodes.push(node.Node { .Unary = self.arena.create(node.Unary, node.Unary.new(self.nodes.pop() catch @panic("TODO"), .Negate)) catch @panic("TODO")}) catch @panic("TODO"),
            else => @panic("TODO"),
        }
    }

    fn binary(self: *Parser, tokenizer: *lexer.Lexer) void {
        const operator = tokenizer.previous.Operator;

        self.parse(@enumFromInt(@intFromEnum(self.rules.items[RuleKind.from_token(tokenizer.previous)].precedence) + 1), tokenizer);

        switch (operator) {
            .BangEqual => {
                const right = self.nodes.pop() catch @panic("TODO");
                const left = self.nodes.pop() catch @panic("TODO");

                const eq = node.Node{ .Binary = self.arena.create(node.Binary, node.Binary.new(left, right, .Eq)) catch @panic("TODO")};
                self.nodes.push(node.Node { .Unary = self.arena.create(node.Unary, node.Unary.new(eq, .Bang)) catch @panic("TODO")}) catch @panic("TODO");
            },
            .GreaterEqual => {
                const right = self.nodes.pop() catch @panic("TODO");
                const left = self.nodes.pop() catch @panic("TODO");

                const lt = node.Node{ .Binary = self.arena.create(node.Binary, node.Binary.new(left, right, .Lt)) catch @panic("TODO")};
                self.nodes.push(node.Node { .Unary = self.arena.create(node.Unary, node.Unary.new(lt, .Bang)) catch @panic("TODO")}) catch @panic("TODO");
            },
            .LessEqual => {
                const right = self.nodes.pop() catch @panic("TODO");
                const left = self.nodes.pop() catch @panic("TODO");

                const gt = node.Node{ .Binary = self.arena.create(node.Binary, node.Binary.new(left, right, .Gt)) catch @panic("TODO")};
                self.nodes.push(node.Node { .Unary = self.arena.create(node.Unary, node.Unary.new(gt, .Bang)) catch @panic("TODO")}) catch @panic("TODO");
            },
            .Minus => {
                const right = node.Node { .Unary = self.arena.create(node.Unary, node.Unary.new(self.nodes.pop() catch @panic("TODO"), .Negate)) catch @panic("TODO")};
                const left = self.nodes.pop() catch @panic("TODO");

                self.nodes.push(node.Node { .Binary = self.arena.create(node.Binary, node.Binary.new(left, right, .Add)) catch @panic("TODO") }) catch @panic("TODO");
            },
            .EqualEqual => self.nodes.push(node.Node { .Binary = self.arena.create(node.Binary, node.Binary.new(self.nodes.pop() catch @panic("TODO"), self.nodes.pop() catch @panic("TODO"), .Eq)) catch @panic("TODO")}) catch @panic("TODO"),
            .Greater => self.nodes.push(node.Node { .Binary = self.arena.create(node.Binary, node.Binary.new(self.nodes.pop() catch @panic("TODO"), self.nodes.pop() catch @panic("TODO"), .Gt)) catch @panic("TODO")}) catch @panic("TODO"),
            .Less => self.nodes.push(node.Node { .Binary = self.arena.create(node.Binary, node.Binary.new(self.nodes.pop() catch @panic("TODO"), self.nodes.pop() catch @panic("TODO"), .Lt)) catch @panic("TODO")}) catch @panic("TODO"),
            .Plus => self.nodes.push(node.Node { .Binary = self.arena.create(node.Binary, node.Binary.new(self.nodes.pop() catch @panic("TODO"), self.nodes.pop() catch @panic("TODO"), .Add)) catch @panic("TODO")}) catch @panic("TODO"),
            .Star => self.nodes.push(node.Node { .Binary = self.arena.create(node.Binary, node.Binary.new(self.nodes.pop() catch @panic("TODO"), self.nodes.pop() catch @panic("TODO"), .Mul)) catch @panic("TODO")}) catch @panic("TODO"),
            .Slash => self.nodes.push(node.Node { .Binary = self.arena.create(node.Binary, node.Binary.new(self.nodes.pop() catch @panic("TODO"), self.nodes.pop() catch @panic("TODO"), .Div)) catch @panic("TODO")}) catch @panic("TODO"),
            else => @panic("TODO"),
        }
    }

    fn identifier(self: *Parser, tokenizer: *lexer.Lexer) void {
        self.nodes.push(node.Node { .Identifier = tokenizer.previous.Identifier}) catch @panic("TODO");
    }

    fn number(self: *Parser, tokenizer: *lexer.Lexer) void {
        self.nodes.push(node.Node { .Number = tokenizer.previous.Number }) catch @panic("TODO");
    }

    fn construct(self: *Parser, tokenizer: *lexer.Lexer) void {
        var values = collections.Vec(node.Construct.Value).new(ARGUMENT_MAX, self.arena) catch @panic("TODO");

        while (!tokenizer.match(token.Token.BRACERIGHT)) {
            if (!tokenizer.match(token.Token.COMMA) and !tokenizer.previous.eql(token.Token.BRACELEFT)) {
                @panic("Do not forget to separete arguments with comma");
            }

            const name = tokenizer.current;

            if (!tokenizer.match(token.Token.IDEN)) @panic("TODO");
            if (!tokenizer.match(token.Token.DOUBLECOLON)) @panic("TODO");

            self.parse(.Assignment, tokenizer);

            values.push(node.Construct.Value.new(name.Identifier, self.nodes.pop() catch @panic("TODO"))) catch @panic("TODO");
        }

        const name = self.nodes.pop() catch unreachable;

        self.nodes.push(node.Node { .Construct = self.arena.create(node.Construct, node.Construct.new(name, values.array(self.arena))) catch @panic("TODO") }) catch @panic("TODO");
    }

    fn call(self: *Parser, tokenizer: *lexer.Lexer) void {
        var arguments = collections.Vec(node.Node).new(ARGUMENT_MAX, self.arena) catch @panic("TODO");

        while (!tokenizer.match(token.Token.PARENTESISRIGHT)) {
            if (!tokenizer.match(token.Token.COMMA) and !tokenizer.previous.eql(token.Token.PARENTESISLEFT)) {
                @panic("TODO: Show that user cannot place a comma at the start of the function call");
            }

            self.parse(.Assignment, tokenizer);
            arguments.push(self.nodes.pop() catch @panic("TODO")) catch @panic("TODO");
        }

        const name = self.nodes.pop() catch @panic("TODO");

        self.nodes.push(node.Node {.Call = self.arena.create(node.Call, node.Call.new(name, arguments.array(self.arena))) catch @panic("TODO")}) catch @panic("TODO");
    }

    fn typ(self: *Parser, tokenizer: *lexer.Lexer) void {
        const name = tokenizer.current;
        if (!tokenizer.match(token.Token.IDEN)) @panic("TODO");

        var fields = collections.Vec(node.Type.Field).new(ARGUMENT_MAX, self.arena);
        var size: ?u32 = null;

        if (tokenizer.match(token.Token.BRACELEFT)) {
            while (!tokenizer.match(token.Token.BRACERIGHT)) {
                const field_name = tokenizer.current;

                if (!tokenizer.match(token.Token.IDEN)) @panic("TODO");
                if (!tokenizer.match(token.Token.DOUBLECOLON)) @panic("TODO");

                const field_type = tokenizer.current;

                if (!tokenizer.match(token.Token.IDEN)) @panic("TODO");
                if (!tokenizer.match(token.Token.COMMA)) @panic("TODO");

                fields.push(node.Type.Field.new(field_name.Identifier, field_type.Identifier)) catch @panic("TODO");
            }
        } else {
            if (!tokenizer.match(token.Token.EQUAL)) @panic("TODO");
            const type_size = tokenizer.current;

            if (!tokenizer.match(token.Token.NUMBER)) @panic("TODO");
            if (!tokenizer.match(token.Token.SEMICOLON)) @panic("TODO");

            size = type_size.Number;
        }

        self.nodes.push(node.Node { .Type = self.arena.create(node.Type, node.Type.new(name.Identifier, fields.array(self.arena, size)) catch @panic("TODO"))}) catch @panic("TODO");
    }

    fn property(self: *Parser, tokenizer: *lexer.Lexer) void {
        const name = tokenizer.current;
        if (!tokenizer.match(token.Token.IDEN)) @panic("TODO");

        const n = self.nodes.pop() catch @panic("TODO");

        self.nodes.push(node.Node { .Property = self.arena.create(node.Property, node.Property.new(n, name.Identifier)) catch @panic("TODO")}) catch @panic("TODO");
    }

    fn procedure(self: *Parser, tokenizer: *lexer.Lexer) void {
        const name = tokenizer.current;

        if (!tokenizer.match(token.Token.IDEN)) @panic("TODO");
        if (!tokenizer.match(token.Token.PARENTESISLEFT)) @panic("TODO");

        var parameters = collections.Vec(node.Procedure.Parameter).new(ARGUMENT_MAX, self.arena) catch @panic("TODO");

        while (!tokenizer.match(token.Token.PARENTESISRIGHT)) {
            if (!tokenizer.match(token.Token.COMMA) and !tokenizer.previous.eql(token.Token.PARENTESISLEFT)) {
                @panic("TODO: Parameters should start with parentesis and the first element is a identifier");
            }

            const param = tokenizer.current;

            if (!tokenizer.match(token.Token.IDEN)) @panic("TODO");
            if (!tokenizer.match(token.Token.DOUBLECOLON)) @panic("TODO");

            const kind = tokenizer.current;

            if (!tokenizer.match(token.Token.IDEN)) @panic("TODO");

            parameters.push(node.Procedure.Parameter.new(param.Identifier, kind.Identifier)) catch @panic("TODO");
        }

        if (!tokenizer.match(token.Token.DOUBLECOLON)) @panic("TODO");

        const return_type = tokenizer.current;

        if (!tokenizer.match(token.Token.IDEN)) @panic("TODO");
        if (!tokenizer.match(token.Token.BRACELEFT)) @panic("TODO");

        self.scope(tokenizer);

        const scp = self.nodes.pop() catch @panic("TODO");

        self.nodes.push(node.Node { .Procedure = self.arena.create(node.Procedure, node.Procedure.new(name.Identifier, return_type.Identifier, parameters.array(self.arena), scp)) catch @panic("TODO")}) catch @panic("TODO");
    }

    fn let(self: *Parser, tokenizer: *lexer.Lexer) void {
        const iden = tokenizer.current;
        const mutable = tokenizer.match(token.Token.MUT);

        if (!tokenizer.match(token.Token.IDEN)) @panic("TODO");
        if (!tokenizer.match(token.Token.DOUBLECOLON)) @panic("TODO");

        const kind = tokenizer.current;

        if (!tokenizer.match(token.Token.IDEN)) @panic("TODO");
        if (!tokenizer.match(token.Token.EQUAL)) @panic("TODO");

        self.parse(.Assignment, tokenizer);

        if (!tokenizer.match(token.Token.SEMICOLON)) @panic("TODO");

        const value = self.nodes.pop() catch @panic("TODO");

        self.nodes.push(node.Node {.Let = self.arena.create(node.Let, node.Let.new(iden.Identifier, kind.Identifier, value, mutable)) catch @panic("TODO")}) catch @panic("TODO");
    }

    fn string(self: *Parser, tokenizer: *lexer.Lexer) void {
        _ = tokenizer;
        _ = self;
        @panic("TODO");
    }

    fn scope(self: *Parser, tokenizer: *lexer.Lexer) void {
        var value: ?node.Node = null;
        var nodes = collections.Vec(node.Node).new(ARGUMENT_MAX, self.arena) catch @panic("TODO");

        while (!tokenizer.match(token.Token.BRACERIGHT)) {
            switch (tokenizer.current) {
                .Keyword => |keyword| switch (keyword) {
                    .Let => {
                        tokenizer.advance();
                        self.let(tokenizer);
                    },
                    else => @panic("TODO"),
                },

                else => {
                    self.parse(.Assignment, tokenizer);

                    if (!tokenizer.previous.eql(token.Token.SEMICOLON)) {
                        if (tokenizer.match(token.Token.BRACERIGHT)) {
                            value = self.nodes.pop() catch @panic("TODO");

                            break;
                        }
                    }
                },
            }

            nodes.push(self.nodes.pop() catch @panic("TODO")) catch @panic("TODO");
        }

        self.nodes.push(node.Node {.Scope = self.arena.create(node.Scope, node.Scope.new(nodes.array(self.arena), value)) catch @panic("TODO")}) catch @panic("TODO");
    }

    fn parse(self: *Parser, precedence: Precedence, tokenizer: *lexer.Lexer) void {
        tokenizer.advance();

        if (self.rules.items[RuleKind.from_token(tokenizer.previous)].prefix) |prefix| {
            prefix(self, tokenizer);
        } else {
            @panic("TODO: Show error");
        }

        var r = self.rules.items[RuleKind.from_token(tokenizer.current)];

        while (@intFromEnum(precedence) <= @intFromEnum(r.precedence)) {
            if (r.infix) |infix| {
                tokenizer.advance();
                infix(self, tokenizer);
            } else {
                break;
            }

            r = self.rules.items[RuleKind.from_token(tokenizer.current)];
        }
    }

    pub fn compile(self: *Parser, stream: collections.Stream(u8)) void {
        _ = self;
        _ = stream;
        // self.checker.generate(stream);
    }

    pub fn deinit(self: *Parser) void {
        self.rules.deinit(self.arena);
        self.nodes.deinit(self.arena);
        self.arena.deinit();
    }
};

// test "Lexer" {
//     _ = lexer;
// }

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
