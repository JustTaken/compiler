const mem = @import("mem");
const util = @import("util");
const collections = @import("collections");

const generator = @import("generator/mod.zig");
const checker = @import("checker/mod.zig");
const node = @import("node/mod.zig");
const lexer = @import("lexer/mod.zig");
const token = lexer.token;

const ARGUMENT_MAX: u32 = 20;

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

const Fn = *const fn (parser: *Parser, lexer: *lexer.Lexer) void;

const Rule = struct {
    prefix: ?Fn,
    infix: ?Fn,
    precedence: Precedence,

    const Kind = enum {
        Let,
        ParentesisLeft,
        CurlyBracketLeft,
        Dot,
        Minus,
        Plus,
        Bang,
        Equality,
        Factor,
        Comparison,
        Identifier,
        String,
        Number,

        fn init_buffer(buffer: *collections.Vec(Rule)) error{OutOfBounds}!void {
            const data = [_]Rule{
                Rule.new(Parser.let, null, .Declaration),
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
            const kind: Kind = switch (t) {
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
                .Keyword => |keyword| switch (keyword) {
                    .Let => .Let,
                    else => @panic("TODO"),
                },
                .Eof => @panic("TODO"),
            };

            return @intFromEnum(kind);
        }
    };

    fn new(prefix: ?Fn, infix: ?Fn, precedence: Precedence) Rule {
        return Rule{
            .prefix = prefix,
            .infix = infix,
            .precedence = precedence,
        };
    }
};

const NodeManager = struct {
    nodes: collections.SliceManager(node.Node),
    type_fields: collections.Vec(node.Type.Field),
    construct_values: collections.SliceManager(node.Construct.Value),
    parameters: collections.Vec(node.Procedure.Parameter),
    words: collections.SliceManager(collections.Slice),

    fn new(allocator: *mem.Arena) error{OutOfMemory}!NodeManager {
        // util.print(.Info, "Scope: {}", .{@sizeOf(node.Scope)});
        // util.print(.Info, "Construct: {}", .{@sizeOf(node.Construct)});
        // util.print(.Info, "Let: {}", .{@sizeOf(node.Let)});
        // util.print(.Info, "Call: {}", .{@sizeOf(node.Call)});
        // util.print(.Info, "Binary: {}", .{@sizeOf(node.Binary)});
        // util.print(.Info, "Unary: {}", .{@sizeOf(node.Unary)});
        // util.print(.Info, "Property: {}", .{@sizeOf(node.Property)});
        // util.print(.Info, "Identifier: {}", .{@sizeOf(node.Identifier)});
        // util.print(.Info, "Number: {}", .{@sizeOf(node.Number)});
        // util.print(.Info, "Node: {}", .{@sizeOf(node.Node)});

        return NodeManager{
            .nodes = try collections.SliceManager(node.Node).new(100, allocator),
            .type_fields = try collections.Vec(node.Type.Field).new(20, allocator),
            .construct_values = try collections.SliceManager(node.Construct.Value).new(20, allocator),
            .parameters = collections.Vec(node.Procedure.Parameter).new(20, allocator),
            .words = try collections.SliceManager(collections.Slice).new(20, allocator),
        };
    }

    fn deinit(self: *NodeManager, allocator: *mem.Arena) void {
        self.words.deinit(allocator);
        self.parameters.deinit(allocator);
        self.construct_values.deinit(allocator);
        self.type_fields.deinit(allocator);
        self.nodes.deinit(allocator);
    }
};

pub const Parser = struct {
    rules: collections.Vec(Rule),
    manager: NodeManager,
    arena: *mem.Arena,

    pub fn new(allocator: *mem.Arena) error{ OutOfBounds, OutOfMemory }!Parser {
        var self = Parser{
            .rules = undefined,
            .manager = undefined,
            .arena = try allocator.child("Parser", mem.PAGE_SIZE),
        };

        errdefer self.arena.deinit();

        self.manager = try NodeManager.new(self.arena);
        errdefer self.manager.deinit(self.arena);

        self.rules = try collections.Vec(Rule).new(@typeInfo(Rule.Kind).@"enum".fields.len, self.arena);
        errdefer self.rules.deinit(self.arena);

        try Rule.Kind.init_buffer(&self.rules);

        return self;
    }

    pub fn next(self: *Parser, tokenizer: *lexer.Lexer) ?node.Node {
        // if (self.nodes.len > 0) {
        //     const n = self.nodes.pop() catch @panic("TODO");
        //     n.deinit(self.arena);
        // }

        const tok = tokenizer.next() orelse return null;

        switch (tok) {
            .Keyword => |keyword| switch (keyword) {
                .Procedure => self.procedure(tokenizer),
                .Type => self.typ(tokenizer),
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
            .Bang => self.manager.nodes.extend(node.Node{ .Unary = node.Unary.new(.Bang) }) catch @panic("TODO"),
            .Minus => self.manager.nodes.extend(node.Node{ .Unary = node.Unary.new(.Negate) }) catch @panic("TODO"),
            else => @panic("TODO"),
        }
    }

    fn binary(self: *Parser, tokenizer: *lexer.Lexer) void {
        const operator = tokenizer.previous.Operator;

        self.parse(
            @enumFromInt(
                @intFromEnum(
                    self.rules.items[Rule.Kind.from_token(tokenizer.previous)].precedence,
                ) + 1,
            ),
            tokenizer,
        );

        switch (operator) {
            .BangEqual => {
                self.manager.nodes.push(node.Node{ .Binary = node.Binary.new(.Eq) }) catch @panic("TODO");
                self.manager.nodes.push(node.Node{ .Unary = node.Unary.new(.Bang) }) catch @panic("TODO");
            },
            .GreaterEqual => {
                self.manager.nodes.push(node.Node{ .Binary = node.Binary.new(.Lt) }) catch @panic("TODO");
                self.manager.nodes.push(node.Node{ .Unary = node.Unary.new(.Bang) }) catch @panic("TODO");
            },
            .LessEqual => {
                self.manager.nodes.push(node.Node{ .Binary = node.Binary.new(.Gt) }) catch @panic("TODO");
                self.manager.nodes.push(node.Node{ .Unary = node.Unary.new(.Bang) }) catch @panic("TODO");
            },
            .Minus => self.manager.nodes.push(node.Node{ .Binary = node.Binary.new(.Add) }) catch @panic("TODO"),
            .EqualEqual => self.manager.nodes.push(node.Node{ .Binary = node.Binary.new(.Eq) }) catch @panic("TODO"),
            .Greater => self.manager.nodes.push(node.Node{ .Binary = node.Binary.new(.Gt) }) catch @panic("TODO"),
            .Less => self.manager.nodes.push(node.Node{ .Binary = node.Binary.new(.Lt) }) catch @panic("TODO"),
            .Plus => self.manager.nodes.push(node.Node{ .Binary = node.Binary.new(.Add) }) catch @panic("TODO"),
            .Star => self.manager.nodes.push(node.Node{ .Binary = node.Binary.new(.Mul) }) catch @panic("TODO"),
            .Slash => self.manager.nodes.push(node.Node{ .Binary = node.Binary.new(.Div) }) catch @panic("TODO"),

            else => @panic("TODO"),
        }
    }

    fn identifier(self: *Parser, tokenizer: *lexer.Lexer) void {
        self.manager.nodes.push(node.Node{
            .Identifier = node.Identifier.new(self.manager.words.push(tokenizer.previous.Identifier) catch @panic("TODO")),
        }) catch @panic("TODO");
    }

    fn number(self: *Parser, tokenizer: *lexer.Lexer) void {
        self.manager.nodes.push(node.Node{ .Number = node.Number.new(
            self.manager.words.push(tokenizer.previous.Number) catch @panic("TODO"),
        ) }) catch @panic("TODO");
    }

    fn construct(self: *Parser, tokenizer: *lexer.Lexer) void {
        const index = self.manager.construct_values.start();

        while (!tokenizer.match(token.Token.BRACERIGHT)) {
            if (!tokenizer.match(token.Token.COMMA) and !tokenizer.previous.eql(token.Token.BRACELEFT)) {
                @panic("Do not forget to separete arguments with comma");
            }

            const name = tokenizer.current;

            if (!tokenizer.match(token.Token.IDEN)) @panic("TODO");
            if (!tokenizer.match(token.Token.DOUBLECOLON)) @panic("TODO");

            self.parse(.Assignment, tokenizer);

            self.manager.construct_values.extend(node.Construct.Value.new(
                self.manager.words.push(name.Identifier) catch @panic("TODO"),
                self.manager.nodes.pop() catch @panic("TODO"),
            )) catch @panic("TODO");
        }

        util.assert(self.nodes.len > 0);
        self.manager.nodes.push(node.Node{
            .Construct = node.Construct.new(index),
        }) catch @panic("TODO");
    }

    fn call(self: *Parser, tokenizer: *lexer.Lexer) void {
        const index = self.manager.call_arguments.start();

        while (!tokenizer.match(token.Token.PARENTESISRIGHT)) {
            if (!tokenizer.match(token.Token.COMMA) and !tokenizer.previous.eql(token.Token.PARENTESISLEFT)) {
                @panic("TODO: Show that user cannot place a comma at the start of the function call");
            }

            self.parse(.Assignment, tokenizer);
            self.manager.call_arguments.push(self.nodes.pop() catch @panic("TODO")) catch @panic("TODO");
        }

        self.manager.nodes.push(node.Node{
            .Call = node.Call.new(index),
        }) catch @panic("TODO");
    }

    fn typ(self: *Parser, tokenizer: *lexer.Lexer) void {
        const name = tokenizer.current;

        if (!tokenizer.match(token.Token.IDEN)) @panic("TODO");

        // var fields = collections.Vec(node.Type.Field).new(ARGUMENT_MAX, self.arena) catch @panic("TODO");
        if (tokenizer.match(token.Token.BRACELEFT)) {
            while (!tokenizer.match(token.Token.BRACERIGHT)) {
                const field_name = tokenizer.current;

                if (!tokenizer.match(token.Token.IDEN)) @panic("TODO");
                if (!tokenizer.match(token.Token.DOUBLECOLON)) @panic("TODO");

                const field_type = tokenizer.current;

                if (!tokenizer.match(token.Token.IDEN)) @panic("TODO");
                if (!tokenizer.match(token.Token.COMMA)) @panic("TODO");

                self.manager.type_fields.push(node.Type.Field.new(
                    self.manager.words.push(field_name.Identifier) catch @panic("TODO"),
                    self.manager.words.push(field_type.Identifier) catch @panic("TODO"),
                )) catch @panic("TODO");
            }
        } else {
            if (!tokenizer.match(token.Token.EQUAL)) @panic("TODO");
            const type_size = tokenizer.current;

            if (!tokenizer.match(token.Token.NUMBER)) @panic("TODO");
            if (!tokenizer.match(token.Token.SEMICOLON)) @panic("TODO");

            self.nodes.push(node.Node{ .Number = node.Number.new(
                self.manager.words.push(type_size.Number) catch @panic("TODO"),
            ) }) catch @panic("TODO");
        }

        self.nodes.push(node.Node{ .Type = node.Type.new(
            self.manager.words.push(name.Identifier) catch @panic("TODO"),
        ) }) catch @panic("TODO");
    }

    fn property(self: *Parser, tokenizer: *lexer.Lexer) void {
        const name = tokenizer.current;
        if (!tokenizer.match(token.Token.IDEN)) @panic("TODO");

        // const n = self.nodes.pop() catch @panic("TODO");

        self.nodes.push(node.Node{
            .Property = node.Property.new(self.manager.words.push(name.Identifier) catch @panic("TODO")),
        }) catch @panic("TODO");
    }

    fn procedure(self: *Parser, tokenizer: *lexer.Lexer) void {
        const name = tokenizer.current;

        if (!tokenizer.match(token.Token.IDEN)) @panic("TODO");
        if (!tokenizer.match(token.Token.PARENTESISLEFT)) @panic("TODO");

        while (!tokenizer.match(token.Token.PARENTESISRIGHT)) {
            if (!tokenizer.match(token.Token.COMMA) and !tokenizer.previous.eql(token.Token.PARENTESISLEFT)) {
                @panic("TODO: Parameters should start with parentesis and the first element is a identifier");
            }

            const param = tokenizer.current;

            if (!tokenizer.match(token.Token.IDEN)) @panic("TODO");
            if (!tokenizer.match(token.Token.DOUBLECOLON)) @panic("TODO");

            const kind = tokenizer.current;

            if (!tokenizer.match(token.Token.IDEN)) @panic("TODO");

            self.manager.parameters.push(node.Procedure.Parameter.new(
                self.manager.words.push(param.Identifier) catch @panic("TODO"),
                self.manager.words.push(kind.Identifier) catch @panic("TODO"),
            )) catch @panic("TODO");
        }

        if (!tokenizer.match(token.Token.DOUBLECOLON)) @panic("TODO");

        const return_type = tokenizer.current;

        if (!tokenizer.match(token.Token.IDEN)) @panic("TODO");
        if (!tokenizer.match(token.Token.BRACELEFT)) @panic("TODO");

        self.scope(tokenizer);

        self.manager.nodes.push(node.Node{ .Procedure = node.Procedure.new(
            self.manager.words.push(name.Identifier) catch @panic("TODO"),
            self.manager.words.push(return_type.Identifier) catch @panic("TODO"),
        ) }) catch @panic("TODO");
    }

    fn let(self: *Parser, tokenizer: *lexer.Lexer) void {
        const iden = tokenizer.current;
        const mutable = tokenizer.match(token.Token.MUT);
        _ = mutable;

        if (!tokenizer.match(token.Token.IDEN)) @panic("TODO");
        if (!tokenizer.match(token.Token.DOUBLECOLON)) @panic("TODO");

        const kind = tokenizer.current;

        if (!tokenizer.match(token.Token.IDEN)) @panic("TODO");
        if (!tokenizer.match(token.Token.EQUAL)) @panic("TODO");

        self.parse(.Assignment, tokenizer);

        if (!tokenizer.match(token.Token.SEMICOLON)) @panic("TODO");

        // const value = self.nodes.pop() catch @panic("TODO");

        self.nodes.push(node.Node{ .Let = node.Let.new(
            self.manager.words.push(iden.Identifier) catch @panic("TODO"),
            self.manager.words.push(kind.Identifier) catch @panic("TODO"),
        ) }) catch @panic("TODO");
    }

    fn string(self: *Parser, tokenizer: *lexer.Lexer) void {
        _ = tokenizer;
        _ = self;
        @panic("TODO");
    }

    fn scope(self: *Parser, tokenizer: *lexer.Lexer) void {
        var len: util.Index = 0;

        while (!tokenizer.match(token.Token.BRACERIGHT)) {
            self.parse(.Assignment, tokenizer);

            len += 1;
        }

        self.nodes.push(node.Node {.Scope = node.Scope.new(len)}) catch @panic("TODO");
    }

    fn parse(self: *Parser, precedence: Precedence, tokenizer: *lexer.Lexer) void {
        tokenizer.advance();

        if (self.rules.items[Rule.Kind.from_token(tokenizer.previous)].prefix) |prefix| {
            prefix(self, tokenizer);
        } else {
            @panic("TODO: Show error");
        }

        var r = self.rules.items[Rule.Kind.from_token(tokenizer.current)];

        while (@intFromEnum(precedence) <= @intFromEnum(r.precedence)) {
            if (r.infix) |infix| {
                tokenizer.advance();
                infix(self, tokenizer);
            } else {
                break;
            }

            r = self.rules.items[Rule.Kind.from_token(tokenizer.current)];
        }
    }

    pub fn compile(self: *Parser, stream: collections.Stream(u8)) void {
        _ = self;
        _ = stream;
        // self.checker.generate(stream);
    }

    pub fn deinit(self: *Parser) void {
        self.rules.deinit(self.arena);
        self.manager.deinit(self.arena);
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
