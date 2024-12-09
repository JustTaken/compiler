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
const Ruler = struct {
    callbacks: collections.Array(Rule),
    transforms: collections.Array(I.Transform),

    const I = Iter(token.Token, Rule);

    fn new(allocator: *mem.Arena) error{OutOfMemory, OutOfBounds}!Ruler {
        var self: Ruler = undefined;
        const Ts = [_]type {token.Keyword, token.Symbol, token.Operator};
        const token_kind_len = @typeInfo(token.TokenKind).@"enum".fields.len;
        comptime var len = token_kind_len - Ts.len;

        inline for (Ts) |T| {
            len += @typeInfo(T).@"enum".fields.len;
        }

        const transforms: [token_kind_len]I.Transform = .{
            token.identity_int, token.identity_int, token.identity_int, token.identity_int, token.keyword_int, token.symbol_int, token.operator_int,
        };

        self.callbacks = try I.new(
            &[_]I.Pair {
                I.Pair { .t = token.Token.IDEN, .f = Rule.new(Parser.identifier, null, .Nothing), },
                I.Pair { .t = token.Token.NUMBER, .f = Rule.new(Parser.number, null, .Nothing), },
                I.Pair { .t = token.Token.LET, .f = Rule.new(Parser.let, null, .Declaration), },
                I.Pair { .t = token.Token.PARENTESISLEFT, .f = Rule.new(Parser.group, Parser.call, .Call), },
                I.Pair { .t = token.Token.BRACELEFT, .f = Rule.new(Parser.scope, Parser.construct, .Assignment), },
                I.Pair { .t = token.Token.DOT, .f = Rule.new(null, Parser.property, .Call) },
                I.Pair { .t = token.Token.MINUS, .f = Rule.new(Parser.unary, Parser.binary, .Term), },
                I.Pair { .t = token.Token.PLUS, .f = Rule.new(null, Parser.binary, .Term), },
                I.Pair { .t = token.Token.BANG, .f = Rule.new(Parser.unary, null, .Equality), },
                I.Pair { .t = token.Token.BANGEQUAL, .f = Rule.new(null, Parser.binary, .Equality), },
                I.Pair { .t = token.Token.EQUALEQUAL, .f = Rule.new(null, Parser.binary, .Equality), },
                I.Pair { .t = token.Token.STAR, .f = Rule.new(null, Parser.binary, .Factor), },
                I.Pair { .t = token.Token.SLASH, .f = Rule.new(null, Parser.binary, .Factor), },
                I.Pair { .t = token.Token.GREATER, .f = Rule.new(null, Parser.binary, .Comparison), },
                I.Pair { .t = token.Token.LESS, .f = Rule.new(null, Parser.binary, .Comparison), },
                I.Pair { .t = token.Token.GREATEREQUAL, .f = Rule.new(null, Parser.binary, .Comparison), },
                I.Pair { .t = token.Token.LESSEQUAL, .f = Rule.new(null, Parser.binary, .Comparison), },
                I.Pair { .t = token.Token.STRING, .f = Rule.new(Parser.string, null, .Nothing), },
            },
            &transforms,
            len,
            Rule.new(null, null, .Nothing),
            allocator,
        );

        errdefer self.callbacks.deinit(allocator);

        self.transforms = try collections.Array(I.Transform).new(transforms.len, allocator);

        for (0..transforms.len) |i| {
            self.transforms.items[i] = transforms[i];
        }

        return self;
    }

    fn from_token(self: *const Ruler, t: token.Token) Rule {
        const index = @intFromEnum(t);
        const pos = self.transforms.items[index](t, index);

        return self.callbacks.items[pos];
    }

    fn deinit(self: *Ruler, allocator: *mem.Arena) void {
        self.callbacks.deinit(allocator);
        self.transforms.deinit(allocator);
    }
};

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
};

fn Iter(T: type, F: type) type {
    return struct {
        const Self = @This();

        const Pair = struct {
            t: T,
            f: F,
        };

        const Transform = *const fn (T, i: usize) usize;

        fn new(
            pairs: []const Pair,
            transforms: []const Transform,
            len: u32,
            zero: F,
            allocator: *mem.Arena,
        ) error{OutOfMemory, OutOfBounds}!collections.Array(F) {
            var callbacks = try collections.Array(F).new(len, allocator);
            errdefer callbacks.deinit(allocator);

            for (0..len) |i| {
                try callbacks.set(zero, i);
            }

            for (pairs) |pair| {
                const i = @intFromEnum(pair.t);
                const pos = transforms[i](pair.t, i);

                try callbacks.set(pair.f, pos);
            }

            return callbacks;
        }
    };
}


const NodeManager = struct {
    nodes: collections.Vec(node.Node),
    type_fields: collections.Vec(node.Type.Field),
    construct_values: collections.SliceManager(node.Construct.Value),
    parameters: collections.Vec(node.Procedure.Parameter),

    fn new(allocator: *mem.Arena) error{OutOfMemory}!NodeManager {
        util.print(.Info, "Scope: {}", .{@sizeOf(node.Scope)});
        util.print(.Info, "Construct: {}", .{@sizeOf(node.Construct)});
        util.print(.Info, "Let: {}", .{@sizeOf(node.Let)});
        util.print(.Info, "Call: {}", .{@sizeOf(node.Call)});
        util.print(.Info, "Binary: {}", .{@sizeOf(node.Binary)});
        util.print(.Info, "Unary: {}", .{@sizeOf(node.Unary)});
        util.print(.Info, "Property: {}", .{@sizeOf(node.Property)});
        util.print(.Info, "Procedure: {}", .{@sizeOf(node.Procedure)});
        util.print(.Info, "Type: {}", .{@sizeOf(node.Type)});
        util.print(.Info, "Identifier: {}", .{@sizeOf(node.Identifier)});
        util.print(.Info, "Number: {}", .{@sizeOf(node.Number)});
        util.print(.Info, "Node: {}", .{@sizeOf(node.Node)});

        return NodeManager{
            .nodes = try collections.Vec(node.Node).new(100, allocator),
            .type_fields = try collections.Vec(node.Type.Field).new(20, allocator),
            .construct_values = try collections.SliceManager(node.Construct.Value).new(20, allocator),
            .parameters = try collections.Vec(node.Procedure.Parameter).new(20, allocator),
        };
    }

    fn deinit(self: *NodeManager, allocator: *mem.Arena) void {
        self.parameters.deinit(allocator);
        self.construct_values.deinit(allocator);
        self.type_fields.deinit(allocator);
        self.nodes.deinit(allocator);
    }
};

pub const Parser = struct {
    ruler: Ruler,
    manager: NodeManager,
    arena: *mem.Arena,

    pub fn new(allocator: *mem.Arena) error{ OutOfBounds, OutOfMemory }!Parser {
        var self = Parser{
            .ruler = undefined,
            .manager = undefined,
            .arena = try allocator.child("Parser", mem.PAGE_SIZE),
        };

        errdefer self.arena.deinit();

        self.manager = try NodeManager.new(self.arena);
        errdefer self.manager.deinit(self.arena);

        self.ruler = try Ruler.new(self.arena);
        errdefer self.ruler.deinit(self.arena);

        return self;
    }

    pub fn next(self: *Parser, tokenizer: *lexer.Lexer) ?node.Node {
        self.manager.nodes.clear();

        _ = tokenizer.next() orelse return null;

        switch (tokenizer.previous) {
            .Keyword => |keyword| switch (keyword) {
                .Procedure => self.procedure(tokenizer),
                .Type => self.typ(tokenizer),
                else => @panic("TODO: No other statement kind is accepted for now"),
            },
            else => @panic("TODO: do not accept expressions here for now"),
        }

        util.assert(self.manager.nodes.len >= 1);

        return self.manager.nodes.pop() catch @panic("TODO");
    }

    fn group(self: *Parser, tokenizer: *lexer.Lexer) void {
        self.parse(.Assignment, tokenizer);
        if (!tokenizer.match(token.Token.PARENTESISRIGHT)) @panic("TODO");
    }

    fn unary(self: *Parser, tokenizer: *lexer.Lexer) void {
        const operator = tokenizer.previous.Operator;
        self.parse(.Unary, tokenizer);

        switch (operator) {
            .Bang => self.manager.nodes.push(node.Node{ .Unary = node.Unary.new(.Bang) }) catch @panic("TODO"),
            .Minus => self.manager.nodes.push(node.Node{ .Unary = node.Unary.new(.Negate) }) catch @panic("TODO"),
            else => @panic("TODO"),
        }
    }

    fn binary(self: *Parser, tokenizer: *lexer.Lexer) void {
        const operator = tokenizer.previous.Operator;

        self.parse(
            @enumFromInt(
                @intFromEnum(
                    self.ruler.from_token(tokenizer.previous).precedence,
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
            .Identifier = node.Identifier.new(tokenizer.previous.Identifier),
        }) catch @panic("TODO");
    }

    fn number(self: *Parser, tokenizer: *lexer.Lexer) void {
        self.manager.nodes.push(node.Node{ .Number = node.Number.new(
            tokenizer.previous.Number,
        ) }) catch @panic("TODO");
    }

    fn construct(self: *Parser, tokenizer: *lexer.Lexer) void {
        const index = self.manager.construct_values.start() catch @panic("TODO");

        while (!tokenizer.match(token.Token.BRACERIGHT)) {
            if (!tokenizer.match(token.Token.COMMA) and !tokenizer.previous.eql(token.Token.BRACELEFT)) {
                @panic("Do not forget to separete arguments with comma");
            }

            const name = tokenizer.current;

            if (!tokenizer.match(token.Token.IDEN)) @panic("TODO");
            if (!tokenizer.match(token.Token.DOUBLECOLON)) @panic("TODO");

            self.parse(.Assignment, tokenizer);

            self.manager.construct_values.extend(node.Construct.Value.new(
                name.Identifier,
                self.manager.nodes.pop() catch @panic("TODO"),
            )) catch @panic("TODO");
        }

        self.manager.nodes.push(node.Node{
            .Construct = node.Construct.new(index),
        }) catch @panic("TODO");
    }

    fn call(self: *Parser, tokenizer: *lexer.Lexer) void {
        var len: util.Index = 0;

        while (!tokenizer.match(token.Token.PARENTESISRIGHT)) {
            if (!tokenizer.match(token.Token.COMMA) and !tokenizer.previous.eql(token.Token.PARENTESISLEFT)) {
                @panic("TODO: Show that user cannot place a comma at the start of the function call");
            }

            self.parse(.Assignment, tokenizer);
            len += 1;
        }

        self.manager.nodes.push(node.Node{
            .Call = node.Call.new(len),
        }) catch @panic("TODO");
    }

    fn typ(self: *Parser, tokenizer: *lexer.Lexer) void {
        const name = tokenizer.current;

        if (!tokenizer.match(token.Token.IDEN)) @panic("TODO");

        if (tokenizer.match(token.Token.BRACELEFT)) {
            while (!tokenizer.match(token.Token.BRACERIGHT)) {
                const field_name = tokenizer.current;

                if (!tokenizer.match(token.Token.IDEN)) @panic("TODO");
                if (!tokenizer.match(token.Token.DOUBLECOLON)) @panic("TODO");

                const field_type = tokenizer.current;

                if (!tokenizer.match(token.Token.IDEN)) @panic("TODO");
                if (!tokenizer.match(token.Token.COMMA)) @panic("TODO");

                self.manager.type_fields.push(node.Type.Field.new(
                    field_name.Identifier,
                    field_type.Identifier,
                )) catch @panic("TODO");
            }
        } else {
            if (!tokenizer.match(token.Token.EQUAL)) @panic("TODO");
            const type_size = tokenizer.current;

            if (!tokenizer.match(token.Token.NUMBER)) @panic("TODO");
            if (!tokenizer.match(token.Token.SEMICOLON)) @panic("TODO");

            self.manager.nodes.push(node.Node{ .Number = node.Number.new(
                type_size.Number,
            ) }) catch @panic("TODO");
        }

        self.manager.nodes.push(node.Node{ .Type = node.Type.new(
            name.Identifier,
        ) }) catch @panic("TODO");
    }

    fn property(self: *Parser, tokenizer: *lexer.Lexer) void {
        const name = tokenizer.current;

        if (!tokenizer.match(token.Token.IDEN)) @panic("TODO");

        self.manager.nodes.push(node.Node{
            .Property = node.Property.new(name.Identifier),
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
                param.Identifier,
                kind.Identifier,
            )) catch @panic("TODO");
        }

        if (!tokenizer.match(token.Token.DOUBLECOLON)) @panic("TODO");

        self.parse(.Call, tokenizer);

        if (!tokenizer.match(token.Token.BRACELEFT)) @panic("TODO");

        self.scope(tokenizer);

        self.manager.nodes.push(node.Node{ .Procedure = node.Procedure.new(
            name.Identifier,
        ) }) catch @panic("TODO");
    }

    fn let(self: *Parser, tokenizer: *lexer.Lexer) void {
        const mutable = tokenizer.match(token.Token.MUT);
        const iden = tokenizer.current;
        _ = mutable;

        if (!tokenizer.match(token.Token.IDEN)) @panic("TODO");
        if (!tokenizer.match(token.Token.DOUBLECOLON)) @panic("TODO");

        self.parse(.Assignment, tokenizer);

        if (!tokenizer.match(token.Token.EQUAL)) @panic("TODO");

        self.parse(.Assignment, tokenizer);

        if (!tokenizer.match(token.Token.SEMICOLON)) @panic("TODO");

        self.manager.nodes.push(node.Node{ .Let = node.Let.new(
            iden.Identifier,
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

        self.manager.nodes.push(node.Node {.Scope = node.Scope.new(len)}) catch @panic("TODO");
    }

    fn parse(self: *Parser, precedence: Precedence, tokenizer: *lexer.Lexer) void {
        tokenizer.advance();

        if (self.ruler.from_token(tokenizer.previous).prefix) |prefix| {
            prefix(self, tokenizer);
        } else {
            @panic("TODO: Show error");
        }

        @import("std").debug.print("current: {}\n", .{tokenizer.current});

        var r = self.ruler.from_token(tokenizer.current);

        while (@intFromEnum(precedence) <= @intFromEnum(r.precedence)) {
            if (r.infix) |infix| {
                _ = tokenizer.next() orelse @panic("TODO");
                infix(self, tokenizer);
            } else {
                break;
            }

            r = self.ruler.from_token(tokenizer.current);
        }
    }

    pub fn deinit(self: *Parser) void {
        self.ruler.deinit(self.arena);
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
