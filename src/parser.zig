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
        const zone = util.tracy.initZone(@src(), .{.name = "Ruler::new"});
        defer zone.deinit();

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
        const zone = util.tracy.initZone(@src(), .{.name = "Ruler::from_token"});
        defer zone.deinit();

        const index = @intFromEnum(t);
        const pos = self.transforms.items[index](t, index);

        return self.callbacks.items[pos];
    }

    fn deinit(self: *Ruler, allocator: *mem.Arena) void {
        const zone = util.tracy.initZone(@src(), .{.name = "Ruler::deinit"});
        defer zone.deinit();

        self.transforms.deinit(allocator);
        self.callbacks.deinit(allocator);
    }
};

const Rule = struct {
    prefix: ?Fn,
    infix: ?Fn,
    precedence: Precedence,

    fn new(prefix: ?Fn, infix: ?Fn, precedence: Precedence) Rule {
        const zone = util.tracy.initZone(@src(), .{.name = "Rule::new"});
        defer zone.deinit();

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
            const zone = util.tracy.initZone(@src(), .{.name = "Iter::new"});
            defer zone.deinit();

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

pub const Parser = struct {
    ruler: Ruler,
    manager: node.Tree,
    arena: *mem.Arena,

    pub fn new(allocator: *mem.Arena) error{ OutOfBounds, OutOfMemory }!Parser {
        const zone = util.tracy.initZone(@src(), .{.name = "Parser::new"});
        defer zone.deinit();

        var self = Parser{
            .ruler = undefined,
            .manager = undefined,
            .arena = try allocator.child("Parser", mem.PAGE_SIZE),
        };

        errdefer self.arena.deinit();

        self.manager = try node.Tree.new(self.arena);
        errdefer self.manager.deinit(self.arena);

        self.ruler = try Ruler.new(self.arena);
        errdefer self.ruler.deinit(self.arena);

        return self;
    }

    pub fn next(self: *Parser, tokenizer: *lexer.Lexer) ?*node.Tree {
        const zone = util.tracy.initZone(@src(), .{.name = "Parser::next"});
        defer zone.deinit();

        self.manager.clear();

        _ = tokenizer.next() orelse return null;

        switch (tokenizer.previous) {
            .Keyword => |keyword| switch (keyword) {
                .proc => self.procedure(tokenizer),
                .@"type" => self.typ(tokenizer),
                else => @panic("TODO: No other statement kind is accepted for now"),
            },
            else => @panic("TODO: do not accept expressions here for now"),
        }

        return &self.manager;
    }

    fn group(self: *Parser, tokenizer: *lexer.Lexer) void {
        const zone = util.tracy.initZone(@src(), .{.name = "Parser::group"});
        defer zone.deinit();

        self.parse(.Assignment, tokenizer);
        if (!tokenizer.match(token.Token.PARENTESISRIGHT)) @panic("TODO");
    }

    fn unary(self: *Parser, tokenizer: *lexer.Lexer) void {
        const zone = util.tracy.initZone(@src(), .{.name = "Parser::unary"});
        defer zone.deinit();

        const operator = tokenizer.previous.Operator;
        self.parse(.Unary, tokenizer);

        switch (operator) {
            .Bang => self.manager.nodes.push(node.Node{ .Unary = node.Unary.new(.Bang) }) catch @panic("TODO"),
            .Minus => self.manager.nodes.push(node.Node{ .Unary = node.Unary.new(.Negate) }) catch @panic("TODO"),
            else => @panic("TODO"),
        }
    }

    fn binary(self: *Parser, tokenizer: *lexer.Lexer) void {
        const zone = util.tracy.initZone(@src(), .{.name = "Parser::binary"});
        defer zone.deinit();

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
        const zone = util.tracy.initZone(@src(), .{.name = "Parser::identifier"});
        defer zone.deinit();

        self.manager.nodes.push(node.Node{
            .Identifier = node.Identifier.new(tokenizer.previous.Identifier),
        }) catch @panic("TODO");
    }

    fn number(self: *Parser, tokenizer: *lexer.Lexer) void {
        const zone = util.tracy.initZone(@src(), .{.name = "Parser::number"});
        defer zone.deinit();

        self.manager.nodes.push(node.Node{ .Number = node.Number.new(
            tokenizer.previous.Number,
        ) }) catch @panic("TODO");
    }

    fn construct(self: *Parser, tokenizer: *lexer.Lexer) void {
        const zone = util.tracy.initZone(@src(), .{.name = "Parser::construct"});
        defer zone.deinit();

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
                @intCast(self.manager.nodes.len - 1),
            )) catch @panic("TODO");
        }

        self.manager.nodes.push(node.Node{
            .Construct = node.Construct.new(index),
        }) catch @panic("TODO");
    }

    fn call(self: *Parser, tokenizer: *lexer.Lexer) void {
        const zone = util.tracy.initZone(@src(), .{.name = "Parser::call"});
        defer zone.deinit();

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
        const zone = util.tracy.initZone(@src(), .{.name = "Parser::typ"});
        defer zone.deinit();

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
        const zone = util.tracy.initZone(@src(), .{.name = "Parser::property"});
        defer zone.deinit();

        const name = tokenizer.current;

        if (!tokenizer.match(token.Token.IDEN)) @panic("TODO");

        self.manager.nodes.push(node.Node{
            .Property = node.Property.new(name.Identifier),
        }) catch @panic("TODO");
    }

    fn procedure(self: *Parser, tokenizer: *lexer.Lexer) void {
        const zone = util.tracy.initZone(@src(), .{.name = "Parser::procedure"});
        defer zone.deinit();

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
        const zone = util.tracy.initZone(@src(), .{.name = "Parser::let"});
        defer zone.deinit();

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
        const zone = util.tracy.initZone(@src(), .{.name = "Parser::string"});
        defer zone.deinit();

        _ = tokenizer;
        _ = self;
        @panic("TODO");
    }

    fn scope(self: *Parser, tokenizer: *lexer.Lexer) void {
        const zone = util.tracy.initZone(@src(), .{.name = "Parser::scope"});
        defer zone.deinit();

        var len: util.Index = 0;

        while (!tokenizer.match(token.Token.BRACERIGHT)) {
            self.parse(.Assignment, tokenizer);

            len += 1;
        }

        self.manager.nodes.push(node.Node {.Scope = node.Scope.new(len)}) catch @panic("TODO");
    }

    fn parse(self: *Parser, precedence: Precedence, tokenizer: *lexer.Lexer) void {
        const zone = util.tracy.initZone(@src(), .{.name = "Parser::parse"});
        defer zone.deinit();

        tokenizer.advance();

        if (self.ruler.from_token(tokenizer.previous).prefix) |prefix| {
            prefix(self, tokenizer);
        } else {
            @panic("TODO: Show error");
        }

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
        const zone = util.tracy.initZone(@src(), .{.name = "Parser::deinit"});
        defer zone.deinit();

        self.ruler.deinit(self.arena);
        self.manager.deinit(self.arena);
        self.arena.deinit();
    }
};
