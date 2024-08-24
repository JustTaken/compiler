const std = @import("std");

const Lexer = @import("lexer.zig").Lexer;
const Vec = @import("collections.zig").Vec;
const Keyword = @import("lexer.zig").Keyword;
const TokenId = @import("lexer.zig").TokenId;
const Arena = @import("collections.zig").Arena;
const HashMap = @import("collections.zig").HashMap;

const TotalSize = 1024 * 8;

const ExpressionType = enum(u8) {
    Call,
    Binary,
    Literal,
    Variable,
    Expression,
    TypeConstruct,

    fn init(parser: *Parser, lexer: *Lexer) ExpressionType {
        const self_pos: u8 = @intCast(parser.expression_start.len);
        parser.expression_start.push(0);
        const self_start = parser.expression_start.get(self_pos);

        var self = ExpressionType.Variable;

        while (lexer.has_next(parser)) {
            const token_id = lexer.next_id();
            const token_start = lexer.next_start();

            switch (token_id) {
                .Keyword => break,
                .Symbol => {
                    const symbol_typ = TokenId.symbol(
                        lexer.content.offset(token_start),
                    );

                    if (symbol_typ == .ParentesisRight) {
                        lexer.consume();
                        break;
                    } else if (symbol_typ == .CurlyBracketLeft) {
                        const start = self_start.*;
                        self_start.* = @intCast(parser.type_construct.len);

                        parser.type_construct.push(TypeConstruct.init(
                            start,
                            parser,
                            lexer,
                        ));

                        self = .TypeConstruct;

                        break;
                    } else if (symbol_typ != .ParentesisLeft) {
                        break;
                    } else {
                        self_start.* = @intCast(parser.expression.len);
                        self = .Expression;

                        parser.expression.push(init(parser, lexer));
                    }
                },
                .Operator => {
                    if (self == .Binary) {
                        Binary.extend(
                            @intCast(self_start.*),
                            parser,
                            lexer,
                        );
                    } else {
                        const start = self_start.*;
                        self_start.* = @intCast(parser.binary.len);

                        parser.binary.push(Binary.collect(
                            start,
                            self,
                            parser,
                            lexer,
                        ));

                        self = .Binary;
                    }
                },
                .Identifier => {
                    self = .Variable;
                    self_start.* = token_start;
                },
                .Literal => {
                    self = .Literal;
                    self_start.* = token_start;
                },
            }

            lexer.consume();
        }

        return self;
    }
};

const TypeFieldConstruct = struct {
    expression: u8,

    fn init(parser: *Parser, lexer: *Lexer) TypeFieldConstruct {
        var self: TypeFieldConstruct = undefined;

        const start = lexer.next_start();
        parser.type_field_construct_start.push(start);

        lexer.consume();
        lexer.consume(); // Equal,

        parser.expression.push(ExpressionType.init(parser, lexer));
        self.expression = @intCast(parser.expression.len - 1);

        return self;
    }
};

const TypeConstruct = struct {
    field: u8,
    count: u8,

    fn init(start: u16, parser: *Parser, lexer: *Lexer) TypeConstruct {
        var self = TypeConstruct{
            .field = @intCast(parser.type_field_construct.len),
            .count = 0,
        };

        parser.type_construct_start.push(start);

        lexer.consume(); // CurlyBracketLeft
        while (lexer.has_next(parser)) {
            const id = lexer.next_id();
            if (id != .Identifier) break;

            parser.type_field_construct.push(
                TypeFieldConstruct.init(parser, lexer),
            );

            self.count += 1;
            lexer.consume(); // Colon
        }

        lexer.consume(); // CurlyBracketRight

        return self;
    }
};

const Binary = struct {
    left: u8,
    right: u8,

    fn collect(
        first: u16,
        typ: ExpressionType,
        parser: *Parser,
        lexer: *Lexer,
    ) Binary {
        var self: Binary = undefined;
        const left: u8 = @intCast(parser.expression.len);

        parser.expression_start.push(first);
        parser.expression.push(
            if (typ == .Literal) .Literal else .Variable,
        );

        const start = lexer.next_start();
        parser.binary_start.push(start);

        lexer.consume();

        const token_id = lexer.next_id();
        const token_start = lexer.next_start();

        const right: u8 = @intCast(parser.expression.len);

        parser.expression_start.push(token_start);
        parser.expression.push(
            if (token_id == .Literal) .Literal else .Variable,
        );

        self.left = left;
        self.right = right;

        return self;
    }

    fn extend(s: u8, parser: *Parser, lexer: *Lexer) void {
        var self = parser.binary.get(s);
        const start = parser.binary_start.get(s);

        const op = lexer.next_start();
        lexer.consume();

        const token_id = lexer.next_id();
        const token_start = lexer.next_start();
        lexer.consume();

        const other_op = TokenId.operator(lexer.content.offset(op));
        const self_op = TokenId.operator(lexer.content.offset(start.*));

        const new: u8 = @intCast(parser.expression.len);
        parser.expression_start.push(token_start);
        parser.expression.push(
            if (token_id == .Identifier) .Variable else .Literal,
        );

        var new_start = op;
        var binary = Binary{
            .left = self.right,
            .right = new,
        };

        if (self_op.precedence() > other_op.precedence()) {
            binary.left = self.left;
            binary.right = self.right;

            self.right = new;
            self.left = @intCast(parser.expression.len);

            new_start = start.*;
            parser.binary_start.items[s] = op;
        } else self.right = @intCast(parser.expression.len);

        parser.expression_start.push(@intCast(parser.binary.len));
        parser.expression.push(.Binary);

        parser.binary_start.push(new_start);
        parser.binary.push(binary);
    }
};

const TypeField = struct {
    typ: u8,

    fn init(parser: *Parser, lexer: *Lexer) TypeField {
        var self = TypeField{
            .typ = 0,
        };

        parser.type_field_start.push(lexer.next_start());

        lexer.consume();
        lexer.consume(); // DoubleColon

        const inner = lexer.next_start();
        lexer.consume();

        const name = TokenId.identifier(
            lexer.content.offset(inner),
        );

        const typ = parser.typ.get(name);

        if (typ) |t| {
            self.typ = @intCast(t);
        } else {
            self.typ = @intCast(parser.type_start.len);

            parser.typ.push(name, Type.zero());
            parser.type_start.push(inner);
        }

        return self;
    }
};

const Type = struct {
    fields: u8,
    count: u8,

    fn init(parser: *Parser, lexer: *Lexer) void {
        const start = lexer.next_start();
        var self = Type{
            .count = 0,
            .fields = @intCast(parser.type_field.len),
        };

        lexer.consume();
        lexer.consume(); // CurlyBracketLeft

        while (lexer.has_next(parser)) {
            const token_id = lexer.next_id();

            if (token_id != .Identifier) break;

            parser.type_field.push(TypeField.init(parser, lexer));
            self.count += 1;
            lexer.consume(); // Colon
        }

        lexer.consume(); // CurlyBracketRight

        const name = TokenId.identifier(lexer.content.offset(start));

        parser.typ.push(name, self);
        parser.type_start.push(start);
    }

    pub fn zero() Type {
        return .{
            .fields = 0,
            .count = 0,
        };
    }
};

const LetSignature = packed struct {
    start: u15,
    mutable: bool,
};

const Let = struct {
    expression: u8,

    fn init(parser: *Parser, lexer: *Lexer) Let {
        var self = Let{
            .expression = 0,
        };

        const pos = parser.let_signature.len;
        parser.let_signature.push(
            LetSignature{ .start = 0, .mutable = false },
        );

        const first_id = lexer.next_id();
        const first_start = lexer.next_start();
        lexer.consume();

        switch (first_id) {
            .Identifier => {
                parser.let_signature.items[pos].start = @intCast(first_start);
            },
            .Keyword => {
                const keyword = TokenId.keyword(
                    lexer.content.offset(first_start),
                );

                if (.Mut != keyword) @panic("Should not be here");

                const name_start = lexer.next_start();

                parser.let_signature.items[pos].start = @intCast(name_start);
                parser.let_signature.items[pos].mutable = true;

                lexer.consume();
            },
            else => @panic("Should not be here"),
        }

        lexer.consume(); // Equal

        parser.expression.push(ExpressionType.init(parser, lexer));
        self.expression = @intCast(parser.expression.len - 1);
        lexer.consume(); // SemiColon

        return self;
    }
};

const Match = struct {
    branch: u8,
    count: u8,

    fn init(parser: *Parser, lexer: *Lexer) Match {
        var self = Match{
            .branch = @intCast(parser.match_branch.len),
            .count = 0,
        };

        const id = lexer.next_id();

        switch (id) {
            .Identifier => {
                lexer.consume();
                lexer.consume(); // CurlyBracketLeft

                parser.match_branch.push(MatchBranch.init(parser, lexer));
                self.count += 1;
            },
            else => @panic("Should not be here"),
        }

        lexer.consume(); // CurlyBracketRight

        return self;
    }
};

const MatchBranch = struct {
    expression: u8,

    fn init(parser: *Parser, lexer: *Lexer) MatchBranch {
        var self = MatchBranch{
            .expression = 0,
        };

        const start = lexer.next_start();
        parser.match_branch_start.push(start);

        lexer.consume();
        lexer.consume(); // Equal
        lexer.consume(); // Greater

        self.expression = @intCast(parser.expression.len);
        parser.expression.push(ExpressionType.init(parser, lexer));

        lexer.consume(); // Colon

        return self;
    }
};

const Parameter = struct {
    typ: u8,

    fn init(parser: *Parser, lexer: *Lexer) Parameter {
        var self = Parameter{
            .typ = 0,
        };

        const start = lexer.next_start();
        parser.parameter_name.push(start);

        lexer.consume();
        lexer.consume(); // DoubleColon

        const inner = lexer.next_start();
        lexer.consume();

        const name = TokenId.identifier(
            lexer.content.offset(inner),
        );

        const typ = parser.typ.get(name);

        if (typ) |t| {
            self.typ = @intCast(t);
        } else {
            self.typ = @intCast(parser.type_start.len);

            parser.typ.push(name, Type.zero());
            parser.type_start.push(inner);
        }

        return self;
    }
};

pub const Function = struct {
    typ: u8,
    parameter_start: u8,
    parameter_count: u8,
    match_start: u8,
    match_count: u8,
    let_start: u8,
    let_count: u8,

    fn init(parser: *Parser, lexer: *Lexer) void {
        var self = Function{
            .typ = 0,
            .match_start = @intCast(parser.match.len),
            .match_count = 0,
            .parameter_start = @intCast(parser.function.len),
            .parameter_count = 0,
            .let_start = @intCast(parser.let.len),
            .let_count = 0,
        };

        const start = lexer.next_start();
        parser.function_name.push(start);

        lexer.consume();
        lexer.consume(); // Parentesis

        while (lexer.has_next(parser)) {
            const token_id = lexer.next_id();
            const token_start = lexer.next_start();
            if (.Symbol == token_id) {
                lexer.consume();
                const symbol = TokenId.symbol(
                    lexer.content.offset(token_start),
                );

                switch (symbol) {
                    .ParentesisRight => break,
                    .Colon => {},
                    else => {
                        @panic("Should not be here");
                    },
                }
            }

            parser.parameter.push(Parameter.init(parser, lexer));
            self.parameter_count += 1;
        }

        lexer.consume(); // DoubleColon

        const inner = lexer.next_start();
        const name = TokenId.identifier(
            lexer.content.offset(inner),
        );
        const typ = parser.typ.get(name);

        if (typ) |t| {
            self.typ = @intCast(t);
        } else {
            self.typ = @intCast(parser.type_start.len);

            parser.typ.push(name, Type.zero());
            parser.type_start.push(inner);
        }

        lexer.consume();
        lexer.consume(); // CurlyBracketLeft

        while (lexer.has_next(parser)) {
            const token_id = lexer.next_id();
            const token_start = lexer.next_start();

            if (token_id != .Keyword) break;

            lexer.consume();

            const keyword = TokenId.keyword(
                lexer.content.offset(token_start),
            );

            if (.Let == keyword) {
                parser.let.push(Let.init(parser, lexer));
                self.let_count += 1;
            } else if (.Match == keyword) {
                parser.match.push(Match.init(parser, lexer));
                self.match_count += 1;
            } else @panic("Support more keywords");
        }

        const next = lexer.next_id();
        if (next == .Identifier or next == .Literal) {
            lexer.consume(); // return value
        }

        lexer.consume(); // CurlyBracketRight

        parser.function.push(self);
    }

    pub fn generate(
        id: u8,
        parser: *Parser,
        lexer: *Lexer,
        buffer: *Vec(u8),
    ) void {
        const self = parser.function.get(id);
        const start = parser.function_name.get(id).*;
        buffer.extend(TokenId.identifier(lexer.content.offset(start)));
        buffer.extend(":\n");

        for (0..self.parameter_count) |i| {
            const p = &parser.parameter.items[self.parameter_start + i];
            const type_start = parser.type_start.items[p.typ];

            const name = TokenId.identifier(lexer.content.offset(type_start));
            _ = name;
        }
    }
};

const Root = struct {
    fn parse(parser: *Parser, lexer: *Lexer) void {
        while (lexer.has_next(parser)) {
            const token_id = lexer.next_id();
            const token_start = lexer.next_start();

            switch (token_id) {
                .Keyword => {
                    const keyword = TokenId.keyword(
                        lexer.content.offset(token_start),
                    );

                    lexer.consume();
                    switch (keyword) {
                        .Function => Function.init(parser, lexer),
                        .Type => Type.init(parser, lexer),
                        else => @panic("Should not be here"),
                    }
                },
                else => @panic("Should not be here"),
            }
        }
    }
};

pub const Parser = struct {
    arena: Arena,
    function: Vec(Function),
    function_name: Vec(u16),
    let: Vec(Let),
    let_signature: Vec(LetSignature),
    parameter: Vec(Parameter),
    parameter_name: Vec(u16),
    expression: Vec(ExpressionType),
    expression_start: Vec(u16),
    binary: Vec(Binary),
    binary_start: Vec(u16),
    match: Vec(Match),
    match_branch: Vec(MatchBranch),
    match_branch_start: Vec(u16),
    typ: HashMap(Type),
    type_start: Vec(u16),
    type_field: Vec(TypeField),
    type_field_start: Vec(u16),
    type_construct: Vec(TypeConstruct),
    type_construct_start: Vec(u16),
    type_field_construct: Vec(TypeFieldConstruct),
    type_field_construct_start: Vec(u16),
    type_file: std.fs.File,

    pub fn init(arena: *Arena) Parser {
        var self: Parser = undefined;

        self.arena = Arena.init(arena.alloc(u8, TotalSize)[0..TotalSize]);
        self.typ = HashMap(Type).init(64, &self.arena);
        self.type_start = Vec(u16).init(64, &self.arena);
        self.function = Vec(Function).init(64, &self.arena);
        self.function_name = Vec(u16).init(64, &self.arena);
        self.let = Vec(Let).init(256, &self.arena);
        self.let_signature = Vec(LetSignature).init(256, &self.arena);
        self.parameter = Vec(Parameter).init(128, &self.arena);
        self.parameter_name = Vec(u16).init(128, &self.arena);
        self.expression = Vec(ExpressionType).init(256, &self.arena);
        self.expression_start = Vec(u16).init(256, &self.arena);
        self.binary = Vec(Binary).init(128, &self.arena);
        self.binary_start = Vec(u16).init(128, &self.arena);
        self.type_field = Vec(TypeField).init(128, &self.arena);
        self.type_field_start = Vec(u16).init(128, &self.arena);

        self.match = Vec(Match).init(32, &self.arena);
        self.match_branch_start = Vec(u16).init(
            64,
            &self.arena,
        );

        self.match_branch = Vec(MatchBranch).init(
            64,
            &self.arena,
        );

        self.type_construct_start = Vec(u16).init(
            128,
            &self.arena,
        );

        self.type_construct = Vec(TypeConstruct).init(
            128,
            &self.arena,
        );

        self.type_field_construct_start = Vec(u16).init(
            128,
            &self.arena,
        );

        self.type_field_construct = Vec(TypeFieldConstruct).init(
            128,
            &self.arena,
        );

        self.type_file = std.fs.cwd().createFile("typ.txt", .{ .read = true }) catch @panic("Could not open the file");

        return self;
    }

    pub fn parse(self: *Parser, lexer: *Lexer) void {
        Root.parse(self, lexer);
    }

    pub fn reset(self: *Parser, lexer: *Lexer) void {
        var buffer: [100]u8 = undefined;

        const size: u8 = @intCast(self.typ.len);
        _ = self.type_file.write(&.{size}) catch @panic("Could not write to file");

        var offset: u16 = 0;

        for (0..self.typ.len) |i| {
            const count: u8 = @intCast(self.typ.value[i].count);
            const start = self.type_start.items[i];
            const name = TokenId.identifier(lexer.content.offset(start));
            std.debug.print("count: {d} offset: {d}, name: {s}\n", .{ count, offset ,name});
            _ = self.type_file.write(
                &.{
                    count,
                    @intCast((offset & 0xFF00) >> 1),
                    @intCast(offset & 0x00FF),
                },
            ) catch @panic("Could not write to file");

            offset += count + 2;
        }

        self.type_file.seekTo(0) catch unreachable;
        const b = self.type_file.read(&buffer) catch unreachable;

        for (buffer[0..b]) |c| {
            std.debug.print("{d} ", .{c});
        }

        self.function.clear();
        self.function_name.clear();
        self.let.clear();
        self.let_signature.clear();
        self.parameter.clear();
        self.parameter_name.clear();
        self.expression.clear();
        self.expression_start.clear();
        self.binary.clear();
        self.binary_start.clear();
        self.match.clear();
        self.match_branch.clear();
        self.match_branch_start.clear();
        self.typ.clear();
        self.type_start.clear();
        self.type_field.clear();
        self.type_field_start.clear();
        self.type_construct.clear();
        self.type_construct_start.clear();
        self.type_field_construct.clear();
        self.type_field_construct_start.clear();

    }

    pub fn deinit(self: *Parser) void {
        self.type_file.close();
    }
};
