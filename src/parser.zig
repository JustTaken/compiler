const std = @import("std");
const util = @import("util.zig");

const Lexer = @import("lexer.zig").Lexer;
const Vec = @import("collections.zig").Vec;
const Keyword = @import("lexer.zig").Keyword;
const Operator = @import("lexer.zig").Operator;
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
    Property,

    fn init(parser: *Parser, lexer: *Lexer) void {
        parser.expression.push(.TypeConstruct);
        parser.expression_start.push(0);

        const self_start = parser.expression_start.last();
        const self = parser.expression.last();

        while (lexer.has_next(parser)) {
            const token_id = lexer.next_id();
            const token_start = lexer.next_start();

            switch (token_id) {
                .Keyword => @panic("Should not happen"),
                .Symbol => {
                    const symbol = TokenId.symbol(
                        lexer.content.offset(token_start),
                    );

                    switch (symbol) {
                        .CurlyBracketRight, .SemiColon, .Colon => break,
                        .ParentesisRight => lexer.consume(),
                        .Dot => {
                            lexer.consume();

                            const start = self_start.*;
                            self_start.* = @intCast(parser.expression.len);

                            Property.init(start, parser, lexer);
                            self.* = .Property;
                        },
                        .CurlyBracketLeft => {
                            const start = self_start.*;
                            self_start.* = @intCast(
                                parser.type_construct.len,
                            );
                            TypeConstruct.init(
                                start,
                                parser,
                                lexer,
                            );

                            self.* = .TypeConstruct;
                        },
                        .ParentesisLeft => {
                            lexer.consume();

                            if (self.* == .Variable) {
                                const start = self_start.*;

                                self.* = ExpressionType.Call;
                                self_start.* = @intCast(
                                    parser.function_call.len,
                                );
                                FunctionCall.init(
                                    start,
                                    parser,
                                    lexer,
                                );
                            } else {
                                self.* = .Expression;
                                self_start.* = @intCast(parser.expression.len);

                                init(parser, lexer);
                            }
                        },
                        else => {
                            @panic("Should not be here");
                        },
                    }
                },
                .Operator => {
                    const start = self_start.*;
                    self_start.* = @intCast(parser.binary.len);

                    Binary.init(
                        start,
                        self.*,
                        parser,
                        lexer,
                    );

                    self.* = .Binary;
                },
                .Identifier => {
                    self.* = .Variable;
                    self_start.* = token_start;
                    lexer.consume();
                },
                .Literal => {
                    self.* = .Literal;
                    self_start.* = token_start;
                    lexer.consume();
                },
            }
        }
    }

    fn evaluate(
        index: u8,
        size: u8,
        location: []const u8,
        parser: *Parser,
        lexer: *Lexer,
        buffer: *Vec(u8),
    ) void {
        const typ = parser.expression.items[index];
        const start = parser.expression_start.items[index];

        switch (typ) {
            .Literal => {
                if (location.len > 0) {
                    buffer.extend("    mov ");
                    buffer.extend(location);
                    buffer.extend(", ");
                }

                const content = TokenId.literal(lexer.content.offset(start));
                buffer.extend(content);
                buffer.push('\n');
            },
            .Variable => {
                if (location.len > 0) {
                    buffer.extend("    mov ");
                    buffer.extend(location);
                    buffer.extend(", ");
                }

                const name = TokenId.identifier(lexer.content.offset(start));
                const variable = parser.variable.get(name).?;
                const pointer = parser.variable_pointer.items[variable];

                buffer.extend(util.to_word(size));
                buffer.extend(" [rsp + ");
                util.parse(pointer, buffer) catch unreachable;
                buffer.extend("]\n");
            },
            .Call => {
                const function_start = parser.function_call_start.items[start];
                const function_name = TokenId.identifier(
                    lexer.content.offset(function_start),
                );

                buffer.extend("    call ");
                buffer.extend(function_name);
                buffer.push('\n');
            },
            .Binary => {
                const binary = &parser.binary.items[start];
                const op = parser.binary_start.items[start];
                const char = lexer.content.offset(op);
                const register = util.register(size);

                if (location.len > 0) {
                    buffer.extend("    mov ");
                    buffer.extend(register);
                    buffer.extend(", ");
                    evaluate(binary.left, size, "", parser, lexer, buffer);

                    buffer.extend(op_string(TokenId.operator(
                        char,
                    )));

                    buffer.extend(register);
                    buffer.extend(", ");

                    evaluate(binary.right, size, "", parser, lexer, buffer);

                    buffer.extend("    mov ");
                    buffer.extend(location);
                    buffer.extend(", ");
                    buffer.extend(register);
                    buffer.push('\n');
                } else {
                    evaluate(binary.left, size, "", parser, lexer, buffer);
                    buffer.extend(op_string(TokenId.operator(
                        char,
                    )));

                    buffer.extend(register);
                    buffer.extend(", ");
                    evaluate(binary.right, size, "", parser, lexer, buffer);
                }
            },
            else => @panic("Should not be here"),
        }
    }
};

const Property = struct {
    expression: u8,

    fn init(start: u16, parser: *Parser, lexer: *Lexer) void {
        parser.property_start.push(start);
        parser.property.push(Property{
            .expression = @intCast(parser.expression.len),
        });

        ExpressionType.init(parser, lexer);
    }
};

fn op_string(op: Operator) []const u8 {
    return switch (op) {
        .Plus => "    add ",
        .Dash => "    sub ",
        .Star => "    imul ",
        else => @panic("Should not be here"),
    };
}

const FunctionCall = struct {
    arg: u8,
    count: u8,

    fn init(start: u16, parser: *Parser, lexer: *Lexer) void {
        parser.function_call.push(undefined);
        parser.function_call_start.push(start);

        var self = parser.function_call.last();

        self.arg = @intCast(parser.function_call_argument.len);
        self.count = 0;

        while (lexer.has_next(parser)) {
            FunctionCallArgument.init(parser, lexer);
            self.count += 1;

            const s = lexer.next_start();
            const id = lexer.next_id();

            switch (id) {
                .Symbol => {
                    const symbol = TokenId.symbol(
                        lexer.content.offset(s),
                    );

                    switch (symbol) {
                        .Colon => {},
                        else => break,
                    }
                },
                else => @panic("Should not happen"),
            }

            lexer.consume();
        }
    }
};

const FunctionCallArgument = struct {
    expression: u8,

    fn init(parser: *Parser, lexer: *Lexer) void {
        parser.function_call_argument.push(
            FunctionCallArgument{
                .expression = @intCast(parser.expression.len),
            },
        );

        ExpressionType.init(parser, lexer);
    }
};

const TypeFieldConstruct = struct {
    expression: u8,

    fn init(parser: *Parser, lexer: *Lexer) void {
        const start = lexer.next_start();

        parser.type_field_construct.push(undefined);
        parser.type_field_construct_start.push(start);

        const self = parser.type_field_construct.last();

        lexer.consume();
        lexer.consume(); // Equal,

        self.expression = @intCast(parser.expression.len);
        ExpressionType.init(parser, lexer);
    }
};

const TypeConstruct = struct {
    field: u8,
    count: u8,

    fn init(start: u16, parser: *Parser, lexer: *Lexer) void {
        parser.type_construct_start.push(start);
        parser.type_construct.push(undefined);

        const self = parser.type_construct.last();
        self.field = @intCast(parser.type_field_construct.len);
        self.count = 0;

        lexer.consume(); // CurlyBracketLeft
        while (lexer.has_next(parser)) {
            const id = lexer.next_id();
            if (id != .Identifier) break;

            TypeFieldConstruct.init(parser, lexer);

            self.count += 1;
            lexer.consume(); // Colon
        }

        lexer.consume(); // CurlyBracketRight
    }
};

const Binary = struct {
    left: u8,
    right: u8,

    fn init(
        first: u16,
        typ: ExpressionType,
        parser: *Parser,
        lexer: *Lexer,
    ) void {
        parser.binary.push(undefined);
        parser.binary_start.push(lexer.next_start());
        lexer.consume();

        const self = parser.binary.last();
        const self_start = parser.binary_start.last();

        self.left = @intCast(parser.expression.len);
        parser.expression_start.push(first);
        parser.expression.push(typ);

        self.right = @intCast(parser.expression.len);
        ExpressionType.init(parser, lexer);

        switch (parser.expression.items[self.right]) {
            .Binary => {
                const other_start = parser.expression_start.items[self.right];
                const other_binary = &parser.binary_start.items[other_start];

                const self_op = TokenId.operator(
                    lexer.content.offset(self_start.*),
                );

                const other_op = TokenId.operator(
                    lexer.content.offset(other_binary.*),
                );

                if (self_op.precedence() > other_op.precedence()) {
                    const other = &parser.binary.items[other_start];
                    const left = self.left;
                    const new_start = self_start.*;

                    self.left = self.right;
                    self.right = other.right;

                    other.right = other.left;
                    other.left = left;

                    self_start.* = other_binary.*;
                    other_binary.* = new_start;
                }
            },
            .Literal, .Variable => {},
            else => @panic("Should not happen"),
        }
    }
};

const TypeField = struct {
    typ: u8,

    fn init(parser: *Parser, lexer: *Lexer) void {
        parser.type_field_start.push(lexer.next_start());
        parser.type_field.push(undefined);

        const self = parser.type_field.last();
        self.typ = 0;

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
            parser.type_size.push(0);
        }
    }

    fn persist(parser: *Parser, _: *Lexer) void {
        const len = parser.type_field.len;
        const first = parser.type_field.items[len];
        const start = parser.type_field_start.items[len];

        parser.type_field_start.clear();
        parser.type_field.clear();

        parser.type_field.push(first);
        parser.type_field_start.push(start);
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

            TypeField.init(parser, lexer);
            self.count += 1;
            lexer.consume(); // Colon
        }

        lexer.consume(); // CurlyBracketRight

        const name = TokenId.identifier(lexer.content.offset(start));

        if (parser.typ.get(name)) |u| {
            parser.typ.value[u] = self;
            parser.type_start.items[u] = start;
        } else {
            parser.typ.push(name, self);
            parser.type_start.push(start);
            parser.type_size.push(0);
        }
    }

    pub fn is_zero(self: *const Type) bool {
        return self.count > 0;
    }

    fn get_size(
        index: u8,
        parser: *Parser,
        lexer: *Lexer,
    ) u8 {
        const self = &parser.typ.value[index];
        const start = parser.type_start.items[index];
        var size = parser.type_size.items[index];

        if (size == 0) {
            if (self.fields == 0 and self.count == 0) {
                const name = TokenId.identifier(lexer.content.offset(start));

                switch (name[0]) {
                    'u' => {
                        if (util.eql("usize", name)) {
                            size = 8;
                        } else if (util.eql("u32", name)) {
                            size = 4;
                        } else if (util.eql("u16", name)) {
                            size = 2;
                        } else if (util.eql("u8", name)) {
                            size = 1;
                        } else {
                            @panic("Shold not happend");
                        }
                    },
                    'i' => {
                        if (util.eql("isize", name)) {
                            size = 8;
                        } else if (util.eql("i32", name)) {
                            size = 4;
                        } else if (util.eql("i16", name)) {
                            size = 2;
                        } else if (util.eql("i8", name)) {
                            size = 1;
                        } else {
                            @panic("Shold not happend");
                        }
                    },
                    else => @panic("Shold not happend"),
                }
            }

            for (0..self.count) |j| {
                const field = &parser.type_field.items[
                    j + self.fields
                ];

                size += Type.get_size(field.typ, parser, lexer);
            }

            parser.type_size.items[index] = size;
        }

        return size;
    }

    fn persist(parser: *Parser, lexer: *Lexer) void {
        const len = parser.type_start.len;
        const first = parser.typ.value[len];
        const start = parser.type_start.items[len];
        const name = TokenId.identifier(lexer.content.offset(start));

        parser.typ.clear();
        parser.type_start.clear();

        parser.typ.push(name, first);
        parser.type_start.push(start);
        parser.type_size.push(0);
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
    typ: u8,

    fn init(parser: *Parser, lexer: *Lexer) void {
        parser.let.push(undefined);
        parser.let_signature.push(undefined);

        const self = parser.let.last();
        const signature = parser.let_signature.last();

        const first_id = lexer.next_id();
        const first_start = lexer.next_start();
        lexer.consume();

        switch (first_id) {
            .Identifier => {
                signature.start = @intCast(first_start);
            },
            .Keyword => {
                const keyword = TokenId.keyword(
                    lexer.content.offset(first_start),
                );

                if (.Mut != keyword) @panic("Should not be here");

                const name_start = lexer.next_start();

                signature.start = @intCast(name_start);
                signature.mutable = true;

                lexer.consume();
            },
            else => @panic("Should not be here"),
        }

        lexer.consume(); // DoubleColon

        const start = lexer.next_start();
        lexer.consume();

        const name = TokenId.identifier(lexer.content.offset(start));
        const typ = parser.typ.get(name);

        if (typ) |t| {
            self.typ = @intCast(t);
        } else {
            self.typ = @intCast(parser.typ.len);
            parser.typ.push(name, Type.zero());
            parser.type_start.push(start);
            parser.type_size.push(0);
        }

        lexer.consume(); // Equal

        self.expression = @intCast(parser.expression.len);
        ExpressionType.init(parser, lexer);

        lexer.consume(); // SemiColon
    }
};

const Match = struct {
    branch: u8,
    count: u8,

    fn init(parser: *Parser, lexer: *Lexer) void {
        parser.match.push(undefined);

        const self = parser.match.last();

        self.branch = @intCast(parser.match_branch.len);
        self.count = 0;

        const id = lexer.next_id();

        switch (id) {
            .Identifier => {
                lexer.consume();
                lexer.consume(); // CurlyBracketLeft
                MatchBranch.init(parser, lexer);

                self.count += 1;
            },
            else => @panic("Should not be here"),
        }

        lexer.consume(); // CurlyBracketRight
    }
};

const MatchBranch = struct {
    expression: u8,

    fn init(parser: *Parser, lexer: *Lexer) void {
        const start = lexer.next_start();

        parser.match_branch.push(undefined);
        parser.match_branch_start.push(start);

        const self = parser.match_branch.last();
        self.expression = @intCast(parser.expression.len);

        lexer.consume();
        lexer.consume(); // Equal
        lexer.consume(); // Greater

        ExpressionType.init(parser, lexer);

        lexer.consume(); // Colon
    }
};

const Parameter = struct {
    typ: u8,

    fn init(parser: *Parser, lexer: *Lexer) Parameter {
        var self = Parameter{
            .typ = 0,
        };

        parser.parameter_start.push(lexer.next_start());

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
            parser.type_size.push(0);
        }

        return self;
    }
};
const ScopeInnerType = enum(u8) {
    Let,
    Match,
    Expression,
};

const ScopeExpression = struct {
    typ: ScopeInnerType,
    start: u8,
};

const Scope = struct {
    expression: u8,
    count: u8,

    fn init(parser: *Parser, lexer: *Lexer) void {
        parser.scope.push(undefined);
        const self = parser.scope.last();

        self.expression = @intCast(parser.scope_expression.len);
        self.count = 0;

        while (lexer.has_next(parser)) {
            parser.scope_expression.push(undefined);

            const expression = parser.scope_expression.last();
            const token_id = lexer.next_id();
            const token_start = lexer.next_start();

            switch (token_id) {
                .Keyword => {
                    lexer.consume();

                    const keyword = TokenId.keyword(
                        lexer.content.offset(token_start),
                    );

                    switch (keyword) {
                        .Let => {
                            expression.typ = .Let;
                            expression.start = @intCast(parser.let.len);
                            Let.init(parser, lexer);
                        },
                        .Match => {
                            expression.typ = .Match;
                            expression.start = @intCast(parser.match.len);
                            Match.init(parser, lexer);
                        },
                        else => @panic("Should not be here"),
                    }
                },
                .Identifier => {
                    expression.typ = .Expression;
                    expression.start = @intCast(parser.expression.len);
                    ExpressionType.init(parser, lexer);

                    const id = lexer.next_id();
                    switch (id) {
                        .Symbol => {
                            const symbol = TokenId.symbol(
                                lexer.content.offset(lexer.next_start()),
                            );

                            if (.SemiColon == symbol) lexer.consume() else break;
                        },
                        else => @panic("Should not happen"),
                    }
                },
                else => break,
            }

            self.count += 1;
        }
    }

    fn evaluate(
        index: u8,
        typ: u8,
        parser: *Parser,
        lexer: *Lexer,
        buffer: *Vec(u8),
    ) void {
        const self = parser.scope.items[index];

        var offset: u16 = 0;
        const buffer_offset = buffer.len;
        _ = buffer_offset;

        for (0..self.count) |i| {
            const expression = parser.scope_expression.items[
                self.expression + i
            ];

            switch (expression.typ) {
                .Let => {
                    const let = &parser.let.items[expression.start];
                    const size = Type.get_size(let.typ, parser, lexer);

                    Variable.init(
                        parser.let_signature.items[expression.start].start,
                        offset,
                        let.typ,
                        parser,
                        lexer,
                    );

                    parser.buffer.clear();
                    parser.buffer.extend(util.to_word(size));
                    parser.buffer.extend(" [rbp + ");
                    util.parse(offset, &parser.buffer) catch unreachable;
                    parser.buffer.extend("]");

                    ExpressionType.evaluate(
                        let.expression,
                        size,
                        parser.buffer.offset(0),
                        parser,
                        lexer,
                        buffer,
                    );

                    offset += Type.get_size(let.typ, parser, lexer);
                },
                .Expression => {
                    const size = Type.get_size(typ, parser, lexer);

                    ExpressionType.evaluate(
                        expression.start,
                        size,
                        "rax",
                        parser,
                        lexer,
                        buffer,
                    );
                },

                else => @panic("Should not happen"),
            }
        }
    }
};

pub const Function = struct {
    typ: u8,
    parameter_start: u8,
    parameter_count: u8,
    scope: u8,

    fn init(parser: *Parser, lexer: *Lexer) void {
        const start = lexer.next_start();
        lexer.consume();
        lexer.consume(); // Parentesis

        parser.function_start.push(start);
        parser.function.push(.{
            .typ = 0,
            .scope = @intCast(parser.scope.len),
            .parameter_start = @intCast(parser.function.len),
            .parameter_count = 0,
        });

        const self = parser.function.last();

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
            parser.type_size.push(0);
        }

        lexer.consume();
        lexer.consume(); // CurlyBracketLeft

        Scope.init(parser, lexer);

        lexer.consume(); // CurlyBracketRight
    }

    pub fn generate(
        id: u8,
        parser: *Parser,
        lexer: *Lexer,
        buffer: *Vec(u8),
    ) void {
        const self = parser.function.get(id);
        const start = parser.function_start.get(id).*;

        parser.buffer.clear();
        buffer.extend(TokenId.identifier(lexer.content.offset(start)));
        buffer.extend(":\n");

        var offset: u16 = 0;
        for (0..self.parameter_count) |i| {
            const index = self.parameter_start + self.parameter_count - i - 1;
            const param = &parser.parameter.items[index];
            const param_start = parser.parameter_start.items[index];

            offset += Type.get_size(param.typ, parser, lexer);

            Variable.init(
                param_start,
                offset,
                param.typ,
                parser,
                lexer,
            );
        }

        Scope.evaluate(self.scope, self.typ, parser, lexer, buffer);
        buffer.extend("    ret\n");
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

const Variable = struct {
    typ: u8,

    fn init(
        start: u16,
        stack_pointer: u16,
        typ: u8,
        parser: *Parser,
        lexer: *Lexer,
    ) void {
        const name = TokenId.identifier(lexer.content.offset(start));

        parser.variable.push(name, .{ .typ = typ });
        parser.variable_pointer.push(stack_pointer);
    }

    pub fn is_zero(_: *const Variable) bool {
        return true;
    }
};

pub const Parser = struct {
    arena: Arena,
    buffer: Vec(u8),
    variable: HashMap(Variable),
    variable_pointer: Vec(u16),
    function: Vec(Function),
    function_start: Vec(u16),
    let: Vec(Let),
    let_signature: Vec(LetSignature),
    parameter: Vec(Parameter),
    parameter_start: Vec(u16),
    expression: Vec(ExpressionType),
    expression_start: Vec(u16),
    binary: Vec(Binary),
    binary_start: Vec(u16),
    match: Vec(Match),
    match_branch: Vec(MatchBranch),
    match_branch_start: Vec(u16),
    typ: HashMap(Type),
    type_start: Vec(u16),
    type_size: Vec(u8),
    type_field: Vec(TypeField),
    type_field_start: Vec(u16),
    type_construct: Vec(TypeConstruct),
    type_construct_start: Vec(u16),
    type_field_construct: Vec(TypeFieldConstruct),
    type_field_construct_start: Vec(u16),
    property: Vec(Property),
    property_start: Vec(u16),
    function_call: Vec(FunctionCall),
    function_call_argument: Vec(FunctionCallArgument),
    function_call_start: Vec(u16),
    scope: Vec(Scope),
    scope_inner_type: Vec(ScopeInnerType),
    scope_expression: Vec(ScopeExpression),
    type_file: std.fs.File,

    pub fn init(arena: *Arena) Parser {
        var self: Parser = undefined;

        self.arena = Arena.init(arena.alloc(u8, TotalSize)[0..TotalSize]);
        self.buffer = Vec(u8).init(512, &self.arena);
        self.variable = HashMap(Variable).init(64, &self.arena);
        self.variable_pointer = Vec(u16).init(64, &self.arena);
        self.typ = HashMap(Type).init(64, &self.arena);
        self.type_start = Vec(u16).init(64, &self.arena);
        self.type_size = Vec(u8).init(64, &self.arena);
        self.function = Vec(Function).init(64, &self.arena);
        self.function_start = Vec(u16).init(64, &self.arena);
        self.let = Vec(Let).init(256, &self.arena);
        self.let_signature = Vec(LetSignature).init(256, &self.arena);
        self.parameter = Vec(Parameter).init(128, &self.arena);
        self.parameter_start = Vec(u16).init(128, &self.arena);
        self.expression = Vec(ExpressionType).init(256, &self.arena);
        self.expression_start = Vec(u16).init(256, &self.arena);
        self.binary = Vec(Binary).init(128, &self.arena);
        self.binary_start = Vec(u16).init(128, &self.arena);
        self.type_field = Vec(TypeField).init(128, &self.arena);
        self.type_field_start = Vec(u16).init(128, &self.arena);
        self.scope = Vec(Scope).init(64, &self.arena);
        self.scope_inner_type = Vec(ScopeInnerType).init(64, &self.arena);
        self.scope_expression = Vec(ScopeExpression).init(64, &self.arena);
        self.function_call = Vec(FunctionCall).init(64, &self.arena);
        self.function_call_start = Vec(u16).init(64, &self.arena);
        self.function_call_argument = Vec(FunctionCallArgument).init(
            64,
            &self.arena,
        );

        self.property = Vec(Property).init(64, &self.arena);
        self.property_start = Vec(u16).init(64, &self.arena);

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
            64,
            &self.arena,
        );

        self.type_construct = Vec(TypeConstruct).init(
            64,
            &self.arena,
        );

        self.type_field_construct_start = Vec(u16).init(
            64,
            &self.arena,
        );

        self.type_field_construct = Vec(TypeFieldConstruct).init(
            64,
            &self.arena,
        );

        return self;
    }

    pub fn parse(self: *Parser, lexer: *Lexer) void {
        Root.parse(self, lexer);
    }

    const Persist = struct {
        let: bool,
        parameter: bool,
        type_field: bool,
        type_field_construct: bool,
        match_branch: bool,
    };

    // fn clear(parser: *Parser, lexer: *Lexer, persist: Persist) void {
    //     if (persist.type_field) {
    //         TypeField.persist(parser, lexer);
    //         Type.persist(parser, lexer);
    //     } else {
    //         parser.typ.clear();
    //         parser.type_start.clear();
    //         parser.type_field.clear();
    //         parser.type_field_start.clear();
    //     }

    //     if (persist.parameter) {
    //         const len = parser.parameter.len;
    //         const last = parser.parameter.items[len];
    //         const start = parser.parameter_start.items[len];
    //         parser.parameter.clear();
    //         parser.parameter_start.clear();
    //         parser.parameter.push(last);
    //         parser.parameter_start.push(start);
    //     } else {
    //         parser.parameter.clear();
    //         parser.parameter_start.clear();
    //     }

    //     if (persist.let) {
    //         const len = parser.let.len;
    //         const last = parser.let.items[len];
    //         const sig = parser.let_signature.items[len];
    //         parser.let.clear();
    //         parser.let_signature.clear();
    //         parser.let.push(last);
    //         parser.let_signature.push(sig);
    //     } else {
    //         parser.let.clear();
    //         parser.let_signature.clear();
    //     }
    // }

    pub fn reset(self: *Parser, lexer: *Lexer) void {
        // var buffer: [100]u8 = undefined;

        // const size: u8 = @intCast(self.typ.len);
        // _ = self.type_file.write(&.{size}) catch
        //     @panic("Could not write to file");

        // var offset: u16 = 0;
        // var written: usize = 0;

        // for (0..self.typ.len) |i| {
        //     const typ = &self.typ.value[i];
        //     const start = self.type_start.items[i];
        //     const name = TokenId.identifier(lexer.content.offset(start));

        //     _ = self.type_file.write(
        //         &.{
        //             typ.count,
        //             @intCast((offset & 0xFF00) >> 1),
        //             @intCast(offset & 0x00FF),
        //         },
        //     ) catch @panic("Could not write to file");

        //     offset += typ.count + 2;
        //     written += 1;
        // }

        // self.type_file.seekTo(0) catch unreachable;
        // const b = self.type_file.read(&buffer) catch unreachable;

        _ = lexer;
        self.function.clear();
        self.function_start.clear();
        self.let.clear();
        self.let_signature.clear();
        self.parameter.clear();
        self.parameter_start.clear();
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
        _ = self;
    }
};
