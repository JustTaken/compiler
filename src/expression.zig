const Parser = @import("parser.zig").Parser;
const Lexer = @import("lexer.zig").Lexer;
const TokenId = @import("lexer.zig").TokenId;
const Function = @import("function.zig").Function;
const Vec = @import("collections.zig").Vec;
const Arena = @import("collections.zig").Arena;
const Generator = @import("generator.zig").Generator;

const Inner = struct {
    handle: Vec(Handle),
    start: Vec(u16),

    const Handle = enum(u8) {
        Call,
        Binary,
        Literal,
        Variable,
        Expression,
        TypeConstruct,
        Property,

        fn to_string(self: *Handle) []const u8 {
            return switch (self.*) {
                .Call => "Call",
                .Binary => "Binary",
                .Literal => "Literal",
                .Variable => "Variable",
                .Expression => "Expression",
                .TypeConstruct => "TypeConstruct",
                .Property => "Property",
            };
        }
    };

    fn init(arena: *Arena) Inner {
        return Inner{
            .handle = Vec(Handle).init(64, arena),
            .start = Vec(u16).init(64, arena),
        };
    }

    fn evaluate(
        expression: *Expression,
        index: u8,
        generator: *Generator,
    ) void {
        const handle = &expression.inner.handle.items[index];
        const start = expression.inner.start.items[index];

        generator.content.extend("expression ");

        switch (handle.*) {
            .Call => generator.parser.function.evaluate(
                .FunctionCall,
                @intCast(start),
                generator,
            ),
            .Literal => generator.content.extend(
                TokenId.literal(generator.parser.lexer.content.offset(start)),
            ),
            .Binary => Binary.evaluate(expression, @intCast(start), generator),
            .Variable => generator.content.extend(
                TokenId.identifier(
                    generator.parser.lexer.content.offset(start),
                ),
            ),
            .Expression => evaluate(expression, @intCast(start), generator),
            .TypeConstruct => generator.parser.typ.evaluate(
                .TypeConstruct,
                @intCast(start),
                generator,
            ),
            .Property => @panic("Not implemented yet"),
        }

        generator.content.push('\n');
    }

    fn parse(expression: *Expression, parser: *Parser) void {
        const self = &expression.inner;

        self.start.push(0);
        self.handle.push(.TypeConstruct);

        const start = self.start.last();
        const handle = self.handle.last();

        while (parser.lexer.has_next(parser)) {
            const token_id = parser.lexer.next_id();
            const token_start = parser.lexer.next_start();

            switch (token_id) {
                .Keyword => @panic("Should not happen"),
                .Symbol => {
                    const symbol = TokenId.symbol(
                        parser.lexer.content.offset(token_start),
                    );

                    switch (symbol) {
                        .CurlyBracketRight, .SemiColon, .Colon => break,
                        .ParentesisRight => parser.lexer.consume(),
                        .Dot => {
                            parser.lexer.consume();

                            const s: u16 = @intCast(self.handle.len);

                            parser.typ.parse(start.*, .TypeProperty, parser);
                            start.* = s;
                            handle.* = .Property;
                        },
                        .CurlyBracketLeft => {
                            const s: u16 = @intCast(
                                parser.typ.len(.TypeConstruct),
                            );

                            parser.typ.parse(start.*, .TypeConstruct, parser);
                            start.* = s;
                            handle.* = .TypeConstruct;
                        },
                        .ParentesisLeft => {
                            parser.lexer.consume();

                            if (handle.* == .Variable) {
                                handle.* = .Call;
                                const s = parser.function.len(.FunctionCall);

                                parser.function.parse(
                                    start.*,
                                    .FunctionCall,
                                    parser,
                                );

                                start.* = s;
                            } else {
                                handle.* = .Expression;
                                start.* = @intCast(self.handle.len);
                                parse(expression, parser);
                            }
                        },
                        else => {
                            @panic("Should not be here");
                        },
                    }
                },
                .Operator => {
                    const s = start.*;
                    start.* = expression.len(.Binary);

                    Binary.parse(
                        expression,
                        s,
                        handle.*,
                        parser,
                    );

                    handle.* = .Binary;
                },
                .Identifier => {
                    handle.* = .Variable;
                    start.* = token_start;
                    parser.lexer.consume();
                },
                .Literal => {
                    handle.* = .Literal;
                    start.* = token_start;
                    parser.lexer.consume();
                },
            }
        }
    }
};

const Binary = struct {
    handle: Vec(Handle),
    start: Vec(u16),

    const Handle = struct {
        left: u8,
        right: u8,
    };

    fn init(arena: *Arena) Binary {
        return Binary{
            .handle = Vec(Handle).init(64, arena),
            .start = Vec(u16).init(64, arena),
        };
    }

    fn evaluate(
        expression: *Expression,
        index: u8,
        generator: *Generator,
    ) void {
        const start = expression.binary.start.items[index];

        generator.content.push(
            generator.parser.lexer.content.items[start],
        );
    }

    fn parse(
        expression: *Expression,
        first: u16,
        typ: Inner.Handle,
        parser: *Parser,
    ) void {
        const self = &expression.binary;

        self.handle.push(undefined);
        self.start.push(parser.lexer.next_start());

        parser.lexer.consume();

        const handle = self.handle.last();
        const start = self.start.last();

        handle.left = expression.len(.Expression);
        expression.inner.start.push(first);
        expression.inner.handle.push(typ);

        handle.right = expression.len(.Expression);
        Inner.parse(expression, parser);

        switch (expression.inner.handle.items[handle.right]) {
            .Binary => {
                const other_start = expression.inner.start.items[handle.right];
                const other_binary = &self.start.items[other_start];

                const self_op = TokenId.operator(
                    parser.lexer.content.offset(start.*),
                );

                const other_op = TokenId.operator(
                    parser.lexer.content.offset(other_binary.*),
                );

                if (self_op.precedence() > other_op.precedence()) {
                    const other = &self.handle.items[other_start];
                    const left = handle.left;
                    const new_start = start.*;

                    handle.left = handle.right;
                    handle.right = other.right;

                    other.right = other.left;
                    other.left = left;

                    start.* = other_binary.*;
                    other_binary.* = new_start;
                }
            },
            .Literal, .Variable => {},
            else => @panic("Should not happen"),
        }
    }
};

pub const ExpressionKind = enum {
    Expression,
    Binary,
};

pub const Expression = struct {
    inner: Inner,
    binary: Binary,

    pub fn init(arena: *Arena) Expression {
        return Expression{
            .inner = Inner.init(arena),
            .binary = Binary.init(arena),
        };
    }

    pub fn evaluate(self: *Expression, index: u8, generator: *Generator) void {
        Inner.evaluate(self, index, generator);
    }

    pub fn parse(
        self: *Expression,
        parser: *Parser,
    ) void {
        Inner.parse(self, parser);
    }

    pub fn len(self: *const Expression, kind: ExpressionKind) u8 {
        const l = switch (kind) {
            .Binary => self.binary.handle.len,
            .Expression => self.inner.handle.len,
        };

        return @intCast(l);
    }
};
