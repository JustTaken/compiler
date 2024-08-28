const Parser = @import("parser.zig").Parser;
const Lexer = @import("lexer.zig").Lexer;
const TokenId = @import("lexer.zig").TokenId;
const Function = @import("function.zig").Function;
const Vec = @import("collections.zig").Vec;
const Arena = @import("collections.zig").Arena;

const Kind = struct {
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
    };

    fn init(arena: *Arena) Kind {
        return Kind{
            .handle = Vec(Handle).init(64, arena),
            .start = Vec(u16).init(64, arena),
        };
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

    fn parse(
        expression: *Expression,
        first: u16,
        typ: Kind.Handle,
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
        Kind.parse(expression, parser);

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
    inner: Kind,
    binary: Binary,

    pub fn init(arena: *Arena) Expression {
        return Expression{
            .inner = Kind.init(arena),
            .binary = Binary.init(arena),
        };
    }

    pub fn parse(
        self: *Expression,
        parser: *Parser,
    ) void {
        Kind.parse(self, parser);
    }

    pub fn len(self: *const Expression, kind: ExpressionKind) u8 {
        const l = switch (kind) {
            .Binary => self.binary.handle.len,
            .Expression => self.inner.handle.len,
        };

        return @intCast(l);
    }
};
