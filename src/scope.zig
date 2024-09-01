const Parser = @import("parser.zig").Parser;
const Lexer = @import("lexer.zig").Lexer;
const TokenId = @import("lexer.zig").TokenId;
const Expression = @import("expression.zig").Expression;
const Let = @import("let.zig").Let;
const Match = @import("match.zig").Match;
const Vec = @import("collections.zig").Vec;
const Arena = @import("collections.zig").Arena;
const Generator = @import("generator.zig").Generator;

pub const Kind = enum(u8) {
    Let,
    Match,
    Expression,
};

const Inner = struct {
    kind: Vec(Kind),
    start: Vec(u8),

    fn init(arena: *Arena) Inner {
        return Inner{
            .kind = Vec(Kind).init(64, arena),
            .start = Vec(u8).init(64, arena),
        };
    }

    fn evaluate(scope: *Scope, index: u8, generator: *Generator) void {
        const start = scope.start.items[index];
        const self = &scope.inner;

        for (0..scope.count.items[index]) |i| {
            const kind = self.kind.items[i + start];
            const s = self.start.items[i + start];

            switch (kind) {
                .Let => generator.parser.let.evaluate(
                    s,
                    generator,
                ),
                .Expression => generator.parser.expression.evaluate(
                    s,
                    generator,
                ),
                .Match => generator.parser.match.evaluate(
                    s,
                    generator,
                ),
            }
        }
    }

    fn keyword(scope: *Scope, parser: *Parser) void {
        const self = &scope.inner;

        const token_start = parser.lexer.next_start();
        const k = TokenId.keyword(
            parser.lexer.content.offset(token_start),
        );

        parser.lexer.consume();

        switch (k) {
            .Let => {
                self.kind.push(.Let);
                self.start.push(parser.let.len());
                parser.let.parse(parser);
            },
            .Match => {
                self.kind.push(.Match);
                self.start.push(parser.match.len());
                parser.match.parse(parser);
            },
            else => @panic("Should not be here"),
        }
    }

    fn expression(scope: *Scope, parser: *Parser) void {
        const self = &scope.inner;

        self.kind.push(.Expression);
        self.start.push(parser.expression.len(.Expression));

        parser.expression.parse(parser);

        const id = parser.lexer.next_id();
        switch (id) {
            .Symbol => {
                const symbol = TokenId.symbol(
                    parser.lexer.content.offset(
                        parser.lexer.next_start(),
                    ),
                );

                if (.SemiColon == symbol) {
                    parser.lexer.consume();
                }
            },
            else => @panic("Should not happen"),
        }
    }
};

pub const Scope = struct {
    inner: Inner,
    start: Vec(u8),
    count: Vec(u8),

    pub fn init(arena: *Arena) Scope {
        return Scope{
            .inner = Inner.init(arena),
            .start = Vec(u8).init(32, arena),
            .count = Vec(u8).init(32, arena),
        };
    }

    pub fn len(self: *const Scope) u8 {
        return @intCast(self.start.len);
    }

    pub fn evaluate(self: *Scope, index: u8, generator: *Generator) void {
        Inner.evaluate(self, index, generator);
    }

    pub fn parse(self: *Scope, parser: *Parser) void {
        self.start.push(@intCast(self.inner.kind.len));
        self.count.push(0);

        const count = self.count.last();

        while (parser.lexer.has_next(parser)) {
            const token_id = parser.lexer.next_id();

            switch (token_id) {
                .Keyword => Inner.keyword(self, parser),
                .Identifier => Inner.expression(self, parser),
                else => break,
            }

            count.* += 1;
        }
    }
};
