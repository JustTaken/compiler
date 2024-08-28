const util = @import("util.zig");

const Lexer = @import("lexer.zig").Lexer;
const Vec = @import("collections.zig").Vec;
const Keyword = @import("lexer.zig").Keyword;
const Operator = @import("lexer.zig").Operator;
const TokenId = @import("lexer.zig").TokenId;
const Arena = @import("collections.zig").Arena;
const HashMap = @import("collections.zig").HashMap;
const Function = @import("function.zig").Function;
const Type = @import("type.zig").Type;
const Match = @import("match.zig").Match;
const Expression = @import("expression.zig").Expression;
const Let = @import("let.zig").Let;
const Scope = @import("scope.zig").Scope;
const Variable = @import("generator.zig").Variable;

const TotalSize = 1024 * 8;

const Root = struct {
    fn parse(parser: *Parser) void {
        while (parser.lexer.has_next(parser)) {
            const token_id = parser.lexer.next_id();
            const token_start = parser.lexer.next_start();

            switch (token_id) {
                .Keyword => {
                    const keyword = TokenId.keyword(
                        parser.lexer.content.offset(token_start),
                    );

                    parser.lexer.consume();
                    switch (keyword) {
                        .Function => parser.function.parse(
                            0,
                            .Function,
                            parser,
                        ),
                        .Type => parser.typ.parse(
                            0,
                            .Type,
                            parser,
                        ),
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
    lexer: *Lexer,
    buffer: Vec(u8),

    function: Function,
    typ: Type,
    let: Let,
    expression: Expression,
    match: Match,
    scope: Scope,

    variable: HashMap(Variable),
    variable_pointer: Vec(u16),

    pub fn init(arena: *Arena, lexer: *Lexer) Parser {
        var self: Parser = undefined;
        self.lexer = lexer;

        self.arena = Arena.init(arena.alloc(u8, TotalSize)[0..TotalSize]);
        self.buffer = Vec(u8).init(512, &self.arena);

        self.function = Function.init(&self.arena);
        self.let = Let.init(&self.arena);
        self.typ = Type.init(&self.arena);
        self.expression = Expression.init(&self.arena);
        self.scope = Scope.init(&self.arena);
        self.match = Match.init(&self.arena);

        self.variable = HashMap(Variable).init(64, &self.arena);
        self.variable_pointer = Vec(u16).init(64, &self.arena);

        return self;
    }

    pub fn parse(self: *Parser) void {
        Root.parse(self);
    }

    pub fn deinit(self: *Parser) void {
        _ = self;
    }
};
