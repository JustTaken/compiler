const Parser = @import("parser.zig").Parser;
const Lexer = @import("lexer.zig").Lexer;
const TokenId = @import("lexer.zig").TokenId;
const Expression = @import("expression.zig").Expression;
const Vec = @import("collections.zig").Vec;
const Arena = @import("collections.zig").Arena;

const Branch = struct {
    handle: Vec(u8),
    start: Vec(u16),

    fn init(arena: *Arena) Branch {
        return Branch {
            .handle = Vec(u8).init(64, arena),
            .start = Vec(u16).init(64, arena),
        };
    }

    fn parse(match: *Match, parser: *Parser) void {
        const self = &match.branch;

        self.handle.push(0);
        self.start.push(parser.lexer.next_start());

        const handle = self.handle.last();

        parser.lexer.consume();
        parser.lexer.consume(); // Equal
        parser.lexer.consume(); // Greater

        handle.*= parser.expression.len(.Expression);
        parser.expression.parse(parser);

        parser.lexer.consume(); // Colon
    }
};

pub const Match = struct {
    handle: Vec(Handle),
    start: Vec(u16),
    branch: Branch,

    const Handle = struct {
        branch: u8,
        count: u8,
    };

    pub fn init(arena: *Arena) Match {
        return Match {
            .handle = Vec(Handle).init(64, arena),
            .start = Vec(u16).init(64, arena),
            .branch = Branch.init(arena),
        };
    }

    pub fn len(self:*const Match) u8 {
        return @intCast(self.handle.len);
    }

    pub fn parse(self: *Match, parser: *Parser) void {
        self.start.push(parser.lexer.next_start());
        self.handle.push(Handle {
            .branch = @intCast(self.branch.handle.len),
            .count = 0,
        });

        const handle = self.handle.last();

        parser.lexer.consume();

        parser.lexer.consume();
        parser.lexer.consume(); // CurlyBracketLeft

        while (parser.lexer.has_next(parser)) {
            const token_id = parser.lexer.next_id();

            switch (token_id) {
                .Identifier => {
                    Branch.parse(self, parser);
                    handle.count += 1;

                    parser.lexer.consume();
                },
                .Symbol => {
                    break;
                },
                else => @panic("Should not happen"),
            }
        }

        parser.lexer.consume(); // CurlyBracketRight
    }
};

