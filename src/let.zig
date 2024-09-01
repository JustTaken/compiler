const Parser = @import("parser.zig").Parser;
const Lexer = @import("lexer.zig").Lexer;
const TokenId = @import("lexer.zig").TokenId;
const Expression = @import("expression.zig").Expression;
const Type = @import("type.zig").Type;
const Vec = @import("collections.zig").Vec;
const Arena = @import("collections.zig").Arena;
const Generator = @import("generator.zig").Generator;

pub const Signature = packed struct {
    start: u15,
    mutable: bool,
};

pub const Let = struct {
    handle: Vec(Handle),
    signature: Vec(Signature),

    const Handle = struct {
        expression: u8,
        typ: u8,
    };

    pub fn init(arena: *Arena) Let {
        return Let{
            .handle = Vec(Handle).init(64, arena),
            .signature = Vec(Signature).init(64, arena),
        };
    }

    pub fn evaluate(self: *Let, index: u8, generator: *Generator) void {
        generator.content.extend("let ");

        if (self.signature.items[index].mutable) {
            generator.content.extend("mutable ");
        }

        const s = self.signature.items[index].start;
        const name = TokenId.identifier(
            generator.parser.lexer.content.offset(s),
        );

        generator.content.extend(name);
        generator.content.push('\n');
    }

    pub fn parse(self: *Let, parser: *Parser) void {
        self.handle.push(undefined);
        self.signature.push(undefined);

        const handle = self.handle.last();
        const signature = self.signature.last();

        const first_id = parser.lexer.next_id();
        const first_start = parser.lexer.next_start();

        parser.lexer.consume();

        switch (first_id) {
            .Identifier => {
                signature.start = @intCast(first_start);
            },
            .Keyword => {
                const keyword = TokenId.keyword(
                    parser.lexer.content.offset(first_start),
                );

                if (.Mut != keyword) @panic("Should not be here");

                const name_start = parser.lexer.next_start();

                signature.start = @intCast(name_start);
                signature.mutable = true;

                parser.lexer.consume();
            },
            else => @panic("Should not be here"),
        }

        parser.lexer.consume(); // DoubleColon

        handle.typ = parser.typ.register(parser);

        parser.lexer.consume(); // Equal

        handle.expression = parser.expression.len(.Expression);
        parser.expression.parse(parser);

        parser.lexer.consume(); // SemiColon
    }

    pub fn len(self: *const Let) u8 {
        return @intCast(self.handle.len);
    }
};
