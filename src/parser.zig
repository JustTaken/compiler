const std = @import("std");
const Vec = @import("collections.zig").Vec;
const Arena = @import("collections.zig").Arena;
const Lexer = @import("lexer.zig").Lexer;
const Token = @import("lexer.zig").Token;
const TokenId = @import("lexer.zig").TokenId;
const Iter = @import("collections.zig").Iter;
const Keyword = @import("lexer.zig").Keyword;

const TotalSize = 8192;
const Count = 9;
const EachSize = TotalSize / Count;

// Make this relative to the source code, ie: asks to the lexer how
// many of each are there and create proportional vectors, this would
// make better usage of memory
const FunctionSize: u32 = EachSize / @sizeOf(Function);
const LetSize: u32 = EachSize / @sizeOf(Let);
const ParameterSize: u32 = EachSize / @sizeOf(Parameter);
const ExpressionSize: u32 = EachSize / @sizeOf(Expression);
const BinarySize: u32 = EachSize / @sizeOf(Binary);
const MatchSize: u32 = EachSize / @sizeOf(Match);
const MatchBranchSize: u32 = EachSize / @sizeOf(MatchBranch);
const StructSize: u32 = EachSize / @sizeOf(Struct);
const StructFieldSize: u32 = EachSize / @sizeOf(StructField);

const ExpressionType = enum(u16) {
    Binary,
    Literal,
    Variable,
    Expression,
    Call,
};

const Binary = struct {
    start: u16,
    right: u16,
    left: u16,

    fn collect(
        first: u16,
        typ: ExpressionType,
        parser: *Parser,
        iter: *Iter(Token),
    ) Binary {
        var self: Binary = undefined;

        const left: u16 = @intCast(parser.expression.len);
        parser.expression.push(Expression{
            .start = first,
            .typ = if (typ == .Literal) .Literal else .Variable,
        });

        self.start = iter.next().?.start;
        iter.consume();

        const token = iter.next().?;

        const right: u16 = @intCast(parser.expression.len);
        parser.expression.push(Expression{
            .start = token.start,
            .typ = if (token.id == .Literal) .Literal else .Variable,
        });

        self.left = left;
        self.right = right;

        return self;
    }

    fn extend(s: u16, parser: *Parser, iter: *Iter(Token)) void {
        var self = parser.binary.get(s);

        const op = iter.next().?.start;
        iter.consume();

        const token = iter.next().?;
        iter.consume();

        const other_op = TokenId.operator(parser.content.offset(op));
        const self_op = TokenId.operator(parser.content.offset(self.start));

        const new: u16 = @intCast(parser.expression.len);
        parser.expression.push(Expression{
            .start = token.start,
            .typ = if (token.id == .Identifier) .Variable else .Literal,
        });

        var binary = Binary{
            .start = op,
            .left = self.right,
            .right = new,
        };

        if (self_op.precedence() > other_op.precedence()) {
            binary.left = self.left;
            binary.right = self.right;
            binary.start = self.start;

            self.right = new;
            self.left = @intCast(parser.expression.len);
            self.start = op;
        } else self.right = @intCast(parser.expression.len);

        parser.expression.push(Expression{
            .start = @intCast(parser.binary.len),
            .typ = .Binary,
        });

        parser.binary.push(binary);
    }
};

const Expression = struct {
    start: u16,
    typ: ExpressionType,

    fn init(parser: *Parser, iter: *Iter(Token)) Expression {
        var self = Expression{
            .start = 0,
            .typ = .Variable,
        };

        while (iter.next()) |token| {
            switch (token.id) {
                .Keyword => break,
                .Symbol => {
                    const symbol_typ = TokenId.symbol(
                        parser.content.offset(token.start),
                    );

                    if (symbol_typ == .ParentesisRight) {
                        iter.consume();
                        break;
                    } else if (symbol_typ != .ParentesisLeft) break;

                    self.start = @intCast(parser.expression.len);
                    self.typ = .Expression;

                    parser.expression.push(init(parser, iter));
                },
                .Operator => {
                    if (self.typ == .Binary) {
                        Binary.extend(
                            self.start,
                            parser,
                            iter,
                        );
                    } else {
                        const start = self.start;
                        self.start = @intCast(parser.binary.len);

                        parser.binary.push(Binary.collect(
                            start,
                            self.typ,
                            parser,
                            iter,
                        ));

                        self.typ = .Binary;
                    }
                },
                .Identifier => {
                    self.typ = .Variable;
                    self.start = token.start;
                },
                .Literal => {
                    self.typ = .Literal;
                    self.start = token.start;
                },
            }

            iter.consume();
        }

        return self;
    }
};

const StructField = struct {
    start: u16,
    typ: u16,

    fn init(iter: *Iter(Token)) StructField {
        var self = StructField{
            .start = iter.next().?.start,
            .typ = 0,
        };

        iter.consume();
        iter.consume(); // DoubleColon
        self.typ = iter.next().?.start;
        iter.consume();

        return self;
    }
};

const Struct = struct {
    start: u16,
    fields: u16,
    count: u16,

    fn init(parser: *Parser, iter: *Iter(Token)) Struct {
        var self = Struct{
            .start = iter.next().?.start,
            .count = 0,
            .fields = @intCast(parser.struct_field.len),
        };

        iter.consume();
        iter.consume(); // CurlyBracketLeft

        while (iter.next()) |token| {
            if (token.id != .Identifier) break;

            parser.struct_field.push(StructField.init(iter));
            self.count += 1;
            iter.consume(); // Colon
        }

        iter.consume(); // CurlyBracketRight

        return self;
    }
};

const LetSignature = packed struct {
    start: u15,
    mutable: bool,
};

const Let = struct {
    signature: LetSignature,
    expression: u16,

    fn init(parser: *Parser, iter: *Iter(Token)) Let {
        var self = Let{
            .signature = LetSignature{ .start = 0, .mutable = false },
            .expression = 0,
        };

        const first = iter.next().?;
        iter.consume();

        switch (first.id) {
            .Identifier => self.signature.start = @intCast(first.start),
            .Keyword => {
                const keyword = TokenId.keyword(
                    parser.content.offset(first.start),
                );

                if (.Mut != keyword) @panic("Should not be here");

                self.signature.mutable = true;
                const name = iter.next().?;

                self.signature.start = @intCast(name.start);
                iter.consume();
            },
            else => @panic("Should not be here"),
        }

        iter.consume(); // Equal

        self.expression = @intCast(parser.expression.len);
        parser.expression.push(Expression.init(parser, iter));

        iter.consume(); // SemiColon

        return self;
    }
};

const Match = struct {
    branch: u16,
    count: u16,

    fn init(parser: *Parser, iter: *Iter(Token)) Match {
        var self = Match{
            .branch = @intCast(parser.match_branch.len),
            .count = 0,
        };

        const first = iter.next().?;

        switch (first.id) {
            .Identifier => {
                iter.consume();
                iter.consume(); // CurlyBracketLeft

                parser.match_branch.push(MatchBranch.init(parser, iter));
                self.count += 1;
            },
            else => @panic("Should not be here"),
        }

        iter.consume(); // CurlyBracketRight

        return self;
    }
};

const MatchBranch = struct {
    start: u16,
    expression: u16,

    fn init(parser: *Parser, iter: *Iter(Token)) MatchBranch {
        var self = MatchBranch{
            .start = 0,
            .expression = 0,
        };

        self.start = iter.next().?.start;
        iter.consume();
        iter.consume(); // Equal
        iter.consume(); // Greater

        self.expression = @intCast(parser.expression.len);
        parser.expression.push(Expression.init(parser, iter));

        iter.consume(); // Colon

        return self;
    }
};

const Parameter = struct {
    name: u16,
    typ: u16,

    fn init(parser: *Parser, iter: *Iter(Token)) Parameter {
        var self = Parameter{
            .name = 0,
            .typ = 0,
        };
        self.name = iter.next().?.start;
        iter.consume();

        const symbol = iter.next().?;
        if (.DoubleColon !=
            TokenId.symbol(parser.content.offset(symbol.start)))
        {
            @panic("Should not be here");
        }

        iter.consume();
        self.typ = iter.next().?.start;
        iter.consume();

        return self;
    }
};

const Parameters = packed struct {
    start: u16,
    count: u8,
};

const Lets = packed struct {
    start: u16,
    count: u8,
};

const Matchs = packed struct {
    start: u16,
    count: u8,
};

const Function = struct {
    start: u16,
    typ: u16,
    parameters: Parameters,
    matchs: Matchs,
    lets: Lets,

    fn init(parser: *Parser, iter: *Iter(Token)) Function {
        var self = Function{
            .start = 0,
            .typ = 0,
            .matchs = Matchs{
                .start = @intCast(parser.match.len),
                .count = 0,
            },
            .parameters = Parameters{
                .start = @intCast(parser.function.len),
                .count = 0,
            },
            .lets = Lets{
                .start = @intCast(parser.let.len),
                .count = 0,
            },
        };

        self.start = iter.next().?.start;
        iter.consume();
        iter.consume(); // Parentesis

        while (iter.next()) |token| {
            if (.Symbol == token.id) {
                iter.consume();
                const symbol = TokenId.symbol(
                    parser.content.offset(token.start),
                );

                switch (symbol) {
                    .ParentesisRight => break,
                    .Colon => {},
                    else => {
                        @panic("Should not be here");
                    },
                }
            }

            parser.parameter.push(Parameter.init(parser, iter));
            self.parameters.count += 1;
        }

        iter.consume(); // DoubleColon
        self.typ = iter.next().?.start;
        iter.consume();
        iter.consume(); // CurlyBracketLeft

        while (iter.next()) |token| {
            if (token.id != .Keyword) break;
            iter.consume();

            const keyword = TokenId.keyword(
                parser.content.offset(token.start),
            );

            if (.Let == keyword) {
                parser.let.push(Let.init(parser, iter));
                self.lets.count += 1;
            } else if (.Match == keyword) {
                parser.match.push(Match.init(parser, iter));
                self.matchs.count += 1;
            } else @panic("Support more keywords");
        }

        iter.consume(); // CurlyBracketRight

        return self;
    }
};

const Root = struct {
    fn parse(parser: *Parser, iter: *Iter(Token)) void {
        while (iter.next()) |token| {
            switch (token.id) {
                .Keyword => {
                    switch (TokenId.keyword(parser.content.offset(token.start))) {
                        .Function => {
                            iter.consume();
                            parser.function.push(Function.init(parser, iter));
                        },
                        .Struct => {
                            iter.consume();
                            parser.struc.push(Struct.init(parser, iter));
                        },
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
    content: *const Vec(u8),
    function: Vec(Function),
    let: Vec(Let),
    parameter: Vec(Parameter),
    expression: Vec(Expression),
    binary: Vec(Binary),
    match: Vec(Match),
    match_branch: Vec(MatchBranch),
    struc: Vec(Struct),
    struct_field: Vec(StructField),

    pub fn init(arena: *Arena) Parser {
        var self: Parser = undefined;

        self.arena = Arena.init(arena.alloc(u8, TotalSize)[0..TotalSize]);

        self.function = Vec(Function).init(FunctionSize, &self.arena);
        self.let = Vec(Let).init(LetSize, &self.arena);
        self.parameter = Vec(Parameter).init(ParameterSize, &self.arena);
        self.expression = Vec(Expression).init(ExpressionSize, &self.arena);
        self.binary = Vec(Binary).init(BinarySize, &self.arena);
        self.match = Vec(Match).init(MatchSize, &self.arena);
        self.match_branch = Vec(MatchBranch).init(MatchBranchSize, &self.arena);
        self.struc = Vec(Struct).init(StructSize, &self.arena);
        self.struct_field = Vec(StructField).init(StructFieldSize, &self.arena);

        return self;
    }

    pub fn parse(self: *Parser, lexer: *const Lexer) void {
        self.content = &lexer.content;
        var iter = Iter(Token).init(&lexer.tokens);

        Root.parse(self, &iter);
    }

    pub fn reset(self: *Parser) void {
        self.function.clear();
        self.let.clear();
        self.parameter.clear();
        self.expression.clear();
        self.binary.clear();
        self.match.clear();
        self.match.clear();
        self.struc.clear();
        self.struct_field.clear();
    }
};
