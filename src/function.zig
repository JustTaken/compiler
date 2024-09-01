const Arena = @import("collections.zig").Arena;
const Parser = @import("parser.zig").Parser;
const TokenId = @import("lexer.zig").TokenId;
const Expression = @import("expression.zig").Expression;
const Scope = @import("scope.zig").Scope;
const Let = @import("let.zig").Let;
const Type = @import("type.zig").Type;
const Vec = @import("collections.zig").Vec;
const Generator = @import("generator.zig").Generator;

const Call = struct {
    handle: Vec(Handle),
    start: Vec(u16),

    const Handle = struct {
        arg: u8,
        count: u8,
    };

    fn init(arena: *Arena) Call {
        return Call{
            .handle = Vec(Handle).init(64, arena),
            .start = Vec(u16).init(64, arena),
        };
    }

    inline fn evaluate(function: *Function, index: u8, generator: *Generator) void {
        _ = function;
        _ = index;

        generator.content.extend("function call\n");
    }

    inline fn parse(function: *Function, arg: u16, parser: *Parser) void {
        const self = &function.call;

        self.start.push(arg);
        self.handle.push(Handle{
            .arg = function.len(.FunctionCallArgument),
            .count = 0,
        });

        parser.lexer.consume();

        const handle = self.handle.last();

        while (parser.lexer.has_next(parser)) {
            CallArgument.parse(function, parser);
            handle.count += 1;

            const s = parser.lexer.next_start();
            const id = parser.lexer.next_id();

            switch (id) {
                .Symbol => {
                    const symbol = TokenId.symbol(
                        parser.lexer.content.offset(s),
                    );

                    switch (symbol) {
                        .Colon => {},
                        else => break,
                    }
                },
                else => @panic("Should not happen"),
            }

            parser.lexer.consume();
        }
    }
};

const CallArgument = struct {
    handle: Vec(u8),

    fn init(arena: *Arena) CallArgument {
        return CallArgument{
            .handle = Vec(u8).init(64, arena),
        };
    }

    inline fn evaluate(function: *Function, index: u8, generator: *Generator) void {
        _ = index;
        _ = function;
        generator.content.extend("function call argument\n");
    }

    inline fn parse(function: *Function, parser: *Parser) void {
        const self = &function.call_argument;

        self.handle.push(parser.expression.len(.Expression));
        parser.expression.parse(parser);
    }
};

const Inner = struct {
    handle: Vec(Handle),
    start: Vec(u16),

    const Handle = struct {
        typ: u8,
        scope: u8,
        parameter_start: u8,
        parameter_count: u8,
    };

    fn init(arena: *Arena) Inner {
        return .{
            .handle = Vec(Handle).init(64, arena),
            .start = Vec(u16).init(64, arena),
        };
    }

    pub fn evaluate(function: *Function, index: u8, generator: *Generator) void {
        const self = &function.inner;

        const name = TokenId.identifier(
            generator.parser.lexer.content.offset(self.start.items[index]),
        );

        generator.content.extend("function: ");
        generator.content.extend(name);
        generator.content.push('\n');

        generator.parser.scope.evaluate(@intCast(index), generator);
    }

    inline fn parse(function: *Function, parser: *Parser) void {
        const self = &function.inner;
        self.start.push(parser.lexer.next_start());

        self.handle.push(Handle{
            .typ = 0,
            .scope = parser.scope.len(),
            .parameter_start = function.len(.Parameter),
            .parameter_count = 0,
        });

        const handle = self.handle.last();

        parser.lexer.consume();
        parser.lexer.consume(); // Parentesis

        while (parser.lexer.has_next(parser)) {
            const token_id = parser.lexer.next_id();
            const token_start = parser.lexer.next_start();

            if (.Symbol == token_id) {
                parser.lexer.consume();

                const symbol = TokenId.symbol(
                    parser.lexer.content.offset(token_start),
                );

                switch (symbol) {
                    .ParentesisRight => break,
                    .Colon => {},
                    else => {
                        @panic("Should not be here");
                    },
                }
            }

            Parameter.parse(function, parser);
            handle.parameter_count += 1;
        }

        parser.lexer.consume(); // DoubleColon

        handle.typ = parser.typ.register(parser);

        parser.lexer.consume(); // CurlyBracketLeft

        parser.scope.parse(parser);

        parser.lexer.consume(); // CurlyBracketRight
    }

};

const Parameter = struct {
    handle: Vec(u8),
    start: Vec(u16),

    fn init(arena: *Arena) Parameter {
        return Parameter{
            .handle = Vec(u8).init(64, arena),
            .start = Vec(u16).init(64, arena),
        };
    }

    inline fn evaluate(function: *Function, index: u8, generator: *Generator,) void {
        _ =index;
        _ = function;

        generator.content.extend("function parameter\n");
    }

    fn parse(function: *Function, parser: *Parser) void {
        const self = &function.parameter;

        self.start.push(parser.lexer.next_start());
        self.handle.push(0);

        const handle = self.handle.last();

        parser.lexer.consume();
        parser.lexer.consume(); // DoubleColon

        handle.* = parser.typ.register(parser);
    }
};

pub const Kind = enum {
    Function,
    FunctionCall,
    FunctionCallArgument,
    Parameter,
};

pub const Function = struct {
    inner: Inner,
    call: Call,
    call_argument: CallArgument,
    parameter: Parameter,

    pub fn init(arena: *Arena) Function {
        return Function{
            .inner = Inner.init(arena),
            .call_argument = CallArgument.init(arena),
            .call = Call.init(arena),
            .parameter = Parameter.init(arena),
        };
    }

    pub fn parse(self: *Function, arg: u16, kind: Kind, parser: *Parser) void {
        switch (kind) {
            .Function => Inner.parse(self, parser),
            .FunctionCall => Call.parse(self, arg, parser),
            else => @panic("Should not happen"),
        }
    }

    pub fn evaluate(
        self: *Function,
        kind: Kind,
        index: u8,
        generator: *Generator,
    ) void {
        switch (kind) {
            .Function => Inner.evaluate(self, index, generator),
            .FunctionCall => Call.evaluate(self, index, generator),
            .Parameter => Parameter.evaluate(self, index, generator),
            .FunctionCallArgument => CallArgument.evaluate(
                self,
                index,
                generator,
            ),
        }
    }

    pub fn len(self: *const Function, kind: Kind) u8 {
        const l = switch (kind) {
            .Function => self.inner.handle.len,
            .FunctionCall => self.call.handle.len,
            .FunctionCallArgument => self.call_argument.handle.len,
            .Parameter => self.parameter.handle.len,
        };

        return @intCast(l);
    }
};

