const util = @import("util.zig");
const Parser = @import("parser.zig").Parser;
const Lexer = @import("lexer.zig").Lexer;
const TokenId = @import("lexer.zig").TokenId;
const Expression = @import("expression.zig").Expression;
const Vec = @import("collections.zig").Vec;
const HashMap = @import("collections.zig").HashMap;
const Arena = @import("collections.zig").Arena;

const FieldConstruct = struct {
    handle: Vec(u8),
    start: Vec(u16),

    fn init(arena: *Arena) FieldConstruct {
        return FieldConstruct{
            .handle = Vec(u8).init(64, arena),
            .start = Vec(u16).init(64, arena),
        };
    }

    fn parse(typ: *Type, parser: *Parser) void {
        const self = &typ.field_construct;
        const start = parser.lexer.next_start();

        self.handle.push(0);
        self.start.push(start);

        const handle = self.handle.last();

        parser.lexer.consume();
        parser.lexer.consume(); // Equal,

        handle.* = parser.expression.len(.Expression);
        parser.expression.parse(parser);
    }
};

const Construct = struct {
    handle: Vec(Handle),
    start: Vec(u16),

    const Handle = struct {
        field: u8,
        count: u8,
    };

    fn init(arena: *Arena) Construct {
        return Construct{
            .handle = Vec(Handle).init(64, arena),
            .start = Vec(u16).init(64, arena),
        };
    }

    fn parse(typ: *Type, arg: u16, parser: *Parser) void {
        const self = &typ.construct;

        self.start.push(arg);
        self.handle.push(Handle{
            .field = typ.len(.TypeFieldConstruct),
            .count = 0,
        });

        const handle = self.handle.last();

        parser.lexer.consume(); // CurlyBracketLeft
        while (parser.lexer.has_next(parser)) {
            const id = parser.lexer.next_id();
            if (id != .Identifier) break;

            FieldConstruct.parse(typ, parser);

            handle.count += 1;
            parser.lexer.consume(); // Colon
        }

        parser.lexer.consume(); // CurlyBracketRight
    }
};

const Field = struct {
    handle: Vec(u8),
    start: Vec(u16),

    fn init(arena: *Arena) Field {
        return Field{
            .handle = Vec(u8).init(64, arena),
            .start = Vec(u16).init(64, arena),
        };
    }

    fn parse(typ: *Type, parser: *Parser) void {
        const self = &typ.field;

        self.start.push(parser.lexer.next_start());
        self.handle.push(0);

        const handle = self.handle.last();

        parser.lexer.consume();
        parser.lexer.consume(); // DoubleColon

        handle.* = typ.register(parser);
    }
};

const Inner = struct {
    handle: HashMap(Handle),
    start: Vec(u16),
    size: Vec(u8),

    const Handle = struct {
        fields: u8,
        count: u8,

        pub fn is_zero(self: *const Type) bool {
            return self.count > 0;
        }

        pub fn zero() Handle {
            return Handle{
                .fields = 0,
                .count = 0,
            };
        }
    };

    fn init(arena: *Arena) Inner {
        return Inner{
            .handle = HashMap(Handle).init(64, arena),
            .start = Vec(u16).init(64, arena),
            .size = Vec(u8).init(64, arena),
        };
    }

    inline fn get_size(
        typ: *Type,
        index: u8,
        parser: *Parser,
    ) u8 {
        const self = &typ.inner;

        var size = parser.type_size.items[index];
        const handle = self.handle.value[index];
        const start = parser.type_start.items[index];

        if (size == 0) {
            if (handle.fields == 0 and handle.count == 0) {
                const name = TokenId.identifier(
                    parser.lexer.content.offset(start),
                );

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
                const field = &typ.field.handle.items[
                    j + self.fields
                ];

                size += get_size(field.typ, parser, parser.lexer);
            }

            self.size.items[index] = size;
        }

        return size;
    }

    inline fn parse(typ: *Type, parser: *Parser) void {
        const self = &typ.inner;
        const start = parser.lexer.next_start();

        parser.lexer.consume();
        parser.lexer.consume(); // CurlyBracketLeft

        var handle = Handle{
            .count = 0,
            .fields = typ.len(.TypeField),
        };

        while (parser.lexer.has_next(parser)) {
            const token_id = parser.lexer.next_id();

            if (token_id != .Identifier) break;

            Field.parse(typ, parser);
            handle.count += 1;
            parser.lexer.consume(); // Colon
        }

        parser.lexer.consume(); // CurlyBracketRight

        const name = TokenId.identifier(parser.lexer.content.offset(start));

        if (self.handle.get(name)) |u| {
            self.handle.value[u] = handle;
            self.start.items[u] = start;
        } else {
            self.handle.push(name, handle);
            self.start.push(start);
            self.size.push(0);
        }
    }
};

const Property = struct {
    handle: Vec(u8),
    start: Vec(u16),

    fn init(arena: *Arena) Property {
        return Property{
            .handle = Vec(u8).init(64, arena),
            .start = Vec(u16).init(64, arena),
        };
    }

    inline fn parse(typ: *Type, arg: u16, parser: *Parser) void {
        const self = &typ.property;

        self.start.push(arg);
        self.handle.push(parser.expression.len(.Expression));
        parser.expression.parse(parser);
    }
};

pub const Kind = enum {
    Type,
    TypeConstruct,
    TypeField,
    TypeFieldConstruct,
    TypeProperty,
};

pub const Type = struct {
    inner: Inner,
    construct: Construct,
    field: Field,
    field_construct: FieldConstruct,
    property: Property,

    pub fn init(arena: *Arena) Type {
        return Type{
            .inner = Inner.init(arena),
            .construct = Construct.init(arena),
            .field = Field.init(arena),
            .field_construct = FieldConstruct.init(arena),
            .property = Property.init(arena),
        };
    }

    pub fn parse(self: *Type, arg: u16, kind: Kind, parser: *Parser) void {
        switch (kind) {
            .Type => Inner.parse(self, parser),
            .TypeConstruct => Construct.parse(self, arg, parser),
            .TypeProperty => Property.parse(self, arg, parser),
            else => @panic("Should not happen"),
        }
    }

    pub fn register(self: *Type, parser: *Parser) u8 {
        const start = parser.lexer.next_start();
        parser.lexer.consume();

        const name = TokenId.identifier(
            parser.lexer.content.offset(start),
        );

        if (self.inner.handle.get(name)) |pos| {
            return @intCast(pos);
        } else {
            self.inner.handle.push(name, Inner.Handle.zero());
            self.inner.start.push(start);
            self.inner.size.push(0);

            return @intCast(self.inner.handle.len - 1);
        }
    }

    pub fn len(self: *const Type, kind: Kind) u8 {
        const l = switch (kind) {
            .Type => self.inner.handle.len,
            .TypeConstruct => self.construct.handle.len,
            .TypeField => self.field.handle.len,
            .TypeFieldConstruct => self.field_construct.handle.len,
            .TypeProperty => self.property.handle.len,
        };

        return @intCast(l);
    }
};
