const mem = @import("mem");
const util = @import("util");
const collections = @import("collections");

pub const NodeKind = enum(u8) {
    Procedure,
    Type,
    Scope,
    Construct,
    Let,
    Call,
    Binary,
    Unary,
    Property,
    Identifier,
    Number,
};

pub const Node = union(NodeKind) {
    Procedure: Procedure,
    Type: Type,
    Scope: Scope,
    Construct: Construct,
    Let: Let,
    Call: Call,
    Binary: Binary,
    Unary: Unary,
    Property: Property,
    Identifier: Identifier,
    Number: Number,

    // pub fn deinit(self: Node, arena: *mem.Arena) void {
    //     switch (self) {
    //         .Type => |typ| {
    //             typ.fields.deinit(arena);
    //             arena.destroy(Type, 1);
    //         },
    //         .Scope => |scope| {
    //             for (scope.childs.offset(0) catch unreachable) |node| {
    //                 node.deinit(arena);
    //             }

    //             scope.childs.deinit(arena);
    //             arena.destroy(Scope, 1);
    //         },
    //         .Procedure => |procedure| {
    //             procedure.scope.deinit(arena);
    //             procedure.parameters.deinit(arena);
    //             arena.destroy(Procedure, 1);
    //         },
    //         .Construct => |construct| {
    //             for (construct.values.offset(0) catch unreachable) |value| {
    //                 value.node.deinit(arena);
    //             }

    //             construct.values.deinit(arena);
    //             construct.name.deinit(arena);
    //             arena.destroy(Construct, 1);
    //         },
    //         .Let => |let| {
    //             let.value.deinit(arena);
    //             arena.destroy(Let, 1);
    //         },
    //         .Call => |call| {
    //             for (call.arguments.offset(0) catch unreachable) |node| {
    //                 node.deinit(arena);
    //             }

    //             call.arguments.deinit(arena);
    //             call.name.deinit(arena);
    //             arena.destroy(Call, 1);
    //         },
    //         .Binary => |binary| {
    //             binary.left.deinit(arena);
    //             binary.right.deinit(arena);
    //             arena.destroy(Binary, 1);
    //         },
    //         .Unary => |unary| {
    //             unary.node.deinit(arena);
    //             arena.destroy(Unary, 1);
    //         },
    //         .Property => |property| {
    //             property.node.deinit(arena);
    //             arena.destroy(Property, 1);
    //         },
    //         .Identifier => |_| {
    //             arena.destroy(Identifier, 1);
    //         },
    //         .Number => {},

    //     }
    // }
};

pub const Type = struct {
    name: util.Index,

    pub const Field = struct {
        name: util.Index,
        typ: util.Index,

        pub fn new(name: util.Index, typ: util.Index) Field {
            return Field {
                .name = name,
                .typ = typ,
            };
        }
    };

    pub fn new(name: util.Index) Type {
        return Type {
            .name = name,
        };
    }
};

pub const Number = struct {
    value: collections.Slice,

    pub fn new(value: collections.Slice) Number {
        return Number {
            .value = value,
        };
    }
};

pub const Identifier = struct {
    name: collections.Slice,

    pub fn new(name: collections.Slice) Identifier {
        return Identifier {
            .name = name,
        };
    }
};

pub const Scope = struct {
    len: util.Index,

    pub fn new(len: util.Index) Scope {
        return Scope {
            .len = len,
        };
    }
};

pub const Construct = struct {
    values: util.Index,

    pub const Value = struct {
        name: util.Index,
        node: Node,

        pub fn new(name: util.Index, node: Node) Value {
            return Value {
                .name = name,
                .node = node,
            };
        }
    };

    pub fn new(name: util.Index, values: util.Index) Construct {
        return Construct {
            .name = name,
            .values = values,
        };
    }
};

pub const Procedure = struct {
    name: util.Index,
    typ: util.Index,

    pub const Parameter = struct {
        name: util.Index,
        typ: util.Index,

        pub fn new(name: util.Index, typ: util.Index) Parameter {
            return Parameter {
                .name = name,
                .typ = typ,
            };
        }
    };

    pub fn new(name: util.Index, typ: util.Index) Procedure {
        return Procedure {
            .name = name,
            .typ = typ,
        };
    }
};

pub const Let = struct {
    name: util.Index,
    typ: util.Index,
    // value: collections.Index,
    // mut: bool,

    pub fn new(name: util.Index, typ: util.Index) Let {
        return Let {
            .name = name,
            .typ = typ,
        };        
    }
};

pub const Call = struct {
    arguments: util.Index,

    pub fn new(arguments: util.Index) Call {
        return Call {
            .arguments = arguments,
        };
    }
};

pub const Property = struct {
    // node: util.Index,
    name: util.Index,

    pub fn new(name: util.Index) Property {
        return Property {
            .name = name,
        };
    }
};

pub const Unary = struct {
    // node: util.Index,
    op: Operator,

    const Operator = enum(u8) {
        Bang, Negate,
    };

    pub fn new(op: Operator) Unary {
        return Unary {
            .op = op,
        };
    }
};

pub const Binary = struct {
    // left: util.Index,
    // right: util.Index,
    op: Operator,

    pub const Operator = enum(u8) { 
        Add, Sub, Mul, Div,
        Eq, Gt, Lt,
    };

    pub fn new(op: Operator) Binary {
        return Binary {
            .op = op,
        };
    }
};
