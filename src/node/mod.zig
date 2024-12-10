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
        // const zone = util.tracy.initZone(@src(), .{.name = "Generator::deinit"});
        // defer zone.deinit();

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
            const zone = util.tracy.initZone(@src(), .{.name = "Field::new"});
            defer zone.deinit();

            return Field {
                .name = name,
                .typ = typ,
            };
        }
    };

    pub fn new(name: util.Index) Type {
        const zone = util.tracy.initZone(@src(), .{.name = "Type::new"});
        defer zone.deinit();

        return Type {
            .name = name,
        };
    }
};

pub const Number = struct {
    value: util.Index,

    pub fn new(value: util.Index) Number {
        const zone = util.tracy.initZone(@src(), .{.name = "Number::new"});
        defer zone.deinit();

        return Number {
            .value = value,
        };
    }
};

pub const Identifier = struct {
    name: util.Index,

    pub fn new(name: util.Index) Identifier {
        const zone = util.tracy.initZone(@src(), .{.name = "Identifier::new"});
        defer zone.deinit();

        return Identifier {
            .name = name,
        };
    }
};

pub const Scope = struct {
    len: util.Index,

    pub fn new(len: util.Index) Scope {
        const zone = util.tracy.initZone(@src(), .{.name = "Scope::new"});
        defer zone.deinit();

        return Scope {
            .len = len,
        };
    }
};

pub const Construct = struct {
    values: util.Index,

    pub const Value = struct {
        name: util.Index,
        node: util.Index,

        pub fn new(name: util.Index, node: util.Index) Value {
        const zone = util.tracy.initZone(@src(), .{.name = "Value::new"});
        defer zone.deinit();

            return Value {
                .name = name,
                .node = node,
            };
        }
    };

    pub fn new(values: util.Index) Construct {
        const zone = util.tracy.initZone(@src(), .{.name = "Construct::new"});
        defer zone.deinit();

        return Construct {
            .values = values,
        };
    }
};

pub const Procedure = struct {
    name: util.Index,

    pub const Parameter = struct {
        name: util.Index,
        typ: util.Index,

        pub fn new(name: util.Index, typ: util.Index) Parameter {
        const zone = util.tracy.initZone(@src(), .{.name = "Parameter::new"});
        defer zone.deinit();

            return Parameter {
                .name = name,
                .typ = typ,
            };
        }
    };

    pub fn new(name: util.Index) Procedure {
        const zone = util.tracy.initZone(@src(), .{.name = "Procedure::new"});
        defer zone.deinit();

        return Procedure {
            .name = name,
        };
    }
};

pub const Let = struct {
    name: util.Index,

    pub fn new(name: util.Index) Let {
        const zone = util.tracy.initZone(@src(), .{.name = "Let::new"});
        defer zone.deinit();

        return Let {
            .name = name,
        };
    }
};

pub const Call = struct {
    len: util.Index,

    pub fn new(len: util.Index) Call {
        const zone = util.tracy.initZone(@src(), .{.name = "Call::new"});
        defer zone.deinit();

        return Call {
            .len = len,
        };
    }
};

pub const Property = struct {
    // node: util.Index,
    name: util.Index,

    pub fn new(name: util.Index) Property {
        const zone = util.tracy.initZone(@src(), .{.name = "Property::new"});
        defer zone.deinit();

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
        const zone = util.tracy.initZone(@src(), .{.name = "Unary::new"});
        defer zone.deinit();

        return Unary {
            .op = op,
        };
    }
};

pub const Binary = struct {
    op: Operator,

    pub const Operator = enum(u8) { 
        Add, Sub, Mul, Div,
        Eq, Gt, Lt,
    };

    pub fn new(op: Operator) Binary {
        const zone = util.tracy.initZone(@src(), .{.name = "Binary::new"});
        defer zone.deinit();

        return Binary {
            .op = op,
        };
    }
};
