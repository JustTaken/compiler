const mem = @import("mem");
const collections = @import("collections");

pub const NodeKind = enum {
    Type,
    Scope,
    Procedure,
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
    Type: *Type,
    Scope: *Scope,
    Procedure: *Procedure,
    Construct: *Construct,
    Let: *Let,
    Call: *Call,
    Binary: *Binary,
    Unary: *Unary,
    Property: *Property,
    Identifier: []const u8,
    Number: usize,

    pub fn deinit(self: Node, arena: *mem.Arena) void {
        switch (self) {
            .Type => |typ| {
                typ.fields.deinit(arena);
                arena.destroy(Type, 1);
            },
            .Scope => |scope| {
                for (scope.childs.offset(0) catch unreachable) |node| {
                    node.deinit(arena);
                }

                scope.childs.deinit(arena);
                arena.destroy(Scope, 1);
            },
            .Procedure => |procedure| {
                procedure.scope.deinit(arena);

                for (procedure.parameters.offset(0) catch unreachable) |node| {
                    node.deinit(arena);
                }

                procedure.parameters.deinit(arena);
                arena.destroy(Procedure, 1);
            },
            .Construct => |construct| {
                for (construct.values.offset(0) catch unreachable) |node| {
                    node.deinit(arena);
                }

                construct.values.deinit(arena);
                construct.name.deinit(arena);
                arena.destroy(Construct, 1);
            },
            .Let => |let| {
                let.value.deinit(arena);
                arena.destroy(Let, 1);
            },
            .Call => |call| {
                for (call.arguments.offset(0) catch unreachable) |node| {
                    node.deinit(arena);
                }

                call.arguments.deinit(arena);
                call.name.deinit(arena);
                arena.destroy(Call, 1);
            },
            .Binary => |binary| {
                binary.left.deinit(arena);
                binary.right.deinit(arena);
                arena.destroy(Binary, 1);
            },
            .Unary => |unary| {
                unary.node.deinit();
                arena.destroy(Unary, 1);
            },
            .Property => |property| {
                property.node.deinit(arena);
                arena.destroy(Property, 1);
            },
            .Identifier => {},
            .Number => {},

        }
    }
};

pub const Type = struct {
    name: []const u8,
    fields: collections.Array(Field),
    size: ?u32,

    pub const Field = struct {
        name: []const u8,
        typ: []const u8,

        pub fn new(name: []const u8, typ: []const u8) Field {
            return Field {
                .name = name,
                .typ = typ,
            };
        }
    };

    pub fn new(name: []const u8, fields: collections.Array(Field), size: ?u32) Type {
        return Type {
            .name = name,
            .fields = fields,
            .size = size,
        };
    }
};

pub const Scope = struct {
    childs: collections.Array(Node),
    return_value: ?Node,

    pub fn new(childs: collections.Array(Node), return_value: ?Node) Scope {
        return Scope {
            .childs = childs,
            .return_value = return_value,
        };
    }
};

pub const Construct = struct {
    name: Node,
    values: collections.Array(Value),

    pub const Value = struct {
        name: []const u8,
        value: Node,

        pub fn new(name: []const u8, value: Node) Value {
            return Value {
                .name = name,
                .value = value,
            };
        }
    };

    pub fn new(name: Node, values: collections.Array(Value)) Construct {
        return Construct {
            .name = name,
            .values = values,
        };
    }
};

pub const Procedure = struct {
    name: []const u8,
    typ: []const u8,
    parameters: collections.Array(Parameter),
    scope: Node,

    pub const Parameter = struct {
        name: []const u8,
        typ: []const u8,

        pub fn new(name: []const u8, typ: []const u8) Parameter {
            return Parameter {
                .name = name,
                .typ = typ,
            };
        }
    };

    pub fn new(name: []const u8, typ: []const u8, parameters: collections.Array(*const Type), scope: Node) Procedure {
        return Procedure {
            .name = name,
            .typ = typ,
            .parameters = parameters,
            .scope = scope,
        };
    }
};

pub const Let = struct {
    name: []const u8,
    typ: []const u8,
    value: Node,
    mut: bool,

    pub fn new(name: []const u8, typ: []const u8, value: Node, mut: bool) Let {
        return Let {
            .name = name,
            .typ = typ,
            .value = value,
            .mut = mut,
        };        
    }
};

pub const Call = struct {
    name: Node,
    arguments: collections.Array(Node),

    pub fn new(name: Node, arguments: collections.Array(Node)) Call {
        return Call {
            .name = name,
            .arguments = arguments,
        };
    }
};

pub const Property = struct {
    node: Node,
    name: []const u8,

    pub fn new(node: Node, name: []const u8) Property {
        return Property {
            .node = node,
            .name = name,
        };
    }
};

pub const Unary = struct {
    node: Node,
    op: Operator,

    const Operator = enum {
        Bang, Negate,
    };

    pub fn new(node: Node, op: Operator) Unary {
        return Unary {
            .node = node,
            .op = op,
        };
    }
};

pub const Binary = struct {
    left: Node,
    right: Node,
    op: Operator,

    pub const Operator = enum { 
        Add, Sub, Mul, Div,
        Eq, Gt, Lt,
    };

    pub fn new(left: Node, right: Node, op: Operator) Binary {
        return Binary {
            .left = left,
            .right = right,
            .op = op,
        };
    }
};
