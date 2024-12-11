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

pub const Tree = struct {
    nodes: collections.Vec(Node),
    type_fields: collections.Vec(Type.Field),
    construct_values: collections.SliceManager(Construct.Value),
    parameters: collections.Vec(Procedure.Parameter),

    pub fn new(allocator: *mem.Arena) error{OutOfMemory}!Tree {
        const zone = util.tracy.initZone(@src(), .{.name = "Tree::new"});
        defer zone.deinit();

        // util.print(.Info, "Scope: {}", .{@sizeOf(node.Scope)});
        // util.print(.Info, "Construct: {}", .{@sizeOf(node.Construct)});
        // util.print(.Info, "Let: {}", .{@sizeOf(node.Let)});
        // util.print(.Info, "Call: {}", .{@sizeOf(node.Call)});
        // util.print(.Info, "Binary: {}", .{@sizeOf(node.Binary)});
        // util.print(.Info, "Unary: {}", .{@sizeOf(node.Unary)});
        // util.print(.Info, "Property: {}", .{@sizeOf(node.Property)});
        // util.print(.Info, "Procedure: {}", .{@sizeOf(node.Procedure)});
        // util.print(.Info, "Type: {}", .{@sizeOf(node.Type)});
        // util.print(.Info, "Identifier: {}", .{@sizeOf(node.Identifier)});
        // util.print(.Info, "Number: {}", .{@sizeOf(node.Number)});
        // util.print(.Info, "Node: {}", .{@sizeOf(node.Node)});

        return Tree{
            .nodes = try collections.Vec(Node).new(100, allocator),
            .type_fields = try collections.Vec(Type.Field).new(20, allocator),
            .construct_values = try collections.SliceManager(Construct.Value).new(20, allocator),
            .parameters = try collections.Vec(Procedure.Parameter).new(20, allocator),
        };
    }

    pub fn clear(self: *Tree) void {
        const zone = util.tracy.initZone(@src(), .{.name = "Tree::clear"});
        defer zone.deinit();

        self.parameters.clear();
        self.construct_values.clear();
        self.type_fields.clear();
        self.nodes.clear();
    }

    pub fn deinit(self: *Tree, allocator: *mem.Arena) void {
        const zone = util.tracy.initZone(@src(), .{.name = "Tree::deinit"});
        defer zone.deinit();

        self.parameters.deinit(allocator);
        self.construct_values.deinit(allocator);
        self.type_fields.deinit(allocator);
        self.nodes.deinit(allocator);
    }
};

