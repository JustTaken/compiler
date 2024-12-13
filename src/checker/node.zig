const util = @import("util");

pub const NodeKind = enum(u8) {
    Number,
    Binary,
    Ref,
};

pub const Node = union(NodeKind) {
    Number: Number,
    Binary: Binary,
    Ref: Ref,
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

pub const Ref = struct {
    node: util.Index,

    pub fn new(node: util.Index) Ref {
        const zone = util.tracy.initZone(@src(), .{.name = "Ref::new"});
        defer zone.deinit();

        return Ref {
            .node = node,
        };
    }
};
