const util = @import("util.zig");
const Range = util.Range;
const Index = util.Index;

pub const ConstantKind = enum(u8) {
    Number, Identifier,
};

pub const Constant = struct {
    kind: ConstantKind,
    range: Range,
    typ: Index,

    pub fn new(range: Range, kind: ConstantKind) Constant {
        return Constant {
            .range = range,
            .kind = kind,
            .typ = 0,
        };
    }

    pub fn set_type(self: *Constant, typ: Index) void {
        self.typ = typ;
    }

    pub fn is_variable(self: *const Constant) bool {
        return self.kind == .Identifier;
    }

    pub fn has_type(self: *const Constant) bool {
        return self.typ != 0;
    }
};

