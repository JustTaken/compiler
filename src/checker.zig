const collections = @import("collections.zig");
const allocator = @import("allocator.zig");
const util = @import("util.zig");

const Arena = allocator.Arena;
const Vec = collections.Vec;
const RangeMap = collections.RangeMap;
const Range = util.Range;
const Index = util.Index;

pub const Instruction = enum(u8) {
    Not,
    Equal,
    Negate,
    Greater,
    Less,
    Add,
    Subtract,
    Multiply,
    Divide,
    False,
    True,
    Constant,
    Let,
    Mut,
    Procedure,
    Argument,
    Parameter,
    Ret,
    Call,
};

// pub const TempValueBuilder = struct {
//     buffer: Vec(u8),
//     next: u32,

//     pub fn new(arena: *Arena) TempValueBuilder {
//         return TempValueBuilder{
//             .buffer = Vec(u8).new(1024, arena),
//             .next = 0,
//         };
//     }

//     pub fn get(self: *TempValueBuilder) []const u8 {
//         const start = self.buffer.len;
//         self.buffer.extend("%temp");
//         util.parse(@intCast(self.next), &self.buffer);
//         self.inc();

//         return self.buffer.content()[start..];
//     }

//     pub fn reset(self: *TempValueBuilder) void {
//         self.next = 0;
//     }

//     pub fn inc(self: *TempValueBuilder) void {
//         self.next += 1;
//     }
// };

const BinaryOperator = enum(u8) {
    Add,
    Sub,
    Mul,
    Div,

    fn from_instruction(instruction: Instruction) BinaryOperator {
        return switch (instruction) {
            .Add => .Add,
            .Subtract => .Sub,
            .Multiply => .Mul,
            .Divide => .Div,
            else => @panic("should not happen"),
        };
    }

    pub fn to_string(self: BinaryOperator) []const u8 {
        return switch (self) {
            .Add => "sum",
            .Sub => "sub",
            .Mul => "mul",
            .Div => "div",
        };
    }
};

const ConstantBinary = struct {
    left: *Constant,
    right: *Constant,
    operator: BinaryOperator,

    fn set_type(self: *const ConstantBinary, index: Index) bool {
        return self.left.set_type(index) and self.right.set_type(index);
    }

    fn get_type(self: *const ConstantBinary) Index {
        return self.left.get_type();
    }
};

const ConstantRawKind = enum(u8) {
    number,
    identifier,
    boolean,
};

const ConstantRawValue = union(ConstantRawKind) {
    number: Range,
    identifier: Range,
    boolean: bool,
};

const ConstantRaw = struct {
    value: ConstantRawValue,
    index: Index,

    pub fn range(self: *const ConstantRaw) Range {
        switch (self.value) {
            .identifier, .number => |r| return r,
            else => @panic("Should not happen"),
        }
    }

    pub fn is_identifier(self: *const ConstantRaw) bool {
        return switch (self.value) {
            .identifier => true,
            else => false,
        };
    }
};

const ConstKind = enum(u8) { raw, binary };
pub const Constant = union(ConstKind) {
    raw: ConstantRaw,
    binary: ConstantBinary,

    pub fn is_raw(self: Constant) bool {
        return switch (self) {
            .raw => true,
            .binary => false,
        };
    }

    fn is_variable(self: Constant) bool {
        return switch (self) {
            .raw => |r| r.is_identifier(),
            .binary => false,
        };
    }

    fn set_type(self: *Constant, index: Index) bool {
        switch (self.*) {
            .binary => |b| return b.set_type(index),
            .raw => |*r| {
                if (r.index != 0) {
                    return r.index == index;
                } else {
                    r.index = index;
                    return true;
                }
            },
        }
    }

    fn range(self: Constant) Range {
        return switch (self) {
            .raw => |r| r.range(),
            .binary => @panic("Should not happen"),
        };
    }
};

pub const Procedure = struct {
    parameters: []const Index,
    return_type: Index,
};

pub const TypeChecker = struct {
    ranges: Vec(Range),
    constants: Vec(Constant),
    parameters: Vec(Index),
    types: Vec(Range),
    type_indices: RangeMap(Index),
    procedures: Vec(Procedure),
    procedure_indices: RangeMap(Index),
    variables: RangeMap(Index),
    arena: Arena,
    error_content_buffer: Vec(u8),
    last_parameter_offset: Index,

    pub fn new(arena: *Arena) TypeChecker {
        return TypeChecker{
            .ranges = Vec(Range).new(2, arena),
            .constants = Vec(Constant).new(4, arena),
            .parameters = Vec(Index).new(10, arena),
            .types = Vec(Range).new(64, arena),
            .type_indices = RangeMap(Index).new(64, arena),
            .procedure_indices = RangeMap(Index).new(64, arena),
            .procedures = Vec(Procedure).new(64, arena),
            .variables = RangeMap(Index).new(64, arena),
            .arena = Arena.new(arena.bytes(1024)),
            .error_content_buffer = Vec(u8).new(512, arena),
            .last_parameter_offset = 0,
        };
    }

    pub fn push_range(self: *TypeChecker, range: Range) void {
        self.ranges.push(range);
    }

    pub fn push_identifier(self: *TypeChecker, range: Range) void {
        self.constants.push(
            Constant{
                .raw = ConstantRaw{
                    .value = ConstantRawValue{
                        .identifier = range,
                    },
                    .index = 0,
                },
            },
        );
    }

    pub fn push_boolean(self: *TypeChecker, value: bool) void {
        self.constants.push(
            Constant{
                .raw = ConstantRaw{
                    .value = ConstantRawValue{
                        .boolean = value,
                    },
                    .index = 0,
                },
            },
        );
    }

    pub fn push_number(self: *TypeChecker, range: Range) void {
        self.constants.push(
            Constant{
                .raw = ConstantRaw{
                    .value = ConstantRawValue{
                        .number = range,
                    },
                    .index = 0,
                },
            },
        );
    }

    pub fn push_instruction(
        self: *TypeChecker,
        instruction: Instruction,
        words: *const Vec(u8),
    ) void {
        switch (instruction) {
            .Parameter => self.register_parameter(words),
            .Let => self.register_let(words),
            .Procedure => self.register_procedure(words),
            .Add, .Multiply, .Divide, .Subtract => {
                const op = BinaryOperator.from_instruction(instruction);
                self.register_binary(op, words);
            },
            else => {},
        }
    }

    fn get_type(self: *TypeChecker, range: Range, words: *const Vec(u8)) Index {
        if (self.type_indices.get(range, words)) |index| {
            return index.*;
        }

        const index: Index = @intCast(self.types.len);

        self.types.push(range);
        self.type_indices.push(range, index, words);

        return index;
    }

    fn register_binary(
        self: *TypeChecker,
        op: BinaryOperator,
        words: *const Vec(u8),
    ) void {
        var first = self.constants.pop();
        var second = self.constants.pop();

        if (first.is_variable()) {
            if (self.variables.get(first.range(), words)) |i| {
                if (!second.set_type(i.*) and !first.set_type(i.*)) {
                    @panic("Could not set varaible type");
                }
            }
        }

        if (second.is_variable()) {
            if (self.variables.get(second.range(), words)) |i| {
                if (!second.set_type(i.*) and !first.set_type(i.*)) {
                    @panic("Could not set varaible type");
                }
            }
        }

        self.constants.push(
            Constant{ .binary = ConstantBinary{
                .left = self.arena.create(Constant, first),
                .right = self.arena.create(Constant, second),
                .operator = op,
            } },
        );
    }

    fn register_let(self: *TypeChecker, words: *const Vec(u8)) void {
        const type_range = self.ranges.pop();
        const name_range = self.ranges.pop();
        const index = self.get_type(type_range, words);

        if (!self.constants.last().set_type(index)) {
            @panic("Could not set type of let declaration");
        }

        self.variables.push(name_range, index, words);
    }

    fn register_procedure(self: *TypeChecker, words: *const Vec(u8)) void {
        const type_range = self.ranges.pop();
        const name_range = self.ranges.pop();
        const index = self.get_type(type_range, words);

        if (self.constants.len > 0) {
            if (!self.constants.last().set_type(index)) {
                @panic("Could not set type of let declaration");
            }
        }

        const len: Index = @intCast(self.procedures.len);

        self.procedure_indices.push(name_range, len, words);
        self.procedures.push(Procedure {
            .parameters = self.parameters.offset(self.last_parameter_offset),
            .return_type = index,
        });

        self.last_parameter_offset = @intCast(self.parameters.len);
    }

    fn register_parameter(self: *TypeChecker, words: *const Vec(u8)) void {
        const type_range = self.ranges.pop();
        const name_range = self.ranges.pop();
        const index = self.get_type(type_range, words);

        self.parameters.push(index);
        self.variables.push(name_range, index, words);
    }
};
