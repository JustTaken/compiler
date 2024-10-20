const collections = @import("collections/mod.zig");
const allocator = @import("allocator/mod.zig");
const util = @import("util/mod.zig");

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
    Case,
    Match,
    Ret,
    Call,
};

fn is_type(index: Index) bool {
    return index != 0;
}

const BinaryOperator = enum(u8) {
    Add,
    Sub,
    Mul,
    Div,
    Gt,
    Lt,
    Eq,

    fn from_instruction(instruction: Instruction) BinaryOperator {
        return switch (instruction) {
            .Add => .Add,
            .Subtract => .Sub,
            .Multiply => .Mul,
            .Divide => .Div,
            .Greater => .Gt,
            .Less => .Lt,
            .Equal => .Eq,
            else => unreachable,
        };
    }

    pub fn to_string(self: BinaryOperator) []const u8 {
        return switch (self) {
            .Add => "add",
            .Sub => "sub",
            .Mul => "mul",
            .Div => "div",
            .Gt => "gt",
            .Lt => "lt",
            .Eq => "icmp eq",
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
};

const ConstantRawKind = enum(u8) {
    number,
    identifier,
    parameter,
    boolean,
};

const ConstantRawValue = union(ConstantRawKind) {
    number: Range,
    identifier: Range,
    parameter: Index,
    boolean: bool,
};

const ConstantRaw = struct {
    value: ConstantRawValue,
    index: Index,

    pub fn range(self: *const ConstantRaw) Range {
        switch (self.value) {
            .identifier, .number => |r| return r,
            else => unreachable,
        }
    }

    pub fn has_prefix(self: *const ConstantRaw) bool {
        return switch (self.value) {
            .number => false,
            else => true,
        };
    }

    pub fn content(self: *const ConstantRaw, words: *Vec(u8)) []const u8 {
        return switch (self.value) {
            .boolean => @panic("TODO"),
            .number, .identifier => |r| return words.range(r),
            .parameter => |p| {
                const start = words.len;
                util.parse(p, words);

                return words.offset(start);
            },
        };
    }
};

const ConstantCall = struct {
    arguments: [*]Constant,
    len: Index,
    index: Index,
    range: Range,
};

const ConstKind = enum(u8) { raw, binary, call };
pub const Constant = union(ConstKind) {
    raw: ConstantRaw,
    binary: ConstantBinary,
    call: ConstantCall,

    pub fn is_raw(self: Constant) bool {
        return switch (self) {
            .raw => true,
            else => false,
        };
    }

    fn set_type(self: *Constant, index: Index) bool {
        switch (self.*) {
            .binary => |b| return b.set_type(index),
            .call => |c| return c.index == index,
            .raw => |*r| {
                if (is_type(r.index)) {
                    return r.index == index;
                } else {
                    r.index = index;
                    return true;
                }
            },
        }
    }

    pub fn get_type(self: Constant) Index {
        return switch (self) {
            .binary => |b| b.left.get_type(),
            .call => |c| c.index,
            .raw => |r| r.index,
        };
    }

    pub fn range(self: Constant) Range {
        return switch (self) {
            .raw => |r| r.range(),
            else => unreachable,
        };
    }
};

const Parameter = struct {
    pos: Index,
    index: Index,
};

const VariableKind = enum(u8) { raw, parameter, alias };
const Variable = union(VariableKind) {
    raw: Index,
    parameter: Parameter,
    alias: ConstantRaw,
};

pub const Procedure = struct {
    parameters: Vec(Index),
    index: Index,

    fn new(arena: *Arena) Procedure {
        return Procedure {
            .parameters = Vec(Index).new(10, arena),
            .index = 0,
        };
    }
};

const MatchCase = struct {
    of: ConstantRaw,
    expression: Constant,
};

pub const Match = struct {
    cases: Vec(MatchCase),
    expression: Constant,

    fn new(arena: *Arena) Match {
        return Match {
            .cases = Vec(MatchCase).new(10, arena),
            .expression = undefined,
        };
    }
};

const Scope = struct {
    variable_count: Index,
    variable_offset: Index,
};

pub const TypeChecker = struct {
    ranges: Vec(Range),
    constants: Vec(Constant),
    scopes: Vec(Scope),
    types: Vec(Range),
    type_indices: RangeMap(Index),
    procedures: Vec(Procedure),
    procedure_indices: RangeMap(Index),
    variables: RangeMap(Variable),
    variable_indices: Vec(Index),
    match: Match,
    arena: Arena,
    error_buffer: Vec(u8),

    pub fn new(arena: *Arena) TypeChecker {
        return TypeChecker{
            .ranges = Vec(Range).new(2, arena),
            .constants = Vec(Constant).new(10, arena),
            .scopes = Vec(Scope).new(10, arena),
            .types = Vec(Range).new(64, arena),
            .type_indices = RangeMap(Index).new(64, arena),
            .procedure_indices = RangeMap(Index).new(64, arena),
            .procedures = Vec(Procedure).new(64, arena),
            .variables = RangeMap(Variable).new(64, arena),
            .variable_indices = Vec(Index).new(64, arena),
            .match = Match.new(arena),
            .arena = Arena.new(arena.bytes(1024)),
            .error_buffer = Vec(u8).new(512, arena),
        };
    }

    pub fn clear(self: *TypeChecker) void {
        self.error_buffer.clear();
        self.constants.clear();
        self.variables.clear();
        self.arena.clear();
    }

    pub fn push_range(self: *TypeChecker, range: Range) void {
        self.ranges.push(range);
    }

    pub fn push_procedure(self: *TypeChecker) void {
        self.procedures.push(Procedure.new(&self.arena));
    }

    pub fn push_scope(self: *TypeChecker) void {
        self.scopes.push(Scope{
            .variable_offset = @intCast(self.variable_indices.len),
            .variable_count = 0,
        });
    }

    pub fn pop_scope(self: *TypeChecker) void {
        const scope = self.scopes.pop();

        for (0..scope.variable_count) |i| {
            const variable_pos = self.variable_indices.items[scope.variable_offset + i];
            self.variables.key[variable_pos] = Range.new(0, 0);
        }

        self.variable_indices.len -= scope.variable_count;
    }

    pub fn push_identifier(
        self: *TypeChecker,
        range: Range,
        words: *const Vec(u8),
    ) void {
        var constant: Constant = undefined;

        if (self.variables.get(range, words)) |variable| {
            switch (variable.*) {
                .raw => |i| {
                    constant = Constant{
                        .raw = ConstantRaw{
                            .value = ConstantRawValue{
                                .identifier = range,
                            },
                            .index = i,
                        },
                    };
                },
                .parameter => |p| {
                    constant = Constant{
                        .raw = ConstantRaw{
                            .value = ConstantRawValue{
                                .parameter = p.pos,
                            },
                            .index = p.index,
                        },
                    };
                },
                .alias => |c| {
                    constant = Constant{
                        .raw = c,
                    };
                },
            }
        } else {
            constant = Constant{
                .raw = ConstantRaw{
                    .value = ConstantRawValue{
                        .identifier = range,
                    },
                    .index = 0,
                },
            };
        }

        self.constants.push(constant);
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
            .Call => self.register_call(words),
            .Case => self.register_case(words),
            .Add, .Multiply, .Divide, .Subtract, .Greater, .Less, .Equal, => {
                const op = BinaryOperator.from_instruction(instruction);
                self.register_binary(op);
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
    ) void {
        var second = self.constants.pop();
        var first = self.constants.pop();

        const first_type = first.get_type();
        const second_type = second.get_type();

        if (is_type(first_type)) {
            _ = second.set_type(first_type);
        } else if (is_type(second_type)) {
            _ = first.set_type(second_type);
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
            self.error_buffer.extend("Could not set the type of let delcaration");
            return;
        }

        var variable: Variable = undefined;

        switch (self.constants.last().*) {
            .raw => |r| variable = Variable{ .alias = r },
            else => variable = Variable{
                .raw = index,
            },
        }

        const position = self.variables.put(
            name_range,
            variable,
            words,
        );

        self.variable_indices.push(@intCast(position));
        self.scopes.last().variable_count += 1;
    }

    fn register_call(self: *TypeChecker, words: *const Vec(u8)) void {
        const name_range = self.ranges.pop();
        var procedure: Procedure = undefined;

        if (self.procedure_indices.get(name_range, words)) |index| {
            procedure = self.procedures.value(index.*);
        } else {
            self.error_buffer.extend("Could not find function declaration");
        }

        const argument_len: Index = @intCast(procedure.parameters.len);
        var call = ConstantCall{
            .arguments = self.arena.alloc(Constant, argument_len),
            .index = procedure.index,
            .len = argument_len,
            .range = name_range,
        };

        for (0..argument_len) |i| {
            var constant = self.constants.pop();
            const parameter = procedure.parameters.items[argument_len - i - 1];

            if (!constant.set_type(parameter)) {
                self.error_buffer.extend("Type mismatch inside procedure call");
            }

            call.arguments[argument_len - i - 1] = constant;
        }

        self.constants.push(Constant{
            .call = call,
        });
    }

    fn register_case(self: *TypeChecker, words: *const Vec(u8)) void {
        self.match.cases.push(
            MatchCase{
                .expression = self.constants.pop(),
                .of = self.constants.pop().raw,
            },
            
        );
        _ = words;
    }

    fn register_match(self: *TypeChecker, words: *const Vec(u8)) void {
        _ = words;
        var constant = self.constants.pop();
        var t = constant.get_type();

        if (!is_type(t)) {
            t = 1;
        }

        constant.set_type(t);

        self.match.expression = constant;

        for (self.match.cases.offset(0)) |*case| {
            case.of.index = t;
        }
    }

    fn register_procedure(self: *TypeChecker, words: *const Vec(u8)) void {
        const type_range = self.ranges.pop();
        const name_range = self.ranges.pop();
        const index = self.get_type(type_range, words);

        if (self.constants.len > 0) {
            if (!self.constants.last().set_type(index)) {
                self.error_buffer.extend("Could not set type of let declaration");
            }
        }

        self.procedures.last().index = index;
        self.procedure_indices.push(name_range, @intCast(self.procedures.len - 1), words);
    }

    fn register_parameter(self: *TypeChecker, words: *const Vec(u8)) void {
        const type_range = self.ranges.pop();
        const name_range = self.ranges.pop();
        const index = self.get_type(type_range, words);

        self.variables.push(
            name_range,
            Variable{
                .alias = ConstantRaw{
                    .value = .{
                        .parameter = @intCast(self.procedures.last().parameters.len),
                    },
                    .index = index,
                },
            },
            words,
        );

        self.procedures.last().parameters.push(index);
    }
};
