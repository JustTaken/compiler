const collections = @import("collections");
const mem = @import("mem");
const util = @import("util");
const generator = @import("generator.zig");

const Generator = generator.Generator;
const Operation = generator.Operation;
const Arena = mem.Arena;

const Range = util.Range;
const String = collections.String;
const Array = collections.Array;

const BinaryOperation = generator.BinaryOperation;
const BinaryOperationKind = generator.BinaryKind;
const Destination = generator.Destination;
const DestinationKind = generator.DestinationKind;
const SourceKind = generator.SourceKind;
const Source = generator.Source;
const Memory = generator.Memory;
const Register = generator.Register;

pub const ConstantBinary = struct {
    operator: Operator,
    left: Constant,
    right: Constant,
    inner: ?*const ConstantType,
    usage: usize,

    pub const Operator = enum(usize) {
        Add,
        Sub,
        Mul,
        Div,
        Gt,
        Lt,
        Eq,

        pub fn is_comparison(self: Operator) bool {
            return switch (self) {
                .Gt, .Lt, .Eq => true,
                else => false,
            };
        }

        pub fn to_gen_operation(self: Operator) BinaryOperationKind {
            return switch (self) {
                Operator.Add => BinaryOperationKind.Add,
                Operator.Sub => BinaryOperationKind.Sub,
                Operator.Mul => BinaryOperationKind.Mul,
                else => @panic("TODO"),
            };
        }

        pub fn print(self: Operator, formater: util.Formater) error{Overflow}!void {
            switch (self) {
                Operator.Add => try formater("Operator::Add", .{}),
                Operator.Sub => try formater("Operator::Sub", .{}),
                Operator.Mul => try formater("Operator::Mul", .{}),
                Operator.Div => try formater("Operator::Div", .{}),
                Operator.Gt => try formater("Operator::Gt", .{}),
                Operator.Lt => try formater("Operator::Lt", .{}),
                Operator.Eq => try formater("Operator::Eq", .{}),
            }
        }
    };
};

pub const ConstantUnary = struct {
    operator: Operator,
    constant: Constant,
    usage: usize,

    pub const Operator = enum(usize) {
        Bang,
        Minus,

        pub fn print(self: Operator, formater: util.Formater) error{Overflow}!void {
            switch (self) {
                Operator.Bang => try formater("Operator::Bang", .{}),
                Operator.Minus => try formater("Operator::Minus", .{}),
            }
        }
    };
};

pub const ConstantCall = struct {
    procedure: *const ConstantProcedure,
    arguments: Array(Constant),
    usage: usize,
};

pub const ConstantParameter = struct {
    offset: usize,
    inner: *const ConstantType,
    usage: usize,
};

pub const ConstantNumber = struct {
    value: usize,
    inner: ?*const ConstantType,
    usage: usize,
};

pub const ConstantScope = struct {
    return_value: ?Constant,
    constants: Array(Constant),
    usage: usize,
    used: bool,

    pub fn set_type(self: ConstantScope, inner: *const ConstantType) bool {
        if (self.return_value) |constant| {
            return constant.set_type(inner);
        } else {
            return inner.size == 0;
        }
    }

    pub fn get_type(self: ConstantScope) ?*const ConstantType {
        if (self.return_value) |constant| {
            return constant.get_type();
        } else {
            return null;
        }
    }

    pub fn print(self: ConstantConstruct, formater: util.Formater) error{Overflow}!void {
        try formater("\n", .{});

        if (self.return_value) |constant| {
            try formater("{}", .{constant});
        }

        try formater("\n", .{});
    }
};

pub const ConstantConstruct = struct {
    inner: *const ConstantType,
    constants: Array(Constant),
    usage: usize,

    pub fn print(self: ConstantConstruct, formater: util.Formater) error{Overflow}!void {
        try formater("\n", .{});

        for (self.constants.offset(0) catch unreachable) |constant| {
            try formater("{}\n", .{constant});
        }

        try formater("\n", .{});
    }
};

pub const ConstantFieldAcess = struct {
    constant: Constant,
    inner: *const ConstantType,
    index: usize,
    usage: usize,
};

const ConstantUsageModification = enum(usize) {
    Remove,
    Add,

    pub fn apply(self: ConstantUsageModification, number: usize) usize {
        return switch (self) {
            .Remove => number - 1,
            .Add => number + 1,
        };
    }
};

pub const ConstantType = struct {
    name: []const u8,
    size: u32,
    alignment: u32,
    fields: Array(Field),
    usage: usize,

    pub const Field = struct {
        name: []const u8,
        inner: *ConstantType,
    };

    pub fn field_index(self: *const ConstantType, name: []const u8) error{NotFound}!usize {
        for (self.fields.offset(0) catch unreachable, 0..) |field, i| {
            if (mem.equal(u8, field.name, name)) {
                return i;
            }
        }

        return error.NotFound;
    }

    pub fn offset(self: *const ConstantType, index: usize) usize {
        var size: usize = 0;

        for (0..index) |i| {
            const field = self.fields.items[i];
            size += field.inner.size;
        }

        return size;
    }
};

pub const ConstantProcedure = struct {
    offset: usize,
    inner: *ConstantType,
    parameters: Array(*const ConstantType),
    usage: usize,
};

pub const ConstantKind = enum {
    Number,
    Parameter,
    Call,
    Binary,
    Unary,
    Construct,
    FieldAcess,
    Ref,
    Scope,
    Procedure,
    Type,
};

pub const Constant = union(ConstantKind) {
    Number: *ConstantNumber,
    Parameter: *ConstantParameter,
    Call: *ConstantCall,
    Binary: *ConstantBinary,
    Unary: *ConstantUnary,
    Construct: *ConstantConstruct,
    FieldAcess: *ConstantFieldAcess,
    Ref: *Constant,
    Scope: *ConstantScope,
    Procedure: *ConstantProcedure,
    Type: *ConstantType,

    pub fn set_type(self: Constant, inner: *const ConstantType) bool {
        switch (self) {
            .Procedure, .Type => unreachable,
            .Parameter => |parameter| return parameter.inner == inner,
            .Call => |call| return call.procedure.inner == inner,
            .Unary => |unary| return unary.constant.set_type(inner),
            .Ref => |constant| return constant.set_type(inner),
            .Construct => |construct| return construct.inner == inner,
            .Binary => |binary| {
                if (binary.inner) |typ| {
                    return typ == inner;
                } else {
                    binary.inner = inner;
                }
            },
            .FieldAcess => |field| return field.inner == inner,
            .Number => |number| {
                if (number.inner) |typ| {
                    return typ == inner;
                } else {
                    number.inner = inner;
                }
            },
            .Scope => |scope| return scope.set_type(inner),
        }

        return true;
    }

    pub fn get_type(self: Constant) ?*const ConstantType {
        return switch (self) {
            .Procedure, .Type => unreachable,
            .Parameter => |parameter| parameter.inner,
            .Call => |call| call.procedure.inner,
            .FieldAcess => |field| field.inner,
            .Construct => |construct| construct.inner,
            .Unary => |unary| unary.constant.get_type(),
            .Ref => |constant| constant.get_type(),
            .Binary => |binary| binary.inner,
            .Number => |number| number.inner,
            .Scope => |scope| scope.get_type(),
        };
    }

    pub fn usage_count(self: Constant, mod: ConstantUsageModification) void {
        switch (self) {
            .Number => |number| number.usage = mod.apply(number.usage),
            .Parameter => |parameter| parameter.usage = mod.apply(parameter.usage),
            .Procedure => |procedure| procedure.usage = mod.apply(procedure.usage),
            .Type => |typ| typ.usage = mod.apply(typ.usage),
            .FieldAcess => |field| {
                field.usage = mod.apply(field.usage);
                field.constant.usage_count(mod);
            },
            .Unary => |unary| {
                unary.usage = mod.apply(unary.usage);

                if (mod == .Add) {
                    unary.constant.usage_count(mod);
                }
            },
            .Binary => |binary| {
                binary.usage = mod.apply(binary.usage);

                if (mod == .Add) {
                    binary.left.usage_count(mod);
                    binary.right.usage_count(mod);
                }
            },
            .Call => |call| {
                call.usage = mod.apply(call.usage);

                if (mod == .Add) {
                    for (call.arguments.offset(0) catch unreachable) |*arg| {
                        arg.usage_count(mod);
                    }
                }
            },
            .Scope => |scope| {
                scope.usage = mod.apply(scope.usage);

                if (mod == .Add) {
                    if (scope.return_value) |constant| {
                        constant.usage_count(mod);
                    }

                    for (scope.constants.offset(0) catch unreachable) |constant| {
                        constant.usage_count(mod);
                    }
                }
            },
            .Construct => |construct| {
                construct.usage = mod.apply(construct.usage);

                if (mod == .Add) {
                    for (construct.constants.offset(0) catch unreachable) |constant| {
                        constant.usage_count(mod);
                    }
                }
            },
            .Ref => |constant| {
                if (mod == .Add) {
                    constant.usage_count(mod);
                }
            },
        }
    }

    pub fn get_usage(self: Constant) usize {
        return switch (self) {
            .Parameter => |parameter| parameter.usage,
            .Binary => |binary| binary.usage,
            .Procedure => |procedure| procedure.usage,
            .Type => |typ| typ.usage,
            .FieldAcess => |field| field.usage,
            .Scope => |scope| scope.usage,
            .Number => |number| number.usage,
            .Unary => |unary| unary.usage,
            .Call => |call| call.usage,
            .Construct => |construct| construct.usage,
            .Ref => |ref| ref.get_usage(),
        };
    }

    fn unuse(self: Constant, gen: *Generator) void {
        switch (self) {
            .Parameter, .Number => {},
            .Procedure, .Type => unreachable,
            .Ref => |constant| constant.unuse(gen),
            .FieldAcess => |field| field.constant.unuse(gen),
            .Call => |call| {
                if (call.usage > 0) return;
                if (call.procedure.inner.size > 4) {
                    gen.push_operation(Operation{ .Binary = BinaryOperation{
                        .kind = BinaryOperationKind.Add,
                        .source = Source{ .Immediate = call.procedure.inner.size },
                        .destination = Destination{ .Register = Register.Rsp },
                    } });
                } else {
                    gen.give_back(Source{ .Register = Register.Rax }) catch @panic("TODO");
                }
            },
            .Unary => |unary| unary.constant.unuse(gen),
            .Binary => |binary| {
                if (binary.usage > 0) return;
                if (binary.source) |source| {
                    gen.give_back(source) catch @panic("TODO");
                    binary.source = null;
                }
            },
            .Construct => |construct| {
                if (construct.usage > 0) return;
                if (construct.source) |source| {
                    gen.give_back(source) catch @panic("TODO");
                    construct.source = null;
                }
            },
            .Scope => |scope| {
                if (scope.usage > 0) return;
                if (scope.return_value) |constant| {
                    constant.unuse(gen);
                }
            },
        }
    }

    pub fn deinit(self: Constant, arena: *Arena) void {
        switch (self) {
            .Ref => {},
            .Parameter => arena.destroy(ConstantParameter, 1),
            .Number => arena.destroy(ConstantNumber, 1),
            .FieldAcess => |field| {
                arena.destroy(ConstantFieldAcess, 1);

                if (@as(ConstantKind, field.constant) == .FieldAcess) {
                    field.constant.deinit(arena);
                }
            },
            .Call => |call| {
                arena.destroy(ConstantCall, 1);

                for (call.arguments.offset(0) catch unreachable) |argument| {
                    argument.deinit(arena);
                }

                call.arguments.deinit(arena);
            },
            .Unary => |unary| {
                arena.destroy(ConstantUnary, 1);
                unary.constant.deinit(arena);
            },
            .Binary => |binary| {
                arena.destroy(ConstantBinary, 1);
                binary.left.deinit(arena);
                binary.right.deinit(arena);
            },
            .Construct => |construct| {
                arena.destroy(ConstantConstruct, 1);

                for (construct.constants.offset(0) catch unreachable) |constant| {
                    constant.deinit(arena);
                }

                construct.constants.deinit(arena);
            },
            .Procedure => |procedure| {
                arena.destroy(ConstantProcedure, 1);
                procedure.parameters.deinit(arena);
            },
            .Type => |typ| {
                arena.destroy(ConstantType, 1);
                typ.fields.deinit(arena);
            },
            .Scope => |scope| {
                arena.destroy(ConstantScope, 1);

                if (scope.return_value) |constant| {
                    constant.deinit(arena);
                }

                for (scope.constants.offset(0) catch unreachable) |constant| {
                    constant.deinit(arena);
                }

                scope.constants.deinit(arena);
            },
        }
    }
};
