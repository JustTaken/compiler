const collections = @import("collections");
const mem = @import("mem");
const util = @import("util");
const generator = @import("generator.zig");

const Arena = mem.Arena;
const Stream = collections.Stream;
const Range = collections.Range;
const Vec = collections.Vec;
const RangeMap = collections.RangeMap;
const Array = collections.Array;
const String = collections.String;
const Operation = generator.Operation;
const BinaryOperation = generator.BinaryOperation;
const BinaryOperationKind = generator.BinaryKind;
const Destination = generator.Destination;
const DestinationKind = generator.DestinationKind;
const Source = generator.Source;
const Memory = generator.Memory;
const Register = generator.Register;

const Generator = generator.Generator;

const ConstantBinary = struct {
    operator: Operator,
    left: Constant,
    right: Constant,
    inner: ?*const ConstantType,
    source: ?Source,
    usage: usize,

    const Operator = enum(usize) {
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
    };
};

const ConstantUnary = struct {
    operator: Operator,
    constant: Constant,
    source: ?Source,
    usage: usize,

    const Operator = enum(usize) {
        Bang,
        Minus,
    };
};

const ConstantCall = struct {
    procedure: *const ConstantProcedure,
    arguments: Array(Constant),
    source: ?Source,
    usage: usize,
};

const ConstantParameter = struct {
    offset: usize,
    inner: *const ConstantType,
    usage: usize,
};

const ConstantNumber = struct {
    value: usize,
    inner: ?*const ConstantType,
    usage: usize,
};

const ConstantScope = struct {
    return_value: ?Constant,
    constants: Array(Constant),
    usage: usize,
    used: bool,

    fn set_type(self: ConstantScope, inner: *const ConstantType) bool {
        if (self.return_value) |constant| {
            return constant.set_type(inner);
        } else {
            return inner.size == 0;
        }
    }

    fn get_type(self: ConstantScope) ?*const ConstantType {
        if (self.return_value) |constant| {
            return constant.get_type();
        } else {
            return null;
        }
    }
};

const ConstantConstruct = struct {
    inner: *const ConstantType,
    constants: Array(Constant),
    source: ?Source,
    usage: usize,
};

const ConstantUsageModification = enum(usize) {
    Remove,
    Add,

    fn apply(self: ConstantUsageModification, number: usize) usize {
        return switch (self) {
            .Remove => number - 1,
            .Add => number + 1,
        };
    }
};

const ConstantType = struct {
    size: u32,
    alignment: u32,
    fields: Array(Field),
    usage: usize,

    const Field = struct {
        name: Range,
        inner: *ConstantType,
    };
};

const ConstantProcedure = struct {
    offset: usize,
    inner: *ConstantType,
    parameters: Array(*const ConstantType),
    usage: usize,
};

const ConstantKind = enum {
    Number,
    Parameter,
    Call,
    Binary,
    Unary,
    Construct,
    Ref,
    Scope,
    Procedure,
    Type,
};

const Constant = union(ConstantKind) {
    Number: *ConstantNumber,
    Parameter: *ConstantParameter,
    Call: *ConstantCall,
    Binary: *ConstantBinary,
    Unary: *ConstantUnary,
    Construct: *ConstantConstruct,
    Ref: *Constant,
    Scope: *ConstantScope,
    Procedure: *ConstantProcedure,
    Type: *ConstantType,

    fn set_type(self: Constant, inner: *const ConstantType) bool {
        switch (self) {
            .Procedure, .Type => @panic("Why?"),
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

    fn get_type(self: Constant) ?*const ConstantType {
        return switch (self) {
            .Procedure, .Type => @panic("Why?"),
            .Parameter => |parameter| parameter.inner,
            .Call => |call| call.procedure.inner,
            .Construct => |construct| construct.inner,
            .Unary => |unary| unary.constant.get_type(),
            .Ref => |constant| constant.get_type(),
            .Binary => |binary| binary.inner,
            .Number => |number| number.inner,
            .Scope => |scope| scope.get_type(),
        };
    }

    fn usage_count(self: Constant, mod: ConstantUsageModification) void {
        switch (self) {
            .Number => |number| number.usage = mod.apply(number.usage),
            .Parameter => |parameter| parameter.usage = mod.apply(parameter.usage),
            .Procedure => |procedure| procedure.usage = mod.apply(procedure.usage),
            .Type => |typ| typ.usage = mod.apply(typ.usage),
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
                    for (call.arguments.offset(0)) |*arg| {
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

                    for (scope.constants.offset(0)) |constant| {
                        constant.usage_count(mod);
                    }
                }
            },
            .Construct => |construct| {
                construct.usage = mod.apply(construct.usage);

                if (mod == .Add) {
                    for (construct.constants.offset(0)) |constant| {
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

    fn get_usage(self: Constant) usize {
        return switch (self) {
            .Parameter => |parameter| parameter.usage,
            .Binary => |binary| binary.usage,
            .Procedure => |procedure| procedure.usage,
            .Type => |typ| typ.usage,
            .Scope => |scope| scope.usage,
            .Number => |number| number.usage,
            .Unary => |unary| unary.usage,
            .Call => |call| call.usage,
            .Construct => |construct| construct.usage,
            .Ref => |ref| ref.get_usage(),
        };
    }

    fn set_source(self: Constant, source: ?Source) void {
        switch (self) {
            .Procedure, .Type => @panic("What???"),
            .Parameter, .Number => {},
            .Call => |call| call.source = source,
            .Construct => |construct| construct.source = source,
            .Binary => |binary| binary.source = source,
            .Scope => |scope| {
                if (scope.return_value) |*constant| {
                    constant.set_source(source);
                }
            },
            .Unary => |unary| unary.source = source,
            .Ref => |ref| ref.set_source(source),
        }
    }

    fn get_source(self: Constant) ?Source {
        return switch (self) {
            .Procedure, .Type => @panic("Why?"),
            .Parameter => |parameter| Source{ .Memory = Memory{ .register = Register.Rbp, .offset = parameter.offset } },
            .Number => |number| Source{ .Immediate = number.value },
            .Call => |call| call.source,
            .Construct => |construct| construct.source,
            .Binary => |binary| binary.source,
            .Scope => |scope| blk: {
                if (scope.return_value) |constant| {
                    break :blk constant.get_source();
                } else {
                    break :blk null;
                }
            },
            .Unary => |unary| unary.source,
            .Ref => |ref| ref.get_source(),
        };
    }

    fn evaluate(self: Constant, operation: BinaryOperationKind, destination: ?Destination, gen: *Generator, change_count: bool) void {
        if (change_count) {
            self.usage_count(.Remove);
        }

        switch (self) {
            .Procedure, .Type => @panic("What?"),
            .Number => |number| {
                gen.operations.push(Operation{ .Binary = BinaryOperation{
                    .kind = operation,
                    .source = Source{ .Immediate = number.value },
                    .destination = destination orelse @panic("Should not happen"),
                } });
            },
            .Parameter => |parameter| {
                gen.operations.push(Operation{ .Binary = BinaryOperation{
                    .kind = operation,
                    .source = Source{ .Memory = .{ .register = .Rbp, .offset = parameter.offset } },
                    .destination = destination orelse @panic("Should not happen"),
                } });
            },
            .Call => |call| {
                for (call.arguments.offset(0)) |argument| {
                    argument.evaluate(.Mov, Destination{ .Stack = {} }, gen, change_count);
                }

                gen.operations.push(Operation{ .Call = call.procedure.offset });

                if (destination) |dst| {
                    if (@as(DestinationKind, dst) == .Register and dst.Register == Register.Rax) {} else {
                        gen.operations.push(Operation{ .Binary = BinaryOperation{
                            .kind = operation,
                            .source = Source{ .Register = .Rax },
                            .destination = dst,
                        } });
                    }
                }
            },
            .Binary => |binary| {
                const dst = destination orelse @panic("SHould not happen");

                if (binary.source) |source| {
                    gen.operations.push(Operation{ .Binary = BinaryOperation{
                        .kind = operation,
                        .source = source,
                        .destination = dst,
                    } });
                } else {
                    binary.left.evaluate(operation, dst, gen, change_count);
                    binary.right.evaluate(binary.operator.to_gen_operation(), dst, gen, change_count);
                }
            },
            .Unary => |unary| {
                const dst = destination orelse @panic("SHould not happen");

                if (unary.source) |source| {
                    gen.operations.push(Operation{ .Binary = BinaryOperation{
                        .kind = operation,
                        .source = source,
                        .destination = dst,
                    } });
                } else {
                    unary.constant.evaluate(operation, dst, gen, change_count);
                }
            },
            .Scope => |scope| {
                if (!scope.used) {
                    for (scope.constants.offset(0)) |constant| {
                        constant.evaluate(operation, null, gen, false);
                    }
                }

                if (scope.return_value) |constant| {
                    constant.evaluate(operation, destination orelse @panic("Should not happen"), gen, change_count);
                }

                scope.used = true;
            },
            .Construct => @panic("TODO"),
            .Ref => |constant| {
                const dst = destination orelse @panic("Should not happen");

                if (constant.get_source()) |source| {
                    constant.evaluate(operation, dst, gen, change_count);

                    if (constant.get_usage() == 0) {
                        gen.give_back(source);
                        constant.set_source(null);
                    }
                } else if (constant.get_usage() > 1) {
                    const register = gen.manager.get();

                    constant.evaluate(BinaryOperationKind.Mov, Destination{ .Register = register }, gen, change_count);
                    constant.set_source(Source{ .Register = register });
                    constant.evaluate(operation, dst, gen, false);
                } else {
                    constant.evaluate(operation, dst, gen, change_count);
                }
            },
        }
    }

    fn deinit(self: Constant, arena: *Arena) void {
        switch (self) {
            .Ref => {},
            .Parameter => arena.destroy(ConstantParameter, 1),
            .Number => arena.destroy(ConstantNumber, 1),
            .Call => |call| {
                arena.destroy(ConstantCall, 1);
                call.arguments.deinit(arena);

                for (call.arguments.offset(0)) |argument| {
                    argument.deinit(arena);
                }
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
                construct.constants.deinit(arena);

                for (construct.constants.offset(0)) |constant| {
                    constant.deinit(arena);
                }
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

                scope.constants.deinit(arena);

                for (scope.constants.offset(0)) |constant| {
                    constant.deinit(arena);
                }
            },
        }
    }
};

pub const TypeChecker = struct {
    parameters: Vec(*const ConstantType),
    constants: Vec(Constant),

    ranges: Vec(Range),

    variables: RangeMap(*Constant),
    variable_constants: Vec(Constant),
    variable_refs: Vec(*Range),

    generator: Generator,
    arena: *Arena,

    const VARIABLE_MAX: u32 = 20;

    pub fn new(stream: Stream, allocator: *Arena) TypeChecker {
        const arena = allocator.child(mem.PAGE_SIZE);

        return TypeChecker{
            .parameters = Vec(*const ConstantType).new(10, arena),
            .constants = Vec(Constant).new(10, arena),
            .variables = RangeMap(*Constant).new(VARIABLE_MAX, arena),
            .variable_constants = Vec(Constant).new(VARIABLE_MAX, arena),
            .variable_refs = Vec(*Range).new(VARIABLE_MAX, arena),
            .ranges = Vec(Range).new(10, arena),

            .generator = Generator.new(stream, arena),
            .arena = arena,
        };
    }

    fn push_variable(self: *TypeChecker, name: Range, constant: Constant, words: *const String) void {
        self.variable_constants.push(constant);

        const ptr = self.variable_constants.last();
        const index = self.variables.put(name, ptr, words);
        const ref_name = &self.variables.key[index];

        self.variable_refs.push(ref_name);
    }

    pub fn push_scope(self: *TypeChecker, expression_count: u32, declaration_count: u32, has_return: bool) void {
        var constants = Array(Constant).new(expression_count, self.arena);
        var return_value: ?Constant = null;

        if (has_return) {
            return_value = self.constants.pop();
        }

        for (0..expression_count) |i| {
            constants.set(self.constants.pop(), expression_count - i - 1);
        }

        for (0..declaration_count) |_| {
            self.variable_refs.pop().reset();
        }

        self.constants.push(Constant{ .Scope = self.arena.create(ConstantScope, ConstantScope{
            .constants = constants,
            .return_value = return_value,
            .used = false,
            .usage = 0,
        }) });
    }

    pub fn push_identifier(self: *TypeChecker, words: *const String) void {
        const range = self.ranges.pop();

        if (self.variables.get(words.range(range), words)) |variable| {
            self.constants.push(Constant{ .Ref = variable.* });
        } else {
            @panic("Undeclared variable");
        }
    }

    pub fn push_number(self: *TypeChecker, range: Range, words: *const String) void {
        const number = util.parse(words.range(range));

        self.constants.push(Constant{ .Number = self.arena.create(ConstantNumber, ConstantNumber{
            .inner = null,
            .value = number,
            .usage = 0,
        }) });
    }

    pub fn push_binary(self: *TypeChecker, operator: ConstantBinary.Operator, words: *const String) void {
        var right = self.constants.pop();
        var left = self.constants.pop();
        var inner: ?*const ConstantType = null;

        const left_inner = left.get_type();
        const right_inner = right.get_type();

        if (operator.is_comparison()) {
            inner = self.get_type("bool", words);
        } else if (left_inner) |typ| {
            if (!right.set_type(typ)) {
                @panic("Could not set inner type of constant");
            } else {
                inner = typ;
            }
        } else if (right_inner) |typ| {
            if (!left.set_type(typ)) {
                @panic("Could not set inner type of constant");
            } else {
                inner = typ;
            }
        }

        self.constants.push(Constant{ .Binary = self.arena.create(ConstantBinary, ConstantBinary{
            .left = left,
            .right = right,
            .inner = inner,
            .operator = operator,
            .source = null,
            .usage = 0,
        }) });
    }

    pub fn push_unary(self: *TypeChecker, operator: ConstantUnary.Operator) void {
        const constant = self.constants.pop();

        self.constants.push(Constant{ .Unary = self.arena.create(ConstantUnary, ConstantUnary{
            .constant = constant,
            .operator = operator,
            .source = null,
            .usage = 0,
        }) });
    }

    pub fn push_call(self: *TypeChecker, argument_count: u32, words: *const String) void {
        const range = self.ranges.pop();
        const name = words.range(range);

        const procedure = self.get_procedure(name, words);
        if (procedure.parameters.len != argument_count) {
            @panic("Should not happen");
        }

        var arguments = Array(Constant).new(argument_count, self.arena);

        for (0..argument_count) |i| {
            var argument = self.constants.pop();

            if (!argument.set_type(procedure.parameters.items[i])) {
                @panic("Should not happen");
            }

            arguments.set(argument, i);
        }

        self.constants.push(Constant{ .Call = self.arena.create(ConstantCall, ConstantCall{
            .procedure = procedure,
            .arguments = arguments,
            .source = null,
            .usage = 0,
        }) });
    }

    pub fn push_parameter(self: *TypeChecker, current_size: u32, words: *const String) u32 {
        const type_range = self.ranges.pop();
        const name_range = self.ranges.pop();
        const inner = self.get_type(words.range(type_range), words);
        const size = inner.size;

        self.parameters.push(inner);
        self.push_variable(name_range, Constant{ .Parameter = self.arena.create(ConstantParameter, ConstantParameter{
            .offset = current_size,
            .inner = inner,
            .usage = 0,
        }) }, words);

        return size;
    }

    pub fn push_property(self: *TypeChecker, range: Range, words: *const String) void {
        const constant = self.constants.pop();
        var construct: ConstantConstruct = undefined;

        switch (constant) {
            .Ref => |ref| {
                if (@as(ConstantKind, ref.*) != ConstantKind.Construct) @panic("Should not happen");
                construct = ref.Construct.*;
            },
            .Construct => |c| construct = c.*,
            else => @panic("Should not happen"),
        }

        var property: ?Constant = null;

        for (construct.inner.fields.offset(0), 0..) |field, i| {
            if (mem.equal(u8, words.range(field.name), words.range(range))) {
                property = Constant{ .Ref = &construct.constants.items[i] };

                break;
            }
        }

        if (property) |p| {
            self.constants.push(p);
        }
    }

    pub fn push_let(self: *TypeChecker, words: *const String) void {
        const type_range = self.ranges.pop();
        const name_range = self.ranges.pop();
        const inner = self.get_type(words.range(type_range), words);
        var constant = self.constants.pop();

        if (!constant.set_type(inner)) {
            @panic("Should not happen");
        }

        self.push_variable(name_range, constant, words);
    }

    pub fn push_type(self: *TypeChecker, field_count: u32, annotated_size: u32, words: *const String) void {
        const name_range = self.ranges.pop();
        var fields = Array(ConstantType.Field).new(field_count, self.arena);

        var size: u32 = annotated_size;
        var alignment: u32 = annotated_size;

        for (0..field_count) |i| {
            const field_type_range = self.ranges.pop();
            const field_name_range = self.ranges.pop();
            const inner = self.get_type(words.range(field_type_range), words);

            size += inner.size;
            alignment = util.max(alignment, inner.alignment);

            fields.set(ConstantType.Field{
                .name = field_name_range,
                .inner = inner,
            }, i);
        }

        self.push_variable(name_range, Constant{ .Type = self.arena.create(ConstantType, ConstantType{
            .fields = fields,
            .size = size,
            .alignment = alignment,
            .usage = 0,
        }) }, words);
    }

    pub fn push_construct(self: *TypeChecker, field_count: u32, words: *const String) void {
        const type_range = self.ranges.get_back(field_count);
        const inner = self.get_type(words.range(type_range), words);

        if (inner.fields.len != field_count) @panic("Should not happen");

        var constants = Array(Constant).new(field_count, self.arena);

        for (0..field_count) |i| {
            const name = self.ranges.pop();

            if (!mem.equal(u8, words.range(name), words.range(inner.fields.items[i].name))) {
                @panic("For now the type construction have to be in the same order as the definitino");
            }

            var constant = self.constants.pop();

            if (!constant.set_type(inner.fields.items[i].inner)) @panic("Should not happen");
            constants.set(constant, i);
        }

        _ = self.ranges.pop();
        self.constants.push(Constant{ .Construct = self.arena.create(ConstantConstruct, ConstantConstruct{
            .constants = constants,
            .inner = inner,
            .usage = 0,
            .source = null,
        }) });
    }

    pub fn push_procedure(self: *TypeChecker, parameter_count: u32, variable_start: u32, words: *const String) void {
        const type_range = self.ranges.pop();
        const name_range = self.ranges.pop();
        const inner = self.get_type(words.range(type_range), words);
        const offset: usize = 0;

        self.generator.operations.clear();

        var constant = self.constants.pop();
        if (!constant.set_type(inner)) @panic("Should not happen");

        constant.usage_count(.Add);
        constant.evaluate(BinaryOperationKind.Mov, Destination{ .Register = Register.Rax }, &self.generator, true);
        constant.deinit(self.arena);

        self.generator.check();

        for (0..parameter_count) |_| {
            self.variable_refs.pop().reset();
        }

        for (variable_start..self.variable_constants.len) |_| {
            self.variable_constants.pop().deinit(self.arena);
        }

        if (self.constants.len > 0) @panic("Should not happen");
        if (self.variable_refs.len != variable_start) @panic("Should not happen");
        if (self.variable_constants.len != variable_start) @panic("Should not happen");

        var parameters = Array(*const ConstantType).new(parameter_count, self.arena);
        for (0..parameter_count) |i| {
            parameters.set(self.parameters.pop(), i);
        }

        self.push_variable(name_range, Constant{ .Procedure = self.arena.create(ConstantProcedure, ConstantProcedure{
            .offset = offset,
            .inner = inner,
            .parameters = parameters,
            .usage = 0,
        }) }, words);

        util.print("usage: {}\n", .{self.arena.usage});
    }

    fn get_type(self: *TypeChecker, name: []const u8, words: *const String) *ConstantType {
        const constant = (self.variables.get(name, words) orelse @panic("Should not happen")).*.*;

        if (@as(ConstantKind, constant) != Constant.Type) @panic("Should not happen");

        return constant.Type;
    }

    fn get_procedure(self: *TypeChecker, name: []const u8, words: *const String) *const ConstantProcedure {
        const constant = (self.variables.get(name, words) orelse @panic("Should not happen")).*.*;

        if (@as(ConstantKind, constant) != Constant.Procedure) @panic("Should not happen");

        return constant.Procedure;
    }

    pub fn deinit(self: *TypeChecker) void {
        for (self.variable_constants.offset(0)) |variable| {
            variable.deinit(self.arena);
        }

        self.generator.deinit();

        self.parameters.deinit(self.arena);
        self.constants.deinit(self.arena);
        self.variables.deinit(self.arena);

        self.variable_constants.deinit(self.arena);
        self.variable_refs.deinit(self.arena);
        self.ranges.deinit(self.arena);

        self.arena.deinit("TypeChecker");
    }
};
