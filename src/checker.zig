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
const Source = generator.Source;
const Memory = generator.Memory;
const Register = generator.Register;

const Generator = generator.Generator;

const ConstantBinary = struct {
    operator: Operator,
    left: Constant,
    right: Constant,
    inner: ?*const Type,
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
    constant: *Constant,
    source: ?Source,
    usage: usize,

    const Operator = enum(usize) {
        Bang,
        Minus,
    };
};

const ConstantCall = struct {
    procedure: *const Procedure,
    arguments: Array(Constant),
    usage: usize,
};

const ConstantParameter = struct {
    offset: usize,
    inner: *const Type,
    usage: usize,
};

const ConstantNumber = struct {
    value: usize,
    inner: ?*const Type,
    usage: usize,
};

const ConstantScope = struct {
    return_value: ?Constant,
    constants: Array(Constant),
    usage: usize,
    used: bool,

    fn set_type(self: ConstantScope, inner: *const Type) bool {
        if (self.return_value) |constant| {
            return constant.set_type(inner);
        } else {
            return inner.size == 0;
        }
    }

    fn get_type(self: ConstantScope) ?*const Type {
        if (self.return_value) |constant| {
            return constant.get_type();
        } else {
            return null;
        }
    }
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

const ConstantKind = enum {
    Number,
    Parameter,
    Call,
    Binary,
    Unary,
    Ref,
    Scope,
};

const Constant = union(ConstantKind) {
    Number: *ConstantNumber,
    Parameter: *ConstantParameter,
    Call: *ConstantCall,
    Binary: *ConstantBinary,
    Unary: *ConstantUnary,
    Ref: *Constant,
    Scope: *ConstantScope,

    fn set_type(self: Constant, inner: *const Type) bool {
        switch (self) {
            .Parameter => |parameter| return parameter.inner == inner,
            .Call => |call| return call.procedure.inner == inner,
            .Unary => |unary| return unary.constant.set_type(inner),
            .Ref => |constant| return constant.set_type(inner),
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

    fn get_type(self: Constant) ?*const Type {
        return switch (self) {
            .Parameter => |parameter| parameter.inner,
            .Call => |call| call.procedure.inner,
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
            .Unary => |unary| {
                unary.constant.usage_count(mod);
                unary.usage = mod.apply(unary.usage);
            },
            .Binary => |binary| {
                binary.left.usage_count(mod);
                binary.right.usage_count(mod);
                binary.usage = mod.apply(binary.usage);
            },
            .Call => |call| {
                for (call.arguments.offset(0)) |*arg| {
                    arg.usage_count(mod);
                }

                call.usage = mod.apply(call.usage);
            },
            .Scope => |scope| {
                if (scope.return_value) |constant| {
                    constant.usage_count(mod);
                }

                for (scope.constants.offset(0)) |constant| {
                    constant.usage_count(mod);
                }

                scope.usage = mod.apply(scope.usage);
            },
            .Ref => |constant| {
                if (mod == .Add) {
                    constant.usage_count(.Add);
                }
            },
        }
    }

    fn get_usage(self: Constant) usize {
        return switch (self) {
            .Parameter => |parameter| parameter.usage,
            .Binary => |binary| binary.usage,
            .Scope => |scope| scope.usage,
            .Number => |number| number.usage,
            .Unary => |unary| unary.usage,
            .Call => |call| call.usage,
            .Ref => |ref| ref.get_usage(),
        };
    }

    fn set_source(self: Constant, source: ?Source) void {
        switch (self) {
            .Parameter => @panic("Should not happen"),
            .Number => @panic("Should not happen"),
            .Call => @panic("Should not happen"),
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
            .Parameter => |parameter| Source{ .Memory = Memory{ .register = Register.Rbp, .offset = parameter.offset } },
            .Number => |number| Source{ .Immediate = number.value },
            .Call => Source{ .Register = Register.Rax },
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

    fn evaluate(self: Constant, operation: BinaryOperationKind, destination: ?Destination, gen: *Generator, lead: bool) void {
        if (lead) {
            self.usage_count(.Remove);
        }

        switch (self) {
            .Number => |number| {
                gen.operations.push(Operation{ .Binary = BinaryOperation{
                    .kind = operation,
                    .source = Source{ .Immediate = number.value },
                    .destination = destination orelse @panic("Should not happen"),
                } });
            },
            .Binary => |binary| {
                const dst = destination orelse @panic("Should not happen");

                if (binary.source) |source| {
                    gen.operations.push(Operation{ .Binary = BinaryOperation{
                        .kind = operation,
                        .source = source,
                        .destination = dst,
                    } });

                    if (binary.usage == 0) {
                        gen.give_back(source);
                        binary.source = null;
                    }
                } else {
                    binary.left.evaluate(operation, dst, gen, false);
                    binary.right.evaluate(binary.operator.to_gen_operation(), dst, gen, false);
                }
            },
            .Unary => |unary| {
                const dst = destination orelse @panic("Should not happen");

                if (unary.source) |source| {
                    gen.operations.push(Operation{ .Binary = BinaryOperation{
                        .kind = operation,
                        .source = source,
                        .destination = dst,
                    } });

                    if (unary.usage == 0) {
                        gen.give_back(source);
                        unary.source = null;
                    }
                } else {
                    unary.constant.evaluate(operation, dst, gen, false);
                }
            },
            .Scope => |scope| {
                if (!scope.used) {
                    for (scope.constants.offset(0)) |constant| {
                        constant.evaluate(operation, null, gen, false);
                    }
                }

                if (scope.return_value) |constant| {
                    constant.evaluate(operation, destination, gen, lead);
                }

                scope.used = true;
            },
            .Ref => |constant| {
                const dst = destination orelse @panic("Should not happen");

                if (constant.get_source()) |_| {
                    constant.evaluate(operation, dst, gen, lead);
                } else {
                    if (lead) {
                        constant.evaluate(operation, dst, gen, lead);
                        constant.set_source(dst.as_source());
                    } else {
                        const register = gen.manager.get();

                        constant.evaluate(BinaryOperationKind.Mov, Destination{ .Register = register }, gen, false);
                        constant.set_source(Source{ .Register = register });
                        constant.evaluate(operation, dst, gen, lead);
                    }
                }

                if (constant.get_usage() == 0) {
                    const source = constant.get_source().?;

                    gen.give_back(source);
                    constant.set_source(null);
                }
            },
            else => @panic("Should not happen"),
        }
    }

    fn deinit(self: Constant, arena: *Arena) void {
        switch (self) {
            .Ref => {},
            .Parameter => arena.destroy(ConstantParameter, 1),
            .Number => arena.destroy(ConstantNumber, 1),
            .Call => |call| {
                arena.destroy(ConstantCall, 1);
                arena.destroy(Constant, call.arguments.len);

                for (call.arguments.offset(0)) |argument| {
                    argument.deinit(arena);
                }
            },
            .Unary => |unary| {
                arena.destroy(ConstantUnary, 1);
                arena.destroy(Constant, 1);
                unary.constant.deinit(arena);
            },
            .Binary => |binary| {
                arena.destroy(ConstantBinary, 1);
                binary.left.deinit(arena);
                binary.right.deinit(arena);
            },
            .Scope => |scope| {
                arena.destroy(ConstantScope, 1);

                if (scope.return_value) |constant| {
                    constant.deinit(arena);
                }

                arena.destroy(Constant, scope.constants.len);

                for (scope.constants.offset(0)) |constant| {
                    constant.deinit(arena);
                }
            },
        }
    }
};

const Type = struct {
    size: u32,
    alignment: u32,
    fields: Array(Field),

    const Field = struct {
        name: Range,
        inner: *Type,
    };
};

const Procedure = struct {
    offset: usize,
    inner: *const Type,
    parameters: Array(*const Type),
};

const VariableRef = struct {
    name: *Range,
    constant: *const Constant,
};

pub const TypeChecker = struct {
    procedures: RangeMap(Procedure),
    parameters: Vec(*const Type),
    constants: Vec(Constant),
    variables: RangeMap(Constant),
    variable_refs: Vec(VariableRef),
    ranges: Vec(Range),
    types: RangeMap(Type),

    generator: Generator,
    arena: Arena,

    pub fn new(stream: Stream, allocator: *Arena) TypeChecker {
        var arena = Arena.new(allocator.bytes(mem.PAGE_SIZE));

        return TypeChecker{
            .procedures = RangeMap(Procedure).new(10, &arena),
            .parameters = Vec(*const Type).new(10, &arena),
            .constants = Vec(Constant).new(10, &arena),
            .variables = RangeMap(Constant).new(10, &arena),
            .variable_refs = Vec(VariableRef).new(10, &arena),
            .ranges = Vec(Range).new(10, &arena),
            .types = RangeMap(Type).new(10, &arena),

            .generator = Generator.new(stream, &arena),
            .arena = arena,
        };
    }

    fn push_variable(self: *TypeChecker, name: Range, constant: Constant, words: *const String) void {
        const index = self.variables.put(name, constant, words);

        const ref_name = &self.variables.key[index];
        const ref_constant = &self.variables.value[index];

        self.variable_refs.push(VariableRef{
            .name = ref_name,
            .constant = ref_constant,
        });
    }

    pub fn push_scope(self: *TypeChecker, expression_count: u32, has_return: bool) void {
        var constants = Array(Constant).new(expression_count, &self.arena);
        var return_value: ?Constant = null;

        if (has_return) {
            return_value = self.constants.pop();
        }

        for (0..expression_count) |i| {
            constants.set(self.constants.pop(), expression_count - i - 1);
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
            self.constants.push(Constant{ .Ref = variable });
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
        var left = self.constants.pop();
        var right = self.constants.pop();
        var inner: ?*const Type = null;

        const left_inner = left.get_type();
        const right_inner = right.get_type();

        if (operator.is_comparison()) {
            inner = self.types.get("bool", words) orelse @panic("Missing boolean type declaration");
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
        const constant = self.arena.create(Constant, self.constants.pop());

        self.constants.push(Constant{ .Unary = self.arena.create(ConstantUnary, ConstantUnary{
            .constant = constant,
            .operator = operator,
            .source = null,
            .usage = 0,
        }) });
    }

    pub fn push_call(self: *TypeChecker, argument_count: u32, words: *const String) void {
        const range = self.ranges.pop();

        if (self.procedures.get(words.range(range), words)) |procedure| {
            if (procedure.parameters.len != argument_count) {
                @panic("Should not happen");
            }

            var arguments = Array(Constant).new(argument_count, &self.arena);

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
                .usage = 0,
            }) });
        } else {
            @panic("Procedure not found");
        }
    }

    pub fn push_parameter(self: *TypeChecker, current_size: u32, words: *const String) u32 {
        const type_range = self.ranges.pop();
        const name_range = self.ranges.pop();
        const inner = self.types.get(words.range(type_range), words) orelse @panic("Should not happen");
        const size = inner.size;

        self.parameters.push(inner);
        self.push_variable(name_range, Constant{ .Parameter = self.arena.create(ConstantParameter, ConstantParameter{
            .offset = current_size,
            .inner = inner,
            .usage = 0,
        }) }, words);

        return size;
    }

    pub fn push_let(self: *TypeChecker, words: *const String) void {
        const type_range = self.ranges.pop();
        const name_range = self.ranges.pop();
        const inner = self.types.get(words.range(type_range), words) orelse @panic("Should not happen");
        var constant = self.constants.pop();

        if (!constant.set_type(inner)) {
            @panic("Should not happen");
        }

        self.push_variable(name_range, constant, words);
    }

    pub fn push_type(self: *TypeChecker, field_count: u32, annotated_size: u32, words: *const String) void {
        const name_range = self.ranges.pop();
        var fields = Array(Type.Field).new(field_count, &self.arena);

        var size: u32 = annotated_size;
        var alignment: u32 = annotated_size;

        for (0..field_count) |i| {
            const field_type_range = self.ranges.pop();
            const field_name_range = self.ranges.pop();
            const inner = self.types.get(words.range(field_type_range), words) orelse @panic("Should not happen");

            size += inner.size;
            alignment = util.max(alignment, inner.alignment);

            fields.set(Type.Field{
                .name = field_name_range,
                .inner = inner,
            }, i);
        }

        self.types.push(name_range, Type{
            .fields = fields,
            .size = size,
            .alignment = alignment,
        }, words);
    }

    pub fn push_procedure(self: *TypeChecker, parameter_count: u32, words: *const String) void {
        const type_range = self.ranges.pop();
        const name_range = self.ranges.pop();
        const inner = self.types.get(words.range(type_range), words) orelse @panic("Type not declared");
        const offset: usize = 0;

        if (self.constants.len != 1) @panic("Should not happen");

        var constant = self.constants.pop();
        if (!constant.set_type(inner)) @panic("Should not happen");

        constant.usage_count(.Add);
        constant.evaluate(BinaryOperationKind.Mov, Destination{ .Register = Register.Rax }, &self.generator, true);
        constant.deinit(&self.arena);

        for (0..self.variable_refs.len) |_| {
            const ref = self.variable_refs.pop();

            ref.name.reset();
            ref.constant.deinit(&self.arena);
        }

        var parameters = Array(*const Type).new(parameter_count, &self.arena);
        for (0..parameter_count) |i| {
            parameters.set(self.parameters.pop(), i);
        }

        self.procedures.push(name_range, Procedure{
            .offset = offset,
            .inner = inner,
            .parameters = parameters,
        }, words);
    }

    pub fn deinit(self: *TypeChecker) void {
        self.generator.deinit();
    }
};
