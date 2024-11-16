const collections = @import("collections");
const mem = @import("mem");
const util = @import("util");
const constant = @import("constant.zig");
const generator = @import("generator.zig");

const Arena = mem.Arena;
const Stream = collections.Stream;
const Vec = collections.Vec;
const StringMap = collections.StringMap;
const Array = collections.Array;
const String = collections.String;
const Operation = generator.Operation;

const Generator = generator.Generator;

const Destination = generator.Destination;
const Memory = generator.Memory;
const Register = generator.Register;

const ConstantBinary = constant.ConstantBinary;
const ConstantCall = constant.ConstantCall;
const ConstantUnary = constant.ConstantUnary;
const ConstantParameter = constant.ConstantParameter;
const ConstantNumber = constant.ConstantNumber;
const ConstantScope = constant.ConstantScope;
const ConstantConstruct = constant.ConstantConstruct;
const ConstantFieldAcess = constant.ConstantFieldAcess;
const ConstantType = constant.ConstantType;
const ConstantProcedure = constant.ConstantProcedure;
const ConstantKind = constant.ConstantKind;
const Constant = constant.Constant;

pub const TypeChecker = struct {
    parameters: Vec(*const ConstantType),
    constants: Vec(Constant),

    names: Vec([]const u8),

    variables: StringMap(*Constant),
    variable_constants: Vec(Constant),
    variable_refs: Vec(*[]const u8),

    generator: Generator,
    arena: *Arena,

    const VARIABLE_MAX: u32 = 20;

    pub fn new(allocator: *Arena) error{OutOfMemory}!TypeChecker {
        var self: TypeChecker = undefined;

        self.arena = try allocator.child("TypeChecker", mem.PAGE_SIZE * 2 + (mem.PAGE_SIZE >> 1) - 2 * @sizeOf(Arena));
        errdefer self.arena.deinit();

        self.parameters = try Vec(*const ConstantType).new(VARIABLE_MAX, self.arena);
        errdefer self.parameters.deinit(self.arena);

        self.constants = try Vec(Constant).new(VARIABLE_MAX, self.arena);
        errdefer self.constants.deinit(self.arena);

        self.variables = try StringMap(*Constant).new(VARIABLE_MAX, self.arena);
        errdefer self.variables.deinit(self.arena);

        self.variable_constants = try Vec(Constant).new(VARIABLE_MAX, self.arena);
        errdefer self.variable_constants.deinit(self.arena);

        self.variable_refs = try Vec(*[]const u8).new(VARIABLE_MAX, self.arena);
        errdefer self.variable_refs.deinit(self.arena);

        self.names = try Vec([]const u8).new(VARIABLE_MAX, self.arena);
        errdefer self.names.deinit(self.arena);

        self.generator = try Generator.new(self.arena);
        errdefer self.generator.deinit();

        return self;
    }

    fn push_variable(self: *TypeChecker, name: []const u8, cons: Constant) void {
        self.variable_constants.push(cons) catch @panic("TODO");

        const ptr = self.variable_constants.last() catch @panic("TODO");
        const index = self.variables.put(name, ptr) catch @panic("TODO");
        const ref_name = &self.variables.key[index];

        self.variable_refs.push(ref_name) catch @panic("TODO");
    }

    pub fn push_scope(self: *TypeChecker, expression_count: u32, declaration_count: u32, has_return: bool) void {
        var constants = Array(Constant).new(expression_count, self.arena) catch @panic("TODO");
        var return_value: ?Constant = null;

        if (has_return) {
            return_value = self.constants.pop() catch @panic("TODO");
        }

        for (0..expression_count) |i| {
            constants.set(self.constants.pop() catch @panic("TODO"), expression_count - i - 1) catch unreachable;
        }

        for (0..declaration_count) |_| {
            const c = self.variable_refs.pop() catch @panic("TODO");
            c.len = 0;
        }

        self.constants.push(Constant{ .Scope = self.arena.create(ConstantScope, ConstantScope{
            .constants = constants,
            .return_value = return_value,
            .used = false,
            .usage = 0,
        }) catch @panic("TODO") }) catch @panic("TODO");
    }

    pub fn push_identifier(self: *TypeChecker) void {
        const name = self.names.pop() catch @panic("TODO");

        if (self.variables.get(name)) |variable| {
            self.constants.push(Constant{ .Ref = variable.* }) catch @panic("TODO");
        } else {
            @panic("TODO");
        }
    }

    pub fn push_number(self: *TypeChecker, string: []const u8) void {
        const number = util.parse(string);

        self.constants.push(Constant{ .Number = self.arena.create(ConstantNumber, ConstantNumber{
            .inner = null,
            .value = number,
            .usage = 0,
        }) catch @panic("TODO") }) catch @panic("TODO");
    }

    pub fn push_binary(self: *TypeChecker, operator: ConstantBinary.Operator) void {
        var right = self.constants.pop() catch @panic("TODO");
        var left = self.constants.pop() catch @panic("TODO");
        var inner: ?*const ConstantType = null;

        const left_inner = left.get_type();
        const right_inner = right.get_type();

        if (operator.is_comparison()) {
            inner = self.get_type("bool");
        } else if (left_inner) |typ| {
            if (!right.set_type(typ)) {
                @panic("TODO: Show type divergency");
            } else {
                inner = typ;
            }
        } else if (right_inner) |typ| {
            if (!left.set_type(typ)) {
                @panic("TODO: Show type divergency");
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
        }) catch @panic("TODO") }) catch @panic("TODO");
    }

    pub fn push_unary(self: *TypeChecker, operator: ConstantUnary.Operator) void {
        const cons = self.constants.pop() catch @panic("TODO");

        self.constants.push(Constant{ .Unary = self.arena.create(ConstantUnary, ConstantUnary{
            .constant = cons,
            .operator = operator,
            .source = null,
            .usage = 0,
        }) catch @panic("TODO") }) catch @panic("TODO");
    }

    pub fn push_call(self: *TypeChecker, argument_count: u32) void {
        const name = self.names.pop() catch @panic("TODO");

        const procedure = self.get_procedure(name);
        if (procedure.parameters.len != argument_count) @panic("TODO: show wrong arguments");

        var arguments = Array(Constant).new(argument_count, self.arena) catch @panic("TODO");

        for (0..argument_count) |i| {
            var argument = self.constants.pop() catch @panic("TODO");

            if (!argument.set_type(procedure.parameters.items[i])) {
                @panic("TODO: Show type divergency");
            }

            arguments.set(argument, i) catch unreachable;
        }

        self.constants.push(Constant{ .Call = self.arena.create(ConstantCall, ConstantCall{
            .procedure = procedure,
            .arguments = arguments,
            .source = null,
            .usage = 0,
        }) catch @panic("TODO") }) catch @panic("TODO");
    }

    pub fn push_parameter(self: *TypeChecker, current_size: u32) u32 {
        const type_ = self.names.pop() catch @panic("TODO");
        const name = self.names.pop() catch @panic("TODO");
        const inner = self.get_type(type_);
        const size = inner.size;

        self.parameters.push(inner) catch @panic("TODO");
        self.push_variable(name, Constant{ .Parameter = self.arena.create(ConstantParameter, ConstantParameter{
            .offset = current_size,
            .inner = inner,
            .usage = 0,
        }) catch @panic("TODO") });

        return size;
    }

    fn get_property(self: *TypeChecker, cons: Constant, name: []const u8) Constant {
        switch (cons) {
            .Ref => |ref| {
                return self.get_property(ref.*, name);
            },
            .Construct => |construct| {
                const index = construct.inner.field_index(name) catch @panic("Show that property do not exist");
                return Constant{ .Ref = &construct.constants.items[index] };
            },
            .FieldAcess => |field| {
                const index = field.inner.field_index(name) catch @panic("TODO: Show that property do not exist");

                return Constant{ .FieldAcess = self.arena.create(ConstantFieldAcess, ConstantFieldAcess{
                    .index = index,
                    .constant = Constant{ .FieldAcess = field },
                    .usage = 0,
                    .source = null,
                    .inner = field.inner.fields.items[index].inner,
                }) catch @panic("TODO") };
            },
            .Call => |call| {
                const index = call.procedure.inner.field_index(name) catch @panic("TODO");

                return Constant{ .FieldAcess = self.arena.create(ConstantFieldAcess, ConstantFieldAcess{
                    .index = index,
                    .constant = Constant{ .Call = call },
                    .usage = 0,
                    .source = null,
                    .inner = call.procedure.inner.fields.items[index].inner,
                }) catch @panic("TODO") };
            },
            else => @panic("TODO: Treat others constants when trying to acess their fields"),
        }
    }

    pub fn push_property(self: *TypeChecker, name: []const u8) void {
        const cons = self.constants.pop() catch @panic("TODO");
        self.constants.push(self.get_property(cons, name)) catch @panic("TODO");
    }

    pub fn push_let(self: *TypeChecker) void {
        const type_ = self.names.pop() catch @panic("TODO");
        const name = self.names.pop() catch @panic("TODO");
        const inner = self.get_type(type_);
        var cons = self.constants.pop() catch @panic("TODO");

        if (!cons.set_type(inner)) {
            @panic("TODO: Show type divergency");
        }

        self.push_variable(name, cons);
    }

    pub fn push_type(self: *TypeChecker, field_count: u32, annotated_size: u32) void {
        const name = self.names.pop() catch @panic("TODO");

        var fields = Array(ConstantType.Field).new(field_count, self.arena) catch @panic("TODO");

        var size: u32 = annotated_size;
        var alignment: u32 = annotated_size;

        for (0..field_count) |i| {
            const field_type = self.names.pop() catch @panic("TODO");
            const field_name = self.names.pop() catch @panic("TODO");
            const inner = self.get_type(field_type);

            size += inner.size;
            alignment = util.max(alignment, inner.alignment);

            fields.set(ConstantType.Field{
                .name = field_name,
                .inner = inner,
            }, i) catch unreachable;
        }

        self.push_variable(name, Constant{ .Type = self.arena.create(ConstantType, ConstantType{
            .name = name,
            .fields = fields,
            .size = size,
            .alignment = alignment,
            .usage = 0,
        }) catch @panic("TODO") });
    }

    pub fn push_construct(self: *TypeChecker, field_count: u32) void {
        const type_ = self.names.get_back(field_count) catch @panic("TODO");
        const inner = self.get_type(type_);

        if (inner.fields.len != field_count) @panic("Should not happen");

        var constants = Array(Constant).new(field_count, self.arena) catch @panic("TODO");

        for (0..field_count) |i| {
            const name = self.names.pop() catch @panic("TODO");

            if (!mem.equal(u8, name, inner.fields.items[i].name)) {
                util.print(.Info, "{s} - {s}\n", .{ name, inner.fields.items[i].name });
                @panic("TODO: accept a different field order than the defined type order");
            }

            var cons = self.constants.pop() catch @panic("TODO");

            if (!cons.set_type(inner.fields.items[i].inner)) @panic("Should not happen");
            constants.set(cons, i) catch unreachable;
        }

        _ = self.names.pop() catch unreachable;

        self.constants.push(Constant{ .Construct = self.arena.create(ConstantConstruct, ConstantConstruct{
            .constants = constants,
            .inner = inner,
            .usage = 0,
            .source = null,
        }) catch @panic("TODO") }) catch @panic("TODO");
    }

    pub fn push_procedure(self: *TypeChecker, parameter_count: u32, variable_start: u32) void {
        const type_ = self.names.pop() catch @panic("TODO");
        const name = self.names.pop() catch @panic("TODO");
        const inner = self.get_type(type_);
        const offset: usize = self.generator.code.len;

        var cons = self.constants.pop() catch @panic("TODO");
        if (!cons.set_type(inner)) @panic("Should not happen");

        const return_destination = blk: {
            if (inner.size <= 4) {
                break :blk Destination{
                    .Register = Register.Rax,
                };
            } else {
                break :blk Destination{
                    .Memory = Memory{ .register = Register.Rbp, .offset = inner.size },
                };
            }
        };

        cons.usage_count(.Add);
        cons.evaluate(.Mov, return_destination, &self.generator, true);
        cons.deinit(self.arena);

        for (0..parameter_count) |_| {
            const c = self.variable_refs.pop() catch @panic("TODO");
            c.len = 0;
        }

        for (variable_start..self.variable_constants.len) |_| {
            const c = self.variable_constants.pop() catch @panic("TODO");
            c.deinit(self.arena);
        }

        if (self.constants.len > 0) @panic("TODO");
        if (self.variable_refs.len != variable_start) @panic("TODO");
        if (self.variable_constants.len != variable_start) @panic("TODO");

        var parameter_size: u32 = 0;
        var parameters = Array(*const ConstantType).new(parameter_count, self.arena) catch @panic("TODO");
        for (0..parameter_count) |i| {
            const parameter = self.parameters.pop() catch @panic("TODO");
            parameter_size += parameter.size;

            parameters.set(parameter, i) catch unreachable;
        }

        util.print(.Info, "--------------------- Procedure start ({}) - Offset ({}) ---------------", .{ name, offset });
        self.generator.push_procedure(parameter_size, inner.size);
        util.print(.Info, "--------------------- Procedure end ---------------", .{});

        self.push_variable(name, Constant{ .Procedure = self.arena.create(ConstantProcedure, ConstantProcedure{
            .offset = offset,
            .inner = inner,
            .parameters = parameters,
            .usage = 0,
        }) catch @panic("TODO") });
    }

    fn get_type(self: *TypeChecker, name: []const u8) *ConstantType {
        const cons = (self.variables.get(name) orelse @panic("TODO: treat type not declared")).*.*;

        if (@as(ConstantKind, cons) != Constant.Type) @panic("TODO");

        return cons.Type;
    }

    fn get_procedure(self: *TypeChecker, name: []const u8) *const ConstantProcedure {
        const cons = (self.variables.get(name) orelse @panic("TODO: treat type not declared")).*.*;

        if (@as(ConstantKind, cons) != Constant.Procedure) @panic("TODO: treat type not declared");

        return cons.Procedure;
    }

    pub fn generate(self: *TypeChecker, stream: Stream) void {
        const main_procedure = self.get_procedure("main");
        if (main_procedure.inner.size != 4) @panic("TODO: treat main procedure mismatch return type");

        self.generator.generate(stream, main_procedure.offset);
    }

    pub fn deinit(self: *TypeChecker) void {
        for (self.variable_constants.offset(0) catch unreachable) |variable| {
            variable.deinit(self.arena);
        }

        self.generator.deinit();

        self.parameters.deinit(self.arena);
        self.constants.deinit(self.arena);
        self.variables.deinit(self.arena);

        self.variable_constants.deinit(self.arena);
        self.variable_refs.deinit(self.arena);
        self.names.deinit(self.arena);

        self.arena.deinit();
    }
};
