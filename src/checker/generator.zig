const collections = @import("collections");
const mem = @import("mem");
const util = @import("util");
const elf = @import("elf.zig");
const constant = @import("constant.zig");

const BASE_SIZE: u32 = 4;

pub const Register = enum(u8) {
    Rax,
    Rcx,
    Rdx,
    Rbx,
    Rsp,
    Rbp,
    Rsi,
    Rdi,

    fn value(self: Register) u8 {
        return @intFromEnum(self);
    }

    pub fn print(self: Register, formater: util.Formater) error{Overflow}!void {
        switch (self) {
            .Rax => try formater("Register::Rax", .{}),
            .Rcx => try formater("Register::Rcx", .{}),
            .Rdx => try formater("Register::Rdx", .{}),
            .Rbx => try formater("Register::Rbx", .{}),
            .Rsp => try formater("Register::Rsp", .{}),
            .Rbp => try formater("Register::Rbp", .{}),
            .Rsi => try formater("Register::Rsi", .{}),
            .Rdi => try formater("Register::Rdi", .{}),
        }
    }
};

const Immediate = usize;
const Offset = usize;

fn to_bytes(int: usize) [8]u8 {
    var bytes: [8]u8 = undefined;

    for (0..8) |i| {
        const u: u6 = @intCast(i * 8);
        bytes[i] = @truncate(int >> u);
    }

    return bytes;
}

pub const Memory = struct {
    register: Register,
    offset: Offset,

    pub fn print(self: Memory, formater: util.Formater) error{Overflow}!void {
        try formater("Memory({}, Offset({}))", .{ self.register, self.offset });
    }
};

pub const SourceKind = enum { Register, Memory, Immediate, Stack };
pub const Source = union(SourceKind) {
    Register: Register,
    Memory: Memory,
    Immediate: Immediate,
    Stack,

    fn eql(self: Source, other: SourceKind) bool {
        return @as(SourceKind, self) == other;
    }

    pub fn print(self: Source, formater: util.Formater) error{Overflow}!void {
        return switch (self) {
            .Register => |r| try formater("Source::Register({})", .{r}),
            .Memory => |m| try formater("Source::Memory({})", .{m}),
            .Immediate => |i| try formater("Source::Immediate({})", .{i}),
            .Stack => try formater("Source::Stack", .{}),
        };
    }
};

pub const DestinationKind = enum { Register, Memory, Stack };
pub const Destination = union(DestinationKind) {
    Register: Register,
    Memory: Memory,
    Stack,

    fn eql(self: Destination, other: DestinationKind) bool {
        return @as(DestinationKind, self) == other;
    }

    pub fn as_source(self: Destination) Source {
        return switch (self) {
            .Register => |r| Source{ .Register = r },
            .Memory => |m| Source{ .Memory = m },
            .Stack => Source.Stack,
        };
    }

    pub fn print(self: Destination, formater: util.Formater) error{Overflow}!void {
        return switch (self) {
            .Register => |r| try formater("Destination::Register({})", .{r}),
            .Memory => |m| try formater("Destination::Memory({})", .{m}),
            .Stack => try formater("Destination::Stack", .{}),
        };
    }
};

pub const BinaryKind = enum { Mov, Add, Sub, Mul };
pub const BinaryOperation = struct {
    kind: BinaryKind,
    source: Source,
    destination: Destination,

    pub fn print(self: BinaryOperation, formater: util.Formater) error{Overflow}!void {
        switch (self.kind) {
            .Mov => try formater("BinaryOperation::Mov({} -> {})", .{ self.source, self.destination }),
            .Add => try formater("BinaryOperation::Add({} -> {})", .{ self.source, self.destination }),
            .Sub => try formater("BinaryOperation::Sub({} -> {})", .{ self.source, self.destination }),
            .Mul => try formater("BinaryOperation::Mul({} -> {})", .{ self.source, self.destination }),
        }
    }

    fn write_mov(self: BinaryOperation, buffer: *collections.String) void {
        switch (self.destination) {
            .Stack => switch (self.source) {
                .Register => |r| buffer.push(0x50 + r.value()) catch @panic("TODO"),
                .Memory => |m| buffer.extend(&.{ 0xFF, 0b01110000 + m.register.value(), to_bytes(m.offset)[0] }) catch @panic("TODO"),
                .Immediate => |i| buffer.mult_extend(&.{ &.{0x68}, to_bytes(i)[0 .. mem.BASE_SIZE >> 1] }) catch @panic("TODO"),
                .Stack => unreachable,
            },
            .Register => |rd| switch (self.source) {
                .Stack => buffer.push(0x58 + rd.value()) catch @panic("TODO"),
                .Register => |rs| buffer.extend(&.{ 0x48, 0x89, 0b11000000 + (rs.value() << 3) + rd.value() }) catch @panic("TODO"),
                .Memory => |m| {
                    buffer.extend(&.{
                        0x8B,
                        0b01000000 + (rd.value() << 3) + m.register.value(),
                        to_bytes(m.offset)[0],
                    }) catch @panic("TODO");
                },
                .Immediate => |i| buffer.mult_extend(&.{ &.{0xB8 + rd.value()}, to_bytes(i)[0 .. mem.BASE_SIZE >> 1] }) catch @panic("TODO"),
            },
            .Memory => |m| switch (self.source) {
                .Register => |r| {
                    buffer.extend(&.{
                        0x89,
                        0b01000000 + (r.value() << 3) + m.register.value(),
                        to_bytes(m.offset)[0],
                    }) catch @panic("TODO");
                },
                .Immediate => |i| {
                    buffer.mult_extend(&.{
                        &.{ 0xC7, 0b01000000 + m.register.value(), to_bytes(m.offset)[0] },
                        to_bytes(i)[0 .. mem.BASE_SIZE >> 1],
                    }) catch @panic("TODO");
                },
                .Stack => @panic("TODO"),
                .Memory => @panic("TODO"),
            },
        }
    }

    fn write_add(self: BinaryOperation, buffer: *collections.String) void {
        switch (self.destination) {
            .Register => |rd| switch (self.source) {
                .Register => |rs| buffer.extend(&.{ 0x01, 0b11000000 + (rs.value() << 3) + rd.value() }) catch @panic("TODO"),
                .Immediate => |i| buffer.extend(&.{ 0x83, 0b11000000 + rd.value(), to_bytes(i)[0] }) catch @panic("TODO"),
                .Stack => @panic("TODO"),
                .Memory => |m| {
                    buffer.extend(&.{
                        0x03,
                        0b01000000 + rd.value(),
                        0b00100000 + m.register.value(),
                        to_bytes(m.offset)[0],
                    }) catch @panic("TODO");
                },
            },
            .Memory => |m| switch (self.source) {
                .Register => |r| buffer.extend(&.{ 0x01, 0b01000000 + (r.value() << 3) + m.register.value() }) catch @panic("TODO"),
                .Immediate => @panic("TODO"),
                .Stack => @panic("TODO"),
                .Memory => @panic("TODO"),
            },
            .Stack => @panic("Should not happen"),
        }
    }

    fn write_sub(self: BinaryOperation, buffer: *collections.String) void {
        switch (self.destination) {
            .Register => |rd| switch (self.source) {
                .Register => |rs| buffer.extend(&.{ 0x29, 0b11000000 + (rs.value() << 3) + rd.value() }) catch @panic("TODO"),
                .Immediate => |i| buffer.extend(&.{ 0x48, 0b11101000 + rd.value(), to_bytes(i)[0] }) catch @panic("TODO"),
                .Memory => @panic("TODO"),
                .Stack => @panic("TODO"),
            },
            .Memory => |m| switch (self.source) {
                .Register => |r| buffer.extend(&.{ 0x029, 0b01000000 + (r.value() << 3) + m.register.value() }) catch @panic("TODO"),
                .Immediate => @panic("TODO"),
                .Stack => @panic("TODO"),
                .Memory => @panic("TODO"),
            },
            .Stack => unreachable,
        }
    }

    fn write_mul(self: BinaryOperation, buffer: *collections.String) void {
        switch (self.destination) {
            .Register => |rd| switch (self.source) {
                .Immediate => |i| buffer.extend(&.{ 0x6B, 0b01101000 + rd.value(), to_bytes(i)[0] }) catch @panic("TODO"),
                .Register => @panic("TODO"),
                .Stack => @panic("TODO"),
                .Memory => |m| {
                    buffer.extend(&.{
                        0x0F,
                        0xAF,
                        0b01000000 + rd.value(),
                        0b00100000 + m.register.value(),
                        to_bytes(m.offset)[0],
                    }) catch @panic("TODO");
                },
            },
            .Memory => |_| switch (self.source) {
                .Register => @panic("TODO"),
                .Immediate => @panic("TODO"),
                .Stack => @panic("TODO"),
                .Memory => @panic("TODO"),
            },
            .Stack => unreachable,
        }
    }
};

const OperationKind = enum {
    Binary,
    Call,
    Ret,
    Syscall,
};

pub const Operation = union(OperationKind) {
    Binary: BinaryOperation,
    Call: Offset,
    Ret,
    Syscall,

    pub fn print(self: Operation, formater: util.Formater) error{Overflow}!void {
        switch (self) {
            .Syscall => try formater("Operation::Syscall", .{}),
            .Ret => try formater("Operation::Ret", .{}),
            .Call => |immediate| try formater("Operation::Call(Offset({}))", .{immediate}),
            .Binary => |binary| try formater("Operation::Binary({})", .{binary}),
        }
    }

    fn write(self: Operation, buffer: *collections.String) void {
        util.print(.Info, "{}", .{self});

        switch (self) {
            .Syscall => buffer.extend(&.{ 0x0F, 0x05 }) catch @panic("TODO"),
            .Ret => buffer.push(0xC3) catch @panic("TODO"),
            .Call => |immediate| {
                const call_instruction_size: isize = 0x05;
                const len: isize = @intCast(buffer.len);
                const i: isize = @intCast(immediate);
                const offset = i - len - call_instruction_size;

                buffer.mult_extend(&.{ &.{0xE8}, to_bytes(@bitCast(offset))[0 .. mem.BASE_SIZE >> 1] }) catch @panic("TODO");
            },
            .Binary => |binary| switch (binary.kind) {
                .Mov => binary.write_mov(buffer),
                .Add => binary.write_add(buffer),
                .Sub => binary.write_sub(buffer),
                .Mul => binary.write_mul(buffer),
            },
        }
    }

    pub fn equal(self: Operation, other: Operation) bool {
        if (@as(OperationKind, self) != @as(OperationKind, other)) return false;

        switch (self) {
            .Syscall, .Ret => return true,
            .Call => return self.Call == other.Call,
            .Binary => {
                if (@as(BinaryKind, self.Binary.kind) != @as(BinaryKind, other.Binary.kind)) return false;
                if (@as(DestinationKind, self.Binary.destination) != @as(DestinationKind, other.Binary.destination)) return false;
                if (@as(SourceKind, self.Binary.source) != @as(SourceKind, other.Binary.source)) return false;

                switch (self.Binary.destination) {
                    .Register => return self.Binary.destination.Register == other.Binary.destination.Register,
                    .Memory => {
                        const self_memory = self.Binary.destination.Memory;
                        const other_memory = other.Binary.destination.Memory;

                        return self_memory.offset == other_memory.offset and self_memory.register == other_memory.register;
                    },
                    .Stack => return true,
                }

                switch (self.Binary.source) {
                    .Register => return self.Binary.source.Register == other.Binary.source.Register,
                    .Memory => {
                        const self_memory = self.Binary.source.Memory;
                        const other_memory = other.Binary.source.Memory;

                        return self_memory.offset == other_memory.offset and self_memory.register == other_memory.register;
                    },
                    .Immediate => self.Binary.source.Immediate == other.Binary.source.Immediate,
                    .Stack => return true,
                }
            },
        }
    }
};

pub const Generator = struct {
    code: collections.String,
    data: collections.String,
    operations: collections.Vec(Operation),
    manager: Manager,

    arena: *mem.Arena,

    pub const Error = error{
        OutOfRegisters,
        SourceNotBeingUsed,
    };

    const Manager = struct {
        free: collections.Vec(Register),
        resources: collections.Vec(Resource),
        stack: usize,

        pub const Resource = struct {
            variant: Variant,
            ptr: *const constant.Constant,

            const Kind = enum {
                Register,
                Memory,
            };

            const Variant = union(Kind) {
                Register: Register,
                Memory: Memory,
            };

            fn as_destination(self: Resource) Destination {
                return switch (self.variant) {
                    .Register => |register| Destination{ .Register = register },
                    .Memory => |memory| Destination{
                        .Memory = memory,
                    },
                };
            }
        };

        fn new(arena: *mem.Arena) error{OutOfMemory}!Manager {
            const usable_registers = &.{
                Register.Rcx, Register.Rdi, Register.Rdx, Register.Rbx, Register.Rax,
            };

            var resources = try collections.Vec(Resource).new(usable_registers.len, arena);
            errdefer resources.deinit(arena);

            var free = try collections.Vec(Register).new(usable_registers.len, arena);
            errdefer free.deinit(arena);

            free.extend(usable_registers) catch unreachable;

            return Manager{
                .resources = resources,
                .free = free,
                .stack = 0,
            };
        }

        fn increase(self: *Manager, size: usize) void {
            self.stack += size;
        }

        fn get(self: *Manager, cons: *const constant.Constant) Destination {
            const inner = cons.get_type().?;

            const resource = blk: {
                if (inner.size > BASE_SIZE) {
                    const offset = self.stack;

                    self.increase(inner.size);

                    break :blk Resource{
                        .ptr = cons,
                        .variant = Resource.Variant{
                            .Memory = Memory{
                                .register = Register.Rsp,
                                .offset = offset,
                            },
                        },
                    };
                } else {
                    const register = self.free.pop() catch @panic("TODO");

                    break :blk Resource{
                        .ptr = cons,
                        .variant = Resource.Variant{
                            .Register = register,
                        },
                    };
                }
            };

            self.resources.push(resource) catch @panic("TODO");

            return resource.as_destination();
        }

        fn get_registry(self: Manager, cons: *const constant.Constant) ?Destination {
            for (self.resources.offset(0) catch unreachable) |resource| {
                if (resource.ptr == cons) return resource.as_destination();
            }

            return null;
        }

        fn is_used(self: Manager, register: Register) bool {
            for (self.free.offset(0) catch unreachable) |r| {
                if (r == register) return false;
            }

            return true;
        }

        fn clear(self: *Manager) void {
            self.resources.clear();
        }

        fn deinit(self: *Manager, arena: *mem.Arena) void {
            self.free.deinit(arena);
            self.resources.deinit(arena);
        }
    };

    pub fn new(allocator: *mem.Arena) error{OutOfMemory}!Generator {
        var self: Generator = undefined;

        self.arena = try allocator.child("Generator", mem.PAGE_SIZE >> 1);
        errdefer self.arena.deinit();

        self.code = try collections.String.new(512, self.arena);
        errdefer self.code.deinit(self.arena);

        self.data = try collections.String.new(1, self.arena);
        errdefer self.data.deinit(self.arena);

        self.operations = try collections.Vec(Operation).new(16, self.arena);
        errdefer self.operations.deinit(self.arena);

        self.manager = try Manager.new(self.arena);
        errdefer self.manager.deinit(self.arena);

        return self;
    }

    pub fn give_back(self: *Generator, source: Source) Error!void {
        if (@as(SourceKind, source) == SourceKind.Register) {
            try self.manager.give_back(source.Register);
        } else {
            // @panic("TODO");
        }
    }

    pub fn evaluate(self: *Generator, cons: *constant.Constant, kind: BinaryKind, destination: ?Destination) Source {
        switch (cons.*) {
            .Number => |n| {
                const source = Source{
                    .Immediate = n.value,
                };

                if (destination) |dst| {
                    self.operations.push(Operation{
                        .Binary = BinaryOperation{
                            .kind = kind,
                            .source = source,
                            .destination = dst,
                        },
                    }) catch @panic("TODO");

                    return dst.as_source();
                } else {
                    return source;
                }
            },
            .Binary => |binary| {
                const dst = destination orelse @panic("TODO");

                _ = self.evaluate(&binary.right, .Mov, dst);
                _ = self.evaluate(&binary.left, binary.operator.to_gen_operation(), dst);

                return dst.as_source();
            },
            .Unary => |unary| {
                _ = self.evaluate(&unary.constant, .Mov, null);
            },
            .Parameter => |_| @panic("TODO"),
            .Call => |call| {
                const rax_used = self.manager.is_used(Register.Rax);

                if (rax_used) {
                    self.operations.push(Operation{
                        .Binary = BinaryOperation{
                            .kind = BinaryKind.Mov,
                            .source = Source{
                                .Register = Register.Rax,
                            },
                            .destination = Destination.Stack,
                        },
                    }) catch @panic("TODO");
                }

                if (call.procedure.inner.size > BASE_SIZE) {
                    self.operations.extend(&.{
                        Operation{
                            .Binary = BinaryOperation{
                                .kind = BinaryKind.Sub,
                                .source = Source{
                                    .Immediate = call.procedure.inner.size,
                                },
                                .destination = Destination{
                                    .Register = Register.Rsp,
                                },
                            },
                        },
                        Operation{
                            .Binary = BinaryOperation{
                                .kind = BinaryKind.Mov,
                                .source = Source{
                                    .Register = Register.Rsp,
                                },
                                .destination = Destination{
                                    .Register = Register.Rax,
                                },
                            },
                        },
                    }) catch @panic("TODO");
                }

                self.operations.push(Operation{
                    .Call = call.procedure.offset,
                }) catch @panic("TODO");

                if (rax_used) {
                    self.operations.push(Operation{ .Binary = BinaryOperation{
                        .kind = BinaryKind.Mov,
                        .source = Source.Stack,
                        .destination = Destination{ .Register = Register.Rax },
                    } }) catch @panic("TODO");
                }

                const src = if (destination) |d| d.as_source() else null;

                return src orelse blk: {
                    if (call.procedure.inner.size > BASE_SIZE) {
                        break :blk Source{
                            .Memory = Memory{
                                .register = Register.Rsp,
                                .offset = 0,
                            },
                        };
                    } else break :blk Source{
                        .Register = Register.Rax,
                    };
                };
            },
            .Construct => |construct| {
                var offset: usize = 0;
                const dest = destination orelse @panic("TODO");
                const dst = blk: {
                    if (@as(DestinationKind, dest) == .Register) {
                        break :blk Destination{
                            .Memory = Memory{
                                .register = dest.Register,
                                .offset = 0,
                            },
                        };
                    } else {
                        break :blk dest;
                    }
                };

                for (0..construct.constants.len) |i| {
                    self.operations.push(Operation{
                        .Binary = BinaryOperation{
                            .kind = BinaryKind.Mov,
                            .source = self.evaluate(&construct.constants.items[i], .Mov, null),
                            .destination = Destination{
                                .Memory = Memory{
                                    .register = dst.Memory.register,
                                    .offset = dst.Memory.offset + offset,
                                },
                            },
                        },
                    }) catch @panic("TODO");

                    offset += construct.constants.items[i].get_type().?.size;
                }

                return dest.as_source();
            },
            .FieldAcess => |field| {
                const constant_source = self.evaluate(&field.constant, .Mov, null);
                const register = constant_source.Memory.register;
                const offset = constant_source.Memory.offset;

                if (destination) |dst| {
                    self.operations.push(Operation{
                        .Binary = BinaryOperation{
                            .kind = kind,
                            .source = Source{
                                .Memory = Memory{
                                    .register = register,
                                    .offset = offset + field.offset,
                                },
                            },
                            .destination = dst,
                        },
                    }) catch @panic("TODO");

                    return dst.as_source();
                }

                return Source{ .Memory = Memory{
                    .register = register,
                    .offset = field.offset,
                } };
            },
            .Ref => |ref| {

                const dst = blk: {
                    if (self.manager.get_registry(ref)) |dst| {
                        if (destination) |d| {
                            self.operations.push(Operation{
                                .Binary = BinaryOperation{
                                    .kind = kind,
                                    .source = dst.as_source(),
                                    .destination = d,
                                },
                            }) catch @panic("TODO");

                            break :blk d;
                        } else {
                            break :blk dst;
                        }
                    } else {
                        if (destination) |dst| {
                            _ = self.evaluate(ref, kind, dst);
                            break :blk dst;
                        } else {
                            const dst = self.manager.get(ref);
                            _ = self.evaluate(ref, kind, dst);

                            break :blk dst;
                        }
                    }
                };
                return dst.as_source();
            },
            .Scope => |scope| {
                for (0..scope.constants.len) |i| {
                    _ = self.evaluate(&scope.constants.items[i], .Mov, null);
                }

                if (scope.return_value) |*value| {
                    return self.evaluate(value, .Mov, destination);
                }
            },
            .Procedure, .Type => @panic("Should be unreachable"),
        }

        @panic("TODO");
    }

    pub fn push_procedure(self: *Generator, return_value: *constant.Constant) void {
        util.print(.Info, "--------------------- Procedure start () - Offset () ---------------", .{});

        defer self.operations.clear();

        _ = self.evaluate(return_value, .Mov, Destination{ .Register = Register.Rax });

        var startup_instructions = collections.Vec(Operation).new(2, self.arena) catch @panic("TODO");
        defer startup_instructions.deinit(self.arena);

        self.operations.push(Operation.Ret) catch @panic("TODO");

        for (startup_instructions.offset(0) catch unreachable) |operation| {
            operation.write(&self.code);
        }

        for (self.operations.offset(0) catch unreachable) |operation| {
            operation.write(&self.code);
        }

        self.operations.clear();
        self.manager.clear();
        util.print(.Info, "--------------------- Procedure end ---------------", .{});
    }

    pub fn generate(self: *Generator, stream: collections.Stream, main_procedure_offset: usize) void {
        const program_end = [_]Operation{
            Operation{ .Call = main_procedure_offset },
            Operation{ .Binary = BinaryOperation{
                .kind = BinaryKind.Mov,
                .source = Source{ .Register = Register.Rax },
                .destination = Destination{ .Register = Register.Rdi },
            } },
            Operation{ .Binary = BinaryOperation{
                .kind = BinaryKind.Mov,
                .destination = Destination{ .Register = Register.Rax },
                .source = Source{ .Immediate = 0x3C },
            } },
            Operation.Syscall,
        };

        const elf_header = elf.Header.new(.Exec, 1, self.code.len);
        const program_header = elf.ProgramHeader.new(.Load, &.{
            .Readable,
            .Executable,
        }, self.code.len);

        for (program_end) |operation| {
            operation.write(&self.code);
        }

        const h_size = @sizeOf(elf.Header);
        const ph_size = @sizeOf(elf.ProgramHeader);

        self.code.shift_right(0, h_size + ph_size) catch @panic("TODO");

        const code_len = self.code.len;

        self.code.set_len(0) catch unreachable;

        self.code.extend(mem.as_bytes(elf.Header, &elf_header)) catch @panic("TODO");
        self.code.extend(mem.as_bytes(elf.ProgramHeader, &program_header)) catch @panic("TODO");

        self.code.set_len(code_len) catch unreachable;

        if (false) {
            stream.write(self.code) catch @panic("TODO");
        }
    }

    pub fn deinit(self: *Generator) void {
        self.manager.deinit(self.arena);
        self.operations.deinit(self.arena);
        self.data.deinit(self.arena);
        self.code.deinit(self.arena);

        self.arena.deinit();
    }
};
