const collections = @import("collections");
const mem = @import("mem");
const util = @import("util");

const Arena = mem.Arena;

const Vec = collections.Vec;
const Stream = collections.Stream;
const String = collections.String;

pub const Register = enum {
    Rax,
    Rcx,
    Rdx,
    Rbx,
    Rsp,
    Rbp,
    Rsi,
    Rdi,

    fn value(self: Register) usize {
        return @intFromEnum(self);
    }
};

const Immediate = usize;
const Offset = usize;

fn to_bytes(int: usize) []const u8 {
    const bytes: [8]u8 = undefined;

    for (0..8) |i| {
        bytes[i] = @truncate(int >> (8 * i));
    }

    return bytes;
}

pub const Memory = struct {
    register: Register,
    offset: Offset,
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
            .Stack => Source{ .Stack = {} },
        };
    }
};

const BYTE_SIZE: usize = 1;
const INT_SIZE: usize = 4;

pub const BinaryKind = enum { Mov, Add, Sub, Mul };
pub const BinaryOperation = struct {
    kind: BinaryKind,
    source: Source,
    destination: Destination,

    fn write_mov(self: BinaryOperation, buffer: *String) void {
        switch (self.destination) {
            .Stack => switch (self.source) {
                .Register => |r| buffer.push(0x50 + r.value()),
                .Memory => |m| buffer.extend(&.{ 0xFF, 0b01110000 + m.register.value, to_bytes(m.offset)[0] }),
                .Immediate => |i| buffer.mult_extend(&.{ &.{0x68}, to_bytes(i)[0..INT_SIZE] }),
                .Stack => @panic("Why would you do that?"),
            },
            .Register => |rd| switch (self.source) {
                .Stack => buffer.push(0x58 + rd.value()),
                .Register => |rs| buffer.extend(&.{ 0x48, 0x89, 0b11000000 + (rd.value() << 3) + rs.value() }),
                .Memory => |m| buffer.extend(&.{ 0x8B, 0b01000000 + (rd.value() << 3) + m.register.value(), to_bytes(m.offset)[0] }),
                .Immediate => |i| buffer.mult_extend(&.{ &.{0xB8 + rd.value()}, to_bytes(i)[0..INT_SIZE] }),
            },
            .Memory => |m| switch (self.source) {
                .Register => |r| buffer.extend(&.{ 0x89, 0b01000000 + (r.value() << 3) + m.register.value(), to_bytes(m.offset)[0] }),
                .Immediate => |i| buffer.mult_extend(&.{ &.{ 0xC7, 0b01000000 + m.register.value(), to_bytes(m.offset)[0] }, to_bytes(i)[0..INT_SIZE] }),
                .Stack => @panic("Should not happen"),
                .Memory => @panic("Should not happen"),
            },
        }
    }

    fn write_add(self: BinaryOperation, buffer: *String) void {
        switch (self.destination) {
            .Register => |rd| switch (self.source) {
                .Register => |rs| buffer.extend(&.{ 0x01, 0b11000000 + (rs.value() << 3) + rd.value() }),
                .Immedaite => |i| buffer.extend(&.{ 0x83, 0b11000000 + rd.value(), to_bytes(i)[0] }),
                .Stack => @panic("Why whould you do that?"),
                .Memory => @panic("TODO"),
            },
            .Memory => |m| switch (self.source) {
                .Register => |r| buffer.extend(&.{ 0x01, 0x01000000 + (r.value() << 3) + m.register.value() }),
                .Immediate => @panic("TODO"),
                .Stack => @panic("TODO"),
                .Memory => @panic("Is that possible?"),
            },
            .Stack => @panic("Should not happen"),
        }
    }

    fn write_sub(self: BinaryOperation, buffer: *String) void {
        switch (self.destination) {
            .Register => |rd| switch (self.source) {
                .Register => |rs| buffer.extend(&.{ 0x29, 0b11000000 + (rs.value() << 3) + rd.value() }),
                .Immedaite => |i| buffer.extend(&.{ 0x48, 0b11101000 + rd.value(), to_bytes(i)[0] }),
                .Stack => @panic("Why whould you do that?"),
                .Memory => @panic("TODO"),
            },
            .Memory => |m| switch (self.source) {
                .Register => |r| buffer.extend(&.{ 0x029, 0x01000000 + (r.value() << 3) + m.register.value() }),
                .Immediate => @panic("TODO"),
                .Stack => @panic("TODO"),
                .Memory => @panic("Is that possible?"),
            },
            .Stack => @panic("Should not happen"),
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

    fn write(self: Operation, buffer: *String) void {
        switch (self) {
            .Syscall => buffer.extend(&.{ 0x0F, 0x05 }),
            .Ret => buffer.push(0xC3),
            .Call => |immediate| {
                const call_instruction_size: isize = 0x05;
                const len: isize = @intCast(buffer.len);
                const i: isize = @intCast(immediate);
                const offset = i - len - call_instruction_size;

                buffer.mult_extend(&.{ &.{0xE8}, to_bytes(@intCast(offset)) });
            },
            .Binary => |binary| switch (binary.kind) {
                .Mov => binary.write_mov(buffer),
                .Add => binary.write_add(buffer),
                .Sub => binary.write_sub(buffer),
                .Mul => @panic("TODO"),
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

const RegisterManager = struct {
    free: Vec(Register),
    used: Vec(Register),

    fn new(arena: *Arena) RegisterManager {
        const usable_registers = &.{
            Register.Rax, Register.Rcx, Register.Rdx, Register.Rbx, Register.Rdi,
        };

        const used = Vec(Register).new(usable_registers.len, arena);
        var free = Vec(Register).new(usable_registers.len, arena);

        free.extend(usable_registers);

        return RegisterManager{
            .used = used,
            .free = free,
        };
    }

    pub fn get(self: *RegisterManager) Register {
        const r = self.free.pop();
        self.used.push(r);

        return r;
    }

    pub fn give_back(self: *RegisterManager, r: Register) void {
        for (self.used.offset(0), 0..) |use, i| {
            if (use == r) {
                self.used.remove(i);
                self.free.push(r);

                return;
            }
        }
    }
};

pub const Generator = struct {
    stream: Stream,
    buffer: String,
    operations: Vec(Operation),
    manager: RegisterManager,

    arena: Arena,

    pub fn new(stream: Stream, allocator: *Arena) Generator {
        var arena = Arena.new(allocator.bytes(2048));

        return Generator{
            .stream = stream,
            .buffer = String.new(512, &arena),
            .operations = Vec(Operation).new(16, &arena),
            .manager = RegisterManager.new(&arena),
            .arena = arena,
        };
    }

    pub fn give_back(self: *Generator, source: Source) void {
        if (@as(SourceKind, source) == SourceKind.Register) {
            self.manager.give_back(source.Register);
        }
    }

    pub fn deinit(self: *Generator) void {
        self.stream.close();
    }
};
