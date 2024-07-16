const std = @import("std");
const util = @import("util.zig");

const Allocator = std.mem.Allocator;
const Vec = @import("collections.zig").Vec;
const FixedVec = @import("collections.zig").FixedVec;
const Node = @import("parser.zig").Node;
const Tree = @import("parser.zig").Tree;

const InstructionType = enum {
    Move,
    Syscall,
    Pop,
    Push,
    Ret,
    Global,
    Function,

    fn string(self: *const InstructionType) []const u8 {
        return switch (self.*) {
            .Move    => "    mov",
            .Syscall => "    syscall",
            .Pop     => "    pop",
            .Push    => "    push",
            .Ret     => "    ret",
            .Global  => "global",
            .Function => "",
        };
    }

};

const Register = enum {
    Rax, Rsp, Rbp,

    fn string(self: *const Register) []const u8 {
        return switch (self.*) {
            .Rax => "rax",
            .Rsp => "rsp",
            .Rbp => "rbp",
        };
    }
};

const Memory = struct { offset: u32 };
const Literal = struct { value: []const u8 };
const ArgumentType = enum { register, memory, literal };

const InstructionArgument = union(ArgumentType) {
    register: Register,
    memory: Memory,
    literal: Literal,

    fn literal_init(value: []const u8) InstructionArgument {
        return InstructionArgument {
            .literal = Literal {
                .value = value,
            },
        };
    }

    fn memory_init(offset: u32) InstructionArgument {
        return InstructionArgument {
            .memory = Memory {
                .offset = offset,
            },
        };
    }

    fn register_init(register: Register) InstructionArgument {
        return InstructionArgument {
            .register = register,
        };
    }

    fn write(self: *const InstructionArgument, buffer: *Vec(u8)) !void {
        try buffer.push(' ');

        switch (self.*) {
            .literal => |literal| try buffer.extend_with(literal.value),
            .register => |register| try buffer.extend_with(register.string()),
            .memory => |memory| {
                try buffer.extend_with("[rbp - ");
                try util.parse(memory.offset, buffer);
                try buffer.push(']');
            }
        }
    }
};

const Instruction = struct {
    typ: InstructionType,
    arg1: ?InstructionArgument,
    arg2: ?InstructionArgument,

    fn init(typ: InstructionType, arg1: ?InstructionArgument, arg2: ?InstructionArgument) Instruction {
        return Instruction {
            .typ = typ,
            .arg1 = arg1,
            .arg2 = arg2,
        };
    }

    fn write(self: *const Instruction, buffer: *Vec(u8)) !void {
        try buffer.extend_with(self.typ.string());

        if (self.arg1) |arg| {
            try arg.write(buffer);
        }

        if (self.arg2) |arg| {
            try arg.write(buffer);
        }

        try buffer.push('\n');
    }
};

const Location = InstructionArgument;
const Value = struct {
    hash: u32,
    location: Location,

    fn init(hash: u32, location: Location) Value {
        return Value {
            .hash = hash,
            .location = location,
        };
    }
};

const Assembly = struct {
    instructions: Vec(Instruction),
    values: FixedVec(Value, 100),
    stack: FixedVec(u32, 100),
    registers: FixedVec(u32, 3),

    fn init(allocator: Allocator) !Assembly {
        return Assembly {
            .instructions = try Vec(Instruction).init(20, allocator),
            .stack = FixedVec(u32, 100).init(),
            .values = FixedVec(Value, 100).init(),
            .registers = FixedVec(u32, 3).init(),
        };
    }

    fn write_function_name(self:*Assembly, node: *Node) !void {
        const hash = util.hash(node.token.value.?);
        const location = Literal { .value = node.token.value.? };

        try self.values.push(Value.init(hash, location));

        try self.instructions.push(
            Instruction.init(
                .Function,
                InstructionArgument.literal_init(node.token.value.?),
                InstructionArgument.literal_init(":"),
            )
        );
    }

    fn write_function_body(self:*Assembly, _: *Node) !void {
        try self.instructions.push(
            Instruction.init(
                .Push,
                InstructionArgument.register_init(.Rbp),
                null
            )
        );

        try self.instructions.push(
            Instruction.init(
                .Move,
                InstructionArgument.register_init(.Rbp),
                InstructionArgument.register_init(.Rsp)
            )
        );
    }

    fn write_return(self: *Assembly, node: *Node) !void {
        if (node.next_expression()) |exp| {
            if (exp.token.id == .Identifier) {
                const hash = util.hash(exp.token.value.?);
                for (self.values.items) |variable| {
                    if (variable.hash == hash) {
                        try self.instructions.push(
                            Instruction.init(
                                .Move,
                                InstructionArgument.register_init(.Rax),
                                variable.location,
                            )
                        );
                    }
                }
            } else {
                try self.instructions.push(
                    Instruction.init(
                        .Move,
                        InstructionArgument.register_init(.Rax),
                        InstructionArgument.literal_init(exp.token.value.?)
                    )
                );
            }
        }

        try self.instructions.push(Instruction.init(.Move, InstructionArgument.register_init(.Rsp), InstructionArgument.register_init(.Rbp)));
        try self.instructions.push(Instruction.init(.Pop, InstructionArgument.register_init(.Rbp), null));
        try self.instructions.push(Instruction.init(.Ret, null, null));
    }

    fn write_let(self: *Assembly, node: *Node) !void {
        if (node.next_expression()) |exp| {
            const name = for (node.childs.items) |child| {
                if (child.typ == .VariableName) break child.token.value.?;
            } else return error.VariableNameNotFound;

            const hash = util.hash(name);
            const location = Location {
                .register = .Rax,
            };

            try self.values.push(Value.init(hash, location));
            try self.instructions.push(
                Instruction.init(
                    .Move,
                    location,
                    InstructionArgument.literal_init(exp.token.value.?)
                )
            );
        }
    }

    fn write_node(self: *Assembly, node: *Node) !void {
        switch (node.typ) {
            .FunctionName => try self.write_function_name(node),
            .FunctionBody => try self.write_function_body(node),
            .Let => try self.write_let(node),
            .Return => try self.write_return(node),
            else => {}
        }
    }

    fn write_tree(self: *Assembly, tree: *Tree) !void {
        try self.instructions.push(Instruction.init(.Global, InstructionArgument.literal_init("_start"), null));
        var current_node: ?*Node = tree.root;

        while (current_node) |node| {
            try self.write_node(node);
            current_node = node.next();
        }
    }

    pub fn write_to_file(self: *const Assembly, path: []const u8, allocator: Allocator) !void {
        _ = path;

        var content = try Vec(u8).init(self.instructions.len() * 10, allocator);

        for (self.instructions.items) |instruction| {
            try instruction.write(&content);
        }

        std.debug.print("{s}\n", .{content.items});

        // const file = try std.fs.cwd().openFile(path, .{ .mode = .write_only });
        // defer file.close();

    }

    pub fn deinit(self: *const Assembly) void {
        self.instructions.deinit();
    }
};

pub fn init(tree: *Tree, allocator: Allocator) !Assembly {
    var source = try Assembly.init(allocator);
    try source.write_tree(tree);

    return source;
}
