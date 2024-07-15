const std = @import("std");
const Allocator = std.mem.Allocator;
const Vec = @import("collections.zig").Vec;
const FixedVec = @import("collections.zig").FixedVec;
const Node = @import("parser.zig").Node;
const Tree = @import("parser.zig").Tree;

const InstructionType = enum {
    Move,
    Syscall,
    FunctionDeclaration,
    Global,
    Pop,
    Ret,
};

// const Value = union {
//     Register,
//     Literal,
//     Variable,
// };

const InstructionArg = struct {
    argument: []const u8,

    fn init(argument: []const u8) InstructionArg {
        return InstructionArg {
            .argument = argument,
        };
    }

    fn string(self: *const InstructionArg) []const u8 {
        return self.arguments;
    }
};

const Instruction = struct {
    typ: InstructionType,
    arg1: InstructionArg,
    arg2: InstructionArg,

    fn string(self: *const Instruction) []const u8 {
        return switch (self.typ) {
            .Move    => "    mov",
            .Syscall => "    syscall",
            .Global  => "global",
            .Pop     => "    pop",
            .Ret     => "    ret",
            .FunctionDeclaration => "",
        };
    }

    fn init(typ: InstructionType, string1: ?[]const u8, string2: ?[]const u8) Instruction {
        const arg1 = if (string1) |str| InstructionArg.init(str) else InstructionArg.init("");
        const arg2 = if (string2) |str| InstructionArg.init(str) else InstructionArg.init("");

        return .{
            .typ = typ,
            .arg1 = arg1,
            .arg2 = arg2,
        };
    }

    fn write_string(self: *const Instruction, buffer: Vec(u8)) !void {
        try buffer.extend_with(self.string());

        if (self.arg1) |arg| {
            try buffer.extend_with(arg.string());
        }

        if (self.arg1) |arg| {
            try buffer.extend_with(arg.string());
        }
    }
};

const Assembly = struct {
    instructions: Vec(Instruction),
    string: FixedVec(u8, 50),

    fn init(allocator: Allocator) !Assembly {
        return Assembly {
            .instructions = try Vec(Instruction).init(20, allocator),
            .string = FixedVec(u8, 50).init(),
        };
    }

    fn write_function(self: *Assembly, node: *const Node) !void {
        const first = try node.childs.get(0);
        try self.instructions.push(Instruction.init(.FunctionDeclaration, first.token.value, ":"));
        try self.instructions.push(Instruction.init(.Move, " rpb", " rps"));
        try self.instructions.push(Instruction.init(.Pop, " rbp", null));
        try self.instructions.push(Instruction.init(.Ret, null, null));
    }

    fn write_let(self: *Assembly, node: *const Node) !void {
        const value = node.get_literal() orelse return error.InvalidAssembly;

        try self.instructions.push(Instruction.init(.Move, " rax ", value));
    }

    fn write_node(self: *Assembly, node: *const Node) !void {
        switch (node.typ) {
            .Function => try self.write_function(node),
            else => {}
        }
    }

    fn write_tree(self: *Assembly, tree: *Tree) !void {
        try self.instructions.push(Instruction.init(.Global, " _start", null));
        var current_node: ?*Node = tree.root;

        while (current_node) |node| {
            try self.write_node(node);
            current_node = node.next();
        }
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
