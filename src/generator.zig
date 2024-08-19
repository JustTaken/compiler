const std = @import("std");
const util = @import("util.zig");

const Allocator = std.mem.Allocator;
const Vec = @import("collections.zig").Vec;
const FixedVec = @import("collections.zig").FixedVec;
const Node = @import("parser.zig").Node;

const Register = struct {
    name: []const u8,
    value: usize,
};

const Stack = struct {
    pointer: usize,
};

pub fn init(root: *Node, allocator: Allocator) !void {
    _ = root;
    _ = allocator;
}
