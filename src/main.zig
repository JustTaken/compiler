const std = @import("std");
const util =  @import("util.zig");
const tokenizer = @import("tokenizer.zig");
const Tree = @import("parser.zig").Tree;
const generator = @import("generator.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}) {};
    const allocator = gpa.allocator();

    const content = try util.read_file("testing/test.lang", allocator);
    defer allocator.free(content);

    const tokens = try tokenizer.init(content, allocator);
    defer tokens.deinit();

    var tree = try Tree.init(tokens, allocator);
    defer tree.deinit();

    const assembly = try generator.init(&tree, allocator);
    defer assembly.deinit();
}
