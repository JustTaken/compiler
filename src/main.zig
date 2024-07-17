const std = @import("std");
const util =  @import("util.zig");
const tokenizer = @import("tokenizer.zig");
const parser = @import("parser.zig");
const generator = @import("generator.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}) {};
    const allocator = gpa.allocator();

    const content = try util.read_file("testing/test.lang", allocator);
    defer allocator.free(content);

    const tokens = try tokenizer.init(content, allocator);
    defer tokens.deinit();

    const root = try parser.init(tokens, allocator);
    defer allocator.destroy(root);
    defer root.deinit();

    try generator.init(root, allocator);
}
