const std = @import("std");
const allocator = @import("allocator.zig");

const Arena = allocator.Arena;
const Parser = @import("parser.zig").Parser;
const TypeChecker = @import("checker.zig").TypeChecker;

pub fn main() !void {
    const buffer = allocator.malloc(2);
    defer allocator.free(buffer);

    var arena = Arena.new(buffer);
    var args = std.process.args();

    const program_name = args.next().?;
    _ = program_name;

    var parser = Parser.new(args.next().?, &arena);
    defer parser.deinit();

    var checker = TypeChecker.new(&arena);

    _ = parser.next();
    _ = checker.check(&parser);
}
