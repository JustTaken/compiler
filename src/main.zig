const std = @import("std");
const util = @import("util.zig");
const Arena = @import("collections.zig").Arena;
const Lexer = @import("lexer.zig").Lexer;
const Parser = @import("parser.zig").Parser;
const generator = @import("generator.zig");

const ALLOCATION_LIMIT = 1024 * 16;

pub fn main() !void {
    const buffer: [*]u8 = @ptrCast(std.c.malloc(ALLOCATION_LIMIT) orelse
        return error.AllocationFail);
    defer std.c.free(buffer);

    var arena = Arena.init(buffer[0..ALLOCATION_LIMIT]);
    var args = std.process.args();
    _ = args.next();

    var lexer = Lexer.init(&arena);
    defer lexer.deinit();

    var parser = Parser.init(&arena);
    defer parser.reset();

    while (args.next()) |arg| {
        lexer.set_path(arg);
        lexer.tokenize();
        parser.parse(&lexer);
    }
}
