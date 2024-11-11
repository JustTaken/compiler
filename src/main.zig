const std = @import("std");
const util = @import("util");
const mem = @import("mem");

const Parser = @import("parser.zig").Parser;
const Arena = mem.Arena;

pub fn main() !void {
    if (std.os.argv.len != 3) {
        return error.WrongArugumentCount;
    }

    const start = try std.time.Instant.now();

    const input = std.os.argv[1];
    const output = std.os.argv[2];

    const buffer = mem.malloc(2);
    defer mem.free(buffer);

    var arena = mem.Arena.new(buffer);
    var parser = Parser.new(util.convert(input), util.convert(output), &arena);

    while (parser.next()) {}

    parser.deinit();
    const end = try std.time.Instant.now();

    util.print("Time elapsed: {}\n", .{end.since(start)});
}

test "main test" {
    std.testing.refAllDecls(@This());
}
