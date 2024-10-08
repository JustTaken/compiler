const std = @import("std");
const allocator = @import("allocator/mod.zig");

const Arena = allocator.Arena;
const Parser = @import("parser.zig").Parser;

pub fn main() void {
    var arena = Arena.new(allocator.malloc(3));
    defer arena.deinit();

    var args = std.process.args();

    if (std.os.argv.len != 3) {
        @panic("usage: pass {input path} and {output path}");
    }

    const program_name = args.next().?;
    _ = program_name;

    const input_path = args.next().?;
    const output_path = args.next().?;

    var parser = Parser.new(input_path, output_path, &arena);
    defer parser.deinit();

    while (parser.next()) {
    }
}

test "All" {
    std.testing.refAllDecls(@This());
}
