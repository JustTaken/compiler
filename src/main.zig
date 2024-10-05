const std = @import("std");
const allocator = @import("allocator.zig");
const util = @import("util.zig");

const Arena = allocator.Arena;
const Generator = @import("generator.zig").Generator;

pub fn main() void {
    // const start = try std.time.Instant.now();
    var arena = Arena.new(allocator.malloc(2));
    defer arena.deinit();

    var args = std.process.args();

    if (std.os.argv.len < 2) {
        return;
    }

    const program_name = args.next().?;
    _ = program_name;

    const default_output_path = "zig-out/a.out";
    const output_path = if (std.os.argv.len > 2) args.next().? else default_output_path;

    var generator = Generator.new(args.next().?, output_path, &arena);
    defer generator.deinit();

    generator.compile();

}

test "All" {
    std.testing.refAllDecls(@This());
}
