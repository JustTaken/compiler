const util = @import("util");
const mem = @import("mem");
const collections = @import("collections");

const Parser = @import("parser.zig").Parser;

const DEFAULT_PATH: [:0]const u8 = "a.out";

pub fn main() !void {
    try util.time(start);
}

pub fn start() !void {
    const command_line = try util.CommandLineArgument.new();

    util.Logger.level = command_line.log;

    const input = command_line.input_path orelse return error.MissingInputFile;
    const output = command_line.output_path orelse DEFAULT_PATH;

    var arena = try mem.Arena.new("Main", 4);
    defer arena.deinit();

    try util.Logger.set_buffer(mem.PAGE_SIZE, &arena);
    defer util.Logger.deinit(&arena);

    var input_file = try collections.File.open(input);
    defer input_file.close();

    var parser = try Parser.new(input_file.stream(), &arena);
    defer parser.deinit();

    while (parser.next()) {}

    var output_file = try collections.File.create(output);
    defer output_file.close();

    parser.compile(output_file.stream());
}

test "main test" {
    _ = @import("parser.zig");
}
