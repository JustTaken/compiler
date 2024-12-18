const std = @import("std");
const util = @import("util");
const mem = @import("mem");
const collections = @import("collections");

const Parser = @import("parser.zig").Parser;
const Lexer = @import("lexer/mod.zig").Lexer;
const Checker = @import("checker/mod.zig").Checker;
const Generator = @import("generator/mod.zig").Generator;

const DEFAULT_PATH: [:0]const u8 = "a.out";

pub fn main() !void {
    try util.time(start);
}

pub fn start() !void {
    util.tracy.setThreadName("Main");
    defer util.tracy.message("Shutdown");

    std.time.sleep(1000_000_000);

    const zone = util.tracy.initZone(@src(), .{.name = "Main"});
    defer zone.deinit();

    const command_line = try util.CommandLineArgument.new();
    const input = command_line.input_path orelse return error.MissingInputFile;
    const output = command_line.output_path orelse DEFAULT_PATH;

    util.Logger.level = command_line.log;

    var arena = try mem.Arena.new("Main", 5);
    defer arena.deinit();

    try util.Logger.set_buffer(mem.PAGE_SIZE, &arena);
    defer util.Logger.deinit(&arena);

    var input_file = try collections.File.open(input);
    defer input_file.close();

    var lexer = try Lexer.new(input_file.stream(), &arena);
    defer lexer.deinit();

    var parser = try Parser.new(&arena);
    defer parser.deinit();

    var checker = try Checker.new(&arena);
    defer checker.deinit();

    var generator = try Generator.new(&arena);
    defer generator.deinit();

    while (parser.next(&lexer)) |tree| {
        checker.next(tree, lexer.words);
    }

    var output_file = try collections.File.create(output);
    defer output_file.close();
}

test "main test" {
    _ = @import("parser.zig");
}
