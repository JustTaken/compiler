const std = @import("std");

const Arena = @import("collections.zig").Arena;
const Lexer = @import("lexer.zig").Lexer;
const Parser = @import("parser.zig").Parser;
const Generator = @import("generator.zig").Generator;

const ALLOCATION_LIMIT: u32 = 1024 * 24;

pub fn main() !void {
    var arena = Arena.malloc(ALLOCATION_LIMIT);
    var args = std.process.args();
    const program_name = args.next().?;

    var lexer = Lexer.init(&arena);
    defer lexer.deinit();

    var parser = Parser.init(&arena, &lexer);
    defer parser.deinit();

    var generator = Generator.init("zig-out/out.asm", &parser, &arena);

    if (args.next()) |arg| {
        lexer.set_path(arg);
        lexer.tokenize();
        parser.parse();
        generator.generate();
        generator.reset();
    } else {
        std.debug.print("usage:\n", .{});
        std.debug.print("{s} path\n", .{program_name});
    }

}
