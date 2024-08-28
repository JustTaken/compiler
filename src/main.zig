const std = @import("std");
const util = @import("util.zig");
const Arena = @import("collections.zig").Arena;
const Lexer = @import("lexer.zig").Lexer;
const Parser = @import("parser.zig").Parser;
const Generator = @import("generator.zig").Generator;

const ALLOCATION_LIMIT: u32 = 1024 * 24;

pub fn main() !void {
    var arena = Arena.malloc(ALLOCATION_LIMIT);
    var args = std.process.args();
    _ = args.next();

    var lexer = Lexer.init(&arena);
    defer lexer.deinit();

    var parser = Parser.init(&arena, &lexer);
    defer parser.deinit();

    var generator = Generator.init("zig-out/out.asm", &arena);

    while (args.next()) |arg| {
        lexer.set_path(arg);
        lexer.tokenize();
        parser.parse();
        generator.parse(&parser, &lexer);
        generator.reset();
    }
}
