const std = @import("std");
const util = @import("util.zig");

const Arena = @import("collections.zig").Arena;
const Vec = @import("collections.zig").Vec;
const Node = @import("parser.zig").Root;
const Parser = @import("parser.zig").Parser;
const Function = @import("parser.zig").Function;
const Lexer = @import("lexer.zig").Lexer;

const RegisterName = enum {
    Rax,
    Rbx,
    Rcx,
};

const Register = struct {
    value: usize,
};

const Stack = struct {
    pointer: usize,
};

const Size: u32 = 1024 * 4;

pub const Generator = struct {
    arena: Arena,
    stack: Stack,
    registers: [@typeInfo(RegisterName).Enum.fields.len]Register,
    content: Vec(u8),

    pub fn init(allocator: *Arena) Generator {
        var arena = Arena.init(allocator.alloc(u8, Size)[0..Size]);
        return Generator{
            .content = Vec(u8).init(4096, &arena),
            .arena = arena,
            .stack = Stack{ .pointer = 0 },
            .registers = .{Register{ .value = 0 }} ** 3,
        };
    }

    pub fn parse(self: *Generator, parser: *Parser, lexer: *Lexer) void {
        for (0..parser.function.offset(0).len) |i| {
            Function.generate(@intCast(i), parser, lexer, &self.content);
        }
    }

    pub fn reset(self: *Generator) void {
        std.debug.print("{s}\n", .{self.content.offset(0)});
        self.content.clear();
    }

};
