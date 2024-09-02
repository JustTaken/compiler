const std = @import("std");
const util = @import("util.zig");

const Arena = @import("collections.zig").Arena;
const Vec = @import("collections.zig").Vec;
const Node = @import("parser.zig").Root;
const Parser = @import("parser.zig").Parser;
const Lexer = @import("lexer.zig").Lexer;
const TokenId = @import("lexer.zig").TokenId;

const Size: u32 = 1024 * 4;

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

pub const Variable = struct {
    typ: u8,

    fn init(
        start: u16,
        stack_pointer: u16,
        typ: u8,
        parser: *Parser,
        lexer: *Lexer,
    ) void {
        const name = TokenId.identifier(lexer.content.offset(start));

        parser.variable.push(name, .{ .typ = typ });
        parser.variable_pointer.push(stack_pointer);
    }

    pub fn is_zero(_: *const Variable) bool {
        return true;
    }
};

pub const Generator = struct {
    arena: Arena,
    stack: Stack,
    registers: [@typeInfo(RegisterName).Enum.fields.len]Register,
    parser: *Parser,
    content: Vec(u8),
    file: std.fs.File,

    pub fn init(path: []const u8, parser: *Parser, allocator: *Arena) Generator {
        var arena = Arena.init(allocator.alloc(u8, Size)[0..Size]);

        return Generator{
            .content = Vec(u8).init(4096, &arena),
            .arena = arena,
            .stack = Stack{ .pointer = 0 },
            .registers = .{Register{ .value = 0 }} ** 3,
            .file = std.fs.cwd().createFile(path, .{}) catch unreachable,
            .parser = parser,
        };
    }

    pub fn generate(self: *Generator) void {
        // self.content.extend("global _start\n");
        // self.content.extend("section .text\n");
        // self.content.extend("_start:\n");
        // self.content.extend("    call main\n");
        // self.content.extend("    mov rax, 60\n");
        // self.content.extend("    mov rdi, 0\n");
        // self.content.extend("    syscall\n");

        self.parser.evaluate(self);
    }

    pub fn reset(self: *Generator) void {
        _ = self.file.write(self.content.offset(0)) catch
            @panic("Could not write to file");

        std.debug.print("{s}\n", .{self.content.offset(0)});

        self.content.clear();
        self.file.close();
    }
};
