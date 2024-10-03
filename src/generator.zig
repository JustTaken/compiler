const std = @import("std");
const util = @import("util.zig");

const Arena = @import("collections.zig").Arena;
const Vec = @import("collections.zig").Vec;
const Node = @import("parser.zig").Root;
const Parser = @import("parser.zig").Parser;
const Lexer = @import("lexer.zig").Lexer;
const TokenId = @import("lexer.zig").TokenId;
const HashMap = @import("collections.zig").HashMap;

const Size: u32 = 1024 * 6;

pub const Variable = struct {
        typ: Vec(u8),
        offset: HashMap(u8),

        fn init(
                arena: *Arena,
        ) Variable {
                return Variable{
                        .typ = Vec(u8).init(64, arena),
                        .offset = HashMap(u8).init(64, arena),
                };
        }

        fn push(self: *Variable, name: []const u8, typ: u8, offset: u8) void {
                self.typ.push(typ);
                self.offset.push(name, offset);
        }
};

pub const Generator = struct {
        arena: Arena,
        stack: u8,
        variable: Variable,
        parser: *Parser,
        content: Vec(u8),
        file: std.fs.File,

        pub fn init(
                path: []const u8,
                parser: *Parser,
                allocator: *Arena,
        ) Generator {
                var arena = Arena.init(allocator.alloc(u8, Size)[0..Size]);

                return Generator{
                        .content = Vec(u8).init(4096, &arena),
                        .arena = arena,
                        .variable = Variable.init(&arena),
                        .stack = 0,
                        .file = std.fs.cwd().createFile(path, .{}) catch unreachable,
                        .parser = parser,
                };
        }

        pub fn push_variables(
                self: *Generator,
                starts: []const u16,
                types: []const u8,
        ) void {
                const offset = self.stack;

                for (0..starts.len) |i| {
                        const name = TokenId.identifier(
                                self.parser.lexer.content.offset(starts[i]),
                        );

                        self.variable.push(name, types[i], self.stack);
                        self.stack += self.parser.typ.size(self.parser, types[i]);
                }

                self.content.extend("       sub rbs ");
                util.parse(self.stack - offset, &self.content);
                self.content.push('\n');
        }

        pub fn generate(self: *Generator) void {
                self.content.extend("global _start\n");
                self.content.extend("section .text\n");
                self.content.extend("_start:\n");
                self.content.extend("       call main\n");
                self.content.extend("       mov rax, 60\n");
                self.content.extend("       mov rdi, 0\n");
                self.content.extend("       syscall\n");

                self.parser.evaluate(self);
        }

        pub fn reset(self: *Generator) void {
                // _ = self.file.write(self.content.offset(0)) catch
                //       @panic("Could not write to file");

                std.debug.print("{s}\n", .{self.content.offset(0)});

                self.content.clear();
                self.file.close();
        }
};
