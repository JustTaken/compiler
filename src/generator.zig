const std = @import("std");
const util = @import("util.zig");
const collections = @import("collections.zig");
const checker = @import("checker.zig");
const allocator = @import("allocator.zig");

const Arena = allocator.Arena;
const Range = util.Range;
const Index = util.Index;
const Vec = collections.Vec;
const RangeMap = collections.RangeMap;
const Parser = @import("parser.zig").Parser;
const Constant = checker.Constant;
const TypeChecker = checker.TypeChecker;
const Procedure = checker.Procedure;
const TempValueBuilder = checker.TempValueBuilder;

pub const Generator = struct {
    buffer: Vec(u8),
    file: std.fs.File,

    pub fn new(path: []const u8, arena: *Arena) Generator {
        return Generator{
            .buffer = Vec(u8).new(1024, arena),
            .file = std.fs.cwd().openFile(
                path,
                .{ .mode = .write_only },
            ) catch unreachable,
        };
    }

    pub fn write_let(
        self: *Generator,
        name: []const u8,
        typ: []const u8,
        constant: *const Constant,
        words: *const Vec(u8),
    ) void {
        switch (constant.*) {
            .raw => {},
            .binary => |b| {
                var left_value: []const u8 = undefined;
                var left_prefix = false;
                var right_value: []const u8 = undefined;
                var right_prefix = false;

                if (b.left.is_raw()) {
                    left_value = words.range(b.left.raw.range());
                    left_prefix = b.left.raw.is_identifier();
                } else {
                    left_value = "temp_var";
                    left_prefix = true;
                    self.write_let(left_value, typ, b.left, words);
                }

                if (b.right.is_raw()) {
                    right_value = words.range(b.right.raw.range());
                    right_prefix = b.right.raw.is_identifier();
                } else {
                    right_value = "temp_var";
                    right_prefix = true;
                    self.write_let(right_value, typ, b.right, words);
                }

                self.buffer.mult_extend(&.{
                    "    %",
                    name,
                    " = ",
                    b.operator.to_string(),
                    if (left_prefix) " %" else " ",
                    left_value,
                    if (right_prefix) ", %" else ", ",
                    right_value,
                    "\n",
                });
            },
        }
    }

    pub fn write_procedure(
        self: *Generator,
        name_range: Range,
        procedure: *const Procedure,
        types: []const Range,
        constant: ?Constant,
        words: *const Vec(u8),
    ) void {
        _ = constant;
        self.buffer.extend("}\n");

        const start = self.buffer.len;

        self.buffer.mult_extend(&.{
            "define ",
            words.range(types[procedure.return_type]),
            " @",
            words.range(name_range),
            "(",
        });

        for (procedure.parameters, 0..) |param, i| {
            if (i > 0) {
                self.buffer.extend(", ");
            }

            self.buffer.extend(words.range(types[param]));
        }

        self.buffer.extend(") {\n");

        const content = self.buffer.content();

        _ = self.file.write(content[start..]) catch unreachable;
        _ = self.file.write(content[0..start]) catch unreachable;
    }

    pub fn deinit(self: *const Generator) void {
        self.file.close();
    }
};
