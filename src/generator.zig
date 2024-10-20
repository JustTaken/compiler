const std = @import("std");
const util = @import("util/mod.zig");
const collections = @import("collections/mod.zig");
const allocator = @import("allocator/mod.zig");
const checker = @import("checker.zig");

const Arena = allocator.Arena;
const Range = util.Range;
const Index = util.Index;
const Vec = collections.Vec;
const RangeMap = collections.RangeMap;
const Constant = checker.Constant;
const TypeChecker = checker.TypeChecker;
const Procedure = checker.Procedure;
const Match = checker.Match;

pub const TempValueBuilder = struct {
    buffer: Vec(u8),
    next: u32,

    pub fn new(arena: *Arena) TempValueBuilder {
        return TempValueBuilder{
            .buffer = Vec(u8).new(1024, arena),
            .next = 0,
        };
    }

    pub fn get(self: *TempValueBuilder) []const u8 {
        const start = self.buffer.len;

        self.buffer.extend("temp");
        util.parse(@intCast(self.next), &self.buffer);
        self.inc();

        return self.buffer.content()[start..];
    }

    pub fn reset(self: *TempValueBuilder) void {
        self.next = 0;
        self.buffer.clear();
    }

    pub fn inc(self: *TempValueBuilder) void {
        self.next += 1;
    }
};

pub const Generator = struct {
    buffer: Vec(u8),
    temp_builder: TempValueBuilder,
    file: std.fs.File,

    pub fn new(path: []const u8, arena: *Arena) Generator {
        return Generator{
            .buffer = Vec(u8).new(1024, arena),
            .temp_builder = TempValueBuilder.new(arena),
            .file = std.fs.cwd().createFile(
                path,
                .{},
            ) catch unreachable,
        };
    }

    pub fn write_let(
        self: *Generator,
        name: []const u8,
        typ: []const u8,
        types: []const Range,
        constant: *const Constant,
        words: *Vec(u8),
    ) void {
        switch (constant.*) {
            .raw => {},
            .call => |call| {
                for (0..call.len) |i| {
                    const arg: *Constant = &call.arguments[i];

                    if (!arg.is_raw()) {
                        const arg_name = self.temp_builder.get();
                        const type_index = arg.get_type();

                        self.write_let(
                            arg_name,
                            words.range(types[type_index]),
                            types,
                            arg,
                            words,
                        );

                        arg.* = Constant{
                            .raw = .{
                                .value = .{
                                    .identifier = words.extend_range(arg_name),
                                },
                                .index = type_index,
                            },
                        };
                    }
                }

                self.buffer.mult_extend(&.{
                    "  %",
                    name,
                    " = call ",
                    typ,
                    " @",
                    words.range(call.range),
                    "(",
                });

                for (0..call.len) |i| {
                    if (i > 0) {
                        self.buffer.extend(", ");
                    }

                    const raw = call.arguments[i].raw;

                    self.buffer.mult_extend(
                        &.{
                            words.range(types[raw.index]),
                            if (raw.has_prefix()) " %" else " ",
                            raw.content(words),
                        },
                    );
                }

                self.buffer.extend(")\n");
            },
            .binary => |b| {
                var left_value: []const u8 = undefined;
                var right_value: []const u8 = undefined;
                var left_prefix = false;
                var right_prefix = false;

                if (b.left.is_raw()) {
                    left_value = b.left.raw.content(words);
                    left_prefix = b.left.raw.has_prefix();
                } else {
                    left_value = self.temp_builder.get();
                    left_prefix = true;
                    self.write_let(left_value, typ, types, b.left, words);
                }

                if (b.right.is_raw()) {
                    right_value = b.right.raw.content(words);
                    right_prefix = b.right.raw.has_prefix();
                } else {
                    right_value = self.temp_builder.get();
                    right_prefix = true;
                    self.write_let(right_value, typ, types, b.right, words);
                }

                self.buffer.mult_extend(&.{
                    "  %",
                    name,
                    " = ",
                    b.operator.to_string(),
                    " ",
                    typ,
                    if (left_prefix) " %" else " ",
                    left_value,
                    if (right_prefix) ", %" else ", ",
                    right_value,
                    "\n",
                });
            },
        }
    }

    pub fn write_match(
        self: *Generator,
        match: *const Match,
        words: *const Vec(u8),
    ) void {
        self.buffer.extend("writing match statement\n");
        _ = match;
        _ = words;
    }

    pub fn write_procedure(
        self: *Generator,
        name_range: Range,
        procedure: *const Procedure,
        types: []const Range,
        constant: ?Constant,
        words: *Vec(u8),
    ) void {
        if (constant) |c| {
            const type_name = words.range(types[c.get_type()]);
            var has_prefix = true;
            var temp_value: []const u8 = undefined;

            if (!c.is_raw()) {
                temp_value = self.temp_builder.get();
                self.write_let(temp_value, type_name, types, &c, words);
            } else {
                temp_value = c.raw.content(words);
                has_prefix = c.raw.has_prefix();
            }

            self.buffer.mult_extend(&.{
                "  ret ",
                type_name,
                if (has_prefix) " %" else " ",
                temp_value,
                "\n",
            });
        }

        self.buffer.extend("}\n");
        const start = self.buffer.len;

        self.buffer.mult_extend(&.{
            "define ",
            words.range(types[procedure.index]),
            " @",
            words.range(name_range),
            "(",
        });

        for (0..procedure.parameters.len) |i| {
            if (i > 0) {
                self.buffer.extend(", ");
            }

            self.buffer.extend(words.range(types[procedure.parameters.items[i]]));
        }

        self.buffer.extend(") {\nentry:\n");

        const content = self.buffer.content();

        _ = self.file.write(content[start..]) catch unreachable;
        _ = self.file.write(content[0..start]) catch unreachable;
    }

    pub fn clear(self: *Generator) void {
        self.buffer.clear();
        self.temp_builder.reset();
    }

    pub fn deinit(self: *const Generator) void {
        self.file.close();
    }
};
