const std = @import("std");
const util = @import("util.zig");
const collections = @import("collections.zig");
const checker = @import("checker.zig");
const allocator = @import("allocator.zig");

const Arena = allocator.Arena;
const Range = util.Range;
const Index = util.Index;
const Vec = collections.Vec;
const Iter = collections.Iter;
const RangeMap = collections.RangeMap;
const Parser = @import("parser.zig").Parser;
const Constant = @import("value.zig").Constant;
const TypeChecker = checker.TypeChecker;
const Procedure = checker.Procedure;
const Const = checker.Const;
const TempValueBuilder = checker.TempValueBuilder;

pub const Generator = struct {
    parameters: Vec(Index),
    buffer: Vec(u8),
    constants: Vec(Const),
    aliases: RangeMap(Const),
    variables: RangeMap(Index),
    procedures: RangeMap(Procedure),
    type_checker: TypeChecker,
    parser: Parser,
    temp_builder: TempValueBuilder,
    file: std.fs.File,
    last_parameter_offset: Index,

    pub fn new(
        input_path: []const u8,
        output_path: []const u8,
        arena: *Arena,
    ) Generator {
        return Generator{
            .parameters = Vec(Index).new(10, arena),
            .buffer = Vec(u8).new(1024, arena),
            .constants = Vec(Const).new(10, arena),
            .variables = RangeMap(Index).new(10, arena),
            .aliases = RangeMap(Const).new(10, arena),
            .procedures = RangeMap(Procedure).new(10, arena),
            .type_checker = TypeChecker.new(arena),
            .parser = Parser.new(input_path, arena),
            .temp_builder = TempValueBuilder.new(arena),
            .file = std.fs.cwd().openFile(output_path, .{ .mode = .write_only }) catch unreachable,
            .last_parameter_offset = 0,
        };
    }

    fn write_variable(
        self: *Generator,
        var_name: []const u8,
        constant: Const,
    ) void {
        self.type_checker.write_constant(
            constant,
            &self.buffer,
            &self.parser.scanner.words,
            &self.temp_builder,
            self.parser.constants.items,
            var_name,
        );
    }

    pub fn compile(self: *Generator) void {
        while (self.parser.next()) {
            var const_iter = Iter(Constant).new(&self.parser.constants);
            var ranges = Iter(Range).new(&self.parser.ranges);
            const words: *const Vec(u8) = &self.parser.scanner.words;
            const constants = self.parser.constants.items;

            for (self.parser.instructions.content()) |instruction| {
                switch (instruction) {
                    .Constant => {
                        self.constants.push(Const.new_raw(@intCast(const_iter.index)));
                        const_iter.consume();
                    },
                    .Add, .Subtract, .Multiply => self.type_checker.register_binary_operation(
                        &self.variables,
                        &self.aliases,
                        &self.constants,
                        constants,
                        words,
                        instruction,
                    ),
                    .Call => {
                        const name_range = ranges.next().?.*;

                        self.type_checker.register_call(
                            name_range,
                            &self.parameters,
                            &self.procedures,
                            &self.constants,
                            constants,
                            words,
                        );
                    },
                    .Let => {
                        const name_range = ranges.next().?.*;
                        const type_range = ranges.next().?.*;
                        const index = self.type_checker.push_let(
                            name_range,
                            type_range,
                            &self.constants,
                            constants,
                            words,
                        );

                        const constant = self.constants.pop();
                        const var_name = words.range(name_range);

                        if (constant.is_raw()) {
                            const range = self.parser.scanner.words.extend_range(var_name,);

                            self.aliases.push(range, constant, &self.parser.scanner.words,);
                        } else {
                            self.write_variable(
                                var_name,
                                constant,
                            );

                            self.variables.push(name_range, index, words);
                        }
                    },
                    .Parameter => {
                        const name_range = ranges.next().?.*;
                        const type_range = ranges.next().?.*;
                        const index = self.type_checker.get_type_index(
                            type_range,
                            words,
                        );

                        self.parameters.push(index);
                        self.variables.push(
                            name_range,
                            index,
                            words,
                        );
                    },
                    .Procedure => {
                        const name_range = ranges.next().?.*;
                        const type_range = ranges.next().?.*;
                        const index = self.type_checker.push_procedure(
                            name_range,
                            type_range,
                            &self.constants,
                            constants,
                            words,
                        );

                        const parameter_len: Index = @intCast(
                            self.parameters.len - self.last_parameter_offset,
                        );

                        const procedure = Procedure.new(
                            self.last_parameter_offset,
                            parameter_len,
                            index,
                        );

                        self.procedures.push(
                            name_range,
                            procedure,
                            words,
                        );

                        self.buffer.extend("}\n");
                        const write_start = self.buffer.len;

                        self.buffer.extend("define ");
                        self.buffer.extend(words.range(type_range));
                        self.buffer.extend(" @");
                        self.buffer.extend(words.range(name_range));
                        self.buffer.extend("(");

                        for (0..procedure.len) |i| {
                            const param_type_range = self.type_checker.get_type_range(
                                self.parameters.items[procedure.offset + i],
                            );
                            self.buffer.extend(words.range(param_type_range));

                            if (i + 1 < procedure.len) {
                                self.buffer.extend(", ");
                            }
                        }

                        self.buffer.extend(") {\n");
                        self.buffer.extend("  entry:\n");

                        _ = self.file.write(
                            self.buffer.content()[write_start..],
                        ) catch unreachable;

                        _ = self.file.write(
                            self.buffer.content()[0..write_start],
                        ) catch unreachable;

                        self.buffer.clear();
                        self.variables.clear();
                        self.aliases.clear();
                        self.temp_builder.reset();

                        self.last_parameter_offset = @intCast(self.parameters.len);
                    },
                    else => {},
                }
            }
        }
    }

    pub fn deinit(self: *const Generator) void {
        self.file.close();
        self.parser.deinit();
    }
};

test "Code gen" {
    var arena = Arena.new(allocator.malloc(3));
    defer arena.deinit();

    var generator = Generator.new("zig-out/function.lang", "zig-out/a.out", &arena);
    defer generator.deinit();

    generator.compile();
}
