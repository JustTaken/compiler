const collections = @import("collections.zig");
const value = @import("value.zig");
const allocator = @import("allocator.zig");
const util = @import("util.zig");

const Arena = allocator.Arena;
const Vec = collections.Vec;
const RangeMap = collections.RangeMap;
const Iter = collections.Iter;
const Constant = value.Constant;
const Value = value.Value;
const Range = util.Range;
const Index = util.Index;
const Instruction = @import("parser.zig").Instruction;

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
        self.buffer.extend("%temp");
        util.parse(@intCast(self.next), &self.buffer);
        self.inc();

        return self.buffer.content()[start..];
    }

    pub fn reset(self: *TempValueBuilder) void {
        self.next = 0;
    }

    pub fn inc(self: *TempValueBuilder) void {
        self.next += 1;
    }
};

pub const Procedure = struct {
    offset: Index,
    len: Index,
    return_type: Index,

    pub fn new(offset: Index, len: Index, return_type: Index) Procedure {
        return Procedure{
            .offset = offset,
            .len = len,
            .return_type = return_type,
        };
    }
};

const BinaryOperation = enum(u8) {
    Add,
    Subtract,
    Multiply,

    fn from_instruction(instruction: Instruction) BinaryOperation {
        return switch (instruction) {
            .Add => .Add,
            .Subtract => .Subtract,
            .Multiply => .Multiply,
            else => @panic("should not happen"),
        };
    }

    fn to_string(self: BinaryOperation) []const u8 {
        return switch (self) {
            .Add => "sum",
            .Subtract => "sub",
            .Multiply => "mul",
        };
    }
};

const ConstantBinary = struct {
    first: Index,
    second: Index,
    op: BinaryOperation,

    fn set_type(
        self: ConstantBinary,
        typ: Index,
        consts: *const Vec(Const),
        raw_constants: [*]Constant,
    ) void {
        _ = consts.get(self.first).set_type(typ, consts, raw_constants);
        _ = consts.get(self.second).set_type(typ, consts, raw_constants);
    }
};

const ConstantCall = struct {
    typ: Index,
    len: Index,
    offset: Index,
};

const ConstKind = enum(u8) { binary, call, raw };
pub const Const = union(ConstKind) {
    binary: ConstantBinary,
    call: ConstantCall,
    raw: Index,

    fn has_type(self: Const, consts: *const Vec(Const), raw_constants: [*]Constant) bool {
        return switch (self) {
            .raw => raw_constants[self.raw].has_type(),
            .binary => consts.items[self.binary.first].has_type(consts, raw_constants),
            .call => true,
        };
    }

    fn get_type(
        self: Const,
        consts: *const Vec(Const),
        raw_constants: [*]Constant,
    ) Index {
        return switch (self) {
            .raw => raw_constants[self.raw].get_type(),
            .binary => consts.items[self.binary.first].get_type(consts, raw_constants),
            .call => self.call.typ,
        };
    }

    fn set_type(
        self: Const,
        typ: Index,
        consts: *const Vec(Const),
        raw_constants: [*]Constant,
    ) bool {
        if (self.has_type(consts, raw_constants))
            return self.get_type(consts, raw_constants) == typ;

        switch (self) {
            .raw => raw_constants[self.raw].set_type(typ),
            .binary => self.binary.set_type(typ, consts, raw_constants),
            .call => unreachable,
        }

        return true;
    }

    pub fn new_raw(c: Index) Const {
        return Const{
            .raw = c,
        };
    }

    fn new_binary(first: Index, second: Index, op: BinaryOperation) Const {
        return Const{
            .binary = ConstantBinary{
                .first = first,
                .second = second,
                .op = op,
            },
        };
    }

    fn new_call(typ: Index, len: Index, offset: Index) Const {
        return Const{
            .call = ConstantCall{
                .typ = typ,
                .len = len,
                .offset = offset,
            },
        };
    }

    fn is_binary(self: Const) bool {
        return switch (self) {
            .binary => true,
            else => false,
        };
    }

    fn get_range(self: Const, raw_constants: [*]Constant) Range {
        return switch (self) {
            .raw => raw_constants[self.raw].range,
            else => @panic("Constant does not have range"),
        };
    }

    fn is_variable(self: Const, raw_constants: [*]Constant) bool {
        return switch (self) {
            .raw => raw_constants[self.raw].is_variable(),
            else => false,
        };
    }

    fn is_raw(self: Const) bool {
        return switch (self) {
            .raw => true,
            else => false,
        };
    }

    pub fn write(
        self: Const,
        buffer: *Vec(u8),
        words: *const Vec(u8),
        temp_builder: *TempValueBuilder,
        consts: *const Vec(Const),
        types: *const Vec(Range),
        raw_constants: [*]Constant,
        name: []const u8,
    ) void {
        switch (self) {
            .raw => @panic("not allowed to bind variable to constant"),
            .binary => |b| {
                const first = consts.items[b.first];
                const second = consts.items[b.second];
                const type_name = words.range(
                    types.items[first.get_type(consts, raw_constants)],
                );

                var first_value: []const u8 = undefined;
                var second_value: []const u8 = undefined;

                if (!first.is_raw()) {
                    first_value = temp_builder.get();
                    first.write(
                        buffer,
                        words,
                        temp_builder,
                        consts,
                        types,
                        raw_constants,
                        first_value[1..],
                    );
                } else {
                    first_value = words.range(first.get_range(raw_constants));
                }

                if (!second.is_raw()) {
                    second_value = temp_builder.get();
                    second.write(
                        buffer,
                        words,
                        temp_builder,
                        consts,
                        types,
                        raw_constants,
                        second_value[1..],
                    );
                } else {
                    second_value = words.range(second.get_range(raw_constants));
                }

                buffer.extend("    %");
                buffer.extend(name);
                buffer.extend(" = ");
                buffer.extend(b.op.to_string());
                buffer.extend(" ");
                buffer.extend(type_name);
                buffer.extend(" ");
                buffer.extend(first_value);
                buffer.extend(", ");
                buffer.extend(second_value);
                buffer.extend("\n");
            },
            else => @panic("should not be here"),
        }
    }
};

pub const TypeChecker = struct {
    constants: Vec(Const),
    consts: Vec(Const),
    types: Vec(Range),
    type_indices: RangeMap(Index),
    error_content_buffer: Vec(u8),

    pub fn new(arena: *Arena) TypeChecker {
        // util.print("size of Const: {}\n", .{@sizeOf(Const)});
        // util.print("size of ConstantBinary: {}\n", .{@sizeOf(ConstantBinary)});
        // util.print("size of ConstantCall: {}\n", .{@sizeOf(ConstantCall)});
        // util.print("size of Constant: {}\n", .{@sizeOf(Constant)});
        // util.print("size of ConstKind: {}\n", .{@sizeOf(ConstKind)});

        var self = TypeChecker{
            .constants = Vec(Const).new(64, arena),
            .consts = Vec(Const).new(64, arena),
            .types = Vec(Range).new(64, arena),
            .type_indices = RangeMap(Index).new(64, arena),
            .error_content_buffer = Vec(u8).new(1024, arena),
        };

        self.types.push(Range.new(0, 0));

        return self;
    }

    pub fn write_constant(
        self: *const TypeChecker,
        constant: Const,
        buffer: *Vec(u8),
        words: *const Vec(u8),
        temp_builder: *TempValueBuilder,
        raw_constants: [*]Constant,
        name: []const u8,
    ) void {
        if (constant.is_raw()) {
            @panic("TODO: constant canno be raw righ now");
        }

        constant.write(
            buffer,
            words,
            temp_builder,
            &self.consts,
            &self.types,
            raw_constants,
            name,
        );
    }

    pub fn get_type_index(
        self: *TypeChecker,
        type_range: Range,
        words: *const Vec(u8),
    ) Index {
        if (self.type_indices.get(type_range, words)) |i| {
            return i.*;
        } else {
            const index: Index = @intCast(self.types.len);

            self.type_indices.push(type_range, index, words);
            self.types.push(type_range);

            return index;
        }
    }

    pub fn register_call(
        self: *TypeChecker,
        name_range: Range,
        parameters: *const Vec(Index),
        procedures: *const RangeMap(Procedure),
        constants: *Vec(Const),
        raw_constants: [*]Constant,
        words: *const Vec(u8),
    ) void {
        const procedure = procedures.get(name_range, words);

        if (procedure) |f| {
            for (0..f.len) |i| {
                const param_type_index = parameters.items[f.offset + f.len - i - 1];
                const constant = constants.get_back(i);

                if (!constant.set_type(param_type_index, &self.consts, raw_constants)) {
                    const existing = constant.get_type(&self.consts, raw_constants);
                    const existing_cont = words.range(
                        self.types.items[existing],
                    );
                    const actual_cont = words.range(
                        self.types.items[param_type_index],
                    );

                    self.error_content_buffer.extend("Procedure call expected type: \"");
                    self.error_content_buffer.extend(actual_cont);
                    self.error_content_buffer.extend("\", got: \"");
                    self.error_content_buffer.extend(existing_cont);
                    self.error_content_buffer.extend("\"\n");
                }
            }

            const len: Index = @intCast(constants.len);
            constants.push(Const.new_call(
                f.return_type,
                f.len,
                len - f.len,
            ));
        } else {
            const cont = words.range(name_range);

            self.error_content_buffer.extend("Procedure \"");
            self.error_content_buffer.extend(cont);
            self.error_content_buffer.extend("\" not found\n");
        }
    }

    pub fn register_binary_operation(
        self: *TypeChecker,
        variables: *const RangeMap(Index),
        constants: *Vec(Const),
        raw_constants: [*]Constant,
        words: *const Vec(u8),
        instruction: Instruction,
    ) void {
        const first_index: Index = @intCast(self.consts.len);
        self.consts.push(constants.pop());
        const first = self.consts.last();

        const second_index: Index = @intCast(self.consts.len);
        self.consts.push(constants.pop());
        const second = self.consts.last();

        if (first.is_variable(raw_constants)) {
            if (variables.get(first.get_range(raw_constants), words)) |index| {
                _ = first.set_type(index.*, &self.consts, raw_constants);
            } else {
                const name = words.range(first.get_range(raw_constants));

                self.error_content_buffer.extend("Undefined variable\"");
                self.error_content_buffer.extend(name);
                self.error_content_buffer.extend("\"\n");
            }
        }

        if (second.is_variable(raw_constants)) {
            if (variables.get(second.get_range(raw_constants), words)) |index| {
                _ = second.set_type(index.*, &self.consts, raw_constants);
            } else {
                const name = words.range(second.get_range(raw_constants));

                self.error_content_buffer.extend("Undefined variable\"");
                self.error_content_buffer.extend(name);
                self.error_content_buffer.extend("\"\n");
            }
        }

        if (first.has_type(&self.consts, raw_constants)) {
            if (!second.set_type(first.get_type(&self.consts, raw_constants), &self.consts, raw_constants)) {
                const first_type = self.types.items[first.get_type(&self.consts, raw_constants)];
                const second_type = self.types.items[second.get_type(&self.consts, raw_constants)];

                self.error_content_buffer.extend("Type \"");
                self.error_content_buffer.extend(words.range(first_type));
                self.error_content_buffer.extend("\" do not match with \"");
                self.error_content_buffer.extend(words.range(second_type));
                self.error_content_buffer.extend("\"\n");
            }
        } else if (second.has_type(&self.consts, raw_constants)) {
            if (!first.set_type(second.get_type(&self.consts, raw_constants), &self.consts, raw_constants)) {
                const first_type = self.types.items[first.get_type(&self.consts, raw_constants)];
                const second_type = self.types.items[second.get_type(&self.consts, raw_constants)];

                self.error_content_buffer.extend("Type \"");
                self.error_content_buffer.extend(words.range(first_type));
                self.error_content_buffer.extend("\" do not match with \"");
                self.error_content_buffer.extend(words.range(second_type));
                self.error_content_buffer.extend("\"\n");
            }
        }

        constants.push(Const.new_binary(
            first_index,
            second_index,
            BinaryOperation.from_instruction(instruction),
        ));
    }

    pub fn push_let(
        self: *TypeChecker,
        name_range: Range,
        type_range: Range,
        constants: *const Vec(Const),
        raw_constants: [*]Constant,
        words: *const Vec(u8),
    ) Index {
        const index = self.get_type_index(type_range, words);
        // const constant = self.constants.pop();

        if (!constants.last().set_type(index, &self.consts, raw_constants)) {
            const existing = constants.last().get_type(&self.consts, raw_constants);
            const existing_cont = words.range(
                self.types.items[existing],
            );
            const actual_cont = words.range(
                self.types.items[index],
            );
            const variable_name = words.range(name_range);

            self.error_content_buffer.extend("Expected type \"");
            self.error_content_buffer.extend(actual_cont);
            self.error_content_buffer.extend("\", got \"");
            self.error_content_buffer.extend(existing_cont);
            self.error_content_buffer.extend("\" for variable \"");
            self.error_content_buffer.extend(variable_name);
            self.error_content_buffer.extend("\"\n");

            // return false;
        }

        return index;
    }

    pub fn push_procedure(
        self: *TypeChecker,
        name_range: Range,
        type_range: Range,
        constants: *Vec(Const),
        raw_constants: [*]Constant,
        words: *const Vec(u8),
    ) Index {
        const index = self.get_type_index(type_range, words);

        if (constants.len > 0) {
            // const constant = self.constants.pop();

            if (!constants.last().set_type(index, &self.consts, raw_constants)) {
                const existing = constants.last().get_type(&self.consts, raw_constants);
                const existing_cont = words.range(
                    self.types.items[existing],
                );
                const actual_cont = words.range(
                    self.types.items[index],
                );

                self.error_content_buffer.extend("Procedure return type is \"");
                self.error_content_buffer.extend(actual_cont);
                self.error_content_buffer.extend("\", got \"");
                self.error_content_buffer.extend(existing_cont);
                self.error_content_buffer.extend("\"\n");

                // return false;
            }
        } else {
            const procedure_name = words.range(name_range);
            const type_name = words.range(
                self.types.items[index],
            );

            if (!util.equal(u8, type_name, "void")) {
                self.error_content_buffer.extend("Return value of type \"");
                self.error_content_buffer.extend(type_name);
                self.error_content_buffer.extend("\" missing for procedure \"");
                self.error_content_buffer.extend(procedure_name);
                self.error_content_buffer.extend("\"\n");

                // return false;
            }
        }

        return index;

        // self.variables.clear();
    }

    // pub fn check(self: *TypeChecker, parser: *const Parser) bool {
    //     // var constants_iter = Iter(Constant).new(&parser.constants);
    //     // var ranges = Iter(Range).new(&parser.ranges);

    //     for (parser.instructions.content()) |instruction| {
    //         switch (instruction) {
    //             // .Constant => ,
    //             // .Add, .Multiply => self.register_binary_operation(parser),
    //             // .Call => self.register_call(parser, &ranges),
    //             // .Parameter => ,
    //             // .Let => ,
    //             // .Procedure => ,
    //             else => {},
    //         }
    //     }

    //     return true;

    //     // return !(ranges.has_next() and
    //     // constants_iter.has_next() and
    //     // self.constants.len != 0);
    // }
};

// test "Type check procedure variables" {
//     var arena = Arena.new(allocator.malloc(3));
//     defer arena.deinit();

//     var checker = TypeChecker.new(&arena);
//     var parser = Parser.new("zig-out/function.lang", &arena);
//     defer parser.deinit();

//     while (parser.next()) {
//         if (!checker.check(&parser)) {
//             util.print("error: {s}", .{checker.error_content_buffer.content()});
//             try util.assert(false);
//         }

//         for (parser.constants.content()) |constant| {
//             const range = checker.types.items[constant.typ];
//             const type_name = words.range(range);

//             try util.assert(util.equal(u8, type_name, "i32"));
//         }
//     }
// }

// test "Type check procedure call" {
//     var arena = Arena.new(allocator.malloc(3));
//     defer arena.deinit();

//     var checker = TypeChecker.new(&arena);
//     var parser = Parser.new("zig-out/function_call.lang", &arena);
//     defer parser.deinit();

//     try util.assert(parser.next());
//     try util.assert(checker.check(&parser));

//     for (parser.constants.content()) |constant| {
//         const range = checker.types.items[constant.typ];
//         const type_name = words.range(range);

//         try util.assert(util.equal(u8, type_name, "i32"));
//     }

//     try util.assert(parser.next());
//     try util.assert(!checker.check(&parser));

//     try util.assert(
//         util.equal(
//             u8,
//             "Expected type \"u32\", got \"i32\" for variable \"x\"\n",
//             checker.error_content_buffer.content(),
//         ),
//     );
// }

// test "Type check procedure nested function call" {
//     var arena = Arena.new(allocator.malloc(3));
//     defer arena.deinit();

//     var checker = TypeChecker.new(&arena);
//     var parser = Parser.new("zig-out/nested_function_call.lang", &arena);
//     defer parser.deinit();

//     while (parser.next()) {
//         try util.assert(checker.check(&parser));

//         for (parser.constants.content()) |constant| {
//             const range = checker.types.items[constant.typ];
//             const type_name = words.range(range);

//             try util.assert(util.equal(u8, type_name, "i32"));
//         }
//     }
// }
