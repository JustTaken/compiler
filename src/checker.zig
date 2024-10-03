const collections = @import("collections.zig");
const value = @import("value.zig");
const allocator = @import("allocator.zig");
const util = @import("util.zig");

const Parser = @import("parser.zig").Parser;
const Arena = allocator.Arena;
const Vec = collections.Vec;
const RangeMap = collections.RangeMap;
const Iter = collections.Iter;
const Constant = value.Constant;
const Value = value.Value;
const Range = util.Range;
const Index = util.Index;

const BinaryConstant = struct {
    first: *Const,
    second: *Const,

    fn set_type(self: BinaryConstant, typ: Index) void {
        _ = self.first.set_type(typ);
        _ = self.second.set_type(typ);
    }
};

const AnonymousConstant = struct {
    typ: Index,
};

const ConstKind = enum { binary, anonymous, raw };
const Const = union(ConstKind) {
    binary: BinaryConstant,
    anonymous: AnonymousConstant,
    raw: *Constant,

    fn has_type(self: Const) bool {
        return switch (self) {
            .raw => |c| c.has_type(),
            .binary => |b| b.first.has_type(),
            .anonymous => true,
        };
    }

    fn get_type(self: Const) Index {
        return switch (self) {
            .raw => |c| c.typ,
            .binary => |b| b.first.get_type(),
            .anonymous => |a| a.typ,
        };
    }

    fn set_type(self: Const, typ: Index) bool {
        if (self.has_type()) return self.get_type() == typ;

        switch (self) {
            .raw => |c| c.set_type(typ),
            .binary => |*b| b.set_type(typ),
            .anonymous => unreachable,
        }

        return true;
    }

    fn new_raw(c: *Constant) Const {
        return Const{
            .raw = c,
        };
    }

    fn new_binary(first: *Const, second: *Const) Const {
        return Const{
            .binary = BinaryConstant{
                .first = first,
                .second = second,
            },
        };
    }

    fn new_anonymous(typ: Index) Const {
        return Const{
            .anonymous = AnonymousConstant{
                .typ = typ,
            },
        };
    }

    fn is_binary(self: Const) bool {
        return switch (self) {
            .binary => true,
            else => false,
        };
    }

    fn get_range(self: Const) Range {
        return switch (self) {
            .raw => |c| c.range,
            else => @panic("Constant does not have range"),
        };
    }

    fn is_variable(self: Const) bool {
        return switch (self) {
            .raw => |c| c.is_variable(),
            else => false,
        };
    }
};

const Procedure = struct {
    offset: Index,
    len: Index,
    return_type: Index,

    fn new(offset: Index, len: Index, return_type: Index) Procedure {
        return Procedure{
            .offset = offset,
            .len = len,
            .return_type = return_type,
        };
    }
};

pub const TypeChecker = struct {
    constants: Vec(Const),
    consts: Vec(Const),
    types: Vec(Range),
    parameters: Vec(Index),
    type_indices: RangeMap(Index),
    variables: RangeMap(Index),
    procedures: RangeMap(Procedure),
    last_parameter_offset: Index,

    pub fn new(arena: *Arena) TypeChecker {
        var self = TypeChecker{
            .constants = Vec(Const).new(10, arena),
            .consts = Vec(Const).new(64, arena),
            .types = Vec(Range).new(64, arena),
            .parameters = Vec(Index).new(64, arena),
            .variables = RangeMap(Index).new(64, arena),
            .type_indices = RangeMap(Index).new(64, arena),
            .procedures = RangeMap(Procedure).new(64, arena),
            .last_parameter_offset = 0,
        };

        self.types.push(Range.new(0, 0));

        return self;
    }

    fn get_type_index(
        self: *TypeChecker,
        type_range: Range,
        parser: *const Parser,
    ) Index {
        if (self.type_indices.get(type_range, parser.scanner.words)) |i| {
            return i.*;
        } else {
            const index: Index = @intCast(self.types.len);
            self.type_indices.push(type_range, index, parser.scanner.words);
            self.types.push(type_range);

            return index;
        }
    }

    fn register_call(
        self: *TypeChecker,
        parser: *const Parser,
        ranges: *Iter(Range),
    ) void {
        const name_range = ranges.next().?.*;
        const procedure = self.procedures.get(name_range, parser.scanner.words);

        if (procedure) |f| {
            for (0..f.len) |i| {
                const param_type_index = self.parameters.items[f.offset + f.len - i - 1];
                const constant = self.constants.pop();

                if (!constant.set_type(param_type_index)) {
                    const existing = constant.get_type();
                    const existing_cont = parser.scanner.words.range(
                        self.types.items[existing],
                    );
                    const actual_cont = parser.scanner.words.range(
                        self.types.items[param_type_index],
                    );

                    util.print(
                        "Procedure call expected type: \"{s}\", got: \"{s}\"\n",
                        .{ actual_cont, existing_cont },
                    );

                    @panic("");
                }
            }

            self.constants.push(Const.new_anonymous(f.return_type));
        } else {
            const cont = parser.scanner.words.range(name_range);
            util.print("Procedure \"{s}\" not found\n", .{cont});

            @panic("");
        }
    }

    fn register_binary_operation(self: *TypeChecker, parser: *const Parser) void {
        self.consts.push(self.constants.pop());
        const first = self.consts.last();

        self.consts.push(self.constants.pop());
        const second = self.consts.last();

        if (first.is_variable()) {
            if (self.variables.get(first.get_range(), parser.scanner.words)) |index| {
                _ = first.set_type(index.*);
            } else {
                @panic("Undefined variable");
            }
        }

        if (second.is_variable()) {
            if (self.variables.get(second.get_range(), parser.scanner.words)) |index| {
                _ = second.set_type(index.*);
            } else {
                @panic("Undefined variable");
            }
        }

        if (first.has_type()) {
            if (!second.set_type(first.get_type())) {
                @panic("Types do not match");
            }
        } else if (second.has_type()) {
            if (!first.set_type(second.get_type())) {
                @panic("Types do not match");
            }
        }

        self.constants.push(Const.new_binary(first, second));
    }

    pub fn check(self: *TypeChecker, parser: *const Parser) bool {
        var constants_iter = Iter(Constant).new(&parser.constants);
        var ranges = Iter(Range).new(&parser.ranges);

        for (parser.instructions.content()) |instruction| {
            switch (instruction) {
                .Constant => self.constants.push(
                    Const.new_raw(constants_iter.next().?),
                ),
                .Add, .Multiply => self.register_binary_operation(parser),
                .Call => self.register_call(parser, &ranges),
                .Parameter => {
                    const name_range = ranges.next().?.*;
                    const type_range = ranges.next().?.*;
                    const index = self.get_type_index(type_range, parser);

                    self.parameters.push(index);
                    self.variables.push(name_range, index, parser.scanner.words);
                },
                .Let => {
                    const name_range = ranges.next().?.*;
                    const type_range = ranges.next().?.*;
                    const index = self.get_type_index(type_range, parser);
                    const constant = self.constants.pop();

                    if (!constant.set_type(index)) {
                        const existing = constant.get_type();
                        const existing_cont = parser.scanner.words.range(
                            self.types.items[existing],
                        );
                        const actual_cont = parser.scanner.words.range(
                            self.types.items[index],
                        );
                        const variable_name = parser.scanner.words.range(name_range);

                        util.print(
                            "Expected type: \"{s}\", got: \"{s}\" for variable: \"{s}\", {} {}\n",
                            .{ actual_cont, existing_cont, variable_name, existing, index },
                        );

                        return false;
                    }

                    self.variables.push(name_range, index, parser.scanner.words);
                },
                .Procedure => {
                    const name_range = ranges.next().?.*;
                    const type_range = ranges.next().?.*;
                    const index = self.get_type_index(type_range, parser);
                    const parameter_len: Index = @intCast(
                        self.parameters.len - self.last_parameter_offset,
                    );

                    self.procedures.push(
                        name_range,
                        Procedure.new(self.last_parameter_offset, parameter_len, index),
                        parser.scanner.words,
                    );

                    if (self.constants.len > 0) {
                        const constant = self.constants.pop();

                        if (!constant.set_type(index)) {
                            const existing = constant.get_type();
                            const existing_cont = parser.scanner.words.range(
                                self.types.items[existing],
                            );
                            const actual_cont = parser.scanner.words.range(
                                self.types.items[index],
                            );

                            util.print(
                                "Procedure return type is: \"{s}\", got: \"{s}\"\n",
                                .{ actual_cont, existing_cont },
                            );

                            return false;
                        }
                    } else {
                        const type_name = parser.scanner.words.range(self.types.items[index]);
                        const procedure_name = parser.scanner.words.range(name_range);

                        if (!util.equal(u8, type_name, "void")) {
                            util.print(
                                "Return value of type \"{s}\" for \"{s}\" not found\n",
                                .{ type_name, procedure_name },
                            );

                            return false;
                        }
                    }

                    self.last_parameter_offset = @intCast(self.parameters.len);
                    self.variables.clear();
                },
                else => {},
            }
        }

        return !(ranges.has_next() and
            constants_iter.has_next() and
            self.constants.len != 0);
    }
};

test "Type check procedure variables" {
    var arena = Arena.new(allocator.malloc(2));
    var checker = TypeChecker.new(&arena);
    var parser = Parser.new("zig-out/type_check.lang", &arena);
    defer parser.deinit();

    while (parser.next()) {
        try util.assert(checker.check(&parser));

        for (parser.constants.content()) |constant| {
            const range = checker.types.items[constant.typ];
            const type_name = parser.scanner.words.range(range);

            try util.assert(util.equal(u8, type_name, "i32"));
        }
    }
}

test "Type check procedure call" {
    var arena = Arena.new(allocator.malloc(2));
    var checker = TypeChecker.new(&arena);
    var parser = Parser.new("zig-out/function_call.lang", &arena);
    defer parser.deinit();

    try util.assert(parser.next());
    try util.assert(checker.check(&parser));

    for (parser.constants.content()) |constant| {
        const range = checker.types.items[constant.typ];
        const type_name = parser.scanner.words.range(range);

        try util.assert(util.equal(u8, type_name, "i32"));
    }

    try util.assert(parser.next());
    try util.assert(checker.check(&parser));
}
