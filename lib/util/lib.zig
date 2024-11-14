const std = @import("std");
const collections = @import("collections");
const mem = @import("mem");

const String = collections.String;
const Iter = collections.Iter;
const Arena = mem.Arena;

pub const Formater = *const fn (comptime fmt: []const u8, args: anytype) void;
pub const Logger = struct {
    pub const Level = enum(usize) {
        None,
        Info,
        Debug,
    };

    pub var buffer: String = String{
        .items = undefined,
        .len = 0,
        .capacity = 0,
    };

    pub var level: Level = .None;

    pub fn set_buffer(size: u32, arena: *Arena) void {
        buffer = String.new(size, arena);
    }

    pub fn format(comptime fmt: []const u8, args: anytype) void {
        const ArgsType = @TypeOf(args);
        const args_type_info = @typeInfo(ArgsType);

        if (args_type_info != .Struct) {
            @compileError("expected tuple or struct argument, found " ++ @typeName(ArgsType));
        }

        comptime var iter = Iter(u8).new(fmt);
        comptime var arg_index: usize = 0;

        inline while (comptime iter.next()) |char| {
            if (char == '{') {
                if (comptime iter.next().? != '}') @panic("Do not support other formats for now");
                if (arg_index >= args_type_info.Struct.fields.len) @panic("wrong number of arguments");

                const field = args_type_info.Struct.fields[arg_index];
                const type_info = @typeInfo(field.type);

                switch (type_info) {
                    .Struct, .Union, .Enum => @field(args, field.name).print(format),
                    .ComptimeInt, .Int => parse_int(@intCast(@field(args, field.name))),
                    .Pointer => |p| {
                        const child_info = @typeInfo(p.child);
                        switch (child_info) {
                            .Array => {
                                if (!mem.equal(u8, @typeName(child_info.Array.child), @typeName(u8))) {
                                    @panic("Should not happen");
                                }
                                buffer.extend(@field(args, field.name));
                            },
                            .Int => |i| {
                                if (i.bits != 8) @panic("Should not happen");

                                buffer.extend(@field(args, field.name));
                            },
                            else => {
                                std.debug.print("{}\n", .{child_info});
                                @panic("Should not happen");
                            },
                        }
                    },
                    else => {
                        unreachable;
                    },
                }

                arg_index += 1;
            } else {
                buffer.push(char);
            }
        }
    }

    pub fn print(mode: Level, comptime fmt: []const u8, args: anytype) void {
        if (buffer.len >= buffer.capacity) {
            buffer.clear();

            return;
        }

        if (@intFromEnum(mode) <= @intFromEnum(level)) {
            format(fmt, args);

            const stderr = std.io.getStdErr().writer();
            buffer.push('\n');
            stderr.writeAll(buffer.offset(0)) catch @panic("Should not happen");

            buffer.clear();
        }
    }

    pub fn deinit(arena: *Arena) void {
        buffer.deinit(arena);
    }

    pub fn parse_int(i: isize) void {
        var b: [20]u8 = undefined;

        var k: usize = 0;
        const neg = i < 0;

        if (i == 0) {
            b[0] = '0';
            k += 1;
        }

        var num: u32 = if (neg) @intCast(-i) else @intCast(i);

        while (num > 0) {
            const rem: u8 = @intCast(num % 10);
            b[k] = rem + '0';

            k += 1;
            num /= 10;
        }

        if (neg) {
            b[k] = '-';
            k += 1;
        }

        for (0..k) |index| {
            buffer.push(b[k - index - 1]);
        }
    }
};

pub const print = Logger.print;

pub fn convert(input: [*:0]u8) []const u8 {
    var len: usize = 0;

    while (input[len] != 0) {
        len += 1;
    }

    return input[0..len];
}

pub fn is_number(string: []const u8) bool {
    for (string) |c| {
        if (!is_digit(c)) return false;
    }

    return true;
}

pub fn min(one: usize, two: usize) u32 {
    if (one < two) return @intCast(one);
    return @intCast(two);
}

pub fn max(one: usize, two: usize) u32 {
    if (one > two) return @intCast(one);
    return @intCast(two);
}

pub fn hash(string: []const u8) u32 {
    var h: usize = 0;
    for (string, 0..) |char, i| {
        h += char + i;
    }

    return @intCast(h);
}

pub fn parse(string: []const u8) usize {
    var number: usize = 0;
    var multiply: usize = 1;

    for (0..string.len) |i| {
        number += multiply * (string[string.len - i - 1] - '0');
        multiply *= 10;
    }

    return number;
}

pub fn is_digit(char: u8) bool {
    return char >= '0' and char <= '9';
}

pub fn is_alpha(char: u8) bool {
    return (char >= 'A' and char <= 'Z') or (char >= 'a' and char <= 'z') or char == '_';
}

pub fn is_ascci(char: u8) bool {
    return is_alpha(char) or is_digit(char);
}

pub fn assert(flag: bool) !void {
    if (!flag) {
        return error.AssertionFailed;
    }
}
