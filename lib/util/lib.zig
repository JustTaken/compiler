const std = @import("std");
const collections = @import("collections");
const mem = @import("mem");

pub const Range = struct {
    start: u32,
    end: u32,

    pub fn new(start: u32, end: u32) Range {
        return Range{
            .start = start,
            .end = end,
        };
    }

    pub fn reset(self: *Range) void {
        self.start = 0;
        self.end = 0;
    }
};

pub const Formater = *const fn (comptime fmt: []const u8, args: anytype) error{Overflow}!void;
pub const Logger = struct {
    pub const Level = enum(usize) {
        None,
        Info,
        Debug,
    };

    var normal_buffer = collections.String{
        .items = undefined,
        .len = 0,
        .capacity = 0,
    };

    const BACKUP_BUFFER_LEN: usize = 1024;
    var backup_array: [BACKUP_BUFFER_LEN]u8 = undefined;
    var backup_buffer = collections.String.from_buffer(&backup_array);

    var buffer: collections.String = undefined;

    pub var level: Level = .None;

    pub fn set_buffer(size: u32, arena: *mem.Arena) error{OutOfMemory}!void {
        normal_buffer = try collections.String.new(size, arena);
    }

    pub fn format(comptime fmt: []const u8, args: anytype) error{Overflow}!void {
        const ArgsType = @TypeOf(args);
        const args_type_info = @typeInfo(ArgsType);

        if (args_type_info != .@"struct") {
            @compileError("expected tuple or struct argument, found " ++ @typeName(ArgsType));
        }

        comptime var iter = collections.ComptimeIter(u8).new(fmt);
        comptime var arg_index: usize = 0;

        inline while (comptime iter.next()) |char| {
            if (char == '{') {
                if (comptime iter.next().? != '}') @panic("TODO: support other formats");
                if (arg_index >= args_type_info.@"struct".fields.len) {
                    buffer.extend("\"ERROR: NO MORE ARGUMENTS\"") catch return error.Overflow;

                    continue;
                }

                const field = args_type_info.@"struct".fields[arg_index];
                const type_info = @typeInfo(field.type);

                switch (type_info) {
                    .@"struct", .@"union", .@"enum"=> try @field(args, field.name).print(format),
                    .comptime_int, .int => parse_int(@intCast(@field(args, field.name))),
                    .pointer => |p| {
                        const child_info = @typeInfo(p.child);
                        switch (child_info) {
                            .array => {
                                if (!mem.equal(u8, @typeName(child_info.array.child), @typeName(u8))) @panic("TODO");

                                buffer.extend(@field(args, field.name)) catch return error.Overflow;
                            },
                            .int => |i| {
                                if (i.bits != 8) @panic("TODO");

                                buffer.extend(@field(args, field.name)) catch return error.Overflow;
                            },
                            else => @panic("TODO"),
                        }
                    },
                    else => {
                        std.debug.print("{}\n", .{type_info});
                        @panic("TODO");
                    },
                }

                arg_index += 1;
            } else {
                buffer.push(char) catch return error.Overflow;
            }
        }
    }

    pub fn printf(mode: Level, comptime fmt: []const u8, args: anytype) void {
        if (@intFromEnum(mode) > @intFromEnum(level)) return;

        buffer = if (normal_buffer.capacity == 0) backup_buffer else normal_buffer;

        if (format(fmt, args)) {
            buffer.push('\n') catch overflow();
        } else |_| {
            overflow();
        }

        const stderr = std.io.getStdErr().writer();
        stderr.writeAll(buffer.offset(0) catch unreachable) catch @panic("TODO: Failed to write to 'stderr', bypass that");

        buffer.clear();
    }

    fn overflow() void {
        assert(buffer.capacity < 1024);

        const message = "BUFFER OVERFLOW!! START OF MESSAGE: {";
        const overflow_len_preview: usize = 50;

        buffer.set_len(overflow_len_preview) catch unreachable;
        buffer.shift_right(0, message.len) catch unreachable;
        buffer.set_len(0) catch unreachable;
        buffer.extend(message) catch unreachable;
        buffer.set_len(overflow_len_preview + message.len) catch unreachable;
        buffer.extend("}\n") catch unreachable;
    }

    pub fn deinit(arena: *mem.Arena) void {
        normal_buffer.deinit(arena);
    }

    pub fn parse_int(i: isize) void {
        var b: [20]u8 = undefined;

        var k: usize = 0;
        const neg = i < 0;

        if (i == 0) {
            b[0] = '0';
            k += 1;
        }

        var num: usize = if (neg) @intCast(-i) else @intCast(i);

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
            buffer.push(b[k - index - 1]) catch overflow();
        }
    }
};

pub const CommandLineArgument = struct {
    log: Logger.Level,
    input_path: ?[]const u8,
    output_path: ?[]const u8,

    const ArgumentKind = enum {
        Path,
        Command,
    };

    const Command = enum {
        Output,
        Debug,
        Info,
    };

    const Error = error{
        Argument,
        Command,
        DebugFlagCombination,
        MoreThanOneInputPath,
        MoreThanOneOutputPath,
    };

    fn check_argument(arg: []const u8) error{Argument}!ArgumentKind {
        if (arg.len == 0) return error.Argument;
        if (arg[0] == '-') return ArgumentKind.Command;

        if (is_alpha(arg[0])) {
            return ArgumentKind.Path;
        } else {
            return error.Argument;
        }
    }

    fn check_command(arg: []const u8) error{Command}!Command {
        if (arg.len == 1) {
            return error.Command;
        }

        if (mem.equal(u8, arg[1..], "o")) {
            return Command.Output;
        } else if (mem.equal(u8, arg[1..], "debug")) {
            return Command.Debug;
        } else if (mem.equal(u8, arg[1..], "info")) {
            return Command.Info;
        } else {
            return error.Command;
        }
    }

    pub fn new() Error!CommandLineArgument {
        var input_path: ?[]const u8 = null;
        var output_path: ?[]const u8 = null;
        var log: ?Logger.Level = null;

        var i: u32 = 1;
        while (i < std.os.argv.len) {
            const arg = convert(std.os.argv[i]);

            switch (try check_argument(arg)) {
                .Command => switch (try check_command(arg)) {
                    .Output => {
                        i += 1;
                        if (std.os.argv.len <= i) return Error.Argument;

                        const output_arg = convert(std.os.argv[i]);
                        if (try check_argument(output_arg) != ArgumentKind.Path) return Error.Argument;
                        if (output_path) |_| return Error.MoreThanOneOutputPath;

                        output_path = output_arg;
                    },
                    .Debug => {
                        if (log) |_| return Error.DebugFlagCombination else log = .Debug;
                    },
                    .Info => {
                        if (log) |_| return Error.DebugFlagCombination else log = .Info;
                    },
                },
                .Path => {
                    if (input_path) |_| return Error.MoreThanOneInputPath;
                    input_path = arg;
                },
            }

            i += 1;
        }

        return CommandLineArgument{
            .input_path = input_path,
            .output_path = output_path,
            .log = log orelse .None,
        };
    }
};

pub const DeltaTime = struct {
    kind: Kind,
    value: usize,

    const Kind = enum {
        Seconds,
        MiliSeconds,
        MicroSeconds,
        NanoSeconds,

        fn as_string(self: Kind) []const u8 {
            return switch (self) {
                .Seconds => "s",
                .MiliSeconds => "ms",
                .MicroSeconds => "mu",
                .NanoSeconds => "ns",
            };
        }
    };

    const MICRO_TO_NANO: usize = 1000;
    const MILI_TO_NANO: usize = 1000_000;
    const SECOND_TO_NANO: usize = 1000_000_000;

    pub fn from_nano(nano: usize) DeltaTime {
        if (nano < MICRO_TO_NANO) {
            return DeltaTime{
                .kind = .NanoSeconds,
                .value = nano,
            };
        }

        if (nano < MILI_TO_NANO) {
            return DeltaTime{
                .kind = .MicroSeconds,
                .value = nano / MICRO_TO_NANO,
            };
        }

        if (nano < SECOND_TO_NANO) {
            return DeltaTime{
                .kind = .MiliSeconds,
                .value = nano / MILI_TO_NANO,
            };
        }

        return DeltaTime{
            .kind = .Seconds,
            .value = nano / SECOND_TO_NANO,
        };
    }

    pub fn show(self: DeltaTime) void {
        print(.Info, "{}{}", .{ self.value, self.kind.as_string() });
    }
};

pub const print = Logger.printf;

pub fn time(f: fn () anyerror!void) !void {
    const start = try std.time.Instant.now();
    try f();
    DeltaTime.from_nano((try std.time.Instant.now()).since(start)).show();
}

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

pub fn assert(flag: bool) void {
    if (!flag) {
        unreachable;
    }
}
