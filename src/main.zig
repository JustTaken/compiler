const std = @import("std");
const util = @import("util");
const mem = @import("mem");
const collections = @import("collections");

const Parser = @import("parser.zig").Parser;
const Arena = mem.Arena;

const DEFAULT_PATH: [:0]const u8 = "a.out";

const CommandLineArgument = struct {
    log: util.Logger.Level,
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

        if (util.is_alpha(arg[0])) {
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

    fn new() Error!CommandLineArgument {
        var input_path: ?[]const u8 = null;
        var output_path: ?[]const u8 = null;
        var log: ?util.Logger.Level = null;

        var i: u32 = 1;
        while (i < std.os.argv.len) {
            const arg = util.convert(std.os.argv[i]);

            switch (try check_argument(arg)) {
                .Command => switch (try check_command(arg)) {
                    .Output => {
                        i += 1;
                        if (std.os.argv.len <= i) return Error.Argument;

                        const output_arg = util.convert(std.os.argv[i]);
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
                }
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

const Time = struct {
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
                .MicroSeconds => "mi",
                .NanoSeconds => "ns",
            };
        }
    };

    const MICRO_TO_NANO: usize = 1000;
    const MILI_TO_NANO: usize = 1000_000;
    const SECOND_TO_NANO: usize = 1000_000_000;

    fn from_nano(nano: usize) Time {
        if (nano < MICRO_TO_NANO) {
            return Time{
                .kind = .NanoSeconds,
                .value = nano,
            };
        }

        if (nano < MILI_TO_NANO) {
            return Time{
                .kind = .MicroSeconds,
                .value = nano / MICRO_TO_NANO,
            };
        }

        if (nano < SECOND_TO_NANO) {
            return Time{
                .kind = .MiliSeconds,
                .value = nano / MILI_TO_NANO,
            };
        }

        return Time{
            .kind = .Seconds,
            .value = nano / SECOND_TO_NANO,
        };
    }

    fn print(self: Time) void {
        util.print(.Info, "{}{}", .{ self.value, self.kind.as_string() });
    }
};

pub fn main() !void {
    const start = try std.time.Instant.now();
    const command_line = try CommandLineArgument.new();

    util.Logger.level = command_line.log;

    const input = command_line.input_path orelse @panic("Missing input file");
    const output = command_line.output_path orelse DEFAULT_PATH;

    var arena = try mem.Arena.new("Main", 4);
    defer arena.deinit();

    try util.Logger.set_buffer(mem.PAGE_SIZE, &arena);
    defer util.Logger.deinit(&arena);

    var input_file = try collections.File.open(input);
    defer input_file.close();

    var parser = try Parser.new(input_file.stream(), &arena);
    defer parser.deinit();

    while (parser.next()) {}

    var output_file = try collections.File.create(output);
    defer output_file.close();

    parser.compile(output_file.stream());

    const end = Time.from_nano((try std.time.Instant.now()).since(start));
    end.print();
}

test "main test" {
    _ = @import("parser.zig");
}
