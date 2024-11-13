const std = @import("std");
const util = @import("util");
const mem = @import("mem");

const Parser = @import("parser.zig").Parser;
const Arena = mem.Arena;

const CommandLineArgument = struct {
    debug_mode: bool,
    input_path: ?[]const u8,
    output_path: ?[]const u8,

    const ArgumentKind = enum {
        Path,
        Command,
    };

    const Command = enum {
        OutputPath,
        DebugMode,
    };

    fn check_argument(arg: []const u8) error{InvalidArgument}!ArgumentKind {
        if (arg.len == 0) return error.InvalidArgument;
        if (arg[0] == '-') return ArgumentKind.Command;

        if (util.is_alpha(arg[0])) {
            return ArgumentKind.Path;
        } else {
            return error.InvalidArgument;
        }
    }

    fn check_command(arg: []const u8) error{InvalidCommand}!Command {
        if (arg.len == 1) {
            return error.InvalidCommand;
        }

        if (mem.equal(u8, arg[1..], "o")) {
            return Command.OutputPath;
        } else if (mem.equal(u8, arg[1..], "debug")) {
            return Command.DebugMode;
        } else {
            return error.InvalidCommand;
        }
    }

    fn new() error{ InvalidArgument, InvalidCommand }!CommandLineArgument {
        var input_path: ?[]const u8 = null;
        var output_path: ?[]const u8 = null;
        var debug_mode = false;

        var i: u32 = 1;
        while (i < std.os.argv.len) {
            const arg = util.convert(std.os.argv[i]);

            switch (try check_argument(arg)) {
                .Command => switch (try check_command(arg)) {
                    .OutputPath => {
                        i += 1;
                        if (std.os.argv.len <= i) return error.InvalidArgument;

                        const output_arg = util.convert(std.os.argv[i]);
                        if (try check_argument(output_arg) != ArgumentKind.Path) return error.InvalidArgument;

                        output_path = output_arg;
                    },
                    .DebugMode => debug_mode = true,
                },
                .Path => input_path = arg,
            }

            i += 1;
        }

        return CommandLineArgument{
            .input_path = input_path,
            .output_path = output_path,
            .debug_mode = debug_mode,
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
        util.print(.Info, "{d}{s}\n", .{ self.value, self.kind.as_string() });
    }
};

pub fn main() !void {
    const start = try std.time.Instant.now();
    const command_line = try CommandLineArgument.new();

    util.debug_mode = command_line.debug_mode;

    const input = command_line.input_path orelse @panic("Missing input file");
    const output = command_line.output_path orelse @panic("Missing output file");

    var arena = mem.Arena.new("Main", 3);
    defer arena.deinit();

    var parser = Parser.new(input, output, &arena);
    defer parser.deinit();

    while (parser.next()) {}

    parser.compile();

    const end = Time.from_nano((try std.time.Instant.now()).since(start));
    end.print();
}

test "main test" {
    _ = @import("parser.zig");
}
