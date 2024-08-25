const std = @import("std");
const Allocator = std.mem.Allocator;
const Vec = @import("collections.zig").Vec;

pub fn copy(T: type, src: []const T, dst: []T) void {
    @setRuntimeSafety(false);

    const len = src.len;

    for (0..len) |i| {
        dst[i] = src[i];
    }
}

pub fn eql(one: []const u8, two: []const u8) bool {
    if (one.len != two.len) return false;
    const len = min(one.len, two.len);

    for (0..len) |i| {
        if (one[i] != two[i]) return false;
    }

    return true;
}

pub fn is_number(string: []const u8) bool {
    for (string) |c| {
        if (c < '0' or c > '9') return false;
    }

    return true;
}

pub fn min(one: usize, two: usize) u32 {
    if (one < two) return @intCast(one);
    return @intCast(two);
}

pub fn read_file(path: []const u8, buffer: []u8) ![]const u8 {
    const file = try std.fs.cwd().openFile(path, .{});
    defer file.close();

    const end_pos = try file.getEndPos();

    if (buffer.len < end_pos) return error.BufferDoNotHaveEnougthSize;
    if (try file.read(buffer) != end_pos) return error.IncompleteContetent;

    buffer[end_pos] = '\n';

    return buffer[0 .. end_pos + 1];
}

pub fn stream(file: *std.fs.File, buffer: []u8) ![]const u8 {
    const len = try file.read(buffer);

    return buffer[0..len];
}

pub fn hash(string: []const u8) u32 {
    var h: usize = 0;
    for (string, 0..) |char, i| {
        h += char + i;
    }

    return @intCast(h);
}

pub fn parse(i: u32, out: *Vec(u8)) !void {
    var buffer: [20]u8 = undefined;

    var k: u32 = 0;
    var num = i;

    if (i == 0) {
        buffer[0] = '0';
        k += 1;
    }

    while (num > 0) {
        const rem: u8 = @intCast(num % 10);
        buffer[k] = rem + '0';

        k += 1;
        num /= 10;
    }

    for (0..k) |index| {
        out.push(buffer[k - index - 1]);
    }
}

pub fn to_word(u: usize) []const u8 {
    return switch (u) {
        1 => "byte",
        2 => "word",
        4 => "dword",
        8 => "qword",
        else => @panic("Should not happen"),
    };
}

pub fn register(u: usize) []const u8 {
    return switch (u) {
        1 => "al",
        2 => "ax",
        4 => "eax",
        8 => "rax",
        else => @panic("Should not happen"),
    };
}
