const std = @import("std");
const Allocator = std.mem.Allocator;

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

pub fn min(one: usize, two: usize) u32 {
    if (one < two) return @intCast(one);
    return @intCast(two);
}

pub fn read_file(path: []const u8, allocator: Allocator) ![]const u8 {
    const file = try std.fs.cwd().openFile(path, .{});
    defer file.close();

    const end_pos = try file.getEndPos();
    const buffer = try allocator.alloc(u8, end_pos + 1);

    if (try file.read(buffer) != end_pos) return error.IncompleteContetent;

    buffer[end_pos] = '\n';

    return buffer;
}
