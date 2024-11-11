const std = @import("std");
const util = @import("util");

pub const PAGE_SIZE: usize = std.mem.page_size;

pub fn malloc(pages: usize) []u8 {
    const buffer = std.posix.mmap(
        null,
        pages * PAGE_SIZE,
        0x01 | 0x02,
        .{ .TYPE = .PRIVATE, .ANONYMOUS = true },
        -1,
        0,
    ) catch @panic("Allocation failed");

    return @alignCast(buffer);
}

pub fn free(memory: []const u8) void {
    const ptr: []align(PAGE_SIZE) const u8 = @alignCast(memory);
    std.posix.munmap(ptr);
}

pub fn copy(T: type, src: []const T, dst: []T) void {
    @setRuntimeSafety(false);

    const len = src.len;

    for (0..len) |i| {
        dst[i] = src[i];
    }
}

pub fn equal(T: type, one: []const T, two: []const T) bool {
    if (one.len != two.len) return false;
    const len = util.min(one.len, two.len);

    for (0..len) |i| {
        if (one[i] != two[i]) return false;
    }

    return true;
}

pub const Arena = struct {
    ptr: *anyopaque,
    used: u32,
    capacity: u32,

    pub fn new(buffer: []u8) Arena {
        return .{
            .ptr = @ptrCast(buffer.ptr),
            .capacity = @intCast(buffer.len),
            .used = 0,
        };
    }

    pub fn bytes(self: *Arena, size: u32) []u8 {
        if (self.used + size > self.capacity) @panic("Arena do not have enough size");

        const ptr: [*]u8 = @ptrCast(self.ptr);
        const buffer = ptr[self.used .. self.used + size];

        self.used += size;

        return buffer;
    }

    pub fn alloc(self: *Arena, T: type, size: u32) [*]T {
        if (size == 0) {
            return @ptrCast(@alignCast(self.ptr));
        }

        const lenght = @sizeOf(T) * size;

        if (lenght + self.used > self.capacity) @panic("Arena do not have enhough size");

        var ptr: usize = @intFromPtr(self.ptr) + self.used;
        self.used += lenght;

        const alig = @alignOf(T);
        const rest = ptr % alig;

        if (rest > 0) {
            const offset: u32 = @intCast(alig - rest);
            self.used += offset;
            ptr += offset;
        }

        return @ptrFromInt(ptr);
    }

    pub fn create(self: *Arena, T: type, value: T) *T {
        const ptr = self.alloc(T, 1);
        ptr[0] = value;

        return @ptrCast(ptr);
    }

    pub fn destroy(self: *Arena, T: type, count: u32) void {
        const len: u32 = @intCast(@sizeOf(T));

        self.used -= len * count;
    }
};
