const std = @import("std");

pub fn malloc(pages: usize) []u8 {
    const buffer = std.posix.mmap(
        null,
        pages * std.mem.page_size,
        0x01 | 0x02,
        .{ .TYPE = .PRIVATE, .ANONYMOUS = true },
        -1,
        0,
    ) catch @panic("Allocation failed");

    return @alignCast(buffer);
}

pub fn free(memory: []const u8) void {
    const ptr: []align(std.mem.page_size) const u8 = @alignCast(memory);
    std.posix.munmap(ptr);
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

    pub fn alloc(self: *Arena, T: type, size: u32) [*]T {
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

    pub fn free(self: *Arena, T: type, size: u32) void {
        const length = @sizeOf(T) * size;

        if (length > self.used) @panic("Arena do not have enough size");
        self.used -= length;
    }

    pub fn clear(self: *Arena) void {
        self.used = 0;
    }
};


