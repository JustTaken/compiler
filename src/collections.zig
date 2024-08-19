const std = @import("std");
const util = @import("util.zig");
const copy = util.copy;

const Allocator = std.mem.Allocator;

pub const Arena = struct {
    ptr: *anyopaque,
    free: u32,
    len: u32,

    pub fn init(buffer: []u8) Arena {
        return .{
            .ptr = @ptrCast(buffer.ptr),
            .len = @intCast(buffer.len),
            .free = 0,
        };
    }

    pub fn alloc(self: *Arena, T: type, size: u32) [*]T {
        const lenght = @sizeOf(T) * size;

        if (lenght + self.free > self.len) {
            @panic("Arena do not have enhough size");
        }

        const ptr: usize = @intFromPtr(self.ptr) + self.free;
        self.free += lenght;

        return @ptrFromInt(ptr);
    }

    pub fn free(self: *Arena, T: type, size: u32) void {
        const length = @sizeOf(T) * size;
        if (length > self.free) @panic("Arena do not have enough size");
        self.free -= length;
    }
};

pub fn Vec(T: type) type {
    return struct {
        items: [*]T,
        len: u32,
        capacity: u32,

        const Self = @This();

        pub fn init(size: u32, arena: *Arena) Self {
            return .{
                .items = arena.alloc(T, size),
                .capacity = size,
                .len = 0,
            };
        }

        pub fn push(self: *Self, item: T) void {
            if (self.len >= self.capacity)
                @panic("Vec does not have enough size");

            self.items[self.len] = item;
            self.len += 1;
        }

        pub fn get(self: *Self, index: u32) *T {
            return &self.items[index];
        }

        pub fn clear(self: *Self) void {
            self.len = 0;
        }

        pub fn last(self: *Self) *T {
            return &self.items[self.len - 1];
        }

        pub fn offset(self: *const Self, index: u32) []const T {
            return self.items[index..self.len];
        }
    };
}

pub fn Iter(T: type) type {
    return struct {
        current: usize,
        vec: *const Vec(T),

        const Self = @This();

        pub fn init(vec: *const Vec(T)) Self {
            return .{
                .vec = vec,
                .current = 0,
            };
        }

        pub fn consume(self: *Self) void {
            self.current += 1;
        }

        pub fn next(self: *Self) ?*const T {
            if (self.current >= self.vec.len) return null;

            return &self.vec.items[self.current];
        }
    };
}
