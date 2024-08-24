const std = @import("std");
const util = @import("util.zig");
const copy = util.copy;

pub const Arena = struct {
    ptr: *anyopaque,
    free: u32,
    capacity: u32,

    pub fn malloc(size: u32) Arena {
        const p = std.c.malloc(size);

        if (p) |buffer| {
            return Arena{
                .ptr = buffer,
                .free = 0,
                .capacity = size,
            };
        } else {
            @panic("Could not allocate");
        }
    }

    pub fn init(buffer: []u8) Arena {
        return .{
            .ptr = @ptrCast(buffer.ptr),
            .capacity = @intCast(buffer.len),
            .free = 0,
        };
    }

    pub fn alloc(self: *Arena, T: type, size: u32) [*]T {
        const lenght = @sizeOf(T) * size;

        if (lenght + self.free > self.capacity) {
            @panic("Arena do not have enhough size");
        }

        var ptr: usize = @intFromPtr(self.ptr) + self.free;
        self.free += lenght;

        const alig = @alignOf(T);
        const rest = ptr % alig;

        if (rest > 0) {
            const offset: u32 = @intCast(alig - rest);
            self.free += offset;
            ptr += offset;
        }

        // std.debug.print(
        //     "type: {s}, size: {}, len: {}, free: {} / {}\n",
        //     .{
        //         @typeName(T),
        //         @sizeOf(T),
        //         size,
        //         self.free,
        //         self.capacity,
        //     },
        // );

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

        pub fn extend(self: *Self, items: []const T) void {
            if (self.len + items.len >= self.capacity)
                @panic("Vec does not have enough size");

            for (items, 0..) |item, i| {
                self.items[self.len + i] = item;
            }

            self.len += @intCast(items.len);
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

pub fn HashMap(T: type) type {
    return struct {
        value: [*]T,
        key: [*][]const u8,
        convert: [*]u8,
        len: u32,
        capacity: u32,

        const Self = @This();
        pub fn init(len: u32, arena: *Arena) Self {
            if (len > 256) @panic("Should not be greater than 256");

            const key = arena.alloc([]const u8, len);
            const value = arena.alloc(T, len);
            const convert = arena.alloc(u8, len);

            @memset(key[0..len], "");
            return .{
                .value = value,
                .key = key,
                .capacity = len,
                .convert = convert,
                .len = 0,
            };
        }

        fn hash(key: []const u8) u32 {
            var h: u32 = 0;

            for (0..key.len) |i| {
                h += @intCast(key[i] + i);
            }

            return h;
        }

        fn eql(key: []const u8, other: []const u8) bool {
            if (key.len != other.len) return false;

            for (0..key.len) |i| {
                if (key[i] != other[i]) return false;
            }

            return true;
        }

        pub fn push(self: *Self, key: []const u8, item: T) void {
            const h = hash(key);

            var code = h % self.capacity;
            var count: u32 = 0;
            var flag = false;

            while (self.key[code].len > 0) {
                if (count >= self.capacity) @panic("No more space");
                if (eql(key, self.key[code])) {
                    flag = true;
                    if (self.value[self.convert[code]].fields != 0)
                        @panic("Already initialized");
                    break;
                }

                code = (code + 1) % self.capacity;
                count += 1;
            }

            self.value[self.len] = item;

            if (!flag) {
                self.convert[code] = @intCast(self.len);
                self.key[code] = key;
                self.len += 1;
            }
        }

        pub fn get(self: *const Self, key: []const u8) ?u32 {
            const h = hash(key);

            var count: u32 = 0;
            var code = h % self.capacity;

            while (self.key[code].len > 0 and count < self.capacity) {
                if (eql(self.key[code], key)) {
                    return self.convert[code];
                }

                code = (code + 1) % self.capacity;
                count += 1;
            }

            return null;
        }

        pub fn clear(self: *Self) void {
            @memset(self.key[0..self.capacity], "");
            self.len = 0;
        }
    };
}
