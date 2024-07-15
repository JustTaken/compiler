const std = @import("std");
const copy = @import("util.zig").copy;

const Allocator = std.mem.Allocator;

pub fn Vec(T: type) type {
    const Error = error {
        AllocationFail,
        OutOfLength,
    };

    return struct {
        items: []T,
        capacity: u32,
        allocator: Allocator,

        const Self = @This();

        pub fn init(capacity: u32, allocator: Allocator) Error!Self {
            var items: []T = undefined;
            const memory = allocator.alloc(T, capacity) catch return Error.AllocationFail;

            items.len = 0;
            items.ptr = memory.ptr;

            return Self {
                .items = items,
                .capacity = capacity,
                .allocator = allocator,
            };
        }

        pub fn flagged_push(self: *Self, item: T) Error!bool {
            var flag = false;
            const count: u32 = self.len();

            if (self.capacity <= count) {
                try self.resize(count * 2);
                flag = true;
            }

            self.items.len += 1;
            self.items[count] = item;

            return flag;
        }

        pub fn push(self: *Self, item: T) Error!void {
            const count: u32 = self.len();

            if (self.capacity <= count) try self.resize(count * 2);

            self.items.len += 1;
            self.items[count] = item;
        }

        pub fn repeat(self: *Self, item: T, count: u32) Error!void {
            const self_len = self.len();
            const sum: u32 = self_len + count;

            if (self.capacity <= sum) try self.resize(sum * 2);

            self.items.len += count;
            for (0..count) |i| {
                self.items[i + self_len] = item;
            }
        }

        fn resize(self: *Self, capacity: u32) Error!void {
            const new = self.allocator.alloc(T, capacity) catch return Error.AllocationFail;
            const count = self.len();

            copy(T, self.items, new);
            self.allocator.free(self.items.ptr[0..self.capacity]);

            self.items.ptr = new.ptr;
            self.items.len = count;
            self.capacity = capacity;
        }

        pub fn extend(self: *Self, comptime N: u32, items: FixedVec(T, N)) Error!void {
            const new_len: u32 = @intCast(self.items.len + items.len);

            if (self.capacity <= new_len) try self.resize(new_len * 2);
            copy(T, items.elements(), self.items[self.items.len..]);

            self.items.len = new_len;
        }

        pub fn extend_with(self: *Self, items: []const T) !void {
            const new_len: u32 = @intCast(self.items.len + items.len);

            if (self.capacity <= new_len) try self.resize(new_len * 2);
            copy(T, items, self.items[self.items.len..]);

            self.items.len = new_len;
        }

        pub fn insert(self: *Self, item: T, index: u32) Error!void {
            const count = self.len();

            if (index > count) return Error.OutOfLength;

            if (self.capacity <= count) {
                const new_len = count * 2;
                const new = self.allocator.alloc(T, new_len) catch return Error.AllocationFail;

                copy(T, self.items[0..index], new);
                copy(T, self.items[index..], new[index + 1..]);

                self.allocator.free(self.items.ptr[0..self.capacity]);

                self.items.ptr = new.ptr;
                self.items.len = count + 1;

                self.capacity = new_len;
            } else {
                self.items.len = count;
                const dif = count - index;

                for (0..dif) |i| {
                    self.items[self.items.len - i - 1] = self.items[count - i - 1];
                }
            }

            self.items[index] = item;
        }

        pub fn extend_insert(self: *Self, items: []const T, index: u32) Error!void {
            const self_len: u32 = @intCast(self.items.len);
            const other_len: u32 = @intCast(items.len);
            const count: u32 = self_len + other_len;

            if (self.capacity <= count) {
                const new_len = count * 2;
                const new = self.allocator.alloc(T, new_len) catch return Error.AllocationFail;

                copy(T, self.items[0..index], new);
                copy(T, self.items[index..], new[index + other_len..]);

                self.allocator.free(self.items.ptr[0..self.capacity]);

                self.items.ptr = new.ptr;
                self.items.len = count;
                self.capacity = new_len;
            } else {
                self.items.len = count;

                const diff = self_len - index;
                for (0..diff) |i| {
                    self.items[self.items.len - 1 - i] = self.items[self_len - 1 - i];
                }
            }

            copy(T, items, self.items[index..]);
        }

        pub fn range(self: *const Self, start: u32, end: u32) Error![]const T {
            var e = end + 1;

            if (start > end) return Error.OutOfLength;
            if (start >= self.items.len) return Error.OutOfLength;
            if (e > self.items.len) e = self.len();

            return self.items[start..e];
        }

        pub fn truncate(self: *Self, index: u32) []const T {
            if (index >= self.items.len) return &[_] T {};
            const items = self.items[index..];

            self.items.len = index;
            return items;
        }

        pub fn get(self: *const Self, index: u32) !*const T {
            if (index >= self.items.len) return Error.OutOfLength;

            return &self.items[index];
        }

        pub fn get_mut(self: *Self, index: u32) !*T {
            if (index >= self.items.len) return Error.OutOfLength;

            return &self.items[index];
        }

        pub fn last_mut(self: *Self) !*T {
            if (self.items.len == 0) return Error.OutOfLength;

            return &self.items[self.items.len - 1];
        }

        pub fn last_mut_opt(self: *Self) ?*T {
            if (self.items.len == 0) return null;

            return &self.items[self.items.len - 1];
        }

        pub fn last(self: *const Self) !*const T {
            if (self.items.len == 0) return Error.OutOfLength;

            return &self.items[self.items.len - 1];
        }

        pub fn len(self: *const Self) u32 {
            return @intCast(self.items.len);
        }

        pub fn elements(self: *const Self) []const T {
            return self.items;
        }

        pub fn iter(self: *const Self) Iter(T) {
            return Iter(T).init(self);
        }

        pub fn deinit(self: *const Self) void {
            self.allocator.free(self.items.ptr[0..self.capacity]);
        }
    };
}

pub fn Iter(T: type) type {
    return struct {
        elements: *const Vec(T),
        index: u32,

        const Self = @This();
        pub fn init(elements: *const Vec(T)) Self {
            return Self {
                .elements = elements,
                .index = 0,
            };
        }

        pub fn next(self: *Self) ?*const T {
            const e = self.elements.get(self.index) catch return null;
            self.index += 1;

            return e;
        }

        pub fn take(self: *Self, f: fn (*const T) bool) ?*const T {
            for (self.elements.items) |*e| {
                if (f(e)) return e;
            }

            return null;
        }

        pub fn reset(self: *Self) void {
            self.index = 0;
        }
    };
}

pub fn FixedVec(T: type, L: u32) type {
    return struct {
        items: [L]T,
        len: u32,

        const Self = @This();

        pub fn init() Self {
            return Self {
                .items = undefined,
                .len = 0,
            };
        }

        pub fn push(self: *Self, item: T) error { OutOfLength }!void {
            if (self.items.len <= self.len) return error.OutOfLength;

            self.items[self.len] = item;
            self.len += 1;
        }

        pub fn get(self: *Self, index: u32) !*T {
            if (index >= self.len) return error.OutOfLength;

            return &self.items[index];
        }

        pub fn last(self: *const T) !*const T {
            if (self.len == 0) return error.OutOfLength;

            return &self.items[self.len - 1];
        }

        pub fn elements(self: *const Self) []const T {
            return self.items[0..self.len];
        }
    };
}
