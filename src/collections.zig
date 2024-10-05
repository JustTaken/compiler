const util = @import("util.zig");

const Arena = @import("allocator.zig").Arena;
const Range = util.Range;

pub fn Vec(T: type) type {
    return struct {
        items: [*]T,
        len: u32,
        capacity: u32,

        const Self = @This();

        pub fn new(size: u32, arena: *Arena) Self {
            return Self{
                .items = arena.alloc(T, size),
                .capacity = size,
                .len = 0,
            };
        }

        pub fn from_array(items: []T) Self {
            return Self{
                .items = items.ptr,
                .len = @intCast(items.len),
                .capacity = @intCast(items.len),
            };
        }

        pub fn push(self: *Self, item: T) void {
            if (self.len >= self.capacity) @panic("Vec does not have enough size");

            self.items[self.len] = item;
            self.len += 1;
        }

        pub fn extend(self: *Self, items: []const T) void {
            if (self.len + items.len >= self.capacity)
                @panic("Vec does not have enough size");

            for (0..items.len) |i| {
                self.items[self.len + i] = items[i];
            }

            self.len += @intCast(items.len);
        }

        pub fn extend_range(self: *Self, items: []const T) Range {
            const start = self.len;
            self.extend(items);

            return Range.new(start, self.len);
        }

        pub fn eql(self: *const Self, other: Self) bool {
            if (self.len != other.len) return false;

            for (0..self.len) |i| {
                if (self.items[i] != other.items[i]) return false;
            }

            return true;
        }

        pub fn get(self: *const Self, index: u32) *T {
            if (index >= self.len) @panic("Vec does not have enough size");

            return &self.items[index];
        }

        pub fn last(self: *const Self) *T {
            if (self.len == 0) @panic("Vec len is zero");

            return &self.items[self.len - 1];
        }

        pub fn pop(self: *Self) T {
            if (self.len == 0) @panic("Vec len is zero");

            self.len -= 1;
            return self.items[self.len];
        }

        pub fn get_back(self: *const Self, index: usize) T {
            return self.items[self.len - index - 1];
        }

        pub fn buffer(self: *const Self) []T {
            return self.items[self.len..self.capacity];
        }

        pub fn range(self: *const Self, r: Range) []const T {
            if (r.start > r.end) @panic("Start boundary greater than end");
            if (r.end > self.len) @panic("Out of bounds");

            return self.items[r.start..r.end];
        }

        pub fn content(self: *const Self) []const T {
            return self.range(Range.new(0, self.len));
        }

        pub fn clear(self: *Self) void {
            self.len = 0;
        }
    };
}

pub fn RangeMap(T: type) type {
    return struct {
        value: [*]T,
        key: [*]Range,
        capacity: u32,

        const Self = @This();

        pub fn new(size: u32, arena: *Arena) Self {
            const key = arena.alloc(Range, size);
            const value = arena.alloc(T, size);

            @memset(key[0..size], Range.new(0, 0));

            return .{
                .value = value,
                .key = key,
                .capacity = size,
            };
        }

        pub fn push(self: *Self, key: Range, item: T, buffer: *const Vec(u8)) void {
            const h = util.hash(buffer.range(key));

            var code = h % self.capacity;
            var count: u32 = 0;

            while (self.key[code].end > 0 and count < self.capacity) {
                if (key.eql(self.key[code], buffer)) {
                    break;
                }

                code = (code + 1) % self.capacity;
                count += 1;
            }

            if (count >= self.capacity) @panic("No more space");

            self.value[code] = item;
            self.key[code] = key;
        }

        pub fn get(self: *const Self, key: Range, buffer: *const Vec(u8)) ?*T {
            const h = util.hash(buffer.range(key));

            var count: u32 = 0;
            var code = h % self.capacity;

            while (self.key[code].end > 0 and count < self.capacity) {
                if (key.eql(self.key[code], buffer)) {
                    return &self.value[code];
                }

                code = (code + 1) % self.capacity;
                count += 1;
            }

            return null;
        }

        pub fn clear(self: *Self) void {
            @memset(self.key[0..self.capacity], Range.new(0, 0));
        }
    };
}

pub fn Iter(T: type) type {
    return struct {
        vec: *const Vec(T),
        index: u32,

        const Self = @This();
        pub fn new(vec: *const Vec(T)) Self {
            return Self{
                .vec = vec,
                .index = 0,
            };
        }

        pub fn next(self: *Self) ?*T {
            if (self.index >= self.vec.len) {
                return null;
            } else {
                self.index += 1;
                return self.vec.get(self.index - 1);
            }
        }

        pub fn has_next(self: *const Self) bool {
            return self.index < self.vec.len;
        }

        pub fn consume(self: *Self) void {
            self.index += 1;
        }
    };
}
