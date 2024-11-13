const util = @import("util");
const mem = @import("mem");
const std = @import("std");

const Arena = mem.Arena;
pub const String = Vec(u8);

pub const Range = struct {
    start: u32,
    end: u32,

    pub fn new(start: u32, end: u32) Range {
        return Range{
            .start = start,
            .end = end,
        };
    }

    pub fn reset(self: *Range) void {
        self.start = 0;
        self.end = 0;
    }
};

pub const File = struct {
    handle: std.fs.File,

    pub fn create(path: []const u8) error{Fail}!File {
        const handle = std.fs.cwd().createFile(path, .{}) catch return error.Fail;

        return File{
            .handle = handle,
        };
    }

    pub fn open(path: []const u8) error{Fail}!File {
        const handle = std.fs.cwd().openFile(path, .{}) catch return error.Fail;

        return File{
            .handle = handle,
        };
    }

    pub fn read(payload: *anyopaque, buffer: *String) error{AtEnd}!void {
        const self: *File = @ptrCast(@alignCast(payload));
        const buffer_len = buffer.len;
        const len: u32 = @intCast(self.handle.read(buffer.buffer()) catch return error.AtEnd);

        if (len == 0) {
            return error.AtEnd;
        }

        buffer.set_len(buffer_len + len);
    }

    pub fn write(payload: *anyopaque, buffer: String) error{Fail}!void {
        const self: *File = @ptrCast(@alignCast(payload));
        _ = self.handle.write(buffer.offset(0)) catch return error.Fail;
    }

    pub fn close(payload: *anyopaque) void {
        const self: *File = @ptrCast(@alignCast(payload));
        self.handle.close();
    }

    pub fn stream(self: *File) Stream {
        return Stream{
            .payload = self,
            .read_fn = read,
            .write_fn = write,
            .close_fn = close,
        };
    }
};

pub const Stream = struct {
    payload: *anyopaque,
    read_fn: *const fn (ptr: *anyopaque, buffer: *String) error{AtEnd}!void,
    write_fn: *const fn (ptr: *anyopaque, buffer: String) error{Fail}!void,
    close_fn: *const fn (ptr: *anyopaque) void,

    pub fn read(self: *const Stream, buffer: *String) error{AtEnd}!void {
        try self.read_fn(self.payload, buffer);
    }

    pub fn write(self: *const Stream, buffer: String) error{Fail}!void {
        try self.write_fn(self.payload, buffer);
    }

    pub fn close(self: *const Stream) void {
        self.close_fn(self.payload);
    }
};

pub fn Array(T: type) type {
    return struct {
        items: [*]T,
        len: usize,

        const Self = @This();
        pub fn new(len: u32, arena: *Arena) Self {
            return Self{
                .items = arena.alloc(T, len),
                .len = len,
            };
        }

        pub fn set(self: *Self, item: T, pos: usize) void {
            if (pos >= self.len) {
                @panic("Out of length");
            }

            self.items[pos] = item;
        }

        pub fn offset(self: *const Self, o: usize) []const T {
            if (o > self.len) {
                @panic("Out of length");
            }

            return self.items[o..self.len];
        }

        pub fn deinit(self: *const Self, arena: *Arena) void {
            arena.destroy(T, self.len);
        }
    };
}

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

        pub fn push(self: *Self, item: T) void {
            if (self.len >= self.capacity) @panic("Vec does not have enough size");

            defer self.len += 1;
            self.items[self.len] = item;
        }

        pub fn remove(self: *Self, index: usize) void {
            if (index >= self.len) @panic("Out of length");

            defer self.len -= 1;
            mem.copy(T, self.items[index + 1 .. self.len], self.items[index .. self.len - 1]);
        }

        pub fn extend(self: *Self, items: []const T) void {
            if (self.len + items.len > self.capacity)
                @panic("Vec does not have enough size");

            for (0..items.len) |i| {
                self.items[self.len + i] = items[i];
            }

            self.len += @intCast(items.len);
        }

        pub fn shift(self: *Self, index: usize, count: usize) void {
            if (self.len + count > self.capacity) @panic("Out of length");
            const len = self.len;

            self.len = @intCast(len + count);
            mem.back_copy(T, self.items[index..len], self.items[index + count .. self.len]);
        }

        pub fn mult_extend(self: *Self, args: []const []const T) void {
            for (args) |arg| {
                self.extend(arg);
            }
        }

        pub fn extend_range(self: *Self, items: []const T) Range {
            const start = self.len;
            self.extend(items);

            return Range.new(start, self.len);
        }

        pub fn get(self: *const Self, index: u32) *T {
            if (index >= self.len) @panic("Vec does not have enough size");

            return &self.items[index];
        }

        pub fn value(self: *const Self, index: u32) T {
            return self.items[index];
        }

        pub fn last(self: *const Self) *T {
            if (self.len == 0) @panic("Vec len is zero");

            return &self.items[self.len - 1];
        }

        pub fn pop(self: *Self) T {
            if (self.len == 0) @panic("Vec len is zero");

            defer self.len -= 1;
            return self.items[self.len - 1];
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

        pub fn mut_range(self: *Self, r: Range) []T {
            if (r.start > r.end) @panic("Start boundary greater than end");
            if (r.end > self.len) @panic("Out of bounds");

            return self.items[r.start..r.end];
        }

        pub fn set_len(self: *Self, len: u32) void {
            self.len = len;
        }

        pub fn offset(self: *const Self, o: u32) []const T {
            return self.range(Range.new(o, self.len));
        }

        pub fn clear(self: *Self) void {
            self.len = 0;
        }

        pub fn deinit(self: *const Self, arena: *Arena) void {
            arena.destroy(T, self.capacity);
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
            _ = self.put(key, item, buffer);
        }

        pub fn put(self: *Self, key: Range, item: T, buffer: *const Vec(u8)) u32 {
            const h = util.hash(buffer.range(key));

            var code = h % self.capacity;
            var count: u32 = 0;

            while (self.key[code].end > 0 and count < self.capacity) {
                if (mem.equal(u8, buffer.range(key), buffer.range(self.key[code]))) {
                    break;
                }

                code = (code + 1) % self.capacity;
                count += 1;
            }

            if (count >= self.capacity) @panic("No more space");

            self.value[code] = item;
            self.key[code] = key;

            return code;
        }

        pub fn get(self: *const Self, key: []const u8, buffer: *const Vec(u8)) ?*T {
            const h = util.hash(key);

            var count: u32 = 0;
            var code = h % self.capacity;

            while (self.key[code].end > 0 and count < self.capacity) {
                if (mem.equal(u8, key, buffer.range(self.key[code]))) {
                    return &self.value[code];
                }

                code = (code + 1) % self.capacity;
                count += 1;
            }

            return null;
        }

        pub fn deinit(self: *const Self, arena: *Arena) void {
            arena.destroy(T, self.capacity);
            arena.destroy(Range, self.capacity);
        }
    };
}
