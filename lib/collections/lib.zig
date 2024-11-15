const mem = @import("mem");
const util = @import("util");
const std = @import("std");

const Arena = mem.Arena;
const Range = util.Range;
pub const String = Vec(u8);

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

    pub fn read(payload: *anyopaque, buffer: *String) error{ AtEnd, OutOfBounds }!void {
        const self: *File = @ptrCast(@alignCast(payload));
        const buffer_len = buffer.len;
        const len: u32 = @intCast(self.handle.read(buffer.buffer()) catch return error.AtEnd);

        if (len == 0) {
            return error.AtEnd;
        }

        try buffer.set_len(buffer_len + len);
    }

    pub fn write(payload: *anyopaque, buffer: String) error{Fail}!void {
        const self: *File = @ptrCast(@alignCast(payload));
        const len = self.handle.write(buffer.offset(0) catch unreachable) catch return error.Fail;

        if (len != buffer.len) return error.Fail;
    }

    pub fn close(self: *File) void {
        self.handle.close();
    }

    pub fn stream(self: *File) Stream {
        return Stream{
            .payload = self,
            .read_fn = read,
            .write_fn = write,
        };
    }
};

pub fn string_from_buffer(buffer: []u8) String {
    return String{
        .items = buffer.ptr,
        .len = 0,
        .capacity = @intCast(buffer.len),
    };
}

pub fn read_string(ptr: *anyopaque, buffer: *String) error{ AtEnd, OutOfBounds }!void {
    const self: *String = @ptrCast(@alignCast(ptr));
    try buffer.extend(self.offset(0) catch unreachable);
}

pub fn write_string(ptr: *anyopaque, buffer: String) error{Fail}!void {
    const self: *String = @ptrCast(@alignCast(ptr));
    self.extend(buffer.offset(0) catch unreachable) catch return error.Fail;
}

pub fn string_stream(string: *String) Stream {
    return Stream{
        .payload = @ptrCast(string),
        .read_fn = read_string,
        .write_fn = write_string,
    };
}

pub const Stream = struct {
    payload: *anyopaque,
    read_fn: *const fn (ptr: *anyopaque, buffer: *String) error{ AtEnd, OutOfBounds }!void,
    write_fn: *const fn (ptr: *anyopaque, buffer: String) error{Fail}!void,

    pub fn read(self: *const Stream, buffer: *String) error{ AtEnd, OutOfBounds }!void {
        try self.read_fn(self.payload, buffer);
    }

    pub fn write(self: *const Stream, buffer: String) error{Fail}!void {
        try self.write_fn(self.payload, buffer);
    }
};

pub fn Array(T: type) type {
    return struct {
        items: [*]T,
        len: usize,

        const Self = @This();

        pub fn new(len: u32, arena: *Arena) error{OutOfMemory}!Self {
            return Self{
                .items = try arena.alloc(T, len),
                .len = len,
            };
        }

        pub fn set(self: *Self, item: T, pos: usize) error{OutOfBounds}!void {
            if (pos >= self.len) return error.OutOfBounds;

            self.items[pos] = item;
        }

        pub fn offset(self: *const Self, o: usize) error{OutOfBounds}![]const T {
            if (o > self.len) return error.OutOfBounds;

            return self.items[o..self.len];
        }

        pub fn range(self: *const Self, r: Range) error{OutOfBounds}![]const T {
            if (r.end > self.len) return error.OutOfBounds;
            if (r.start > r.end) return error.OutOfBounds;

            return self.items[r.start..r.end];
        }

        pub fn deinit(self: *Self, arena: *Arena) void {
            defer self.len = 0;
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

        pub fn new(size: u32, arena: *Arena) error{OutOfMemory}!Self {
            return Self{
                .items = try arena.alloc(T, size),
                .capacity = size,
                .len = 0,
            };
        }

        pub fn push(self: *Self, item: T) error{OutOfBounds}!void {
            if (self.len >= self.capacity) return error.OutOfBounds;

            defer self.len += 1;
            self.items[self.len] = item;
        }

        pub fn remove(self: *Self, index: usize) error{OutOfBounds}!void {
            try self.shift_left(index, 1);
        }

        pub fn extend(self: *Self, items: []const T) error{OutOfBounds}!void {
            if (self.len + items.len > self.capacity) return error.OutOfBounds;

            for (0..items.len) |i| {
                self.items[self.len + i] = items[i];
            }

            self.len += @intCast(items.len);
        }

        pub fn shift_left(self: *Self, index: usize, count: usize) error{OutOfBounds}!void {
            if (index + 1 < count) return error.OutOfBounds;
            if (index >= self.len) return error.OutOfBounds;

            defer self.len -= @intCast(count);
            mem.copy(T, self.items[index + count .. self.len], self.items[index .. self.len - count]);
        }

        pub fn shift_right(self: *Self, index: usize, count: usize) error{OutOfBounds}!void {
            if (self.len + count > self.capacity) return error.OutOfBounds;
            if (index >= self.len) return error.OutOfBounds;

            defer self.len += @intCast(count);
            mem.back_copy(T, self.items[index..self.len], self.items[index + count .. self.len + count]);
        }

        pub fn mult_extend(self: *Self, args: []const []const T) error{OutOfBounds}!void {
            for (args) |arg| {
                try self.extend(arg);
            }
        }

        pub fn extend_range(self: *Self, items: []const T) error{OutOfBounds}!Range {
            const start = self.len;

            try self.extend(items);

            return Range.new(start, self.len);
        }

        pub fn get(self: *const Self, index: u32) error{OutOfBounds}!*T {
            if (index >= self.len) return error.OutOfBounds;

            return &self.items[index];
        }

        pub fn value(self: *const Self, index: u32) error{OutOfBounds}!T {
            if (index >= self.len) return error.OutOfBounds;
            return self.items[index];
        }

        pub fn last(self: *const Self) error{OutOfBounds}!*T {
            if (self.len == 0) return error.OutOfBounds;

            return &self.items[self.len - 1];
        }

        pub fn pop(self: *Self) error{OutOfBounds}!T {
            if (self.len == 0) return error.OutOfBounds;

            defer self.len -= 1;
            return self.items[self.len - 1];
        }

        pub fn get_back(self: *const Self, index: usize) error{OutOfBounds}!T {
            if (index >= self.len) return error.OutOfBounds;
            return self.items[self.len - index - 1];
        }

        pub fn buffer(self: *const Self) []T {
            return self.items[self.len..self.capacity];
        }

        pub fn range(self: *const Self, r: Range) error{OutOfBounds}![]const T {
            if (r.start > r.end or r.end > self.len) return error.OutOfBounds;

            return self.items[r.start..r.end];
        }

        pub fn mut_range(self: *Self, r: Range) error{OutOfBounds}![]T {
            if (r.start > r.end or r.end > self.len) return error.OutOfBounds;

            return self.items[r.start..r.end];
        }

        pub fn set_len(self: *Self, len: u32) error{OutOfBounds}!void {
            if (len > self.capacity) return error.OutOfBounds;
            self.len = len;
        }

        pub fn offset(self: *const Self, o: u32) error{OutOfBounds}![]const T {
            if (o > self.len) return error.OutOfBounds;

            return self.items[o..self.len];
        }

        pub fn clear(self: *Self) void {
            self.len = 0;
        }

        pub fn deinit(self: *Self, arena: *Arena) void {
            defer {
                self.clear();
                self.capacity = 0;
            }

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

        pub fn new(size: u32, arena: *Arena) error{OutOfMemory}!Self {
            const key = try arena.alloc(Range, size);
            const value = try arena.alloc(T, size);

            @memset(key[0..size], Range.new(0, 0));

            return .{
                .value = value,
                .key = key,
                .capacity = size,
            };
        }

        pub fn push(self: *Self, key: Range, item: T, buffer: *const Vec(u8)) error{OutOfBounds}!void {
            _ = try self.put(key, item, buffer);
        }

        pub fn put(self: *Self, key: Range, item: T, buffer: *const Vec(u8)) error{OutOfBounds}!u32 {
            if (self.capacity == 0) return error.OutOfBounds;

            const h = util.hash(try buffer.range(key));

            var code = h % self.capacity;
            var count: u32 = 0;

            while (self.key[code].end > 0 and count < self.capacity) {
                if (mem.equal(u8, try buffer.range(key), buffer.range(self.key[code]) catch @panic("TODO: Should be unreachable"))) {
                    break;
                }

                code = (code + 1) % self.capacity;
                count += 1;
            }

            if (count >= self.capacity) return error.OutOfBounds;

            self.value[code] = item;
            self.key[code] = key;

            return code;
        }

        pub fn get(self: *const Self, key: []const u8, buffer: *const Vec(u8)) ?*T {
            if (self.capacity == 0) return null;

            const h = util.hash(key);

            var count: u32 = 0;
            var code = h % self.capacity;

            while (self.key[code].end > 0 and count < self.capacity) {
                if (mem.equal(u8, key, buffer.range(self.key[code]) catch @panic("TODO: I guess this is unreachable"))) {
                    return &self.value[code];
                }

                code = (code + 1) % self.capacity;
                count += 1;
            }

            return null;
        }

        pub fn deinit(self: *Self, arena: *Arena) void {
            defer self.capacity = 0;

            arena.destroy(T, self.capacity);
            arena.destroy(Range, self.capacity);
        }
    };
}

pub fn Iter(T: type) type {
    return struct {
        items: [*]const T,
        len: u32,
        index: u32,

        const Self = @This();

        pub fn new(comptime items: []const T) Self {
            return Self{
                .items = items.ptr,
                .len = @intCast(items.len),
                .index = 0,
            };
        }

        pub fn next(comptime self: *Self) ?T {
            if (self.index >= self.len) return null;

            defer self.index += 1;
            return self.items[self.index];
        }
    };
}
