const mem = @import("mem");
const util = @import("util");
const std = @import("std");

pub const String = Vec(u8);

pub const File = struct {
    handle: std.fs.File,

    pub fn create(path: []const u8) error{Fail}!File {
        const zone = util.tracy.initZone(@src(), .{.name = "File::create"});
        defer zone.deinit();

        const handle = std.fs.cwd().createFile(path, .{}) catch return error.Fail;

        return File{
            .handle = handle,
        };
    }

    pub fn open(path: []const u8) error{Fail}!File {
        const zone = util.tracy.initZone(@src(), .{.name = "File::open"});
        defer zone.deinit();

        const handle = std.fs.cwd().openFile(path, .{}) catch return error.Fail;

        return File{
            .handle = handle,
        };
    }

    pub fn read(payload: *anyopaque, buffer: *String) error{ AtEnd, OutOfBounds }!void {
        const zone = util.tracy.initZone(@src(), .{.name = "File::read"});
        defer zone.deinit();

        const self: *File = @ptrCast(@alignCast(payload));
        const buffer_len = buffer.len;
        const len: u32 = @intCast(self.handle.read(buffer.free_space()) catch return error.AtEnd);

        if (len == 0) {
            return error.AtEnd;
        }

        try buffer.set_len(buffer_len + len);
    }

    pub fn write(payload: *anyopaque, buffer: String) error{Fail}!void {
        const zone = util.tracy.initZone(@src(), .{.name = "File::write"});
        defer zone.deinit();

        const self: *File = @ptrCast(@alignCast(payload));
        const len = self.handle.write(buffer.offset(0) catch unreachable) catch return error.Fail;

        if (len != buffer.len) return error.Fail;
    }

    pub fn close(self: *File) void {
        const zone = util.tracy.initZone(@src(), .{.name = "File::close"});
        defer zone.deinit();

        self.handle.close();
    }

    pub fn stream(self: *File) Stream(u8) {
        const zone = util.tracy.initZone(@src(), .{.name = "File::stream"});
        defer zone.deinit();

        return Stream(u8) {
            .payload = self,
            .read_fn = read,
            .write_fn = write,
        };
    }
};

pub fn Stream(T: type) type {
    return struct {
        payload: *anyopaque,
        read_fn: *const fn (ptr: *anyopaque, buffer: *Vec(T)) error{ AtEnd, OutOfBounds }!void,
        write_fn: *const fn (ptr: *anyopaque, buffer: Vec(T)) error{Fail}!void,

        const Self = @This();

        pub fn read(self: *const Self, buffer: *Vec(T)) error{ AtEnd, OutOfBounds }!void {
        //     const zone = util.tracy.initZone(@src(), .{.name = "Stream::read"});
        //     defer zone.deinit();

            try self.read_fn(self.payload, buffer);
        }

        pub fn write(self: *const Self, buffer: String) error{Fail}!void {
        //     const zone = util.tracy.initZone(@src(), .{.name = "Stream::write"});
        //     defer zone.deinit();

            try self.write_fn(self.payload, buffer);
        }
    };
}

pub fn Array(T: type) type {
    return struct {
        items: [*]T,
        len: usize,

        const Self = @This();

        pub fn new(len: u32, arena: *mem.Arena) error{OutOfMemory}!Self {
            const zone = util.tracy.initZone(@src(), .{.name = "Array::new"});
            defer zone.deinit();

            return Self{
                .items = try arena.alloc(T, len),
                .len = len,
            };
        }

        pub fn set(self: *Self, item: T, pos: usize) error{OutOfBounds}!void {
            const zone = util.tracy.initZone(@src(), .{.name = "Array::set"});
            defer zone.deinit();

            if (pos >= self.len) return error.OutOfBounds;

            self.items[pos] = item;
        }

        pub fn set_back(self: *Self, item: T, pos: usize) error{OutOfBounds}!void {
            const zone = util.tracy.initZone(@src(), .{.name = "Array::set_back"});
            defer zone.deinit();

            if (pos >= self.len) return error.OutOfBounds;

            self.items[self.len - pos - 1] = item;
        }

        pub fn offset(self: *const Self, o: usize) error{OutOfBounds}![]const T {
            const zone = util.tracy.initZone(@src(), .{.name = "Array::offset"});
            defer zone.deinit();

            if (o > self.len) return error.OutOfBounds;

            return self.items[o..self.len];
        }

        pub fn range(self: *const Self, r: util.Range) error{OutOfBounds}![]const T {
            const zone = util.tracy.initZone(@src(), .{.name = "Array::range"});
            defer zone.deinit();

            if (r.end > self.len) return error.OutOfBounds;
            if (r.start > r.end) return error.OutOfBounds;

            return self.items[r.start..r.end];
        }

        pub fn get_back(self: *const Self, o: usize) error{OutOfBounds}!T {
            const zone = util.tracy.initZone(@src(), .{.name = "Array::get_back"});
            defer zone.deinit();

            if (o >= self.len) return error.OutOfBounds;

            return self.items[self.len - o - 1];
        }

        pub fn deinit(self: *Self, arena: *mem.Arena) void {
            const zone = util.tracy.initZone(@src(), .{.name = "Array::deinit"});
            defer zone.deinit();

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

        pub fn new(size: u32, arena: *mem.Arena) error{OutOfMemory}!Self {
            const zone = util.tracy.initZone(@src(), .{.name = "Vec::new"});
            defer zone.deinit();

            return Self{
                .items = try arena.alloc(T, size),
                .capacity = size,
                .len = 0,
            };
        }

        pub fn push(self: *Self, item: T) error{OutOfBounds}!void {
            const zone = util.tracy.initZone(@src(), .{.name = "Vec::push"});
            defer zone.deinit();

            if (self.len >= self.capacity) return error.OutOfBounds;

            defer self.len += 1;
            self.items[self.len] = item;
        }

        pub fn remove(self: *Self, index: usize) error{OutOfBounds}!void {
            const zone = util.tracy.initZone(@src(), .{.name = "Vec::remove"});
            defer zone.deinit();

            try self.shift_left(index, 1);
        }

        pub fn extend(self: *Self, items: []const T) error{OutOfBounds}!void {
            const zone = util.tracy.initZone(@src(), .{.name = "Vec::extend"});
            defer zone.deinit();

            if (self.len + items.len > self.capacity) return error.OutOfBounds;

            for (0..items.len) |i| {
                self.items[self.len + i] = items[i];
            }

            self.len += @intCast(items.len);
        }

        pub fn shift_left(self: *Self, index: usize, count: usize) error{OutOfBounds}!void {
            const zone = util.tracy.initZone(@src(), .{.name = "Vec::shift_left"});
            defer zone.deinit();

            if (index + 1 < count) return error.OutOfBounds;
            if (index >= self.len) return error.OutOfBounds;

            defer self.len -= @intCast(count);
            mem.copy(T, self.items[index + count .. self.len], self.items[index .. self.len - count]);
        }

        pub fn shift_right(self: *Self, index: usize, count: usize) error{OutOfBounds}!void {
            const zone = util.tracy.initZone(@src(), .{.name = "Vec::shift_right"});
            defer zone.deinit();

            if (self.len + count > self.capacity) return error.OutOfBounds;
            if (index >= self.len) return error.OutOfBounds;

            defer self.len += @intCast(count);
            mem.back_copy(T, self.items[index..self.len], self.items[index + count .. self.len + count]);
        }

        pub fn read(ptr: *anyopaque, buffer: *Self) error{ AtEnd, OutOfBounds }!void {
            const zone = util.tracy.initZone(@src(), .{.name = "Vec::read"});
            defer zone.deinit();

            const self: *Self = @ptrCast(@alignCast(ptr));
            try buffer.extend(self.offset(0) catch unreachable);
        }

        pub fn write(ptr: *anyopaque, buffer: Self) error{Fail}!void {
            const zone = util.tracy.initZone(@src(), .{.name = "Vec::write"});
            defer zone.deinit();

            const self: *Self = @ptrCast(@alignCast(ptr));
            self.extend(buffer.offset(0) catch unreachable) catch return error.Fail;
        }

        pub fn stream(self: *Self) Stream(T) {
            const zone = util.tracy.initZone(@src(), .{.name = "Vec::stream"});
            defer zone.deinit();

            return Stream{
                .payload = self,
                .read_fn = read,
                .write_fn = write,
            };
        }

        pub fn get(self: *const Self, index: u32) error{OutOfBounds}!*T {
            const zone = util.tracy.initZone(@src(), .{.name = "Vec::get"});
            defer zone.deinit();

            if (index >= self.len) return error.OutOfBounds;

            return &self.items[index];
        }

        pub fn value(self: *const Self, index: u32) error{OutOfBounds}!T {
            const zone = util.tracy.initZone(@src(), .{.name = "Vec::value"});
            defer zone.deinit();

            if (index >= self.len) return error.OutOfBounds;
            return self.items[index];
        }

        pub fn last(self: *const Self) error{OutOfBounds}!*T {
            const zone = util.tracy.initZone(@src(), .{.name = "Vec::last"});
            defer zone.deinit();

            if (self.len == 0) return error.OutOfBounds;

            return &self.items[self.len - 1];
        }

        pub fn pop(self: *Self) error{OutOfBounds}!T {
            const zone = util.tracy.initZone(@src(), .{.name = "Vec::pop"});
            defer zone.deinit();

            if (self.len == 0) return error.OutOfBounds;

            defer self.len -= 1;
            return self.items[self.len - 1];
        }

        pub fn get_back(self: *const Self, index: usize) error{OutOfBounds}!T {
            const zone = util.tracy.initZone(@src(), .{.name = "Vec::get_back"});
            defer zone.deinit();

            if (index >= self.len) return error.OutOfBounds;
            return self.items[self.len - index - 1];
        }

        pub fn free_space(self: *const Self) []T {
            const zone = util.tracy.initZone(@src(), .{.name = "Vec::free_space"});
            defer zone.deinit();

            return self.items[self.len..self.capacity];
        }

        pub fn range(self: *const Self, r: util.Range) error{OutOfBounds}![]const T {
            const zone = util.tracy.initZone(@src(), .{.name = "Vec::range"});
            defer zone.deinit();

            if (r.start > r.end or r.end > self.len) return error.OutOfBounds;

            return self.items[r.start..r.end];
        }

        pub fn mut_range(self: *Self, r: util.Range) error{OutOfBounds}![]T {
            const zone = util.tracy.initZone(@src(), .{.name = "Vec::mut_range"});
            defer zone.deinit();

            if (r.start > r.end or r.end > self.len) return error.OutOfBounds;

            return self.items[r.start..r.end];
        }

        pub fn slice(self: *const Self, s: Slice) error{OutOfBounds}![]const T {
            const zone = util.tracy.initZone(@src(), .{.name = "Vec::slice"});
            defer zone.deinit();

            if (s.ptr >= self.len or s.ptr + s.len > self.len) return error.OutOfBounds;

            return self.items[s.ptr..s.ptr + s.len];
        }

        pub fn set_len(self: *Self, len: u32) error{OutOfBounds}!void {
            const zone = util.tracy.initZone(@src(), .{.name = "Vec::set_len"});
            defer zone.deinit();

            if (len > self.capacity) return error.OutOfBounds;
            self.len = len;
        }

        pub fn offset(self: *const Self, o: u32) error{OutOfBounds}![]const T {
            const zone = util.tracy.initZone(@src(), .{.name = "Vec::offset"});
            defer zone.deinit();

            if (o > self.len) return error.OutOfBounds;

            return self.items[o..self.len];
        }

        pub fn clear(self: *Self) void {
            const zone = util.tracy.initZone(@src(), .{.name = "Vec::clear"});
            defer zone.deinit();

            self.len = 0;
        }

        pub fn deinit(self: *Self, arena: *mem.Arena) void {
            const zone = util.tracy.initZone(@src(), .{.name = "Vec::deinit"});
            defer zone.deinit();

            defer {
                self.clear();
                self.capacity = 0;
            }

            arena.destroy(T, self.capacity);
        }
    };
}

pub fn StringMap(T: type) type {
    return struct {
        value: [*]T,
        key: [*][]const u8,
        capacity: u32,

        const Self = @This();

        pub fn new(size: u32, arena: *mem.Arena) error{OutOfMemory}!Self {
            const zone = util.tracy.initZone(@src(), .{.name = "StringMap::new"});
            defer zone.deinit();

            const key = try arena.alloc([]const u8, size);
            const value = try arena.alloc(T, size);

            @memset(key[0..size], "");

            return .{
                .value = value,
                .key = key,
                .capacity = size,
            };
        }

        pub fn push(self: *Self, key: []const u8, item: T) error{OutOfBounds}!void {
            const zone = util.tracy.initZone(@src(), .{.name = "StringMap::push"});
            defer zone.deinit();

            _ = try self.put(key, item);
        }

        pub fn put(self: *Self, key: []const u8, item: T) error{OutOfBounds}!u32 {
            const zone = util.tracy.initZone(@src(), .{.name = "StringMap::put"});
            defer zone.deinit();

            if (self.capacity == 0) return error.OutOfBounds;

            const h = util.hash(key);

            var code = h % self.capacity;
            var count: u32 = 0;

            while (self.key[code].len > 0 and count < self.capacity) {
                if (mem.equal(u8, key, self.key[code])) {
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

        pub fn get(self: *const Self, key: []const u8) ?*T {
            const zone = util.tracy.initZone(@src(), .{.name = "StringMap::get"});
            defer zone.deinit();

            if (self.capacity == 0) return null;

            const h = util.hash(key);

            var count: u32 = 0;
            var code = h % self.capacity;

            while (self.key[code].len > 0 and count < self.capacity) {
                if (mem.equal(u8, key, self.key[code])) {
                    return &self.value[code];
                }

                code = (code + 1) % self.capacity;
                count += 1;
            }

            return null;
        }

        pub fn deinit(self: *Self, arena: *mem.Arena) void {
            const zone = util.tracy.initZone(@src(), .{.name = "StringMap::deinit"});
            defer zone.deinit();

            defer self.capacity = 0;

            arena.destroy(T, self.capacity);
            arena.destroy([]const u8, self.capacity);
        }
    };
}

pub const Slice = struct {
    ptr: util.Index,
    len: util.Index,

    pub fn new(ptr: util.Index, len: util.Index) Slice {
        const zone = util.tracy.initZone(@src(), .{.name = "Slice::new"});
        defer zone.deinit();

        return Slice {
            .ptr = ptr,
            .len = len,
        };
    }
};

pub fn SliceManager(T: type) type {
    return struct {
        slices: Vec(Slice),
        buffer: Vec(T),

        const Self = @This();

        pub fn new(size: u32, allocator: *mem.Arena) error{OutOfMemory}!Self {
            const zone = util.tracy.initZone(@src(), .{.name = "SliceManager::new"});
            defer zone.deinit();

            return Self {
                .slices = try Vec(Slice).new(size >> 2, allocator),
                .buffer = try Vec(T).new(size, allocator),
            };
        }

        pub fn push(self: *Self, items: []const T) error{OutOfBounds}!u8 {
            const zone = util.tracy.initZone(@src(), .{.name = "SliceManager::push"});
            defer zone.deinit();

            const index: u8 = @intCast(self.slices.len);
            const ptr: u8 = @intCast(self.buffer.len);
            const len: u8 = @intCast(items.len);

            try self.buffer.extend(items);
            try self.slices.push(Slice.new(ptr, len));

            return index;
        }

        pub fn start(self: *Self) error{OutOfBounds}!u8 {
            const zone = util.tracy.initZone(@src(), .{.name = "SliceManager::start"});
            defer zone.deinit();

            const index: u8 = @intCast(self.slices.len);
            const ptr: u8 = @intCast(self.buffer.len);

            try self.slices.push(Slice.new(ptr, 0));

            return index;
        }

        pub fn get(self: *const Self, index: u8) error{OutOfBounds}![]const T {
            const zone = util.tracy.initZone(@src(), .{.name = "SliceManager::get"});
            defer zone.deinit();

            const slice = try self.slices.value(index);
            return try self.buffer.slice(slice);
        }

        pub fn extend(self: *Self, item: T) error{OutOfBounds}!void {
            const zone = util.tracy.initZone(@src(), .{.name = "SliceManager::extend"});
            defer zone.deinit();

            try self.buffer.push(item);
            const last = try self.slices.last();
            last.len += 1;
        }

        pub fn clear(self: *Self) void {
            const zone = util.tracy.initZone(@src(), .{.name = "SliceManager::clear"});
            defer zone.deinit();

            self.slices.clear();
            self.buffer.clear();
        }

        pub fn deinit(self: *Self, arena: *mem.Arena) void {
            const zone = util.tracy.initZone(@src(), .{.name = "SliceManager::deinit"});
            defer zone.deinit();

            self.buffer.deinit(arena);
            self.slices.deinit(arena);
        }
    };
}

pub fn ComptimeIter(T: type) type {
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
