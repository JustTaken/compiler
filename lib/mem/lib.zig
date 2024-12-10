const std = @import("std");
const util = @import("util");

pub const PAGE_SIZE: u32 = std.mem.page_size;
pub const BASE_SIZE: u32 = 8;
pub const BYTE_SIZE: usize = 1;

pub fn malloc(pages: usize) error{Fail}![]u8 {
    const buffer = std.posix.mmap(
        null,
        pages * PAGE_SIZE,
        0x01 | 0x02,
        .{ .TYPE = .PRIVATE, .ANONYMOUS = true },
        -1,
        0,
    ) catch return error.Fail;

    return @alignCast(buffer);
}

pub fn mfree(memory: []const u8) void {
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

pub fn back_copy(T: type, src: []const T, dst: []T) void {
    @setRuntimeSafety(false);

    const len = src.len;

    for (0..len) |i| {
        dst[len - i - 1] = src[len - i - 1];
    }
}

pub fn equal(T: type, one: []const T, two: []const T) bool {
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

pub fn max(one: usize, two: usize) u32 {
    if (one > two) return @intCast(one);
    return @intCast(two);
}

pub fn as_bytes(T: type, ptr: *const T) []const u8 {
    const buffer: [*]const u8 = @ptrCast(ptr);
    const size = @sizeOf(T);

    return buffer[0..size];
}

pub fn align_with(value: usize, with: usize) usize {
    const rest = value % with;

    if (rest == 0) {
        return value;
    }

    return value + with - rest;
}

pub const Arena = struct {
    ptr: *anyopaque,
    usage: u32,
    max_usage: u32,
    capacity: u32,
    name: [:0]const u8,
    // allocations: u32,
    // frees: u32,

    parent: ?*Arena,

    pub fn new(name: [:0]const u8, pages: u32) error{OutOfMemory}!Arena {
        const zone = util.tracy.initZone(@src(), .{.name = "Arena::new"});
        defer zone.deinit();

        const buffer = malloc(pages) catch return error.OutOfMemory;

        return .{
            .ptr = buffer.ptr,
            .capacity = @intCast(buffer.len),
            .usage = 0,
            .max_usage = 0,
            .parent = null,
            // .allocations = 0,
            // .frees = 0,
            .name = name,
        };
    }

    pub fn child(self: *Arena, name: [:0]const u8, size: u32) error{OutOfMemory}!*Arena {
        const zone = util.tracy.initZone(@src(), .{.name = "Arena::child"});
        defer zone.deinit();

        var arena = Arena{
            .ptr = try self.alloc(u8, size),
            .capacity = size,
            .parent = self,
            .max_usage = 0,
            .usage = 0,
            // .allocations = 0,
            // .frees = 0,
            .name = name,
        };

        errdefer self.destroy(u8, size);
        return try arena.create(Arena, arena);
    }

    pub fn alloc(self: *Arena, T: type, count: u32) error{OutOfMemory}![*]T {
        const zone = util.tracy.initZone(@src(), .{.name = "Arena::alloc"});
        defer zone.deinit();

        if (count == 0) {
            return @ptrCast(@alignCast(self.ptr));
        }

        const lenght = align_with(@sizeOf(T) * count, BASE_SIZE);
        util.print(.Debug, "Allocate - {}: ({}:{})/{} - ({}:{})", .{
            self.name,
            self.usage,
            lenght,
            self.capacity,
            @typeName(T),
            count,
        });

        if (lenght + self.usage > self.capacity) return error.OutOfMemory;

        defer self.usage += @intCast(lenght);

        const ptr: usize = @intFromPtr(self.ptr) + self.usage;

        if (self.parent) |_| {
            util.tracyc.___tracy_emit_memory_alloc_named(@ptrFromInt(ptr), lenght, 0, self.name.ptr);
        }

        return @ptrFromInt(ptr);
    }

    pub fn create(self: *Arena, T: type, value: T) error{OutOfMemory}!*T {
        const zone = util.tracy.initZone(@src(), .{.name = "Arena::create"});
        defer zone.deinit();

        const ptr = try self.alloc(T, 1);
        ptr[0] = value;

        return @ptrCast(ptr);
    }

    pub fn destroy(self: *Arena, T: type, count: usize) void {
        const zone = util.tracy.initZone(@src(), .{.name = "Arena::destroy"});
        defer zone.deinit();

        if (count == 0) return;

        self.max_usage = max(self.usage, self.max_usage);

        const length = align_with(@sizeOf(T) * count, BASE_SIZE);

        util.print(.Debug, "Destroy - {}: ({}:{})/{} - ({}:{})", .{
            self.name,
            self.usage,
            length,
            self.capacity,
            @typeName(T),
            count,
        });

        self.usage -= @intCast(length);

        const ptr: *u8 = @ptrFromInt(@intFromPtr(self.ptr) + self.usage);
        // std.debug.print("ptr: {*}\n", .{ptr});
        if (self.parent) |_|  {
            util.tracyc.___tracy_emit_memory_free_named(ptr, 0, self.name.ptr);
        }
    }

    pub fn deinit(self: *Arena) void {
        const zone = util.tracy.initZone(@src(), .{.name = "Arena::deinit"});
        defer zone.deinit();

        if (self.parent) |arena| {
            self.destroy(Arena, 1);
            arena.destroy(u8, self.capacity);
        } else {
            const buffer: [*]u8 = @ptrCast(self.ptr);
            mfree(buffer[0..self.capacity]);
        }

        util.print(.Debug, "Deinit - {}: ({}:{})/{}", .{
            self.name,
            self.usage,
            self.max_usage,
            self.capacity,
        });

        if (self.usage > 0) @panic("Free on used arena");
    }
};
