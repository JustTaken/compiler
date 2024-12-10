const collections = @import("collections");
const mem = @import("mem");
const util = @import("util");
const constant = @import("constant.zig");

pub const TypeChecker = struct {
    arena: *mem.Arena,

    pub fn new(allocator: *mem.Arena) error{OutOfMemory}!TypeChecker {
        var self: TypeChecker = undefined;

        self.arena = try allocator.child("TypeChecker", mem.PAGE_SIZE);
        errdefer self.arena.deinit();

        return self;
    }

    pub fn deinit(self: *TypeChecker) void {
        self.arena.deinit();
    }
};
