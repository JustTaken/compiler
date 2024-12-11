const collections = @import("collections");
const mem = @import("mem");
const util = @import("util");
const constant = @import("constant.zig");

const raw_node = @import("../node/mod.zig");

const Type = struct {
    fields: util.Index,
    size: util.Index,
    alignment: util.Index,

    const Field = struct {
        name: util.Index,
        typ: util.Index,

        fn new(name: util.Index, typ: util.Index) Field {
            const zone = util.tracy.initZone(@src(), .{.name = "Type::Field::new"});
            defer zone.deinit();

            return Field {
                .name = name,
                .typ = typ,
            };
        }
    };

    fn new(fields: util.Index, size: util.Index, alignment: util.Index) Type {
        const zone = util.tracy.initZone(@src(), .{.name = "Type::new"});
        defer zone.deinit();

        return Type {
            .fields = fields,
            .size = size,
            .alignment = alignment,
        };
    }
};

const TypeManager = struct {
    types: collections.Vec(Type),
    type_map: collections.StringMap(util.Index),
    fields: collections.SliceManager(Type.Field),

    fn new(allocator: *mem.Arena) error{OutOfMemory}!TypeManager {
        const zone = util.tracy.initZone(@src(), .{.name = "TypeManager::new"});
        defer zone.deinit();

        var self: TypeManager = undefined;

        self.types = try collections.Vec(Type).new(20, allocator);
        errdefer self.types.deinit(allocator);

        self.type_map = try collections.StringMap(util.Index).new(20, allocator);
        errdefer self.type_map.deinit(allocator);

        self.fields = try collections.SliceManager(Type.Field).new(40, allocator);
        errdefer self.fields.deinit(allocator);

        return self;
    }

    fn get(self: *TypeManager, name: util.Index, words: collections.SliceManager(u8)) ?util.Index {
        const zone = util.tracy.initZone(@src(), .{.name = "TypeManager::get"});
        defer zone.deinit();

        return self.type_map.get(name, words) catch @panic("TODO");
    }

    fn check_not_defined(self: *TypeManager, name: util.Index, words: collections.SliceManager(u8)) error{AlreadyDefined}!void {
        const zone = util.tracy.initZone(@src(), .{.name = "TypeManager::check_not_defined"});
        defer zone.deinit();

        _ = self.type_map.get(name, words) catch @panic("TODO") orelse return;

        return error.AlreadyDefined;
    }

    fn push(self: *TypeManager, name: util.Index, typ: Type, words: collections.SliceManager(u8)) error{AlreadyDefined}!void {
        const zone = util.tracy.initZone(@src(), .{.name = "TypeManager::push"});
        defer zone.deinit();

        try self.check_not_defined(name, words);

        const i: util.Index = @intCast(self.types.len);

        self.types.push(typ) catch @panic("TODO");
        self.type_map.push(name, i, words) catch @panic("TODO");
    }

    fn deinit(self: *TypeManager, allocator: *mem.Arena) void {
        const zone = util.tracy.initZone(@src(), .{.name = "TypeManager::deinit"});
        defer zone.deinit();

        self.fields.deinit(allocator);
        self.type_map.deinit(allocator);
        self.types.deinit(allocator);
    }
};

pub const Checker = struct {
    type_manager: TypeManager,
    arena: *mem.Arena,

    pub fn new(allocator: *mem.Arena) error{OutOfMemory}!Checker {
        const zone = util.tracy.initZone(@src(), .{.name = "Checker::new"});
        defer zone.deinit();

        var self: Checker = undefined;

        self.arena = try allocator.child("Checker", mem.PAGE_SIZE);
        errdefer self.arena.deinit();

        self.type_manager = try TypeManager.new(self.arena);
        errdefer self.type_manager.deinit(self.arena);

        return self;
    }

    pub fn next(self: *Checker, tree: *raw_node.Tree, words: collections.SliceManager(u8)) void {
        const zone = util.tracy.initZone(@src(), .{.name = "Checker::next"});
        defer zone.deinit();

        const nod = tree.nodes.get_back(0) catch @panic("TODO");

        switch (nod) {
            .Type => self.typ(tree, words),
            .Procedure => self.procedure(tree, words),
            else => @panic("TODO"),
        }
    }

    fn typ(self: *Checker, tree: *raw_node.Tree, words: collections.SliceManager(u8)) void {
        const zone = util.tracy.initZone(@src(), .{.name = "Checker::typ"});
        defer zone.deinit();

        const value = tree.nodes.pop() catch unreachable;

        if (tree.type_fields.len == 0) {
            const size_node = tree.nodes.pop() catch @panic("TODO");
            const size: util.Index = @intCast(util.parse(words.get(size_node.Number.value) catch @panic("TODO")));
            const alignment = size;
            const fields = self.type_manager.fields.push(&.{}) catch @panic("TODO");

            self.type_manager.push(value.Type.name, Type.new(fields, size, alignment), words) catch @panic("TODO");

            return;
        }

        const fields = self.type_manager.fields.start() catch @panic("TODO");

        var size: util.Index = 0;
        var alignment: util.Index = 0;

        for (tree.type_fields.offset(0) catch unreachable) |field| {
            const field_type_index = self.type_manager.get(field.typ, words) orelse @panic("TODO: implement usage of undeinfed type");
            const field_type = self.type_manager.types.get(field_type_index) catch @panic("TODO");

            size += field_type.size;
            alignment = @intCast(util.max(field_type.alignment, alignment));

            self.type_manager.fields.extend(Type.Field.new(field.name, field_type_index)) catch @panic("TODO");
        }

        self.type_manager.push(value.Type.name, Type.new(fields, size, alignment), words) catch @panic("TODO");
    }

    fn procedure(self: *Checker, tree: *raw_node.Tree, words: collections.SliceManager(u8)) void {
        _ = self;
        _ = words;
        _ = tree;
    }

    pub fn deinit(self: *Checker) void {
        self.type_manager.deinit(self.arena);
        self.arena.deinit();
    }
};
