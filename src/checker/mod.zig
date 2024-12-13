const collections = @import("collections");
const mem = @import("mem");
const util = @import("util");
const constant = @import("constant.zig");

const raw_node = @import("../node/mod.zig");
const node = @import("node.zig");

const Type = struct {
    fields: util.Index,
    size: util.Index,
    alignment: util.Index,

    const Field = struct {
        name: util.Index,
        typ: util.Index,

        fn new(name: util.Index, typ: util.Index) Field {
            const zone = util.tracy.initZone(@src(), .{ .name = "Type::Field::new" });
            defer zone.deinit();

            return Field{
                .name = name,
                .typ = typ,
            };
        }
    };

    fn new(fields: util.Index, size: util.Index, alignment: util.Index) Type {
        const zone = util.tracy.initZone(@src(), .{ .name = "Type::new" });
        defer zone.deinit();

        return Type{
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
        const zone = util.tracy.initZone(@src(), .{ .name = "TypeManager::new" });
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

    fn get(
        self: *TypeManager,
        name: []const u8,
        words: collections.SliceManager(u8),
    ) ?util.Index {
        const zone = util.tracy.initZone(@src(), .{ .name = "TypeManager::get" });
        defer zone.deinit();

        return self.type_map.get(name, words) catch @panic("TODO");
    }

    fn check_not_defined(
        self: *TypeManager,
        name: util.Index,
        words: collections.SliceManager(u8),
    ) error{AlreadyDefined}!void {
        const zone = util.tracy.initZone(@src(), .{ .name = "TypeManager::check_not_defined" });
        defer zone.deinit();

        _ = self.type_map.get(words.get(name) catch @panic("TODO"), words) catch @panic("TODO") orelse return;

        return error.AlreadyDefined;
    }

    fn push(
        self: *TypeManager,
        name: util.Index,
        typ: Type,
        words: collections.SliceManager(u8),
    ) error{AlreadyDefined}!void {
        const zone = util.tracy.initZone(@src(), .{ .name = "TypeManager::push" });
        defer zone.deinit();

        try self.check_not_defined(name, words);

        const i: util.Index = @intCast(self.types.len);

        self.types.push(typ) catch @panic("TODO");
        self.type_map.push(name, i, words) catch @panic("TODO");
    }

    fn deinit(self: *TypeManager, allocator: *mem.Arena) void {
        const zone = util.tracy.initZone(@src(), .{ .name = "TypeManager::deinit" });
        defer zone.deinit();

        self.fields.deinit(allocator);
        self.type_map.deinit(allocator);
        self.types.deinit(allocator);
    }
};

const Procedure = struct {
    parameters: util.Index,
    return_type: util.Index,

    const Parameter = struct {
        name: util.Index,
        typ: util.Index,

        fn new(name: util.Index, typ: util.Index) Parameter {
            const zone = util.tracy.initZone(@src(), .{ .name = "Procedure::Parameter::new" });
            defer zone.deinit();

            return Parameter {
                .name = name,
                .typ = typ,
            };
        }
    };

    fn new(parameters: util.Index, return_type: util.Index) Procedure {
        const zone = util.tracy.initZone(@src(), .{ .name = "Procedure::new" });
        defer zone.deinit();

        return Procedure {
            .parameters = parameters,
            .return_type = return_type,
        };
    }
};

const ProcedureManager = struct {
    procedures: collections.Vec(Procedure),
    procedure_map: collections.StringMap(util.Index),
    parameters: collections.SliceManager(Procedure.Parameter),

    fn new(allocator: *mem.Arena) error{OutOfMemory}!ProcedureManager {
        const zone = util.tracy.initZone(@src(), .{ .name = "ProcedureManager::new" });
        defer zone.deinit();

        var self: ProcedureManager = undefined;

        self.procedures = try collections.Vec(Procedure).new(20, allocator);
        errdefer self.procedures.deinit(allocator);

        self.procedure_map = try collections.StringMap(util.Index).new(20, allocator);
        errdefer self.procedure_map.deinit(allocator);

        self.parameters = try collections.SliceManager(Procedure.Parameter).new(40, allocator);
        errdefer self.parameters.deinit(allocator);

        return self;
    }

    fn get(
        self: *ProcedureManager,
        name: util.Index,
        words: collections.SliceManager(u8),
    ) ?util.Index {
        const zone = util.tracy.initZone(@src(), .{ .name = "ProcedureManager::get" });
        defer zone.deinit();

        return self.procedure_map.get(name, words) catch @panic("TODO");
    }

    fn check_not_defined(
        self: *ProcedureManager ,
        name: util.Index,
        words: collections.SliceManager(u8),
    ) error{AlreadyDefined}!void {
        const zone = util.tracy.initZone(@src(), .{ .name = "ProcedureManager::check_not_defined" });
        defer zone.deinit();

        _ = self.procedure_map.get(words.get(name) catch @panic("TODO"), words) catch @panic("TODO") orelse return;

        return error.AlreadyDefined;
    }

    fn push(
        self: *ProcedureManager ,
        name: util.Index,
        typ: Procedure,
        words: collections.SliceManager(u8),
    ) error{AlreadyDefined}!void {
        const zone = util.tracy.initZone(@src(), .{ .name = "ProcedureManager::push" });
        defer zone.deinit();

        try self.check_not_defined(name, words);

        const i: util.Index = @intCast(self.procedures.len);

        self.procedures.push(typ) catch @panic("TODO");
        self.procedure_map.push(name, i, words) catch @panic("TODO");
    }

    fn deinit(self: *ProcedureManager , allocator: *mem.Arena) void {
        const zone = util.tracy.initZone(@src(), .{ .name = "ProcedureManager::deinit" });
        defer zone.deinit();

        self.parameters.deinit(allocator);
        self.procedure_map.deinit(allocator);
        self.procedures.deinit(allocator);
    }
};

const Variable = struct {
    value: util.Index,
    typ: util.Index,

    fn new(value: util.Index, typ: util.Index) Variable {
        const zone = util.tracy.initZone(@src(), .{ .name = "Variable::new" });
        defer zone.deinit();

        return Variable {
            .value = value,
            .typ = typ,
        };
    }
};

const NodeManager = struct {
    nodes: collections.Vec(node.Node),
    numbers: collections.Vec(usize),
    variables: collections.StringMap(Variable),

    fn new(allocator: *mem.Arena) error{OutOfMemory}!NodeManager {
        const zone = util.tracy.initZone(@src(), .{ .name = "NodeManager::new" });
        defer zone.deinit();

        var self: NodeManager = undefined;

        self.nodes = try collections.Vec(node.Node).new(20, allocator);
        errdefer self.nodes.deinit(allocator);

        self.numbers = try collections.Vec(usize).new(20, allocator);
        errdefer self.numbers.deinit(allocator);

        self.variables = try collections.StringMap(Variable).new(20, allocator);
        errdefer self.variables.deinit(allocator);

        return self;
    }

    fn check_not_defined(self: *NodeManager, name: util.Index, words: collections.SliceManager(u8)) error{AlreadyDefined}!void {
        const zone = util.tracy.initZone(@src(), .{ .name = "NodeManager::check_not_defined" });
        defer zone.deinit();

        _ = self.variables.get(words.get(name) catch @panic("TODO"), words) catch @panic("TODO") orelse return;

        return error.AlreadyDefined;
    }

    fn get(self: *NodeManager, name: util.Index, words: collections.SliceManager(u8)) ?Variable {
        const zone = util.tracy.initZone(@src(), .{ .name = "NodeManager::get" });
        defer zone.deinit();

        return self.variables.get(words.get(name) catch @panic("TODO"), words) catch @panic("TODO");
    }

    fn push(self: *NodeManager, name: util.Index, variable: Variable, words: collections.SliceManager(u8)) error{AlreadyDefined}!void {
        const zone = util.tracy.initZone(@src(), .{ .name = "NodeManager::push" });
        defer zone.deinit();

        try self.check_not_defined(name, words);

        self.variables.push(name, variable, words) catch @panic("TODO");
    }

    fn deinit(self: *NodeManager, allocator: *mem.Arena) void {
        const zone = util.tracy.initZone(@src(), .{ .name = "NodeManager::deinit" });
        defer zone.deinit();

        self.variables.deinit(allocator);
        self.numbers.deinit(allocator);
        self.nodes.deinit(allocator);
    }
};

pub const Checker = struct {
    type_manager: TypeManager,
    procedure_manager: ProcedureManager,
    node_manager: NodeManager,
    words: collections.SliceManager(u8),
    arena: *mem.Arena,

    pub fn new(allocator: *mem.Arena) error{OutOfMemory}!Checker {
        const zone = util.tracy.initZone(@src(), .{ .name = "Checker::new" });
        defer zone.deinit();

        var self: Checker = undefined;

        self.arena = try allocator.child("Checker", mem.PAGE_SIZE);
        errdefer self.arena.deinit();

        self.type_manager = try TypeManager.new(self.arena);
        errdefer self.type_manager.deinit(self.arena);

        self.procedure_manager = try ProcedureManager.new(self.arena);
        errdefer self.procedure_manager.deinit(self.arena);

        self.node_manager = try NodeManager.new(self.arena);
        errdefer self.node_manager.deinit(self.arena);

        self.words = try collections.SliceManager(u8).new(512, self.arena);
        errdefer self.words.deinit(self.arena);

        return self;
    }

    pub fn next(self: *Checker, tree: *raw_node.Tree, words: collections.SliceManager(u8)) void {
        const zone = util.tracy.initZone(@src(), .{ .name = "Checker::next" });
        defer zone.deinit();

        const nod = tree.nodes.get_back(0) catch @panic("TODO");

        switch (nod) {
            .Type => self.typ(tree, words),
            .Procedure => self.procedure(tree, words),
            else => @panic("TODO"),
        }
    }

    fn typ(self: *Checker, tree: *raw_node.Tree, words: collections.SliceManager(u8)) void {
        const zone = util.tracy.initZone(@src(), .{ .name = "Checker::typ" });
        defer zone.deinit();

        const type_node = tree.nodes.pop() catch unreachable;
        util.assert(type_node == .Type);

        const name = self.words.push(words.get(type_node.Type.name) catch @panic("TODO")) catch @panic("TODO");

        if (tree.type_fields.len == 0) {
            const size_node = tree.nodes.pop() catch @panic("TODO");
            const size: util.Index = @intCast(util.parse(words.get(size_node.Number.value) catch @panic("TODO")) catch @panic("TODO"));
            const alignment = size;
            const fields = self.type_manager.fields.push(&.{}) catch @panic("TODO");

            self.type_manager.push(name, Type.new(fields, size, alignment), self.words) catch @panic("TODO");

            return;
        }

        const fields = self.type_manager.fields.start() catch @panic("TODO");

        var size: util.Index = 0;
        var alignment: util.Index = 0;

        for (tree.type_fields.offset(0) catch unreachable) |field| {
            const type_name = words.get(field.typ) catch @panic("TODO");
            const field_type_index = self.type_manager.get(type_name, self.words) orelse @panic("TODO: implement usage of undeinfed type");
            const field_type = self.type_manager.types.get(field_type_index) catch @panic("TODO");

            size += field_type.size;
            alignment = @intCast(util.max(field_type.alignment, alignment));

            const field_name = self.words.push(words.get(field.name) catch @panic("TODO")) catch @panic("TODO");

            self.type_manager.fields.extend(Type.Field.new(field_name, field_type_index)) catch @panic("TODO");
        }

        self.type_manager.push(name, Type.new(fields, size, alignment), self.words) catch @panic("TODO");
    }

    fn procedure(self: *Checker, tree: *raw_node.Tree, words: collections.SliceManager(u8)) void {
        const zone = util.tracy.initZone(@src(), .{ .name = "Checker::procedure" });
        defer zone.deinit();

        const procedure_node = tree.nodes.pop() catch @panic("TODO");
        util.assert(procedure_node == .Procedure);

        const parameters = self.procedure_manager.parameters.start() catch @panic("TODO");

        for (tree.parameters.offset(0) catch unreachable) |parameter| {
            const parameter_type_name = words.get(parameter.typ) catch @panic("TODO");
            const type_index = self.type_manager.get(parameter_type_name, self.words) orelse @panic("TODO: implement usage of undeclared type");
            const name_index = self.words.push(words.get(parameter.name) catch @panic("TODO")) catch @panic("TODO");

            self.procedure_manager.parameters.extend(Procedure.Parameter.new(name_index, type_index)) catch @panic("TODO");
        }

        self.scope(tree, words);

        const return_type = self.extract_type(tree, words) orelse @panic("TODO: implement usage of undeclared type");
        const name_index = self.words.push(words.get(procedure_node.Procedure.name) catch @panic("TODO")) catch @panic("TODO");

        self.procedure_manager.push(name_index, Procedure.new(parameters, return_type), self.words) catch @panic("TODO");
    }

    fn scope(self: *Checker, tree: *raw_node.Tree, words: collections.SliceManager(u8)) void {
        const zone = util.tracy.initZone(@src(), .{ .name = "Checker::scope" });
        defer zone.deinit();

        const scope_node = tree.nodes.pop() catch @panic("TODO");

        util.assert(scope_node == .Scope);

        for (0..scope_node.Scope.len) |_| {
            const next_node = tree.nodes.get_back(0) catch @panic("TODO");

            switch (next_node) {
                .Let => self.let(tree, words),
                .Scope => self.scope(tree, words),
                else => self.expression(tree, words),
            }
        }
    }

    fn let(self: *Checker, tree: *raw_node.Tree, words: collections.SliceManager(u8)) void {
        const zone = util.tracy.initZone(@src(), .{ .name = "Checker::let" });
        defer zone.deinit();

        const let_node = tree.nodes.pop() catch @panic("TODO");

        util.assert(let_node == .Let);

        const expression_index: util.Index = @intCast(self.node_manager.nodes.len);
        self.expression(tree, words);

        const typ_index = self.extract_type(tree, words) orelse @panic("TODO");
        const name_index = self.words.push(words.get(let_node.Let.name) catch @panic("TODO")) catch @panic("TODO");

        self.node_manager.push(name_index, Variable.new(expression_index, typ_index), self.words) catch @panic("TODO");
    }

    fn expression(self: *Checker, tree: *raw_node.Tree, words: collections.SliceManager(u8)) void {
        const zone = util.tracy.initZone(@src(), .{ .name = "Checker::expression" });
        defer zone.deinit();

        const exp = tree.nodes.pop() catch @panic("TODO");

        switch (exp) {
            .Number => |i| {
                const string = words.get(i.value) catch @panic("TODO");
                const value = util.parse(string) catch @panic("TODO");
                const index: util.Index = @intCast(self.node_manager.numbers.len);

                self.node_manager.numbers.push(value) catch @panic("TODO");
                self.node_manager.nodes.push(node.Node { .Number = node.Number.new(index) }) catch @panic("TODO");
            },
            .Binary => |b| {
                self.expression(tree, words);
                self.expression(tree, words);
                self.node_manager.nodes.push(node.Node { .Binary = node.Binary.new(@enumFromInt(@intFromEnum(b.op)))}) catch @panic("TODO");
            },
            .Identifier => |i| {
                const value = self.words.push(words.get(i.name) catch @panic("TODO")) catch @panic("TODO");
                const variable = self.node_manager.get(value, self.words) orelse @panic("TODO: show that variables does not exist");

                self.node_manager.nodes.push(node.Node { .Ref = node.Ref.new(variable.value) }) catch @panic("TODO");
            },
            else => @panic("TODO"),
        }
    }

    fn extract_type(self: *Checker, tree: *raw_node.Tree, words: collections.SliceManager(u8)) ?util.Index {
        const zone = util.tracy.initZone(@src(), .{ .name = "Checker::extract_type" });
        defer zone.deinit();

        const last = tree.nodes.pop() catch @panic("TODO");

        switch (last) {
            .Property => |_| return self.extract_module(tree, words) orelse @panic("TODO"),
            .Identifier => |ident| return self.type_manager.get(words.get(ident.name) catch @panic("TODO"), self.words),
            else => @panic("GOT HERE"),
        }
    }

    fn extract_module(self: *Checker, tree: *raw_node.Tree, words: collections.SliceManager(u8)) ?util.Index {
        const zone = util.tracy.initZone(@src(), .{ .name = "Checker::extract_module" });
        defer zone.deinit();

        _ = self;
        _ = tree;
        _ = words;

        return null;
    }

    pub fn deinit(self: *Checker) void {
        const zone = util.tracy.initZone(@src(), .{ .name = "Checker::deinit" });
        defer zone.deinit();

        self.words.deinit(self.arena);
        self.node_manager.deinit(self.arena);
        self.procedure_manager.deinit(self.arena);
        self.type_manager.deinit(self.arena);
        self.arena.deinit();
    }
};
