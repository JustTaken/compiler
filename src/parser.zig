const std = @import("std");
const Vec = @import("collections.zig").Vec;
const FixedVec = @import("collections.zig").FixedVec;
const Iter = @import("collections.zig").Iter;
const Token = @import("tokenizer.zig").Token;

const Allocator = std.mem.Allocator;

const NodeType = enum {
    VariableName,
    Type,
    Symbol,
    Mut,
    Literal,

    Root,
    Function,
    FunctionName,
    FunctionParameters,
    FunctionBody,
    Parameter,
    Let,
    Switch,
    SwitchBody,
    SwitchCase,
    Expression,
    Return,
};

pub const Node = struct {
    childs: Vec(Node),
    token: Token,
    parent: ?*Node,
    typ: NodeType,
    next_child: u32,

    fn init(
        typ: NodeType,
        token: *const Token,
        parent: ?*Node,
        len: u32,
        allocator: Allocator
    ) !Node {
         return Node {
            .childs = if (len == 0) undefined else try Vec(Node).init(len, allocator),
            .token = Token {
                .id = token.id,
                .value = token.value,
            },
            .parent = parent,
             .typ = typ,
             .next_child = 0,
        };
    }

    fn childless(self: *const Node) bool {
        switch (self.typ) {
            .VariableName, .FunctionName, .Type, .Symbol, .Mut => return true,
            .Expression => return self.token.id == .String,
            else => {},
        }

        return false;
    }

    pub fn next_expression(self: *Node) ?*Node {
        for (self.childs.items) |*exp| {
            if (exp.typ == .Expression) return exp;
        }

        return null;
    }

    pub fn is_literal(self: *Node) bool {
        if (self.typ != .Expression) return false;
        if (self.childs.len() == 0) return self.token.typ == .Number;

        for (self.childs.items) |last| {
            if (!last.is_literal()) return false;
        }

        return true;
    }

    fn reajust(self: *Node) void {
        for (self.childs.items) |*last| {
            if (last.childless()) continue;

            for (last.childs.items) |*grand_child| {
                grand_child.parent = last;
            }
        }
    }

    fn push_zero(self: *Node, typ: NodeType, token: *const Token) !void {
        if (try self.childs.flagged_push(try init(typ, token, self, 0, self.childs.allocator))) self.reajust();
    }

    fn push_child(self: *Node, typ: NodeType, token: *const Token, len: u32) !?*Node {
        if (try self.childs.flagged_push(try init(typ, token, self, len, self.childs.allocator))) self.reajust();
        return self.childs.last_mut() catch unreachable;
    }

    fn push(self: *Node, token: *const Token) !?*Node {
        switch (self.typ) {
            .Root => {
                switch (token.id) {
                    .Function => return try self.push_child(.Function, token, 4),
                    .CurlyBracketClose => return self,
                    else => return null,
                }
            },
            .Function => {
                switch (token.id) {
                    .Identifier => {
                        if (self.childs.len() == 0) {
                            try self.push_zero(.FunctionName, token);
                        } else {
                            const last = try self.childs.last();

                            if (last.token.id == .DoubleColon) try self.push_zero(.Type, token)
                            else return error.InvalidToken;
                        }
                    },
                    .ParentesisOpen => return try self.push_child(.FunctionParameters, token, 2),
                    .ParentesisClose => {},
                    .DoubleColon => try self.push_zero(.Symbol, token),
                    .CurlyBracketOpen => return try self.push_child(.FunctionBody, token, 3),
                    .CurlyBracketClose => return null,
                    else => return error.InvalidToken,
                }
            },
            .FunctionParameters => {
                switch (token.id) {
                    .Colon => try self.push_zero(.Symbol, token),
                    .Identifier => return try self.push_child(.Parameter, token, 1),
                    .ParentesisClose => return null,
                    else => return error.InvalidToken,
                }
            },
            .Parameter => {
                switch (token.id) {
                    .DoubleColon => {
                        try self.push_zero(.Symbol, token);
                    },
                    .Identifier => {
                        if (self.childs.len() == 0) return error.InvalidToken;

                        const last = try self.childs.last();
                        if (last.token.id != .DoubleColon) return error.InvalidToken;
                        try self.push_zero(.Type, token);
                    },
                    .ParentesisClose, .Colon => return null,
                    else => return error.InvalidToken,
                }
            },
            .FunctionBody => {
                switch (token.id) {
                    .Return => return try self.push_child(.Return, token, 2),
                    .Let => return try self.push_child(.Let, token, 3),
                    .Switch => return try self.push_child(.Switch, token, 2),
                    .SemiColon => {},
                    .CurlyBracketClose => return null,
                    else => return error.InvalidToken,
                }
            },
            .Let => {
                switch (token.id) {
                    .Mut => try self.push_zero(.Mut, token),
                    .Equal => {
                        const last = try self.childs.last();
                        if (last.token.id == .Identifier) try self.push_zero(.Symbol, token)
                        else return error.InvalidToken;
                    },
                    .Number => {
                        const last = try self.childs.last();
                        if (last.token.id == .Equal) return try self.push_child(.Expression, token, 2)
                        else return error.InvalidToken;
                    },
                    .String => try self.push_zero(.Expression, token),
                    .DoubleColon => try self.push_zero(.Symbol, token),
                    .Identifier => {
                        if (self.childs.len() > 1) {
                            const last = try self.childs.last();

                            if (last.token.id == .Mut) try self.push_zero(.VariableName, token)
                            else if (last.token.id == .DoubleColon) try self.push_zero(.Type, token)
                            else if (last.token.id == .Equal) return try self.push_child(.Expression, token, 1)
                            else return error.InvalidToken;
                        } else {
                            try self.push_zero(.VariableName, token);
                        }
                    },
                    .DoubleQuote => {},
                    .SemiColon => return null,
                    else => return error.InvalidToken,
                }
            },
            .Return => {
                switch (token.id) {
                    .Identifier, .Number => return try self.push_child(.Expression, token, 2),
                    .String => try self.push_zero(.Expression, token),
                    .DoubleQuote => {},
                    .SemiColon => return null,
                    else => return error.InvalidToken,
                }
            },
            .Expression => {
                switch (token.id) {
                    .Identifier, .Number => {
                        if (self.childs.len() == 0) return error.InvalidToken;

                        const last = try self.childs.last();
                        if (!last.token.id.is_binary_operator()) return error.InvalidToken;
                        return try self.push_child(.Expression, token, 1);
                    },
                    .Sum, .Multiplication => try self.push_zero(.Symbol, token),
                    .SemiColon => return null,
                    else => return error.InvalidToken,
                }
            },
            .Switch => {
                switch (token.id) {
                    .Identifier => try self.push_zero(.VariableName, token),
                    .CurlyBracketOpen => return try self.push_child(.SwitchBody, token, 3),
                    .CurlyBracketClose => return self.parent,
                    else => return error.InvalidToken,
                }
            },
            .SwitchBody => {
                switch (token.id) {
                    .Number, .Identifier => return try self.push_child(.SwitchCase, token, 2),
                    .SemiColon => {},
                    .CurlyBracketClose => return null,
                    else => return error.InvalidToken,
                }
            },
            .SwitchCase => {
                switch (token.id) {
                    .Equal => {
                        if (self.childs.len() != 0) return error.InvalidToken
                        else try self.push_zero(.Symbol, token);
                    },
                    .Number, .Identifier => return try self.push_child(.Expression, token, 1),
                    .SemiColon => return null,
                    else => return error.InvalidToken,
                }
            },

            else => unreachable,
        }

        return self;
    }

    pub fn next(self: *Node) ?*Node {
        var current_node: ?*Node = self;
        if (self.childless()) current_node = self.parent;

        while (current_node) |node| {
            current_node = node.childs.get_mut(node.next_child) catch {
                current_node = node.parent;
                continue;
            };

            node.next_child += 1;
            return current_node;
        }

        return null;
    }

    fn iter(self: *const Node, f: fn (*const Node) void ) void {
        for (self.childs.items) |last| {
            last.iter(f);
        }

        f(self);
    }
};

pub const Tree = struct {
    root: *Node,
    allocator: Allocator,

    pub fn init(tokens: Vec(Token), allocator: Allocator) !Tree {
        var iter = tokens.iter();
        const root = try allocator.create(Node);
        root.* = try Node.init(.Root, &Token { .id = .Root, .value = null }, null, 2, allocator);
        var current_node: *Node = root;

        out: while (iter.next()) |token| {
            while (true) {
                if (try current_node.push(token)) |node| {
                    current_node = node;
                    break;
                }

                current_node = current_node.parent orelse break :out;
            }
        }

        return Tree {
            .root = root,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *const Tree) void {
        self.root.iter(free);
        self.allocator.destroy(self.root);
    }
};

fn free(node: *const Node) void {
    switch (node.typ) {
        .VariableName, .FunctionName, .Type, .Symbol, .Mut => return,
        .Expression => if (node.token.id == .String) return,
        else => {}
    }

    node.childs.deinit();
}

fn print(node: *const Node) void {
    std.debug.print("node: {} | {}\n", .{node.typ, node.token.id});
}
