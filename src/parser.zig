const std = @import("std");
const Vec = @import("collections.zig").Vec;
const FixedVec = @import("collections.zig").FixedVec;
const Iter = @import("collections.zig").Iter;
const Token = @import("tokenizer.zig").Token;

const Allocator = std.mem.Allocator;

const Name = struct {
    parent: *Node,
    value: []const u8,

    fn init(parent: *Node, value: []const u8) Name {
        return Name {
            .parent = parent,
            .value = value,
        };
    }
};

const ExpressionLiteral = struct {
    parent: *Node,
    name: []const u8,

    fn init(parent: *Node, name: []const u8) ExpressionLiteral {
        return ExpressionLiteral {
            .parent = parent,
            .name = name,
        };
    }

    fn push(self: *Expression, token: *const Token, last: *const Token) void {
        _ = self;
        _ = token;
        _ = last;
    }
};

const ExpressionIdentifier = struct {
    name: []const u8,
    parent: *Node,

    fn init(parent: *Node, name: []const u8) ExpressionIdentifier {
        return ExpressionIdentifier {
            .parent = parent,
            .name = name,
        };
    }

    fn push(self: *Expression, token: *const Token, last: *const Token) void {
        _ = self;
        _ = token;
        _ = last;
    }
};

const ExpressionCall = struct {
    name: []const u8,
    parent: *Node,

    fn init(parent: *Node, name: []const u8) ExpressionCall {
        return ExpressionCall {
            .parent = parent,
            .name = name,
        };
    }

    fn push(self: *Expression, token: *const Token, last: *const Token) void {
        _ = self;
        _ = token;
        _ = last;
    }
};

const BinaryOperation = enum { Sum, Multiplication };
const ExpressionBinary = struct {
    parent: *Node,
    op: BinaryOperation,
    left: *Expression,
    right: *Expression,

    fn init(parent: *Node, op: BinaryOperation, left: *Expression, right: *Expression) ExpressionBinary {
        return ExpressionBinary {
            .parent = parent,
            .op = op,
            .left = left,
            .right = right,
        };
    }
};

const ExpressionType = enum { literal, identifier, binary, call };
const Expression = union(ExpressionType) {
    literal: ExpressionLiteral,
    identifier: ExpressionIdentifier,
    binary: ExpressionBinary,
    call: ExpressionCall,

    fn init(paren: *Node, token: *const Token) error { InvalidToken }!Expression {
        return switch (token.id) {
            .Identifier => Expression { .identifier = .{ .parent = paren, .value = token.value.? } },
            .Number, .String => Expression { .literal = .{ .parent = paren, .value = token.value.? } },
            else => return error.InvalidToken,
        };
    }

    fn parent(self: *Expression) *Node {
        return switch (self.*) {
            .literal => |expression| expression.parent,
            .identifier => |expression| expression.parent,
            .binary => |expression| expression.parent,
            .call => |expression| expression.parent,
        };
    }

    fn set_parent(self: *Expression, paren: *Node) void {
        switch (self.*) {
            .literal => |*expression| expression.parent = paren,
            .identifier => |*expression| expression.parent = paren,
            .binary => |*expression| expression.parent = paren,
            .call => |*expression| expression.parent = paren,
        }
    }

    fn push(self: *Node, token: *const Token, last: *Token) error { InvalidToken }!?*Node {
        const expression = &self.expression;

        switch (expression) {
            .literal => ExpressionLiteral.push(expression, token, last),
            .identifier => ExpressionIdentifier.push(expression, token, last),
            .call => ExpressionCall.push(expression, token, last),
            .binary => ExpressionBinary.push(expression, token, last),
        }
    }
};

const NodeParent = struct {
    childs: Vec(Node),

    fn init(allocator: Allocator) NodeParent {
        return NodeParent {
            .childs = Vec(Node).init(2, allocator) catch @panic("out of memory"),
        };
    }

    fn push(self: *NodeParent, parent: *Node, typ: NodeType) *Node {
        const node = switch (typ) {
            .function => Node {.function = .{ .parent = parent, .handle = NodeParent.init(self.childs.allocator) } },
            .parameter => Node { .parameter = .{ .parent = parent, .handle = NodeParent.init(self.childs.allocator) } },
            .ret => Node { .ret = .{ .parent = parent, .handle = NodeParent.init(self.childs.allocator), } },
            .let => Node { .let = .{ .parent = parent, .handle = NodeParent.init(self.childs.allocator), .mutable = false } },
            else => unreachable,
        };

        if (self.childs.flagged_push(node) catch @panic("out of memory")) {
            for (self.childs.items) |*child| {
                if (child.childs()) |childs| {
                    for (childs) |*grand_child| {
                        grand_child.set_parent(child);
                    }
                }
            }
        }

        return self.childs.last_mut() catch unreachable;
    }

    fn deinit(self: *const NodeParent) void {
        for (self.childs.items) |child| {
            child.deinit();
        }

        self.childs.deinit();
    }
};

const Type = Name;
const Parameter = struct {
    parent: *Node,
    handle: NodeParent,

    fn push(self: *Node, token: *const Token, last: *const Token) error { InvalidToken }!?*Node{
        const childs = &self.parameter.handle.childs;
        switch (token.id) {
            .Colon, .ParentesisClose => return null,
            .DoubleColon => {},
            .Identifier => {
                if (last.id == .DoubleColon) childs.push(Node { .name = Name.init(self, token.value.?) }) catch @panic("out of memory")
                else if (last.id == .ParentesisOpen or last.id == .Colon) childs.push(Node { .typ = Type.init(self, token.value.?) }) catch @panic("out of memory");
            },
            else => return error.InvalidToken,
        }

        return self;
    }
};

const Function = struct {
    parent: *Node,
    handle: NodeParent,

    fn push(self: *Node, token: *const Token, last: *const Token) error { InvalidToken }!?*Node{
        const handle = &self.function.handle;
        switch (token.id) {
            .ParentesisClose, .DoubleColon, .CurlyBracketOpen => {},
            .CurlyBracketClose => return null,
            .ParentesisOpen, .Colon => return handle.push(self, .parameter),
            .Let => return handle.push(self, .let),
            .SemiColon => {},
            .Return => return handle.push(self, .ret),
            .Identifier => {
                if (last.id == .DoubleColon) handle.childs.push(Node { .typ = Type.init(self, token.value.?) }) catch @panic("out of memory")
                else handle.childs.push(Node { .name = Name.init(self, token.value.?) }) catch @panic("out of memory");
            },
            else => return error.InvalidToken
        }

        return self;
    }
};

const Let = struct {
    parent: *Node,
    handle: NodeParent,
    mutable: bool,

    fn push(self: *Node, token: *const Token, last: *const Token) error { InvalidToken }!?*Node{
        const handle = &self.let.handle;
        switch (token.id) {
            .SemiColon => return null,
            .Mut => self.let.mutable = true,
            .Equal => {},
            .Identifier, .Number, => {
                if (last.id == .Mut or handle.childs.len() == 0) handle.childs.push(Node { .name = Name.init(self, token.value.?) }) catch @panic("out of memory")
                else handle.childs.push(Node { .expression = try Expression.init(self, token) }) catch @panic("out of memory");
            },
            else => return error.InvalidToken,
        }

        return self;
    }
};

const Return = struct {
    parent: *Node,
    handle: NodeParent,

    fn push(self: *Node, token: *const Token, _: *const Token) error { InvalidToken }!?*Node{
        const handle = &self.ret.handle;
        switch (token.id) {
            .SemiColon => return null,
            .Identifier, .Number, => handle.childs.push(Node { .expression = try Expression.init(self, token) }) catch @panic("out of memory"),
            else => return error.InvalidToken,
        }

        return self;
    }
};

const Root = struct {
    handle: NodeParent,

    fn push(self: *Node, token: *const Token, _: *const Token) error { InvalidToken }!?*Node{
        switch (token.id) {
            .Function => return self.root.handle.push(self, .function),
            else => return error.InvalidToken,
        }

        return self;
    }
};

const NodeType = enum {
    root,
    function,
    parameter,
    expression,
    let,
    name,
    typ,
    ret,
};

pub const Node = union(NodeType) {
    root: Root,
    function: Function,
    parameter: Parameter,
    expression: Expression,
    let: Let,
    name: Name,
    typ: Type,
    ret: Return,

    fn push(self: *Node, token: *const Token, last: *const Token) error { InvalidToken }!?*Node {
        return switch (self.*) {
            .root => try Root.push(self, token, last),
            .function => try Function.push(self, token, last),
            .parameter => try Parameter.push(self, token, last),
            .let => try Let.push(self, token, last),
            .ret => try Return.push(self, token, last),
            .expression, .typ, .name => unreachable,
        };
    }

    pub fn deinit(self: *const Node) void {
        switch (self.*) {
            .root => |node| node.handle.deinit(),
            .function => |node| node.handle.deinit(),
            .parameter => |node| node.handle.deinit(),
            .let => |node| node.handle.deinit(),
            .ret => |node| node.handle.deinit(),
            .expression, .typ, .name => {},
        }
    }

    fn childs(self: *Node) ?[]Node {
        return switch (self.*) {
            .root => |node| node.handle.childs.items,
            .function => |node| node.handle.childs.items,
            .parameter => |node| node.handle.childs.items,
            .let => |node| node.handle.childs.items,
            .ret => |node| node.handle.childs.items,
            .expression, .typ, .name => null,
        };
    }

    fn set_parent(self: *Node, paren: *Node) void {
        switch (self.*) {
            .function => |*node| node.parent = paren,
            .parameter => |*node| node.parent = paren,
            .let => |*node| node.parent = paren,
            .ret => |*node| node.parent = paren,
            .expression => |*node| node.set_parent(paren),
            .typ => |*node| node.parent = paren,
            .name => |*node| node.parent = paren,
            .root => unreachable,
        }
    }

    fn parent(self: *Node) ?*Node {
        return switch (self.*) {
            .root => null,
            .function => |node| node.parent,
            .expression => |*node| node.parent(),
            .parameter => |node| node.parent,
            .let => |node| node.parent,
            .ret => |node| node.parent,
            .typ => |node| node.parent,
            .name => |node| node.parent,
        };
    }
};

pub fn init(tokens: Vec(Token), allocator: Allocator) !*Node {
    const root = allocator.create(Node) catch @panic("out of memory");
    root.* = Node { .root = .{ .handle = NodeParent.init(allocator) } };

    var current_node: ?*Node = root;

    var iter = tokens.iter();

    var last: *const Token = &Token { .id = .Root, .value = null };
    while (iter.next()) |token| {
        if (current_node) |node| current_node = try node.push(token, last) orelse node.parent()
        else break;

        last = token;
    }

    return root;
}

// const NodeType = enum {
//     VariableName,
//     Type,
//     Symbol,
//     Mut,
//     Literal,

//     Root,
//     Function,
//     FunctionName,
//     FunctionParameters,
//     FunctionBody,
//     Parameter,
//     Let,
//     Match,
//     MatchBody,
//     MatchCase,
//     Expression,
//     Return,
// };

// pub const Node = struct {
//     childs: Vec(Node),
//     token: Token,
//     parent: ?*Node,
//     typ: NodeType,
//     next_child: u32,

//     fn init(
//         typ: NodeType,
//         token: *const Token,
//         parent: ?*Node,
//         len: u32,
//         allocator: Allocator
//     ) !Node {
//          return Node {
//             .childs = if (len == 0) undefined else try Vec(Node).init(len, allocator),
//             .token = Token {
//                 .id = token.id,
//                 .value = token.value,
//             },
//             .parent = parent,
//              .typ = typ,
//              .next_child = 0,
//         };
//     }

//     fn childless(self: *const Node) bool {
//         switch (self.typ) {
//             .VariableName, .FunctionName, .Type, .Symbol, .Mut => return true,
//             .Expression => return self.token.id == .String,
//             else => {},
//         }

//         return false;
//     }

//     pub fn next_expression(self: *Node) ?*Node {
//         for (self.childs.items) |*exp| {
//             if (exp.typ == .Expression) return exp;
//         }

//         return null;
//     }

//     pub fn is_literal(self: *Node) bool {
//         if (self.typ != .Expression) return false;
//         if (self.childs.len() == 0) return self.token.typ == .Number;

//         for (self.childs.items) |last| {
//             if (!last.is_literal()) return false;
//         }

//         return true;
//     }

//     fn reajust(self: *Node) void {
//         for (self.childs.items) |*last| {
//             if (last.childless()) continue;

//             for (last.childs.items) |*grand_child| {
//                 grand_child.parent = last;
//             }
//         }
//     }

//     fn push_zero(self: *Node, typ: NodeType, token: *const Token) !void {
//         if (try self.childs.flagged_push(try Node.init(typ, token, self, 0, self.childs.allocator))) self.reajust();
//     }

//     fn push_child(self: *Node, typ: NodeType, token: *const Token, len: u32) !?*Node {
//         if (try self.childs.flagged_push(try Node.init(typ, token, self, len, self.childs.allocator))) self.reajust();
//         return self.childs.last_mut() catch unreachable;
//     }

//     fn push(self: *Node, token: *const Token) !?*Node {
//         switch (self.typ) {
//             .Root => {
//                 switch (token.id) {
//                     .Function => return try self.push_child(.Function, token, 4),
//                     .CurlyBracketClose => return self,
//                     else => return null,
//                 }
//             },
//             .Function => {
//                 switch (token.id) {
//                     .Identifier => {
//                         if (self.childs.len() == 0) {
//                             try self.push_zero(.FunctionName, token);
//                         } else {
//                             const last = try self.childs.last();

//                             if (last.token.id == .DoubleColon) try self.push_zero(.Type, token)
//                             else return error.InvalidToken;
//                         }
//                     },
//                     .ParentesisOpen => return try self.push_child(.FunctionParameters, token, 2),
//                     .ParentesisClose => {},
//                     .DoubleColon => try self.push_zero(.Symbol, token),
//                     .CurlyBracketOpen => return try self.push_child(.FunctionBody, token, 3),
//                     .CurlyBracketClose => return null,
//                     else => return error.InvalidToken,
//                 }
//             },
//             .FunctionParameters => {
//                 switch (token.id) {
//                     .Colon => try self.push_zero(.Symbol, token),
//                     .Identifier => return try self.push_child(.Parameter, token, 1),
//                     .ParentesisClose => return null,
//                     else => return error.InvalidToken,
//                 }
//             },
//             .Parameter => {
//                 switch (token.id) {
//                     .DoubleColon => {
//                         try self.push_zero(.Symbol, token);
//                     },
//                     .Identifier => {
//                         if (self.childs.len() == 0) return error.InvalidToken;

//                         const last = try self.childs.last();
//                         if (last.token.id != .DoubleColon) return error.InvalidToken;
//                         try self.push_zero(.Type, token);
//                     },
//                     .ParentesisClose, .Colon => return null,
//                     else => return error.InvalidToken,
//                 }
//             },
//             .FunctionBody => {
//                 switch (token.id) {
//                     .Return => return try self.push_child(.Return, token, 2),
//                     .Let => return try self.push_child(.Let, token, 3),
//                     .Match => return try self.push_child(.Match, token, 2),
//                     .SemiColon => {},
//                     .CurlyBracketClose => return null,
//                     else => return error.InvalidToken,
//                 }
//             },
//             .Let => {
//                 switch (token.id) {
//                     .Mut => try self.push_zero(.Mut, token),
//                     .Equal => {
//                         const last = try self.childs.last();
//                         if (last.token.id == .Identifier) try self.push_zero(.Symbol, token)
//                         else return error.InvalidToken;
//                     },
//                     .Number => {
//                         const last = try self.childs.last();
//                         if (last.token.id == .Equal) return try self.push_child(.Expression, token, 2)
//                         else return error.InvalidToken;
//                     },
//                     .String => try self.push_zero(.Expression, token),
//                     .DoubleColon => try self.push_zero(.Symbol, token),
//                     .Identifier => {
//                         if (self.childs.len() > 1) {
//                             const last = try self.childs.last();

//                             if (last.token.id == .Mut) try self.push_zero(.VariableName, token)
//                             else if (last.token.id == .DoubleColon) try self.push_zero(.Type, token)
//                             else if (last.token.id == .Equal) return try self.push_child(.Expression, token, 1)
//                             else return error.InvalidToken;
//                         } else {
//                             try self.push_zero(.VariableName, token);
//                         }
//                     },
//                     .DoubleQuote => {},
//                     .SemiColon => return null,
//                     else => return error.InvalidToken,
//                 }
//             },
//             .Return => {
//                 switch (token.id) {
//                     .Identifier, .Number => return try self.push_child(.Expression, token, 2),
//                     .String => try self.push_zero(.Expression, token),
//                     .DoubleQuote => {},
//                     .SemiColon => return null,
//                     else => return error.InvalidToken,
//                 }
//             },
//             .Expression => {
//                 switch (token.id) {
//                     .Identifier, .Number => {
//                         if (self.childs.len() == 0) return error.InvalidToken;

//                         const last = try self.childs.last();
//                         if (!last.token.id.is_binary_operator()) return error.InvalidToken;
//                         return try self.push_child(.Expression, token, 1);
//                     },
//                     .Sum, .Multiplication => try self.push_zero(.Symbol, token),
//                     .SemiColon => return null,
//                     else => return error.InvalidToken,
//                 }
//             },
//             .Match => {
//                 switch (token.id) {
//                     .Identifier => try self.push_zero(.VariableName, token),
//                     .CurlyBracketOpen => return try self.push_child(.MatchBody, token, 3),
//                     .CurlyBracketClose => return self.parent,
//                     else => return error.InvalidToken,
//                 }
//             },
//             .MatchBody => {
//                 switch (token.id) {
//                     .Number, .Identifier => return try self.push_child(.MatchCase, token, 2),
//                     .SemiColon => {},
//                     .CurlyBracketClose => return null,
//                     else => return error.InvalidToken,
//                 }
//             },
//             .MatchCase => {
//                 switch (token.id) {
//                     .Equal => {
//                         if (self.childs.len() != 0) return error.InvalidToken
//                         else try self.push_zero(.Symbol, token);
//                     },
//                     .Number, .Identifier => return try self.push_child(.Expression, token, 1),
//                     .SemiColon => return null,
//                     else => return error.InvalidToken,
//                 }
//             },

//             else => unreachable,
//         }

//         return self;
//     }

//     pub fn next(self: *Node) ?*Node {
//         var current_node: ?*Node = self;
//         if (self.childless()) current_node = self.parent;

//         while (current_node) |node| {
//             current_node = node.childs.get_mut(node.next_child) catch {
//                 current_node = node.parent;
//                 continue;
//             };

//             node.next_child += 1;
//             return current_node;
//         }

//         return null;
//     }

//     fn iter(self: *const Node, f: fn (*const Node) void ) void {
//         for (self.childs.items) |last| {
//             last.iter(f);
//         }

//         f(self);
//     }

//     pub fn deinit(self: *const Node) void {
//         self.iter(free);
//     }
// };

// pub fn init(tokens: Vec(Token), allocator: Allocator) !*Node {
//     var iter = tokens.iter();
//     const root = try allocator.create(Node);

//     root.* = try Node.init(.Root, &Token { .id = .Root, .value = null }, null, 2, allocator);
//     var current_node: *Node = root;

//     out: while (iter.next()) |token| {
//         while (true) {
//             if (try current_node.push(token)) |node| {
//                 current_node = node;
//                 break;
//             }

//             current_node = current_node.parent orelse break :out;
//         }
//     }

//     return root;
// }

// fn free(node: *const Node) void {
//     switch (node.typ) {
//         .VariableName, .FunctionName, .Type, .Symbol, .Mut => return,
//         .Expression => if (node.token.id == .String) return,
//         else => {}
//     }

//     node.childs.deinit();
// }

// fn print(node: *const Node) void {
//     std.debug.print("node: {} | {}\n", .{node.typ, node.token.id});
// }
