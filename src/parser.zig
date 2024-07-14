const std = @import("std");
const Vec = @import("collections.zig").Vec;
const Iter = @import("collections.zig").Iter;
const Token = @import("tokenizer.zig").Token;

const Allocator = std.mem.Allocator;

const NodeType = enum {
    Root,
    Function,
    FunctionParameters,
    FunctionBody,
    Parameter,
    Let,
    Return,
};

const ExpressionType = enum {
    Type,
    Name,
    Symbol,
    Value,
    Literal,
    Keyword,
};

const Expression = struct {
    token: Token,
    typ: ExpressionType,

    fn init(token: *const Token, typ: ExpressionType) Expression {
        return Expression {
            .typ = typ,
            .token = Token {
                .id = token.id,
                .value = token.value,
            },
        };
    }
};

pub const Node = struct {
    childs: Vec(Node),
    expressions: Vec(Expression),
    parent: ?*Node,
    typ: NodeType,

    fn init(
        typ: NodeType, 
        parent: ?*Node, 
        len: u32, 
        allocator: Allocator
    ) !Node {
         return Node {
            .childs = try Vec(Node).init(len + 1, allocator),
            .expressions = try Vec(Expression).init(4, allocator),
            .parent = parent,
            .typ = typ,
        };
    }

    fn push(self: *Node, token: *const Token) !?*Node {
        switch (self.typ) {
            .Root => {
                switch (token.id) {
                    .Function => {
                        try self.childs.push(try init(.Function, self, 2, self.childs.allocator));
                        return self.childs.last_mut() catch unreachable;
                    },
                    else => return null,
                }
            },
            .Function => {
                switch (token.id) {
                    .Identifier => {
                        if (self.last()) |exp| {
                            if (exp.token.id == .AssignArrow) {
                                try self.expressions.push(Expression.init(token, .Type));
                            } else return error.ExpressionNotSupported;
                        } else {
                            try self.expressions.push(Expression.init(token, .Name));
                        }
                    },
                    .ParentesisOpen => {
                        try self.expressions.push(Expression.init(token, .Symbol));
                        try self.childs.push(try init(.FunctionParameters, self, 3, self.childs.allocator));

                        return self.childs.last_mut() catch unreachable;
                    },
                    .ParentesisClose => try self.expressions.push(Expression.init(token, .Symbol)),
                    .AssignArrow => try self.expressions.push(Expression.init(token, .Symbol)),
                    .CurlyBracketOpen =>  {
                        try self.expressions.push(Expression.init(token, .Symbol));
                        try self.childs.push(try init(.FunctionBody, self, 3, self.childs.allocator));

                        return self.childs.last_mut() catch unreachable;
                    },
                    .CurlyBracketClose => try self.expressions.push(Expression.init(token, .Symbol)),
                    else => return null,
                }
            },
            .FunctionParameters => {
                switch (token.id) {
                    .Colon => {
                        try self.expressions.push(Expression.init(token, .Symbol));
                        try self.childs.push(try init(.Parameter, self, 1, self.childs.allocator));
                        return self.childs.last_mut() catch unreachable;
                    },
                    .Identifier => {
                        try self.childs.push(try init(.Parameter, self, 1, self.childs.allocator));
                        const child = self.childs.last_mut() catch unreachable;
                        return try child.push(token);
                    },
                    else => return null,
                }
            },
            .Parameter => {
                switch (token.id) {
                    .DoubleColon => try self.expressions.push(Expression.init(token, .Symbol)),
                    .Identifier => {
                        if (self.last()) |exp| {
                            if (exp.token.id == .DoubleColon) try self.expressions.push(Expression.init(token, .Type))
                            else return error.ExpressionNotSupported;
                        } else try self.expressions.push(Expression.init(token, .Name));
                    },
                    else => return null,
                }
            },
            .FunctionBody => {
                switch (token.id) {
                    .Return => {
                        try self.childs.push(try init(.Return, self, 3 , self.childs.allocator));
                        return self.childs.last_mut() catch unreachable;
                    },
                    .Let => {
                        try self.childs.push(try init(.Let, self, 3, self.childs.allocator));
                        return self.childs.last_mut() catch unreachable;
                    },
                    else => return null,
                }
            },
            .Let => {
                switch (token.id) {
                    .Mut => try self.expressions.push(Expression.init(token, .Keyword)),
                    .Equal => try self.expressions.push(Expression.init(token, .Symbol)),
                    .Number => try self.expressions.push(Expression.init(token, .Literal)),
                    .String => try self.expressions.push(Expression.init(token, .Literal)),
                    .DoubleQuote => try self.expressions.push(Expression.init(token, .Symbol)),
                    .Identifier => {
                        if (self.last()) |exp| {
                            if (exp.token.id == .Mut) try self.expressions.push(Expression.init(token, .Name))
                            else return error.ExpressionNotSupported;
                        } else try self.expressions.push(Expression.init(token, .Name));
                    },
                    .SemiColon => try self.expressions.push(Expression.init(token, .Symbol)),
                    else => return null,
                }
            },
            .Return => {
                switch (token.id) {
                    .Identifier => try self.expressions.push(Expression.init(token, .Value)),
                    .Number => try self.expressions.push(Expression.init(token, .Literal)),
                    .String => try self.expressions.push(Expression.init(token, .Literal)),
                    .SemiColon => try self.expressions.push(Expression.init(token, .Symbol)),
                    .DoubleQuote => try self.expressions.push(Expression.init(token, .Symbol)),
                    else => return null,
                }
            }
        }

        return self;
    }

    fn get_parent(self: *Node) ?*Node {
        return self.parent;
    }

    fn iter(self: *const Node, f: fn (*const Node) void ) void {
        f(self);
        for (self.childs.items) |child| {
            child.iter(f);
        }
    }

    fn last(self: *const Node) ?*const Expression {
        return self.expressions.last() catch null;
    }
};

pub fn parse(tokens: Vec(Token), allocator: Allocator) !Node {
    var iter = tokens.iter();
    var root = try Node.init(.Root, null, 5, allocator);
    var current_node: *Node = &root;

    out: while (iter.next()) |token| {
        while (true) {
            if (try current_node.push(token)) |node| {
                current_node = node;
                break;
            }

            current_node = current_node.parent orelse break :out;
        }
    }

    root.iter(print);

    return root;
}

fn print(node: *const Node) void {
    std.debug.print("node: {}\n", .{node.typ});

    for (node.expressions.items) |exp| {
        if (exp.token.value) |v| {
            std.debug.print("{} : {s} -> {}\n", .{exp.token.id, v, exp.typ});
        } else {
            std.debug.print("{} -> {}\n", .{exp.token.id, exp.typ});
        }
    }
}
