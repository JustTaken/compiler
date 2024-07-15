const std = @import("std");
const util = @import("util.zig");

const Allocator = std.mem.Allocator;
const Vec = @import("collections.zig").Vec;
const FixedVec = @import("collections.zig").FixedVec;

pub const TokenId = enum {
    Root,
    Function,
    Let,
    Mut,
    Switch,
    Return,
    Colon,
    SemiColon,
    DoubleColon,
    DoubleQuote,
    String,
    Number,
    Identifier,
    ParentesisOpen,
    ParentesisClose,
    CurlyBracketOpen,
    CurlyBracketClose,
    SquareBracketOpen,
    SquareBracketClose,
    Greater,
    Less,
    Equal,
    LessEqual,
    GreaterEqual,
    Not,
    Multiplication,
    Sum,
    Dot,
    Different,
    EqualArrow,
    AssignArrow,

    pub fn is_binary_operator(self: *const TokenId) bool {
        return switch (self.*) {
            .Sum, .Multiplication => true,
            else => false,
        };
    }
};

pub const Token = struct {
    id: TokenId,
    value: ?[]const u8,

    fn identifier(string: []const u8) ?Token {
        if (string.len < 1) return null;

        switch (string[0]) {
            'f' => if (util.eql("fn", string)) return function(),
            'r' => if (util.eql("return", string)) return ret(),
            'l' => if (util.eql("let", string)) return let(),
            'm' => if (util.eql("mut", string)) return mut(),
            's' => if (util.eql("switch", string)) return cases(),
            '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' => if (util.is_number(string)) return number(string),
            else => {},
        }

        return Token { .id = .Identifier, .value = string };
    }

    fn number(string: []const u8) Token { return Token {  .id = .Number, .value = string }; }
    fn function() Token { return Token { .id = .Function, .value = null }; }
    fn let() Token { return Token { .id = .Let , .value = null }; }
    fn mut() Token { return Token { .id = .Mut , .value = null }; }
    fn cases() Token { return Token { .id = .Switch, .value = null }; }
    fn double_quote() Token { return Token { .id = .DoubleQuote, .value = null }; }
    fn ret() Token { return Token { .id = .Return, .value = null }; }
    fn equal_arrow() Token { return Token { .id = .EqualArrow, .value = null }; }
    fn assign_arrow() Token { return Token { .id = .AssignArrow, .value = null }; }

    fn str(string: []const u8) Token { return Token { .id = .String, .value = string }; }

    fn curly_open(string: []const u8) FixedVec(Token, 2) { return concat(string, Token { .id = .CurlyBracketOpen, .value = null }); }
    fn square_open(string: []const u8) FixedVec(Token, 2) { return concat(string, Token { .id = .SquareBracketOpen, .value = null }); }
    fn square_close(string: []const u8) FixedVec(Token, 2) {  return concat(string, Token { .id = .SquareBracketClose, .value = null }); }
    fn paren_open(string: []const u8) FixedVec(Token, 2) { return concat(string, Token { .id = .ParentesisOpen, .value = null }); }
    fn paren_close(string: []const u8) FixedVec(Token, 2) {  return concat(string, Token { .id = .ParentesisClose, .value = null });  }
    fn curly_close(string: []const u8) FixedVec(Token, 2) {  return concat(string, Token { .id = .CurlyBracketClose, .value = null });  }
    fn double(string: []const u8) FixedVec(Token, 2) { return concat(string, Token { .id = .DoubleColon, .value = null });  }
    fn colon(string: []const u8) FixedVec(Token, 2) { return concat(string, Token { .id = .Colon, .value = null });  }
    fn semi(string: []const u8) FixedVec(Token, 2) { return concat(string, Token { .id = .SemiColon, .value = null });  }
    fn not(string: []const u8) FixedVec(Token, 2) { return concat(string, Token { .id = .Not, .value = null });  }
    fn dot(string: []const u8) FixedVec(Token, 2) { return concat(string, Token { .id = .Dot, .value = null });  }
    fn sum(string: []const u8) FixedVec(Token, 2) { return concat(string, Token { .id = .Sum, .value = null });  }
    fn mult(string: []const u8) FixedVec(Token, 2) { return concat(string, Token { .id = .Multiplication, .value = null });  }
    fn greater(string: []const u8) FixedVec(Token, 2) { return concat(string, Token { .id = .Greater, .value = null }); }
    fn less(string: []const u8) FixedVec(Token, 2) { return concat(string, Token { .id = .Less, .value = null }); }

    fn equal(string: []const u8, prev: ?*Token) ?FixedVec(Token, 2) {
        if (prev) |l| {
            switch (l.id) {
                .Greater => l.id = .GreaterEqual,
                .Less => l.id = .LessEqual,
                .Not => l.id = .Different,
                else => return concat(string, Token { .id = .Equal , .value = null }),
            }

            return null;
        }

        return concat(string, Token { .id = .Equal, .value = null });
    }

    fn concat(string: []const u8, token: Token) FixedVec(Token, 2) {
        var tokens = FixedVec(Token, 2).init();

        if (identifier(string)) |iden| tokens.push(iden) catch unreachable;
        tokens.push(token) catch unreachable;

        return tokens;
    } 

    // fn greater(string: []const u8) FixedVec(Token, 2) {
    //     if (string.len >= 1) {
    //         return switch (string[0]) {
    //             '-' => concat("", assign_arrow()),
    //             '=' => concat("", equal_arrow()),
    //             else => concat(string, t),
    //         };
    //     }

    //     return concat("", t);
    // }
};

pub fn init(content: []const u8, allocator: Allocator) !Vec(Token){
    const len: u32 = @intCast(content.len);
    var tokens = try Vec(Token).init(len / 10 + 1, allocator);

    var concatenating = false;
    var stringfying = false;
    var flag = false;
    var start: usize = 0;
    for (0..len) |i| {
        const char = content[i];

        if (stringfying and char != '"') continue;
        if (!flag) {
            concatenating = false;
            start = i;
        }

        flag = false;

        switch (char) {
            ' ', '\n', => if (Token.identifier(content[start..i])) |token| try tokens.push(token), 
            '{' => try tokens.extend(2, Token.curly_open(content[start..i])),
            '}' => try tokens.extend(2, Token.curly_close(content[start..i])),
            '(' => try tokens.extend(2, Token.paren_open(content[start..i])),
            ')' => try tokens.extend(2, Token.paren_close(content[start..i])),
            '[' => try tokens.extend(2, Token.square_open(content[start..i])),
            ']' => try tokens.extend(2, Token.square_close(content[start..i])),
            ',' => try tokens.extend(2, Token.colon(content[start..i])),
            ';' => try tokens.extend(2, Token.semi(content[start..i])),
            ':' => try tokens.extend(2, Token.double(content[start..i])),
            '!' => try tokens.extend(2, Token.not(content[start..i])),
            '>' => try tokens.extend(2, Token.greater(content[start..i])),
            '.' => try tokens.extend(2, Token.dot(content[start..i])),
            '+' => try tokens.extend(2, Token.sum(content[start..i])),
            '*' => try tokens.extend(2, Token.mult(content[start..i])),
            '<' => try tokens.extend(2, Token.less(content[start..i])),
            '=' => try tokens.extend(2, Token.equal(content[start..i], tokens.last_mut_opt()) orelse continue),
            '"' => {
                if (stringfying) { try tokens.push(Token.str(content[start..i])); }
                else {
                    if (Token.identifier(content[start..i])) |token| try tokens.push(token);
                    flag = true;
                }

                try tokens.push(Token.double_quote());
                stringfying = !stringfying;
                start = i + 1;
            },
            else => {
                flag = true;

                if (!concatenating) {
                    concatenating = true; 
                    start = i; 
                }
            },
        }
    }

    return tokens;
}
