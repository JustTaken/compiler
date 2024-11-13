const std = @import("std");

pub var debug_mode = false;

pub fn print(comptime fmt: []const u8, args: anytype) void {
    if (debug_mode) {
        std.debug.print(fmt, args);
    }
}

pub fn convert(input: [*:0]u8) []const u8 {
    var len: usize = 0;

    while (input[len] != 0) {
        len += 1;
    }

    return input[0..len];
}

pub fn is_number(string: []const u8) bool {
    for (string) |c| {
        if (!is_digit(c)) return false;
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

pub fn hash(string: []const u8) u32 {
    var h: usize = 0;
    for (string, 0..) |char, i| {
        h += char + i;
    }

    return @intCast(h);
}

// pub fn parse(i: isize, out: *Vec(u8)) void {
//     var buffer: [20]u8 = undefined;

//     var k: usize = 0;
//     const neg = i < 0;

//     if (i == 0) {
//         buffer[0] = '0';
//         k += 1;
//     }

//     var num: u32 = if (neg) @intCast(-i) else @intCast(i);

//     while (num > 0) {
//         const rem: u8 = @intCast(num % 10);
//         buffer[k] = rem + '0';

//         k += 1;
//         num /= 10;
//     }

//     if (neg) {
//         buffer[k] = '-';
//         k += 1;
//     }

//     for (0..k) |index| {
//         out.push(buffer[k - index - 1]);
//     }
// }

pub fn parse(string: []const u8) usize {
    var number: usize = 0;
    var multiply: usize = 1;

    for (0..string.len) |i| {
        number += multiply * (string[string.len - i - 1] - '0');
        multiply *= 10;
    }

    return number;
}

pub fn is_digit(char: u8) bool {
    return char >= '0' and char <= '9';
}

pub fn is_alpha(char: u8) bool {
    return (char >= 'A' and char <= 'Z') or (char >= 'a' and char <= 'z') or char == '_';
}

pub fn is_ascci(char: u8) bool {
    return is_alpha(char) or is_digit(char);
}

pub fn assert(flag: bool) !void {
    if (!flag) {
        return error.AssertionFailed;
    }
}
