const std = @import("std");
const Allocator = std.mem.Allocator;
const List = std.ArrayListUnmanaged;
const assert = std.debug.assert;

const Token = union(enum) {
    eof,
    add,
    sub,
    mul,
    div,
    pow,
    l_paren,
    r_paren,
    num: []const u8,
};

const Tokenizer = struct {
    index: usize,
    source: [*:0]const u8,

    fn init(source: [*:0]const u8) Tokenizer {
        return .{ .index = 0, .source = source };
    }

    fn next(self: *Tokenizer) !Token {
        scan: switch (self.source[self.index]) {
            // terminate on end-of-string
            0 => return .eof,
            // ignore all whitespace
            ' ', '\t', '\r', '\n' => {
                self.index += 1;
                continue :scan self.source[self.index];
            },
            // single-character tokens
            '+', '-', '*', '/', '^', '(', ')' => {
                defer self.index += 1;
                return switch (self.source[self.index]) {
                    '+' => .add,
                    '-' => .sub,
                    '*' => .mul,
                    '/' => .div,
                    '^' => .pow,
                    '(' => .l_paren,
                    ')' => .r_paren,
                    else => unreachable,
                };
            },
            // base-10 decimal numeric constant
            '0'...'9', '.' => {
                const start = self.index;
                while (true) : (self.index += 1) {
                    const char = self.source[self.index];
                    if ((char < '0' or char > '9') and char != '.') {
                        return .{ .num = self.source[start..self.index] };
                    }
                }
            },
            // All valid input has been handled
            else => return error.InvalidToken,
        }
    }
};

const RpnToken = union(enum) {
    add,
    sub,
    mul,
    div,
    pow,
    neg,
    num: []const u8,
};

/// Expressions are matched based on the correct order of computation:
/// ( and ), before ^, before * and /, before + and -
/// negation is flattened if it appears more than once
/// The output of each function is a slice of postfix tokens
const Parser = struct {
    index: usize,
    source: [*]const Token,

    fn init(source: []const Token) Parser {
        // source must be delimited by .eof
        assert(source[source.len - 1] == .eof);
        return .{ .index = 0, .source = source.ptr };
    }

    const ParseError = Allocator.Error || error{
        InvalidCharacter,
        UnexpectedToken,
    };

    // <expression> ::= <term> (("+" | "-") <term>)*
    fn expression(self: *Parser, alloc: Allocator) ParseError![]const RpnToken {
        var result: List(RpnToken) = .empty;
        defer result.deinit(alloc);
        try result.appendSlice(alloc, try self.term(alloc));
        scan: switch (self.source[self.index]) {
            .add => {
                self.index += 1;
                try result.appendSlice(alloc, try self.term(alloc));
                try result.append(alloc, .add);
                continue :scan self.source[self.index];
            },
            .sub => {
                self.index += 1;
                try result.appendSlice(alloc, try self.term(alloc));
                try result.append(alloc, .sub);
                continue :scan self.source[self.index];
            },
            else => {},
        }
        return try result.toOwnedSlice(alloc);
    }

    // <term> ::= <factor> ((("*" | "/") <factor>)* | <factor>*)
    fn term(self: *Parser, alloc: Allocator) ![]const RpnToken {
        var result: List(RpnToken) = .empty;
        defer result.deinit(alloc);
        try result.appendSlice(alloc, try self.factor(alloc));
        scan: switch (self.source[self.index]) {
            .mul => {
                self.index += 1;
                try result.appendSlice(alloc, try self.factor(alloc));
                try result.append(alloc, .mul);
                continue :scan self.source[self.index];
            },
            .div => {
                self.index += 1;
                try result.appendSlice(alloc, try self.factor(alloc));
                try result.append(alloc, .div);
                continue :scan self.source[self.index];
            },
            .num, .l_paren => {
                // Implied multiplication
                const factor_rpn = try self.factor(alloc);
                try result.appendSlice(alloc, factor_rpn);
                try result.append(alloc, .mul);
                continue :scan self.source[self.index];
            },
            else => {},
        }
        return try result.toOwnedSlice(alloc);
    }

    // <factor> ::= <negation> ('^' <factor>)*
    fn factor(self: *Parser, alloc: Allocator) ![]const RpnToken {
        var result: List(RpnToken) = .empty;
        defer result.deinit(alloc);
        try result.appendSlice(alloc, try self.negation(alloc));
        if (self.source[self.index] == .pow) {
            self.index += 1;
            try result.appendSlice(alloc, try self.factor(alloc));
            try result.append(alloc, .pow);
        }
        return try result.toOwnedSlice(alloc);
    }

    // <negation> ::= ("-" | "+")* <number>
    fn negation(self: *Parser, alloc: Allocator) ![]const RpnToken {
        var result: List(RpnToken) = .empty;
        defer result.deinit(alloc);
        var neg_count: usize = 0;
        scan: switch (self.source[self.index]) {
            .sub => {
                self.index += 1;
                neg_count += 1;
                continue :scan self.source[self.index];
            },
            .add => {
                self.index += 1;
                continue :scan self.source[self.index];
            },
            else => {},
        }
        try result.appendSlice(alloc, try self.number(alloc));
        try result.appendNTimes(alloc, .neg, neg_count % 2);
        return try result.toOwnedSlice(alloc);
    }

    // <number> ::= '(' <expression> ')' | <floating point number>
    fn number(self: *Parser, alloc: Allocator) ![]const RpnToken {
        var result: List(RpnToken) = .empty;
        defer result.deinit(alloc);
        switch (self.source[self.index]) {
            .l_paren => {
                self.index += 1;
                try result.appendSlice(alloc, try self.expression(alloc));
                if (self.source[self.index] == .r_paren) {
                    self.index += 1;
                } else {
                    return error.UnexpectedToken;
                }
            },
            .num => |digits| {
                self.index += 1;
                try result.append(alloc, .{ .num = digits });
            },
            else => return error.UnexpectedToken,
        }
        return try result.toOwnedSlice(alloc);
    }
};

fn tokenize(alloc: Allocator, source: [:0]const u8) ![]const Token {
    var toker: Tokenizer = .init(source);
    var tokens: List(Token) = .empty;
    defer tokens.deinit(alloc);
    while (true) {
        const token = try toker.next();
        try tokens.append(alloc, token);
        if (token == .eof) {
            return try tokens.toOwnedSlice(alloc);
        }
    }
}

fn parse(alloc: Allocator, source: [:0]const u8) ![]const RpnToken {
    const tokens = try tokenize(alloc, source);
    var parser: Parser = .init(tokens);
    return try parser.expression(alloc);
}

fn formatRpn(writer: anytype, tokens: []const RpnToken) !void {
    for (0..tokens.len) |token_idx| {
        if (token_idx != 0) {
            try writer.writeByte(' ');
        }
        const token = tokens[token_idx];
        switch (token) {
            .add, .sub, .mul, .div, .pow => {
                const char: u8 = switch (token) {
                    .add => '+',
                    .sub => '-',
                    .mul => '*',
                    .div => '/',
                    .pow => '^',
                    else => unreachable,
                };
                try writer.writeByte(char);
            },
            .neg => try writer.writeAll("neg"),
            .num => |digits| try writer.writeAll(digits),
        }
    }
}

fn eval(comptime T: type, alloc: Allocator, tokens: []const RpnToken) !T {
    var stack: List(T) = .empty;
    defer stack.deinit(alloc);

    for (tokens) |token| {
        switch (token) {
            .num => |digits| try stack.append(alloc, try std.fmt.parseFloat(T, digits)),
            .neg => stack.appendAssumeCapacity(-stack.pop()),
            .add, .sub, .mul, .div, .pow => {
                const rhs, const lhs = .{ stack.pop(), stack.pop() }; // FILO
                stack.appendAssumeCapacity(switch (token) {
                    .add => lhs + rhs,
                    .sub => lhs - rhs,
                    .mul => lhs * rhs,
                    .div => lhs / rhs,
                    .pow => std.math.pow(T, lhs, rhs),
                    else => unreachable,
                });
            },
        }
    }

    assert(stack.items.len == 1);
    return stack.getLast();
}

pub fn main() !void {
    const alloc = std.heap.page_allocator;
    const stdout = std.io.getStdOut().writer();

    const source = "5+-+1.7^+12-+4*++--(+-+-+3)-+++-3/+-14.2";
    const parsed = try parse(alloc, source);
    defer alloc.free(parsed);
    const result = try eval(f64, alloc, parsed);

    try stdout.print("Input: {s}\n", .{source});

    try stdout.writeAll("RPN: ");
    try formatRpn(stdout, parsed);
    try stdout.writeByte('\n');

    try stdout.print("Output: {d}\n", .{result});
}
