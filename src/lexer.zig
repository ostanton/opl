const std = @import("std");

input: []const u8,
pos: usize,
line: u32,
column: u32,

pub const TokenType = enum {
    Identifier,
    Number,

    // Types
    U8,
    U16,
    U32,
    U64,
    I8,
    I16,
    I32,
    I64,
    F32,
    F64,
    Bool,
    Void,
    Scope,

    // Operators
    Colon,
    Semicolon,
    Comma,
    Dot,
    LeftBracket,
    RightBracket,
    LeftBrace,
    RightBrace,
    Plus,
    Minus,
    Asterisk,
    Slash,
    Question,
    And,
    Or,
    Assignment,
    Equal,
    NotEqual,
    Not,
    GreaterThan,
    LessThan,
    GreaterEqual,
    LessEqual,
    Ampersand,
    Percent,
    Pipe,
    LeftShift,
    RightShift,
    Caret,

    // Other
    EoF,
    Invalid,

    pub fn isType(self: TokenType) bool {
        return @intFromEnum(self) >= @intFromEnum(TokenType.U8) and @intFromEnum(self) <= @intFromEnum(TokenType.Scope);
    }
};

pub const Token = struct {
    type: TokenType,
    value: []const u8,
    line: u32,
    column: u32,
};

const Self = @This();
var gpa = std.heap.GeneralPurposeAllocator(.{}){};
var keywords = std.StringHashMap(TokenType).init(gpa.allocator());

pub fn init(input: []const u8) !Self {
    try keywords.put("u8", .U8);
    try keywords.put("u16", .U16);
    try keywords.put("u32", .U32);
    try keywords.put("u64", .U64);
    try keywords.put("i8", .I8);
    try keywords.put("i16", .I16);
    try keywords.put("i32", .I32);
    try keywords.put("i64", .I64);
    try keywords.put("f32", .F32);
    try keywords.put("f64", .F64);
    try keywords.put("bool", .Bool);
    try keywords.put("void", .Void);
    try keywords.put("scope", .Scope);

    return .{
        .input = input,
        .pos = 0,
        .line = 1,
        .column = 1,
    };
}

pub fn nextToken(self: *Self) Token {
    self.skipWhitespace();

    if (self.pos >= self.input.len) {
        return .{
            .type = .EoF,
            .value = "EOF",
            .line = self.line,
            .column = self.column,
        };
    }

    const char = self.input[self.pos];

    switch (char) {
        else => {},
        '=' => {
            if (self.peek() == '=') {
                self.pos += 2;
                self.column += 2;
                return .{
                    .type = .Equal,
                    .value = "==",
                    .line = self.line,
                    .column = self.column - 2,
                };
            }
            return self.makeToken(.Assignment, "=");
        },
        ';' => return self.makeToken(.Semicolon, ";"),
        ':' => return self.makeToken(.Colon, ":"),
        ',' => return self.makeToken(.Comma, ","),
        '.' => return self.makeToken(.Dot, "."),
        '(' => return self.makeToken(.LeftBracket, "("),
        ')' => return self.makeToken(.RightBracket, ")"),
        '{' => return self.makeToken(.LeftBrace, "{"),
        '}' => return self.makeToken(.RightBrace, "}"),
        '+' => return self.makeToken(.Plus, "+"),
        '-' => return self.makeToken(.Minus, "-"),
        '*' => return self.makeToken(.Asterisk, "*"),
        '/' => return self.makeToken(.Slash, "/"),
        '&' => {
            if (self.peek() == '&') {
                self.pos += 2;
                self.column += 2;
                return .{
                    .type = .And,
                    .value = "&&",
                    .line = self.line,
                    .column = self.column - 2,
                };
            }
            return self.makeToken(.Ampersand, "&");
        },
        '|' => {
            if (self.peek() == '|') {
                self.pos += 2;
                self.column += 2;
                return .{
                    .type = .Or,
                    .value = "||",
                    .line = self.line,
                    .column = self.column - 2,
                };
            }
            return self.makeToken(.Pipe, "|");
        },
        '^' => return self.makeToken(.Caret, "^"),
        '!' => {
            if (self.peek() == '=') {
                self.pos += 2;
                self.column += 2;
                return .{
                    .type = .NotEqual,
                    .value = "!=",
                    .line = self.line,
                    .column = self.column - 2,
                };
            }
            return self.makeToken(.Not, "!");
        },
        '<' => {
            if (self.peek() == '=') {
                self.pos += 2;
                self.column += 2;
                return .{
                    .type = .LessEqual,
                    .value = "<=",
                    .line = self.line,
                    .column = self.column - 2,
                };
            }
            if (self.peek() == '<') {
                self.pos += 2;
                self.column += 2;
                return .{
                    .type = .LeftShift,
                    .value = "<<",
                    .line = self.line,
                    .column = self.column - 2,
                };
            }
            return self.makeToken(.LessThan, "<");
        },
        '>' => {
            if (self.peek() == '=') {
                self.pos += 2;
                self.column += 2;
                return .{
                    .type = .GreaterEqual,
                    .value = ">=",
                    .line = self.line,
                    .column = self.column - 2,
                };
            }
            if (self.peek() == '>') {
                self.pos += 2;
                self.column += 2;
                return .{
                    .type = .RightShift,
                    .value = ">>",
                    .line = self.line,
                    .column = self.column - 2,
                };
            }
            return self.makeToken(.GreaterThan, ">");
        },
        '?' => return self.makeToken(.Question, "?"),
    }

    if (std.ascii.isDigit(char)) {
        return self.readNumber();
    }

    if (std.ascii.isAlphabetic(char)) {
        return self.readIdentifier();
    }

    return .{
        .type = .Invalid,
        .value = "",
        .line = self.line,
        .column = self.column,
    };
}

fn peek(self: Self) ?u8 {
    if (self.pos + 1 >= self.input.len) {
        return null;
    }
    return self.input[self.pos + 1];
}

fn makeToken(self: *Self, tokenType: TokenType, value: []const u8) Token {
    self.pos += 1;
    self.column += 1;
    return .{
        .type = tokenType,
        .value = value,
        .line = self.line,
        .column = self.column - 1,
    };
}

fn skipWhitespace(self: *Self) void {
    while (self.pos < self.input.len) {
        const char: u8 = self.input[self.pos];

        if (std.ascii.isWhitespace(char)) {
            if (char == '\n') {
                self.line += 1;
                self.column = 1;
            } else {
                self.column += 1;
            }
            self.pos += 1;
        } else if (char == '/' and self.pos + 1 < self.input.len and self.input[self.pos + 1] == '/') {
            self.pos += 2;
            self.column += 2;

            while (self.pos < self.input.len and self.input[self.pos] != '\n') {
                self.pos += 1;
                self.column += 1;
            }
        } else if (char == '/' and self.pos + 1 < self.input.len and self.input[self.pos + 1] == '*') {
            self.pos += 2;
            self.column += 2;

            while (self.pos + 1 < self.input.len) {
                if (self.input[self.pos] == '*' and self.input[self.pos + 1] == '/') {
                    self.pos += 2;
                    self.column += 2;
                    break;
                }

                if (self.input[self.pos] == '\n') {
                    self.line += 1;
                    self.column = 1;
                } else {
                    self.column += 1;
                }
                self.pos += 1;
            }
        } else {
            break;
        }
    }
}

fn readNumber(self: *Self) Token {
    const start = self.pos;
    const startCol = self.column;

    while (self.pos < self.input.len and (std.ascii.isDigit(self.input[self.pos]) or self.input[self.pos] == '.')) {
        self.pos += 1;
        self.column += 1;
    }

    return .{
        .type = .Number,
        .value = self.input[start..self.pos],
        .line = self.line,
        .column = startCol,
    };
}

fn readIdentifier(self: *Self) Token {
    const start = self.pos;
    const startCol = self.column;

    while (self.pos < self.input.len and (std.ascii.isAlphanumeric(self.input[self.pos]) or self.input[self.pos] == '_')) {
        self.pos += 1;
        self.column += 1;
    }

    const value = self.input[start..self.pos];
    var tokenType = TokenType.Identifier;
    if (keywords.get(value)) |t| {
        tokenType = t;
    }

    return .{
        .type = tokenType,
        .value = value,
        .line = self.line,
        .column = startCol,
    };
}
