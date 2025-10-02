const std = @import("std");

input: []const u8,
pos: usize,
line: usize,
column: usize,
// Buffer of the current token (index 0) + 2 ahead
tokens: [3]Token,

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

    // Identifiers
    If,
    For,

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
    line: usize,
    column: usize,

    const invalid: Token = .init(.Invalid, "", 0, 0);

    pub fn init(token_type: TokenType, value: []const u8, line: usize, column: usize) Token {
        return .{
            .type = token_type,
            .value = value,
            .line = line,
            .column = column,
        };
    }
};

const Self = @This();
var keywords: std.StringHashMap(TokenType) = undefined;

pub fn init(allocator: std.mem.Allocator, input: []const u8) !Self {
    initKeywords(allocator) catch |err| {
        std.debug.print("Failed to initialise keywords: {}\n", .{err});
        return err;
    };

    var self: Self = .{
        .input = input,
        .pos = 0,
        .line = 1,
        .column = 1,
        .tokens = .{ .invalid, .invalid, .invalid },
    };

    // Prime the first 3 tokens for use
    for (0..self.tokens.len) |_| {
        _ = self.advance();
    }

    return self;
}

pub fn deinit() void {
    keywords.deinit();
}

fn initKeywords(allocator: std.mem.Allocator) !void {
    keywords = .init(allocator);
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
    try keywords.put("scope", .Scope);
    try keywords.put("if", .If);
    try keywords.put("for", .For);
    try keywords.put("and", .And);
    try keywords.put("or", .Or);
}

pub fn advance(self: *Self) Token {
    self.skipWhitespace();

    var result: Token = .invalid;
    defer {
        // Move tokens in buffer down by one, and append this new token to the end
        self.tokens[0] = self.tokens[1];
        self.tokens[1] = self.tokens[2];
        self.tokens[2] = result;
    }

    if (self.pos >= self.input.len) {
        // Reached the end of the file
        result = .{
            .type = .EoF,
            .value = "End of File",
            .line = self.line,
            .column = self.column,
        };
        return result;
    }

    const char = self.input[self.pos];

    switch (char) {
        ';' => result = self.makeToken(.Semicolon, ";"),
        ':' => result = self.makeToken(.Colon, ":"),
        ',' => result = self.makeToken(.Comma, ","),
        '.' => result = self.makeToken(.Dot, "."),
        '(' => result = self.makeToken(.LeftBracket, "("),
        ')' => result = self.makeToken(.RightBracket, ")"),
        '{' => result = self.makeToken(.LeftBrace, "{"),
        '}' => result = self.makeToken(.RightBrace, "}"),
        '+' => result = self.makeToken(.Plus, "+"),
        '-' => result = self.makeToken(.Minus, "-"),
        '*' => result = self.makeToken(.Asterisk, "*"),
        '/' => result = self.makeToken(.Slash, "/"),
        '?' => result = self.makeToken(.Question, "?"),
        '&' => result = self.makeToken(.Ampersand, "&"),
        '|' => result = self.makeToken(.Pipe, "|"),
        '^' => result = self.makeToken(.Caret, "^"),
        '=' => {
            if (self.peekChar() == '=') {
                self.pos += 2;
                self.column += 2;
                result = .{
                    .type = .Equal,
                    .value = "==",
                    .line = self.line,
                    .column = self.column - 2,
                };
            } else {
                result = self.makeToken(.Assignment, "=");
            }
        },
        '!' => {
            if (self.peekChar() == '=') {
                self.pos += 2;
                self.column += 2;
                result = .{
                    .type = .NotEqual,
                    .value = "!=",
                    .line = self.line,
                    .column = self.column - 2,
                };
            } else {
                result = self.makeToken(.Not, "!");
            }
        },
        '<' => {
            if (self.peekChar() == '=') {
                self.pos += 2;
                self.column += 2;
                result = .{
                    .type = .LessEqual,
                    .value = "<=",
                    .line = self.line,
                    .column = self.column,
                };
            } else if (self.peekChar() == '<') {
                self.pos += 2;
                self.column += 2;
                result = .{
                    .type = .LeftShift,
                    .value = "<<",
                    .line = self.line,
                    .column = self.column,
                };
            } else {
                result = self.makeToken(.LessThan, "<");
            }
        },
        '>' => {
            if (self.peekChar() == '=') {
                self.pos += 2;
                self.column += 2;
                result = .{
                    .type = .GreaterEqual,
                    .value = ">=",
                    .line = self.line,
                    .column = self.column,
                };
            } else if (self.peekChar() == '>') {
                self.pos += 2;
                self.column += 2;
                result = .{
                    .type = .RightShift,
                    .value = ">>",
                    .line = self.line,
                    .column = self.column,
                };
            } else {
                result = self.makeToken(.GreaterThan, ">");
            }
        },
        else => {
            if (self.readNumber(char)) |token| {
                result = token;
            } else if (self.readIdentifier(char)) |token| {
                result = token;
            }
        },
    }

    return result;
}

/// Peeks at the token at the offset (0 being the current token)
pub fn peekAt(self: Self, offset: usize) Token {
    if (offset >= self.tokens.len) {
        return .invalid;
    }

    return self.tokens[offset];
}

/// Peeks at the next token
pub fn peek(self: Self) Token {
    return self.peekAt(1);
}

fn peekChar(self: Self) ?u8 {
    if (self.pos >= self.input.len) {
        return null;
    }

    return self.input[self.pos + 1];
}

fn makeToken(self: *Self, token_type: TokenType, value: []const u8) Token {
    self.pos += 1;
    self.column += 1;
    return .{
        .type = token_type,
        .value = value,
        .line = self.line,
        .column = self.column - 1,
    };
}

fn skipWhitespace(self: *Self) void {
    while (self.pos < self.input.len) {
        const char = self.input[self.pos];

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

fn readNumber(self: *Self, char: u8) ?Token {
    if (!std.ascii.isDigit(char)) {
        return null;
    }

    const start = self.pos;
    const start_column = self.column;

    while (self.pos < self.input.len and (std.ascii.isDigit(self.input[self.pos]) or self.input[self.pos] == '.')) {
        self.pos += 1;
        self.column += 1;
    }

    return .{
        .type = .Number,
        .value = self.input[start..self.pos],
        .line = self.line,
        .column = start_column,
    };
}

fn readIdentifier(self: *Self, char: u8) ?Token {
    if (!std.ascii.isAlphanumeric(char)) {
        return null;
    }

    const start = self.pos;
    const start_column = self.column;

    while (self.pos < self.input.len and (std.ascii.isAlphanumeric(self.input[self.pos]) or self.input[self.pos] == '_')) {
        self.pos += 1;
        self.column += 1;
    }

    const value = self.input[start..self.pos];
    var token_type: TokenType = .Identifier;
    if (keywords.get(value)) |t| {
        token_type = t;
    }

    return .{
        .type = token_type,
        .value = value,
        .line = self.line,
        .column = start_column,
    };
}
