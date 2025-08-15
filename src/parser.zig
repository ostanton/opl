const std = @import("std");
const Lexer = @import("lexer.zig");

lexer: *Lexer,
currentToken: Lexer.Token,

const NodeType = enum {
    Program,
    Statement,
    Declaration,
    Identifier,
    Number,
    BinaryOp,
    UnaryOp,
    ValueScope,
    TypeScope,
    Type,
    ParameterList,
    Parameter,
};

const AstNode = struct {
    type: NodeType,
    value: []const u8,
    children: std.ArrayList(AstNode),

    const Self = @This();
    var gpa: std.heap.GeneralPurposeAllocator(.{}) = .{};

    pub fn init(nodeType: NodeType, value: []const u8) @This() {
        return .{
            .type = nodeType,
            .value = value,
            .children = std.ArrayList(AstNode).init(gpa.allocator()),
        };
    }

    pub fn printTree(self: @This(), prefix: []const u8, isLast: bool) !void {
        std.debug.print("{s}", .{prefix});
        std.debug.print("{s}", .{if (isLast) "^-- " else "|-- "});
        if (std.enums.tagName(NodeType, self.type)) |s| {
            std.debug.print("{s}", .{s});
        }

        if (self.value.len > 0) {
            std.debug.print(": \"{s}\"", .{self.value});
        }

        std.debug.print("\n", .{});

        for (0..self.children.items.len) |i| {
            try self.children.items[i].printTree(
                try std.mem.concat(
                    gpa.allocator(),
                    u8,
                    &.{ prefix, if (isLast) "    " else "|   " },
                ),
                i == self.children.items.len - 1,
            );
        }
    }
};

const Self = @This();

pub fn init(lexer: *Lexer) Self {
    return .{
        .lexer = lexer,
        .currentToken = lexer.nextToken(),
    };
}

pub fn parse(self: *Self) AstNode {
    return self.parseProgram();
}

fn advance(self: *Self) void {
    self.currentToken = self.lexer.nextToken();
}

fn match(self: *Self, tokenType: Lexer.TokenType) bool {
    if (self.currentToken.type == tokenType) {
        self.advance();
        return true;
    }

    return false;
}

fn check(self: Self, tokenType: Lexer.TokenType) bool {
    return self.currentToken.type == tokenType;
}

fn parseProgram(self: *Self) AstNode {
    var program: AstNode = .init(.Program, "");

    while (!self.check(.EoF)) {
        if (self.parseStatement()) |statement| {
            program.children.append(statement) catch break;
        } else {
            break;
        }
    }

    return program;
}

fn parseStatement(self: *Self) ?AstNode {
    _ = self.match(.Semicolon);
    return null;
}

fn parseDeclaration(self: *Self) ?AstNode {
    _ = self;
    return null;
}
