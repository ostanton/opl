const std = @import("std");
const Lexer = @import("lexer.zig");

lexer: *Lexer,
currentToken: Lexer.Token,
allocator: std.mem.Allocator,

const NodeType = enum {
    Program,
    Statement,
    Declaration,
    Identifier,
    FunctionCall,
    Number,
    BinaryOp,
    UnaryOp,
    ValueScope,
    TypeScope,
    Type,
    ParameterList,
    Parameter,
    MemberAccess,
    Expression,
    Assignment,
};

pub const AstNode = struct {
    type: NodeType,
    value: []const u8,
    children: std.ArrayList(AstNode),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator, nodeType: NodeType, value: []const u8) !AstNode {
        return .{
            .type = nodeType,
            .value = value,
            .children = try std.ArrayList(AstNode).initCapacity(allocator, 1),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *AstNode) void {
        for (self.children.items) |*child| {
            child.deinit();
        }
        self.children.deinit();
    }

    pub fn addChild(self: *AstNode, child: AstNode) ParseError!void {
        self.children.append(self.allocator, child) catch return ParseError.OutOfMemory;
    }

    pub fn printTree(self: AstNode, prefix: []const u8, isLast: bool) !void {
        std.debug.print("{s}", .{prefix});
        std.debug.print("{s}", .{if (isLast) "^-- " else "|-- "});
        if (std.enums.tagName(NodeType, self.type)) |s| {
            std.debug.print("{s}", .{s});
        }

        if (self.value.len > 0) {
            std.debug.print(": \"{s}\"", .{self.value});
        }

        std.debug.print("\n", .{});

        for (self.children.items, 0..) |child, i| {
            const newPrefix = try std.mem.concat(
                self.allocator,
                u8,
                &.{ prefix, if (isLast) "    " else "|   " },
            );
            defer self.allocator.free(newPrefix);

            try child.printTree(newPrefix, i == self.children.items.len - 1);
        }
    }

    pub fn toString(self: AstNode) ![]u8 {
        var result = try std.ArrayList(u8).initCapacity(self.allocator, 1);
        try self.toStringHelper(&result, "", true);
        return result.toOwnedSlice(self.allocator);
    }

    fn toStringHelper(
        self: AstNode,
        result: *std.ArrayList(u8),
        prefix: []const u8,
        isLast: bool,
    ) !void {
        try result.appendSlice(self.allocator, prefix);
        try result.appendSlice(self.allocator, if (isLast) "^-- " else "|-- ");
        if (std.enums.tagName(NodeType, self.type)) |s| {
            try result.appendSlice(self.allocator, s);
        }

        if (self.value.len > 0) {
            try result.appendSlice(self.allocator, ": \"");
            try result.appendSlice(self.allocator, self.value);
            try result.appendSlice(self.allocator, "\"");
        }

        try result.append(self.allocator, '\"');

        for (self.children.items, 0..) |child, i| {
            const newPrefix = try std.mem.concat(
                self.allocator,
                u8,
                &.{ prefix, if (isLast) "    " else "|   " },
            );
            defer self.allocator.free(newPrefix);
            try child.toStringHelper(
                result,
                newPrefix,
                i == self.children.items.len - 1,
            );
        }
    }
};

const ParseError = error{
    UnexpectedToken,
    OutOfMemory,
    InvalidSyntax,
};

const Self = @This();

pub fn init(allocator: std.mem.Allocator, lexer: *Lexer) Self {
    return .{
        .lexer = lexer,
        .currentToken = lexer.nextToken(),
        .allocator = allocator,
    };
}

pub fn parse(self: *Self) ParseError!AstNode {
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

fn expect(self: *Self, tokenType: Lexer.TokenType) ParseError!void {
    if (!self.match(tokenType)) {
        std.debug.print("Expected {s}, found {s}\n", .{
            @tagName(tokenType),
            @tagName(self.currentToken.type),
        });
        return ParseError.UnexpectedToken;
    }
}

fn synchronise(self: *Self) void {
    // skip invalid tokens until we reach a valid new starting point
    while (!self.check(.EoF)) {
        if (self.check(.Semicolon)) {
            self.advance();
            return;
        }

        if (self.check(.Identifier) or self.check(.LeftBrace)) {
            return;
        }

        self.advance();
    }
}

fn parseProgram(self: *Self) ParseError!AstNode {
    var program: AstNode = try .init(self.allocator, .Program, "");

    while (!self.check(.EoF)) {
        if (self.parseStatement()) |statement| {
            try program.addChild(statement);
        } else |err| {
            std.debug.print("Parse error in statement: {}\n", .{err});
            self.synchronise();
        }
    }

    return program;
}

fn parseStatement(self: *Self) ParseError!AstNode {
    var statement: AstNode = try .init(self.allocator, .Statement, "");
    // TODO - check keywords - TokenType.For etc.
    if (self.check(.Identifier)) {
        const identifier = try self.parseIdentifier();

        if (self.check(.Colon)) {
            const declaration = try self.parseDeclaration(identifier);
            try statement.addChild(declaration);
        } else if (self.check(.Assignment) or self.check(.Dot)) {
            const assignment = try self.parseAssignment(identifier);
            try statement.addChild(assignment);
        } else {
            const expression = try self.parseExpressionFromIdentifier(identifier);
            try statement.addChild(expression);
        }
    } else {
        const expression = try self.parseExpression();
        try statement.addChild(expression);
    }

    try self.expect(.Semicolon);
    return statement;
}

fn parseIdentifier(self: *Self) ParseError!AstNode {
    if (!self.check(.Identifier)) {
        return ParseError.UnexpectedToken;
    }

    const identifier: AstNode = try .init(self.allocator, .Identifier, self.currentToken.value);
    self.advance();
    return identifier;
}

fn parseDeclaration(self: *Self, identifier: AstNode) !AstNode {
    var declaration: AstNode = try .init(self.allocator, .Declaration, "");
    try declaration.addChild(identifier);

    try self.expect(.Colon);

    if (self.check(.Assignment)) { // infer the type
        self.advance();
        const value = try self.parseExpression();
        try declaration.addChild(value);
    } else { // explicit type
        const typeNode = try self.parseType();
        try declaration.addChild(typeNode);

        if (self.match(.Assignment)) {
            const value = try self.parseExpression();
            try declaration.addChild(value);
        }
    }

    return declaration;
}

fn parseAssignment(self: *Self, identifier: AstNode) ParseError!AstNode {
    var assignment: AstNode = try .init(self.allocator, .Assignment, "");

    var left = identifier;
    while (self.check(.Dot)) {
        self.advance();
        const member = try self.parseIdentifier();
        var memberAccess: AstNode = .init(self.allocator, .MemberAccess, "");
        try memberAccess.addChild(left);
        try memberAccess.addChild(member);
        left = memberAccess;
    }

    try assignment.addChild(left);
    try self.expect(.Assignment);

    const value = try self.parseExpression();
    try assignment.addChild(value);

    return assignment;
}

fn parseType(self: *Self) ParseError!AstNode {
    if (self.check(.Identifier) or self.currentToken.type.isType()) {
        const typeIdentifier: AstNode = try .init(self.allocator, .Type, self.currentToken.value);
        self.advance();
        return typeIdentifier;
    } else if (self.check(.LeftBrace)) {
        return try self.parseTypeScope();
    }

    return ParseError.UnexpectedToken;
}

fn parseTypeScope(self: *Self) ParseError!AstNode {
    var typeScope: AstNode = try .init(self.allocator, .TypeScope, "");

    try self.expect(.LeftBrace);

    while (!self.check(.RightBrace) and !self.check(.EoF)) {
        const field = try self.parseStatement();
        try typeScope.addChild(field);
    }

    try self.expect(.RightBrace);
    return typeScope;
}

fn parseExpression(self: *Self) ParseError!AstNode {
    return try self.parseBinaryExpression(0);
}

fn parseExpressionFromIdentifier(self: *Self, identifier: AstNode) ParseError!AstNode {
    var expression = identifier;

    while (true) {
        if (self.check(.Dot)) {
            self.advance();
            const member = try self.parseIdentifier();
            var memberAccess: AstNode = try .init(self.allocator, .MemberAccess, "");
            try memberAccess.addChild(expression);
            try memberAccess.addChild(member);
            expression = memberAccess;
        } else if (self.check(.LeftBracket)) {
            const call = try self.parseFunctionCall(expression);
            expression = call;
        } else {
            break;
        }
    }

    return expression;
}

fn parseBinaryExpression(self: *Self, minPrec: u8) ParseError!AstNode {
    var left = try self.parseUnaryExpression();

    while (self.getBinaryOpPrecedence()) |prec| {
        if (prec < minPrec) {
            break;
        }

        const op = self.currentToken.value;
        self.advance();

        const right = try self.parseBinaryExpression(prec + 1);

        var binaryOp: AstNode = try .init(self.allocator, .BinaryOp, op);
        try binaryOp.addChild(left);
        try binaryOp.addChild(right);
        left = binaryOp;
    }

    return left;
}

fn getBinaryOpPrecedence(self: Self) ?u8 {
    return switch (self.currentToken.type) {
        .Plus, .Minus => 1,
        .Asterisk, .Slash => 2,
        else => null,
    };
}

fn parseUnaryExpression(self: *Self) ParseError!AstNode {
    if (self.check(.Minus) or self.check(.Plus) or self.check(.Not)) {
        const op = self.currentToken.value;
        self.advance();
        const operand = try self.parseUnaryExpression();

        var unaryOp: AstNode = try .init(self.allocator, .UnaryOp, op);
        try unaryOp.addChild(operand);
        return unaryOp;
    }

    return try self.parsePrimaryExpression();
}

fn parsePrimaryExpression(self: *Self) ParseError!AstNode {
    if (self.check(.Number)) {
        const number: AstNode = try .init(self.allocator, .Number, self.currentToken.value);
        self.advance();
        return number;
    } else if (self.check(.Identifier)) {
        const identifier = try self.parseIdentifier();
        return try self.parseExpressionFromIdentifier(identifier);
    } else if (self.check(.LeftBrace)) {
        return try self.parseValueScope();
    } else if (self.check(.LeftBracket)) {
        return try self.parseParameterisedExpressionOrLambda();
    } else {
        std.debug.print("Unexpected token in primary expression: {s}\n", .{@tagName(self.currentToken.type)});
        return ParseError.UnexpectedToken;
    }
}

fn parseValueScope(self: *Self) ParseError!AstNode {
    var valueScope: AstNode = try .init(self.allocator, .ValueScope, "");

    try self.expect(.LeftBrace);

    while (!self.check(.RightBrace) and !self.check(.EoF)) {
        const statement = try self.parseStatement();
        try valueScope.addChild(statement);
    }

    try self.expect(.RightBrace);

    if (self.check(.LeftBracket)) {
        const call = try self.parseFunctionCall(valueScope);
        return call;
    }

    return valueScope;
}

fn parseParameterisedExpressionOrLambda(self: *Self) ParseError!AstNode {
    try self.expect(.LeftBracket);

    if (self.check(.RightBracket)) {
        self.advance();

        if (self.check(.LeftBrace)) {
            return try self.parseLambdaBody();
        } else {
            return try AstNode.init(self.allocator, .Expression, "");
        }
    }

    if (self.looksLikeParameterList()) {
        const parameterList = try self.parseParameterList();
        try self.expect(.RightBracket);
        const body = try self.parseLambdaBody();

        var lambda: AstNode = .init(self.allocator, .ValueScope, "");
        try lambda.addChild(parameterList);
        try lambda.addChild(body);
        return lambda;
    }

    const expression = try self.parseExpression();
    try self.expect(.RightBracket);
    return expression;
}

fn looksLikeParameterList(self: Self) bool {
    return self.check(.Identifier);
}

fn parseParameterList(self: *Self) ParseError!AstNode {
    var parameterList: AstNode = try .init(self.allocator, .ParameterList, "");

    while (!self.check(.RightBracket) and !self.check(.EoF)) {
        const parameter = try self.parseParameter();
        try parameterList.addChild(parameter);

        if (!self.match(.Comma)) {
            break;
        }
    }

    return parameterList;
}

fn parseParameter(self: *Self) ParseError!AstNode {
    var parameter: AstNode = try .init(self.allocator, .Parameter, "");

    const identifier = try self.parseIdentifier();
    try parameter.addChild(identifier);

    if (self.match(.Colon)) {
        const parameterType = try self.parseType();
        try parameter.addChild(parameterType);
    }

    return parameter;
}

fn parseLambdaBody(self: *Self) ParseError!AstNode {
    return try self.parseValueScope();
}

fn parseFunctionCall(self: *Self, function: AstNode) ParseError!AstNode {
    var call: AstNode = try .init(self.allocator, .FunctionCall, "");
    try call.addChild(function);

    try self.expect(.LeftBracket);

    while (!self.check(.RightBracket) and !self.check(.EoF)) {
        const argument = try self.parseExpression();
        try call.addChild(argument);

        if (!self.match(.Comma)) {
            break;
        }
    }

    try self.expect(.RightBracket);
    return call;
}
