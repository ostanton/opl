const std = @import("std");
const Lexer = @import("lexer2.zig");

lexer: Lexer,

const NodeType = enum {
    Program,
    Statement,
    Expression,
    Declaration,
    Assignment,
    ScopeCall,
    CallParameterList,
    Parameter,
    ParameterList,
    LocalAccess,
    ValueScope,
    TypeScope,
    Type,
    BinaryExpr,
    UnaryExpr,
    Identifier,
    Number,
};

pub const AstNode = struct {
    type: NodeType,
    value: []const u8,
    children: std.ArrayList(AstNode),

    pub fn init(node_type: NodeType, value: []const u8) AstNode {
        return .{
            .type = node_type,
            .value = value,
            .children = std.ArrayList(AstNode).empty,
        };
    }

    pub fn deinit(self: *AstNode, allocator: std.mem.Allocator) void {
        for (self.children.items) |*child| {
            child.deinit(allocator);
        }
        self.children.deinit(allocator);
    }

    pub fn addChild(self: *AstNode, allocator: std.mem.Allocator, child: AstNode) !void {
        try self.children.append(allocator, child);
    }

    /// Creates a string representation of the AST using this node as the root.
    /// The returned string must be freed by the same allocator used to allocate it
    pub fn toString(self: AstNode, allocator: std.mem.Allocator) ![]u8 {
        var result: std.ArrayList(u8) = .empty;
        try self.toStringHelper(allocator, &result, "", true);
        return result.toOwnedSlice(allocator);
    }

    fn toStringHelper(
        self: AstNode,
        allocator: std.mem.Allocator,
        result: *std.ArrayList(u8),
        prefix: []const u8,
        is_last: bool,
    ) !void {
        try result.appendSlice(allocator, prefix);
        try result.appendSlice(allocator, if (is_last) "^-- " else "|-- ");
        if (std.enums.tagName(NodeType, self.type)) |s| {
            try result.appendSlice(allocator, s);
        }

        if (self.value.len > 0) {
            try result.appendSlice(allocator, ": \"");
            try result.appendSlice(allocator, self.value);
            try result.appendSlice(allocator, "\"");
        }

        try result.append(allocator, '\n');

        for (self.children.items, 0..) |child, i| {
            const new_prefix = try std.mem.concat(
                allocator,
                u8,
                &.{ prefix, if (is_last) "    " else "|   " },
            );
            defer allocator.free(new_prefix);

            try child.toStringHelper(
                allocator,
                result,
                new_prefix,
                i == self.children.items.len - 1,
            );
        }
    }
};

const ParseError = error{
    UnexpectedToken,
    InvalidSyntax,
};

const Self = @This();

pub fn init(allocator: std.mem.Allocator, input: []const u8) !Self {
    return .{
        .lexer = try .init(allocator, input),
    };
}

pub fn deinit() void {
    Lexer.deinit();
}

pub fn parse(self: *Self, allocator: std.mem.Allocator) !AstNode {
    return try self.parseProgram(allocator);
}

fn currentToken(self: Self) Lexer.Token {
    return self.lexer.peekAt(0);
}

/// Consumes the next token into the current token
fn advance(self: *Self) void {
    _ = self.lexer.advance();
}

/// Checks if the token type is the current token, and advances if so
fn match(self: *Self, token_type: Lexer.TokenType) bool {
    if (self.check(token_type)) {
        self.advance();
        return true;
    }
    return false;
}

/// Requires the token type to be the current token.
/// Returns ParseError.UnexpectedToken if not
fn expect(self: *Self, token_type: Lexer.TokenType) ParseError!void {
    if (!self.match(token_type)) {
        std.debug.print("Expected {s}, found {s} at line {}\n", .{
            @tagName(token_type),
            @tagName(self.currentToken().type),
            self.currentToken().line,
        });
        return ParseError.UnexpectedToken;
    }
}

fn checkAt(self: Self, token_type: Lexer.TokenType, offset: usize) bool {
    return self.lexer.peekAt(offset).type == token_type;
}

/// Checks if the token type is the current token
fn check(self: Self, token_type: Lexer.TokenType) bool {
    return self.checkAt(token_type, 0);
}

/// Skips invalid tokens until it reaches a valid new starting point.
/// Allows the parser to continue parsing after an error
fn synchronise(self: *Self) void {
    while (!self.check(.EoF) and !self.check(.Invalid)) {
        // Recover into the next statement
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

fn parseProgram(self: *Self, allocator: std.mem.Allocator) !AstNode {
    // The top-level node in the tree
    var program: AstNode = .init(.Program, "");

    while (!self.check(.EoF) and !self.check(.Invalid)) {
        if (self.parseStatement(allocator)) |statement| {
            try program.addChild(allocator, statement);
        } else |err| {
            std.debug.print("Parse error in statement: {}\n", .{err});
            self.synchronise();

            program.deinit(allocator);
            return err;
        }
    }

    return program;
}

/// A statement is more or less anything which ends with a semicolon:
/// - Expressions
/// - Declarations
/// - Scope calls
/// - etc.
fn parseStatement(self: *Self, allocator: std.mem.Allocator) anyerror!AstNode {
    var statement: AstNode = .init(.Statement, "");

    // TODO - check for keyword statements (e.g. "if", "for", etc.)

    if (self.check(.If)) {}

    if (self.check(.For)) {}

    // Statements should have a valid expression at least
    std.debug.print("Parsing statement '{s}' of type '{s}' on line {} in column {}\n", .{
        self.currentToken().value,
        @tagName(self.currentToken().type),
        self.currentToken().line,
        self.currentToken().column,
    });
    const expression = try self.parseExpression(allocator);

    if (self.check(.Colon)) {
        // This is a declaration (ident : [type] = ... ;)
        const declaration = try self.parseDeclaration(allocator, expression);
        try statement.addChild(allocator, declaration);
    } else if (self.check(.Assignment)) {
        // Re-assigning an existing name
        const assignment = try self.parseAssignment(allocator, expression);
        try statement.addChild(allocator, assignment);
    } else {
        // Just a simple expression statement
        try statement.addChild(allocator, expression);
    }

    // Statements must end with a semicolon
    self.expect(.Semicolon) catch |err| {
        std.debug.print("Missing semicolon at end of statement\n", .{});
        statement.deinit(allocator);
        return err;
    };
    return statement;
}

/// A declaration defines a variable or type alias using the following syntax:
/// ## Variable/Value Scope
/// `ident : [type] = ... ;`
/// ## Type alias
/// `ident : {[statments]} ;`
fn parseDeclaration(self: *Self, allocator: std.mem.Allocator, identifier: AstNode) !AstNode {
    std.debug.print("Parsing declaration on line {}\n", .{self.currentToken().line});
    var declaration: AstNode = .init(.Declaration, "");
    try declaration.addChild(allocator, identifier);

    self.expect(.Colon) catch |err| {
        std.debug.print("Missing colon in declaration\n", .{});
        declaration.deinit(allocator);
        return err;
    };

    if (self.check(.Assignment)) {
        // Type is omitted so should be inferred (ident := ... ;)
        self.advance();
        // Only expressions can be assigned to names
        const expression = try self.parseExpression(allocator);
        try declaration.addChild(allocator, expression);
    } else {
        // Type is explicitly specified (ident : type = ... ;)
        const type_node = try self.parseType(allocator);
        try declaration.addChild(allocator, type_node);

        std.debug.print("Parsed '{s}' type in declaration on line {} in column {}\n", .{
            self.currentToken().value,
            self.currentToken().line,
            self.currentToken().column,
        });

        if (self.match(.Assignment)) {
            std.debug.print("Assignment declaration current token: '{s}' of type '{s}' on line {} in column {}\n", .{
                self.currentToken().value,
                @tagName(self.currentToken().type),
                self.currentToken().line,
                self.currentToken().column,
            });
            // Without assignment, this is a type scope alias instead of a variable
            const expression = try self.parseExpression(allocator);
            try declaration.addChild(allocator, expression);
        }
    }

    return declaration;
}

/// Assignment is just two expressions surrounding an equal sign:
/// `expression = expression`
fn parseAssignment(self: *Self, allocator: std.mem.Allocator, left_expr: AstNode) !AstNode {
    std.debug.print("Parsing assignment on line {}\n", .{self.currentToken().line});
    var assignment: AstNode = .init(.Assignment, "");
    try assignment.addChild(allocator, left_expr);

    try self.expect(.Assignment);

    const right_expr = try self.parseExpression(allocator);
    try assignment.addChild(allocator, right_expr);
    return assignment;
}

/// Expressions are maths expressions with support for identifiers.
/// They can be comparisons also. However, as soon as an assignment comes in,
/// they become statements.
///
/// A single identifier is a valid expression. The same is true for a single number
fn parseExpression(self: *Self, allocator: std.mem.Allocator) !AstNode {
    return try self.parseLogicalOr(allocator);
}

fn parseLogicalOr(self: *Self, allocator: std.mem.Allocator) !AstNode {
    var expression = try self.parseLogicalAnd(allocator);
    while (self.check(.Or)) {
        var op: AstNode = .init(.BinaryExpr, self.currentToken().value);
        self.advance();
        const right_expr = try self.parseLogicalAnd(allocator);
        try op.addChild(allocator, expression);
        try op.addChild(allocator, right_expr);
        expression = op;
    }

    return expression;
}

fn parseLogicalAnd(self: *Self, allocator: std.mem.Allocator) !AstNode {
    var expression = try self.parseEquality(allocator);
    while (self.check(.And)) {
        var op: AstNode = .init(.BinaryExpr, self.currentToken().value);
        self.advance();
        const right_expr = try self.parseEquality(allocator);
        try op.addChild(allocator, expression);
        try op.addChild(allocator, right_expr);
        expression = op;
    }

    return expression;
}

fn parseEquality(self: *Self, allocator: std.mem.Allocator) !AstNode {
    var expression = try self.parseComparison(allocator);
    while (self.check(.Equal) or self.check(.NotEqual)) {
        var op: AstNode = .init(.BinaryExpr, self.currentToken().value);
        self.advance();
        const right_expr = try self.parseComparison(allocator);
        try op.addChild(allocator, expression);
        try op.addChild(allocator, right_expr);
        expression = op;
    }

    return expression;
}

fn parseComparison(self: *Self, allocator: std.mem.Allocator) !AstNode {
    var expression = try self.parseAddition(allocator);
    while (self.check(.LessThan) or self.check(.GreaterThan) or self.check(.LessEqual) or self.check(.GreaterEqual)) {
        var op: AstNode = .init(.BinaryExpr, self.currentToken().value);
        self.advance();
        const right_expr = try self.parseAddition(allocator);
        try op.addChild(allocator, expression);
        try op.addChild(allocator, right_expr);
        expression = op;
    }

    return expression;
}

fn parseAddition(self: *Self, allocator: std.mem.Allocator) !AstNode {
    var expression = try self.parseMultiplication(allocator);
    while (self.check(.Plus) or self.check(.Minus)) {
        var op: AstNode = .init(.BinaryExpr, self.currentToken().value);
        self.advance();
        const right_expr = try self.parseMultiplication(allocator);
        try op.addChild(allocator, expression);
        try op.addChild(allocator, right_expr);
        expression = op;
    }

    return expression;
}

fn parseMultiplication(self: *Self, allocator: std.mem.Allocator) !AstNode {
    var expression = try self.parseUnary(allocator);
    while (self.check(.Asterisk) or self.check(.Slash)) {
        var op: AstNode = .init(.BinaryExpr, self.currentToken().value);
        self.advance();
        const right_expr = try self.parseUnary(allocator);
        try op.addChild(allocator, expression);
        try op.addChild(allocator, right_expr);
        expression = op;
    }

    return expression;
}

fn parseUnary(self: *Self, allocator: std.mem.Allocator) !AstNode {
    if (self.check(.Plus) or self.check(.Minus) or self.check(.Not)) {
        var expression: AstNode = .init(.UnaryExpr, self.currentToken().value);
        self.advance();
        const operand = try self.parseUnary(allocator);
        try expression.addChild(allocator, operand);
        return expression;
    }

    return try self.parsePostfix(allocator);
}

fn parsePostfix(self: *Self, allocator: std.mem.Allocator) anyerror!AstNode {
    var expression = try self.parsePrimary(allocator);
    while (self.check(.Dot) or self.check(.LeftBracket)) {
        if (self.check(.Dot)) {
            self.advance();
            const field = try self.parseIdentifier();
            var local_access: AstNode = .init(.LocalAccess, "");
            try local_access.addChild(allocator, expression);
            try local_access.addChild(allocator, field);
            expression = local_access;
            continue;
        }

        if (self.match(.LeftBracket)) {
            var parameters: AstNode = .init(.CallParameterList, "");
            while (!self.check(.RightBracket)) {
                const param_expr = try self.parseExpression(allocator);
                try parameters.addChild(allocator, param_expr);

                if (self.check(.Comma)) {
                    self.advance();
                } else {
                    break;
                }
            }
            try self.expect(.RightBracket);
            try expression.addChild(allocator, parameters);
            continue;
        }
    }

    return expression;
}

/// A primary is an identifier, a number literal, or a left bracket in which another expression resides
fn parsePrimary(self: *Self, allocator: std.mem.Allocator) anyerror!AstNode {
    std.debug.print("Parsing primary '{s}' of type '{s}' on line {} in column {}\n", .{
        self.currentToken().value,
        @tagName(self.currentToken().type),
        self.currentToken().line,
        self.currentToken().column,
    });
    if (self.check(.Identifier)) {
        return self.parseIdentifier();
    } else if (self.check(.Number)) {
        return self.parseNumber();
    } else if (self.check(.LeftBracket)) {
        // Check if the bracket signifies a value scope or a group expression
        // Value scopes match:
        // - () {}
        // - (ident: type [, ident: type]) {}
        // We only need to check the next 2 tokens to know if it's a value scope
        if ((self.checkAt(.RightBracket, 1) and self.checkAt(.LeftBrace, 2)) or (self.checkAt(.Identifier, 1) and self.checkAt(.Colon, 2))) {
            return try self.parseValueScope(allocator);
        }

        // Otherwise, just parse the contents of the brackets like a normal expression
        self.advance();
        const expression = try self.parseExpression(allocator);
        try self.expect(.RightBracket);
        return expression;
    }

    std.debug.print("Unknown primary '{s}'\n", .{self.currentToken().value});
    return ParseError.InvalidSyntax;
}

fn parseIdentifier(self: *Self) !AstNode {
    if (!self.check(.Identifier)) {
        return ParseError.UnexpectedToken;
    }

    const identifier: AstNode = .init(.Identifier, self.currentToken().value);
    self.advance();
    return identifier;
}

fn parseNumber(self: *Self) !AstNode {
    if (!self.check(.Number)) {
        return ParseError.UnexpectedToken;
    }

    const number: AstNode = .init(.Number, self.currentToken().value);
    self.advance();
    return number;
}

fn parseType(self: *Self, allocator: std.mem.Allocator) !AstNode {
    if (self.check(.Identifier) or self.currentToken().type.isType()) {
        // The type is either a fundamental or an alias name
        const type_ident: AstNode = .init(.Type, self.currentToken().value);
        self.advance();
        return type_ident;
    } else if (self.check(.LeftBrace)) {
        // Inlined type scope
        return try self.parseTypeScope(allocator);
    }

    std.debug.print(
        "Expected identifier, fundamental, or type scope for type, not '{s}'\n",
        .{self.currentToken().value},
    );
    return ParseError.UnexpectedToken;
}

fn parseParameterList(self: *Self, allocator: std.mem.Allocator) !AstNode {
    self.advance();
    var list: AstNode = .init(.ParameterList, "");
    while (!self.check(.RightBracket)) {
        const parameter = try self.parseParameter(allocator);
        try list.addChild(allocator, parameter);

        if (self.check(.Comma)) {
            self.advance();
        } else {
            break;
        }
    }

    self.expect(.RightBracket) catch |err| {
        std.debug.print("Expected closing bracket in parameter list\n", .{});
        return err;
    };
    return list;
}

fn parseParameter(self: *Self, allocator: std.mem.Allocator) !AstNode {
    var parameter: AstNode = .init(.Parameter, "");

    if (!self.check(.Identifier)) {
        std.debug.print("Expected identifier in parameter\n", .{});
        return ParseError.InvalidSyntax;
    }

    const identifier = try self.parseIdentifier();
    try parameter.addChild(allocator, identifier);

    self.expect(.Colon) catch |err| {
        std.debug.print("Expected colon after identifier in parameter\n", .{});
        return err;
    };

    const type_node = try self.parseType(allocator);
    try parameter.addChild(allocator, type_node);

    return parameter;
}

fn parseTypeScope(self: *Self, allocator: std.mem.Allocator) !AstNode {
    self.expect(.LeftBrace) catch |err| {
        std.debug.print("Missing left brace in type scope definition\n", .{});
        return err;
    };

    var scope: AstNode = .init(.TypeScope, "");
    return try self.parseScope(allocator, &scope);
}

fn parseValueScope(self: *Self, allocator: std.mem.Allocator) !AstNode {
    var scope: AstNode = .init(.ValueScope, "");
    if (self.check(.LeftBracket)) {
        const parameter_list = try self.parseParameterList(allocator);
        try scope.addChild(allocator, parameter_list);
    }

    self.expect(.LeftBrace) catch |err| {
        std.debug.print("Missing left brace in value scope definition", .{});
        return err;
    };

    return try self.parseScope(allocator, &scope);
}

fn parseScope(self: *Self, allocator: std.mem.Allocator, scope: *AstNode) !AstNode {
    while (!self.check(.RightBrace)) {
        const statement = try self.parseStatement(allocator);
        try scope.addChild(allocator, statement);
    }

    try self.expect(.RightBrace);
    return scope.*;
}
