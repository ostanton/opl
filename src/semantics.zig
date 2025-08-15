const std = @import("std");
const Parser = @import("parser.zig");
const AstNode = Parser.AstNode;

const Symbol = struct {
    name: []const u8,
    type: SymbolType,
    scopeDepth: u32,

    const SymbolType = union(enum) {
        type: Type,
        function: FunctionInfo,
        variable: VariableInfo,
    };

    const Type = union(enum) {
        fundamental: FundamentalType,
        customType: TypeInfo,
    };

    const FundamentalType = enum {
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
    };

    const TypeInfo = struct {
        name: []const u8,
        fields: std.StringHashMap(SymbolType),
        allocator: std.mem.Allocator,

        pub fn init(allocator: std.mem.Allocator, name: []const u8) TypeInfo {
            return .{
                .name = name,
                .fields = std.StringHashMap(SymbolType).init(allocator),
                .allocator = allocator,
            };
        }

        pub fn deinit(self: *TypeInfo) void {
            self.fields.deinit();
        }
    };

    const FunctionInfo = struct {
        parameters: []Type,
    };

    const VariableInfo = struct {
        type: Type,
        mutable: bool,
    };
};

const SymbolTable = struct {
    symbols: std.StringHashMap(Symbol),
    scopeDepth: u32,
    allocator: std.mem.Allocator,

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator) SymbolTable {
        return .{
            .symbols = std.StringHashMap(Symbol).init(allocator),
            .scopeDepth = 0,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Self) void {
        self.symbols.deinit();
    }

    pub fn enterScope(self: *Self) void {
        self.scopeDepth += 1;
    }

    pub fn exitScope(self: *Self) void {
        if (self.scopeDepth > 0) {
            self.scopeDepth -= 1;

            var iterator = self.symbols.iterator();
            while (iterator.next()) |entry| {
                if (entry.value_ptr.scopeDepth > self.scopeDepth) {
                    _ = self.symbols.remove(entry.key_ptr.*);
                }
            }
        }
    }

    pub fn define(self: *Self, name: []const u8, symbol: Symbol) !void {
        try self.symbols.put(name, symbol);
    }

    pub fn lookup(self: *Self, name: []const u8) ?Symbol {
        return self.symbols.get(name);
    }
};

const SemanticError = error{
    UndefinedSymbol,
    TypeMismatch,
    DuplicateDefinition,
    OutOfMemory,
    InvalidOperation,
};

pub const Analyser = struct {
    symbolTable: SymbolTable,
    allocator: std.mem.Allocator,

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator) Analyser {
        var analyser: Analyser = .{
            .symbolTable = .init(allocator),
            .allocator = allocator,
        };

        analyser.addBuiltinTypes() catch unreachable;

        return analyser;
    }

    pub fn deinit(self: *Self) void {
        self.symbolTable.deinit();
    }

    pub fn analyse(self: *Self, rootNode: *AstNode) SemanticError!void {
        try self.analyseNode(rootNode);
    }

    fn addBuiltinTypes(self: *Self) !void {
        const builtins = [_]Symbol.FundamentalType{
            .U8,
            .U16,
            .U32,
            .U64,
            .I8,
            .I16,
            .I32,
            .I64,
            .F32,
            .F64,
            .Bool,
            .Void,
        };

        for (builtins) |builtin| {
            const symbol: Symbol = .{
                .name = @tagName(builtin),
                .type = .{ .type = .{ .fundamental = builtin } },
                .scopeDepth = 0,
            };
            try self.symbolTable.define(@tagName(builtin), symbol);
        }
    }

    pub fn analyseNode(self: *Self, node: *AstNode) SemanticError!void {
        _ = self;
        _ = node;
    }
};
