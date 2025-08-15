const std = @import("std");
const Lexer = @import("lexer.zig");
const Parser = @import("parser.zig");
const Semantics = @import("semantics.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    var args = try std.process.ArgIterator.initWithAllocator(gpa.allocator());
    defer args.deinit();

    _ = args.skip();

    var srcPath: []const u8 = "";
    if (args.next()) |src| {
        srcPath = src;
    }

    var outPath: []const u8 = "out.txt";
    while (args.next()) |arg| {
        if (std.mem.eql(u8, arg, "-h") or std.mem.eql(u8, arg, "--help")) {
            std.debug.print(
                \\Oliver's Programming Language (OPL) Compiler
                \\
                \\Usage: opl [commands]
                \\
                \\Commands:
                \\
                \\  -h, --help       This help screen
                \\  -o, --output     The target file to compile to
            , .{});
            return;
        }

        if (std.mem.eql(u8, arg, "-o") or std.mem.eql(u8, arg, "--output")) {
            if (args.next()) |out| {
                outPath = out;
            } else {
                std.debug.print("Invalid output path\n", .{});
                return;
            }
        }
    }

    if (srcPath.len == 0) {
        std.debug.print("No source file given!", .{});
        return;
    }

    const cwd = std.fs.cwd();
    const file = cwd.openFile(srcPath, .{}) catch |err| switch (err) {
        error.FileNotFound => {
            std.debug.print("Failed to find source file\n", .{});
            return;
        },
        else => return err,
    };
    defer file.close();

    const fileStat = try file.stat();
    const buffer = try gpa.allocator().alloc(u8, @intCast(fileStat.size));
    defer gpa.allocator().free(buffer);
    _ = try file.readAll(buffer);

    std.debug.print("--- COMPILING ---\n", .{});
    std.debug.print("Source relative path: '{s}'\n", .{srcPath});

    std.debug.print("\n", .{});

    std.debug.print("--- LEXING ---\n", .{});
    var lexer: Lexer = try .init(buffer);
    std.debug.print("--- LEXING FINISHED ---\n", .{});

    std.debug.print("\n", .{});

    std.debug.print("--- PARSING ---\n", .{});
    var parser: Parser = .init(gpa.allocator(), &lexer);
    var rootNode = parser.parse() catch |err| {
        std.debug.print("Parse error: {}\n", .{err});
        return;
    };
    defer rootNode.deinit();
    std.debug.print("--- PARSING FINISHED ---\n", .{});

    std.debug.print("\n", .{});

    std.debug.print("AST:\n", .{});
    rootNode.printTree("", true) catch {};

    std.debug.print("\n", .{});

    std.debug.print("--- SEMANTIC ANALYSIS ---\n", .{});
    var semanticAnalyser: Semantics.Analyser = .init(gpa.allocator());
    defer semanticAnalyser.deinit();
    semanticAnalyser.analyse(&rootNode) catch |err| {
        std.debug.print("Semantic analysis error: {}\n", .{err});
        return;
    };
    std.debug.print("--- SEMANTIC ANALYSIS FINISHED ---\n", .{});

    std.debug.print("\n", .{});

    std.debug.print("--- COMPILATION FINISHED ---\n", .{});
}
