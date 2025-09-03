const std = @import("std");
const Lexer = @import("lexer.zig");
const Parser = @import("parser2.zig");
const Semantics = @import("semantics.zig");

pub fn main() !void {
    const func = struct {
        pub fn func(str: []const u8) void {
            std.debug.print("{s}\n", .{str});
        }
    }.func;
    func("Hello world!");
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    const allocator = gpa.allocator();

    var args = try std.process.ArgIterator.initWithAllocator(allocator);
    defer args.deinit();

    _ = args.skip();

    var src_path: []const u8 = "";
    if (args.next()) |src| {
        src_path = src;
    }

    var out_path: []const u8 = "out.txt";
    while (args.next()) |arg| {
        if (std.mem.eql(u8, arg, "-h") or std.mem.eql(u8, arg, "--help")) {
            std.debug.print(
                \\Oliver's Programming Language (OPL) Compiler
                \\
                \\Usage: opl [input file] [commands]
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
                out_path = out;
            } else {
                std.debug.print("Invalid output path\n", .{});
                return;
            }
        }
    }

    if (src_path.len == 0) {
        std.debug.print("No source file given!", .{});
        return;
    }

    const cwd = std.fs.cwd();
    const file = cwd.openFile(src_path, .{}) catch |err| switch (err) {
        error.FileNotFound => {
            std.debug.print("Failed to find source file\n", .{});
            return;
        },
        else => return err,
    };
    defer file.close();

    const file_stat = try file.stat();
    const buffer = try allocator.alloc(u8, @intCast(file_stat.size));
    defer allocator.free(buffer);
    _ = try file.readAll(buffer);

    std.debug.print("--- COMPILING ---\n", .{});
    std.debug.print("Source relative path: '{s}'\n", .{src_path});
    std.debug.print("Source contents:\n{s}\n", .{buffer});

    std.debug.print("\n", .{});

    std.debug.print("--- LEXING ---\n", .{});
    var lexer: Lexer = try .init(buffer);
    std.debug.print("--- LEXING FINISHED ---\n", .{});

    std.debug.print("\n", .{});

    std.debug.print("--- PARSING ---\n", .{});
    var parser: Parser = .init(&lexer);
    var root_node = parser.parse(allocator) catch |err| {
        std.debug.print("Parse error: {}\n", .{err});
        return;
    };
    defer root_node.deinit(allocator);
    std.debug.print("--- PARSING FINISHED ---\n", .{});

    std.debug.print("\n", .{});

    std.debug.print("AST:\n", .{});
    //rootNode.printTree("", true) catch {};
    const tree_string = root_node.toString(allocator) catch |err| {
        std.debug.print("Error printing the AST: {}\n", .{err});
        return;
    };
    defer allocator.free(tree_string);
    std.debug.print("{s}\n", .{tree_string});

    const out_file = cwd.createFile(out_path, .{}) catch |err| {
        std.debug.print("Failed to write to output file: '{s}'\n", .{out_path});
        return err;
    };
    defer out_file.close();

    const out_size = try out_file.write(tree_string);
    std.debug.print("Wrote {} bytes to '{s}'\n", .{ out_size, out_path });

    std.debug.print("\n", .{});

    std.debug.print("--- SEMANTIC ANALYSIS ---\n", .{});
    //var semantic_analyser: Semantics.Analyser = .init(allocator);
    //defer semantic_analyser.deinit();
    //semantic_analyser.analyse(&root_node) catch |err| {
    //    std.debug.print("Semantic analysis error: {}\n", .{err});
    //    return;
    //};
    std.debug.print("--- SEMANTIC ANALYSIS FINISHED ---\n", .{});

    std.debug.print("\n", .{});

    std.debug.print("--- COMPILATION FINISHED ---\n", .{});
}
