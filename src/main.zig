const std = @import("std");
const Lexer = @import("lexer.zig");
const Parser = @import("parser.zig");

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

    std.debug.print("Source path: '{s}'\n", .{srcPath});
    std.debug.print("Output path: '{s}'\n", .{outPath});

    // TODO - open source file instead of literal text
    var lexer: Lexer = try .init(
        \\Point: {
        \\    x: f32 = 5;
        \\    y: f32 = 4;
        \\/* HELLO */};
        \\p: Point = {
        \\    x = 2; // x position!
        \\    y = 10;
        \\};
    );
    std.debug.print("Lexer finished\n", .{});
    var parser: Parser = .init(&lexer);
    const rootNode = parser.parse();
    std.debug.print("Parser finished\n", .{});
    try rootNode.printTree("", true);
}
