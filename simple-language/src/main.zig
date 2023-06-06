const std = @import("std");
const Allocator = std.mem.Allocator;
const Arena = std.heap.ArenaAllocator;

const Compiler = @import("compiler.zig").Compiler;
const VM = @import("vm.zig").VM;
const lang = @import("lang.zig");

pub fn main() !void {
    var arena = Arena.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len != 2) {
        std.debug.print("usage: sl script\n", .{});
        return;
    }

    const file_contents = try readFile(allocator, args[1]);
    std.debug.print("Loaded file '{s}'\n", .{args[1]});

    try lang.compileAndRun(arena, file_contents);
}

fn readFile(allocator: Allocator, file_name: []const u8) ![]u8 {
    const file = try std.fs.cwd().openFile(file_name, .{});
    const contents = try file.readToEndAlloc(allocator, (try file.stat()).size);
    return contents;
}
