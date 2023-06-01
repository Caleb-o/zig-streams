const std = @import("std");
const Allocator = std.mem.Allocator;

pub fn main() void {
    std.debug.print("Hello, World!\n", .{});
}

fn readFile(allocator: Allocator, file_name: []const u8) ![]u8 {
    const file = try std.fs.cwd().openFile(file_name, .{});
    const contents = try file.readToEndAlloc(allocator, (try file.stat()).size);
    return contents;
}
