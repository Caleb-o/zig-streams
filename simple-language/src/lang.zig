const std = @import("std");
const Allocator = std.mem.Allocator;

const VM = @import("vm.zig").VM;
const Compiler = @import("compiler.zig").Compiler;
const Function = @import("object.zig").Function;

pub inline fn compile(
    arena: Allocator,
    vm: *VM,
    source: []const u8,
) ?*Function {
    var compiler = Compiler.init(source, vm, arena);
    return compiler.compile() catch |err| {
        std.debug.print("Compiler Error: {s}\n", .{@errorName(err)});
        return null;
    };
}

pub fn compileAndRun(arena: Allocator, source: []const u8) !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const galloc = gpa.allocator();
    defer {
        const leaked = gpa.deinit();
        if (leaked == .leak) std.debug.panic("LEAKED\n", .{});
    }

    var vm = VM.create();
    try vm.init(arena, galloc);
    defer vm.deinit();

    if (compile(arena, &vm, source)) |func| {
        try vm.start(func);
    }
}
