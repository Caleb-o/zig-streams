const std = @import("std");
const Value = @import("value.zig").Value;
const VM = @import("vm.zig").VM;

pub fn clock(vm: *VM, args: []const Value) Value {
    _ = vm;
    _ = args;
    return Value.fromNumber(@intToFloat(f32, std.time.milliTimestamp()) / 1000);
}

pub fn collectGarbage(vm: *VM, args: []const Value) Value {
    _ = args;
    const bytesFreed = vm.gc.collectGarbage() catch {
        vm.runtimeError("Failed to run garbage collector");
        return Value.fromNil();
    };
    return Value.fromNumber(@intToFloat(f32, bytesFreed));
}
