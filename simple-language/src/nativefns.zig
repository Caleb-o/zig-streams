const std = @import("std");
const Value = @import("value.zig").Value;

pub fn clock(args: []Value) Value {
    _ = args;
    return Value.fromNumber(@intToFloat(f32, std.time.milliTimestamp()) / 1000);
}
