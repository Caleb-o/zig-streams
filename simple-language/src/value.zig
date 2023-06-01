const std = @import("std");
const assert = std.debug.assert;

const Object = @import("object.zig").Object;

pub const Value = union(enum) {
    nil: void,
    number: f32,
    boolean: bool,
    object: *Object,

    const Self = @This();

    pub fn fromNil() Self {
        return .{ .nil = {} };
    }

    pub fn fromNumber(value: f32) Self {
        return .{ .number = value };
    }

    pub fn fromBoolean(value: bool) Self {
        return .{ .boolean = value };
    }

    // Helpers
    pub fn isNil(self: *Self) bool {
        return self == .nil;
    }

    pub fn isNumber(self: *Self) bool {
        return self == .number;
    }

    pub fn isBoolean(self: *Self) bool {
        return self == .boolean;
    }

    // -- Casts
    // NOTE: Dumb but for completeness
    pub fn asNil(self: Self) void {
        assert(self.isNil());
        return self.nil;
    }

    pub fn asNumber(self: *Self) f32 {
        assert(self.isNumber());
        return self.number;
    }

    pub fn asBoolean(self: *Self) bool {
        assert(self.isBoolean());
        return self.boolean;
    }

    pub fn print(self: *Self) void {
        const printd = std.debug.print;
        switch (self.*) {
            .nil => printd("nil", .{}),
            .number => |v| printd("{d}", .{v}),
            .boolean => |v| printd("{}", .{v}),
            else => unreachable,
        }
    }
};
