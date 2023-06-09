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

    pub fn fromObject(value: *Object) Self {
        return .{ .object = value };
    }

    // Helpers
    pub fn isNil(self: *const Self) bool {
        return self.* == .nil;
    }

    pub fn isNumber(self: *const Self) bool {
        return self.* == .number;
    }

    pub fn isBoolean(self: *const Self) bool {
        return self.* == .boolean;
    }

    pub fn isObject(self: *const Self) bool {
        return self.* == .object;
    }

    // -- Casts
    // NOTE: Dumb but for completeness
    pub fn asNil(self: Self) void {
        assert(self.isNil());
        return self.nil;
    }

    pub fn asNumber(self: *const Self) f32 {
        assert(self.isNumber());
        return self.number;
    }

    pub fn asBoolean(self: *const Self) bool {
        assert(self.isBoolean());
        return self.boolean;
    }

    pub fn asObject(self: *const Self) *Object {
        assert(self.isObject());
        return self.object;
    }

    pub fn print(self: *const Self) void {
        const printd = std.debug.print;
        switch (self.*) {
            .nil => printd("nil", .{}),
            .number => |v| printd("{d}", .{v}),
            .boolean => |v| printd("{}", .{v}),
            .object => |o| o.print(),
        }
    }
};
