const std = @import("std");
const Allocator = std.mem.Allocator;

const Object = @import("object.zig").Object;

pub const VM = struct {
    allocator: Allocator,
    objects: ?*Object,

    const Self = @This();

    pub fn create(allocator: Allocator) Self {
        return .{
            .allocator = allocator,
            .objects = null,
        };
    }
};
