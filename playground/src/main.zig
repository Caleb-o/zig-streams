const std = @import("std");
const Allocator = std.mem.Allocator;

pub fn main() void {
    std.debug.print("Hello, World!\n", .{});
}

const TupleSliceError = error{
    InvalidType,
};

fn argsToSlice(comptime T: type, allocator: Allocator, tuple: anytype) !?[]T {
    const ArgsType = @TypeOf(tuple);
    const args_type_info = @typeInfo(ArgsType);
    const fields_info = args_type_info.Struct.fields;

    // std.builtin.Type
    if (fields_info.len == 0) {
        return null;
    }

    const slice = try allocator.alloc(T, fields_info.len);
    errdefer allocator.free(slice);
    var idx: usize = 0;

    inline for (fields_info) |field| {
        const fieldT = @typeInfo(field.type);
        const value = @field(tuple, field.name);
        slice[idx] = switch (fieldT) {
            .Int => @intCast(T, value),
            .Enum => |e| std.debug.panic("{any} {any}\n", .{ e.fields[0], value }),
            else => {
                std.debug.print("{s}\n", .{@tagName(fieldT)});
                return TupleSliceError.InvalidType;
            },
        };
        idx += 1;
    }

    return slice;
}

test "Tmp" {
    var buffer: [32]u8 = undefined;
    var fba = std.heap.FixedBufferAllocator.init(&buffer);
    const alloc = fba.allocator();

    const Enum = enum {
        ConstantByte,
        Pop,
        SetLocal,
        GetLocal,
    };

    const Builder = struct {
        buffer: std.ArrayList(u8),

        const Self = @This();

        pub fn init(allocator: Allocator) Self {
            return .{
                .buffer = std.ArrayList(u8).init(allocator),
            };
        }

        pub fn deinit(self: *Self) void {
            self.buffer.deinit();
        }

        pub fn bytes(self: *Self) []const u8 {
            return self.buffer.items;
        }

        pub fn constantByte(self: *Self, location: u8) *Self {
            self.buffer.append(@enumToInt(Enum.ConstantByte)) catch unreachable;
            self.buffer.append(location) catch unreachable;
            return self;
        }

        pub fn pop(self: *Self) *Self {
            self.buffer.append(@enumToInt(Enum.Pop)) catch unreachable;
            return self;
        }

        pub fn setLocal(self: *Self, location: u8) *Self {
            self.buffer.append(@enumToInt(Enum.SetLocal)) catch unreachable;
            self.buffer.append(location) catch unreachable;
            return self;
        }

        pub fn getLocal(self: *Self, location: u8) *Self {
            self.buffer.append(@enumToInt(Enum.GetLocal)) catch unreachable;
            self.buffer.append(location) catch unreachable;
            return self;
        }
    };

    var builder = Builder.init(alloc);
    defer builder.deinit();
    const bytes = builder
        .constantByte(0)
        .pop()
        .bytes();
    try std.testing.expect(std.mem.eql(u8, bytes, &[_]u8{ 0, 0, 1 }));
}
