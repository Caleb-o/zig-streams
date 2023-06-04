const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const testing = std.testing;

// export fn add(a: i32, b: i32) i32 {
//     return a + b;
// }

pub const StringBuilder = struct {
    allocator: Allocator,
    buffer: ArrayList(u8),

    const Self = @This();

    pub fn init(allocator: Allocator) Self {
        return .{
            .allocator = allocator,
            .buffer = ArrayList(u8).init(allocator),
        };
    }

    pub fn initCapacity(allocator: Allocator, num: usize) !Self {
        return .{
            .allocator = allocator,
            .buffer = try ArrayList(u8).initCapacity(allocator, num),
        };
    }

    pub fn deinit(self: *Self) void {
        self.buffer.deinit();
    }

    pub inline fn append(self: *Self, char: u8) !void {
        try self.buffer.append(char);
    }

    pub inline fn appendSlice(self: *Self, string: []const u8) !void {
        try self.buffer.appendSlice(string);
    }

    pub fn appendFmt(self: *Self, comptime fmt: []const u8, args: anytype) !void {
        try self.buffer.appendSlice(try std.fmt.allocPrint(
            self.allocator,
            fmt,
            args,
        ));
    }

    pub fn toString(self: *Self, allocator: Allocator) ![]u8 {
        const str = try allocator.alloc(u8, self.buffer.items.len);
        std.mem.copyForwards(u8, str, self.buffer.items);

        self.buffer.deinit();
        return str;
    }

    pub inline fn borrowStr(self: *Self) []const u8 {
        return self.buffer.items;
    }
};

test "basic functionality" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    var sb = StringBuilder.init(allocator);
    try sb.append('H');
    try sb.appendSlice("ello, ");
    try sb.appendFmt("W{d}rld!", .{0});

    var string = try sb.toString(allocator);
    defer allocator.free(string);

    try testing.expect(std.mem.eql(u8, string, "Hello, W0rld!"));
}

test "Stack String" {
    var buffer: [32]u8 = undefined;

    var fba = std.heap.FixedBufferAllocator.init(&buffer);
    const allocator = fba.allocator();

    var sb = try StringBuilder.initCapacity(allocator, 16);
    try sb.append('H');
    try sb.appendSlice("ello, ");
    try sb.appendFmt("W{d}rld!", .{0});

    try testing.expect(std.mem.eql(u8, sb.borrowStr(), "Hello, W0rld!"));
}
