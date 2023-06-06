const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const compilation = @import("compilation.zig");
const ByteCode = compilation.ByteCode;

// NOTE: ByteBuilder does not handle memory failure
pub const ByteBuilder = struct {
    code: ArrayList(u8),

    const Self = @This();

    pub fn init(allocator: Allocator) Self {
        return .{ .code = ArrayList(u8).init(allocator) };
    }

    pub inline fn deinit(self: *Self) void {
        self.code.deinit();
    }

    pub fn bytes(self: *Self) []const u8 {
        return self.code.items;
    }

    // ====== INSTRUCTIONS
    pub fn constantByte(self: *Self, location: u8) *Self {
        self.code.append(@enumToInt(ByteCode.ConstantByte)) catch unreachable;
        self.code.append(location) catch unreachable;
        return self;
    }

    pub fn pop(self: *Self) *Self {
        self.code.append(@enumToInt(ByteCode.Pop)) catch unreachable;
        return self;
    }

    pub fn add(self: *Self) *Self {
        self.code.append(@enumToInt(ByteCode.Add)) catch unreachable;
        return self;
    }

    pub fn sub(self: *Self) *Self {
        self.code.append(@enumToInt(ByteCode.Sub)) catch unreachable;
        return self;
    }

    pub fn mul(self: *Self) *Self {
        self.code.append(@enumToInt(ByteCode.Mul)) catch unreachable;
        return self;
    }

    pub fn div(self: *Self) *Self {
        self.code.append(@enumToInt(ByteCode.Div)) catch unreachable;
        return self;
    }

    pub fn jump(self: *Self, location: u8) *Self {
        self.code.append(@enumToInt(ByteCode.Jump)) catch unreachable;
        self.code.append(location) catch unreachable;
        return self;
    }

    pub fn jumpNot(self: *Self, location: u8) *Self {
        self.code.append(@enumToInt(ByteCode.JumpNot)) catch unreachable;
        self.code.append(location) catch unreachable;
        return self;
    }

    pub fn getLocal(self: *Self, location: u8) *Self {
        self.code.append(@enumToInt(ByteCode.GetLocal)) catch unreachable;
        self.code.append(location) catch unreachable;
        return self;
    }

    pub fn setLocal(self: *Self, location: u8) *Self {
        self.code.append(@enumToInt(ByteCode.SetLocal)) catch unreachable;
        self.code.append(location) catch unreachable;
        return self;
    }

    pub fn getGlobal(self: *Self, location: u8) *Self {
        self.code.append(@enumToInt(ByteCode.GetGlobal)) catch unreachable;
        self.code.append(location) catch unreachable;
        return self;
    }

    pub fn setGlobal(self: *Self, location: u8) *Self {
        self.code.append(@enumToInt(ByteCode.SetGlobal)) catch unreachable;
        self.code.append(location) catch unreachable;
        return self;
    }

    pub fn function(self: *Self, location: u8) *Self {
        self.code.append(@enumToInt(ByteCode.Function)) catch unreachable;
        self.code.append(location) catch unreachable;
        return self;
    }

    pub fn call(self: *Self, location: u8) *Self {
        self.code.append(@enumToInt(ByteCode.Call)) catch unreachable;
        self.code.append(location) catch unreachable;
        return self;
    }

    pub fn @"true"(self: *Self) *Self {
        self.code.append(@enumToInt(ByteCode.True)) catch unreachable;
        return self;
    }

    pub fn @"false"(self: *Self) *Self {
        self.code.append(@enumToInt(ByteCode.False)) catch unreachable;
        return self;
    }

    pub fn nil(self: *Self) *Self {
        self.code.append(@enumToInt(ByteCode.Nil)) catch unreachable;
        return self;
    }

    pub fn intoList(self: *Self, count: u8) *Self {
        self.code.append(@enumToInt(ByteCode.IntoList)) catch unreachable;
        self.code.append(count) catch unreachable;
        return self;
    }

    pub fn @"return"(self: *Self) *Self {
        self.code.append(@enumToInt(ByteCode.Return)) catch unreachable;
        return self;
    }

    pub fn returnNil(self: *Self) *Self {
        self.code.append(@enumToInt(ByteCode.Nil)) catch unreachable;
        self.code.append(@enumToInt(ByteCode.Return)) catch unreachable;
        return self;
    }

    pub fn print(self: *Self) *Self {
        self.code.append(@enumToInt(ByteCode.Print)) catch unreachable;
        return self;
    }
};
