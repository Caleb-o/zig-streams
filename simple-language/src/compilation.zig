const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const Value = @import("value.zig").Value;

pub const ByteCode = enum(u8) {
    ConstantByte, // Index
    Pop,

    Add,
    Sub,
    Mul,
    Div,

    Jump,
    JumpNot,

    GetLocal, // Index
    SetLocal, // Index
    GetGlobal, // string index
    SetGlobal, // string index

    Function, // Index
    Call, // Count

    True,
    False,
    Nil,

    Return,
    Print,
};

pub const Chunk = struct {
    code: ArrayList(u8),
    constants: ArrayList(Value),

    const Self = @This();

    pub fn init(allocator: Allocator) Self {
        return .{
            .code = ArrayList(u8).init(allocator),
            .constants = ArrayList(Value).init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.code.deinit();
        self.constants.deinit();
    }

    pub fn writeByte(self: *Self, op: u8) !void {
        try self.code.append(op);
    }

    pub fn writeBytes(self: *Self, op1: u8, op2: u8) !void {
        try self.code.append(op1);
        try self.code.append(op2);
    }

    pub fn writeOp(self: *Self, op: ByteCode) !void {
        try self.code.append(@enumToInt(op));
    }

    pub fn writeOps(self: *Self, op1: ByteCode, op2: ByteCode) !void {
        try self.code.append(@enumToInt(op1));
        try self.code.append(@enumToInt(op2));
    }

    pub fn writeOpByte(self: *Self, op1: ByteCode, op2: u8) !void {
        try self.code.append(@enumToInt(op1));
        try self.code.append(op2);
    }

    pub fn addConstant(self: *Self, value: Value) !usize {
        try self.constants.append(value);
        return self.constants.items.len - 1;
    }

    pub fn writeConstant(self: *Self, value: Value) !void {
        const index = try self.addConstant(value);
        // FIXME
        std.debug.assert(index < std.math.maxInt(u8));
        try self.writeOpByte(.ConstantByte, @intCast(u8, index));
    }

    pub fn disassemble(self: *Chunk, name: []const u8) void {
        var index: u32 = 0;
        const len = @intCast(u32, self.code.items.len);

        std.debug.print("=== {s} : {d} ===\n", .{ name, self.code.items.len });

        while (index < len) {
            std.debug.print("{d:0>4}  ", .{index});
            index = self.decode(index);
        }
    }

    fn decode(self: *Self, offset: u32) u32 {
        const instruction = @intToEnum(ByteCode, self.code.items[@intCast(usize, offset)]);
        return switch (instruction) {
            .ConstantByte => constantInstruction("OP_CONSTANT_BYTE", offset, self),
            .Pop => simpleInstruction("OP_POP", offset),

            .GetLocal => byteInstruction("OP_GET_LOCAL", offset, self),
            .SetLocal => byteInstruction("OP_SET_LOCAL", offset, self),

            .GetGlobal => constantInstruction("OP_GET_GLOBAL", offset, self),
            .SetGlobal => constantInstruction("OP_SET_GLOBAL", offset, self),

            .Function => constantInstruction("OP_FUNCTION", offset, self),
            .Call => byteInstruction("OP_CALL", offset, self),

            .Add => simpleInstruction("OP_ADD", offset),
            .Sub => simpleInstruction("OP_SUB", offset),
            .Mul => simpleInstruction("OP_MULTIPLY", offset),
            .Div => simpleInstruction("OP_DIVIDE", offset),

            .Nil => simpleInstruction("OP_NIL", offset),
            .True => simpleInstruction("OP_TRUE", offset),
            .False => simpleInstruction("OP_FALSE", offset),

            .Print => simpleInstruction("OP_PRINT", offset),
            .Return => simpleInstruction("OP_RETURN", offset),

            else => std.debug.panic("Undefined: {}\n", .{instruction}),
        };
    }

    fn simpleInstruction(tag: []const u8, offset: u32) u32 {
        std.debug.print("{s}\n", .{tag});
        return offset + 1;
    }

    fn byteInstruction(comptime name: []const u8, offset: u32, chunk: *Chunk) u32 {
        const slot = chunk.code.items[offset + 1];
        std.debug.print("{s:<16} {d:4}\n", .{ name, slot });
        return offset + 2;
    }

    fn constantInstruction(tag: []const u8, offset: u32, chunk: *Chunk) u32 {
        const index = chunk.code.items[offset + 1];
        std.debug.print("{s:<16} -- {d} '", .{ tag, index });
        chunk.constants.items[@intCast(usize, index)].print();
        std.debug.print("'\n", .{});
        return offset + 2;
    }
};
