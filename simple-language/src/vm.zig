const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const FixedBufferAllocator = std.heap.FixedBufferAllocator;

const compilation = @import("compilation.zig");
const Chunk = compilation.Chunk;
const ByteCode = compilation.ByteCode;

const objects = @import("object.zig");
const Object = objects.Object;
const Function = objects.Function;
const Value = @import("value.zig").Value;

const stack_count = 256;
const stack_size = stack_count * @sizeOf(Value);
var stack_buffer: [stack_size]u8 = undefined;

const VMError = error{
    StackOverflow,
    StackUnderflow,
    TypeError,
} || error{OutOfMemory};

const CallFrame = struct {
    function: *Function,
    slot: usize,
    ip: usize,

    const Self = @This();

    pub fn create(function: *Function, slot: usize) Self {
        return .{
            .function = function,
            .slot = slot,
            .ip = 0,
        };
    }
};

pub const VM = struct {
    allocator: Allocator,
    objects: ?*Object,

    ip: usize,
    running: bool,
    fba: FixedBufferAllocator,

    frames: ArrayList(CallFrame),
    stack: ArrayList(Value),

    const Self = @This();

    pub fn init(allocator: Allocator) !Self {
        var fba = FixedBufferAllocator.init(&stack_buffer);
        var stack_alloc = fba.allocator();

        return .{
            .allocator = allocator,
            .objects = null,
            .running = true,
            .ip = 0,
            .fba = fba,
            .frames = ArrayList(CallFrame).init(allocator),
            .stack = try ArrayList(Value).initCapacity(stack_alloc, stack_count),
        };
    }

    pub fn deinit(self: *Self) void {
        self.stack.deinit();
    }

    pub fn start(self: *Self, function: *Function) void {
        self.callFunc(function) catch {
            std.debug.print("Could not call function!\n", .{});
            return;
        };

        self.run() catch {
            std.debug.print("Failed to run program!\n", .{});
        };
    }

    fn callFunc(self: *Self, function: *Function) !void {
        try self.frames.append(CallFrame.create(function, self.stack.items.len));
    }

    fn push(self: *Self, value: Value) VMError!void {
        if (self.stack.items.len > std.math.maxInt(u8)) {
            std.debug.print("Error: Stack overflow!\n", .{});
            return VMError.StackOverflow;
        }
        try self.stack.append(value);
    }

    fn pop(self: *Self) !Value {
        if (self.stack.items.len == 0) {
            std.debug.print("Error: Stack underflow!\n", .{});
            return VMError.StackUnderflow;
        }

        return self.stack.pop();
    }

    // TODO: Consider a status return instead
    fn run(self: *Self) !void {
        defer std.debug.print("\nDone!\n", .{});
        while (self.running) {
            const instruction = @intToEnum(ByteCode, self.readByte());
            switch (instruction) {
                .ConstantByte => try self.push(self.readConstant()),

                .Add => try self.binaryOp('+'),
                .Sub => try self.binaryOp('-'),
                .Mul => try self.binaryOp('*'),
                .Div => try self.binaryOp('/'),

                .Print => (try self.pop()).print(),

                .Return => {
                    _ = self.frames.pop();
                    if (self.frames.items.len == 0) {
                        return;
                    }
                },

                else => unreachable,
            }
        }
    }

    fn currentFrame(self: *Self) *CallFrame {
        return &self.frames.items[self.frames.items.len - 1];
    }

    fn currentChunk(self: *Self) *Chunk {
        return &self.currentFrame().function.chunk;
    }

    fn readByte(self: *Self) u8 {
        const frame = self.currentFrame();
        defer frame.ip += 1;
        return self.currentChunk().code.items[frame.ip];
    }

    fn readConstant(self: *Self) Value {
        return self.currentChunk().constants.items[self.readByte()];
    }

    fn binaryOp(self: *Self, comptime op: u8) VMError!void {
        const rhs = try self.pop();
        const lhs = try self.pop();

        if (lhs.isNumber() and rhs.isNumber()) {
            const lval = lhs.asNumber();
            const rval = rhs.asNumber();

            switch (op) {
                '+' => try self.push(Value.fromNumber(lval + rval)),
                '-' => try self.push(Value.fromNumber(lval - rval)),
                '*' => try self.push(Value.fromNumber(lval * rval)),
                '/' => try self.push(Value.fromNumber(lval / rval)),
                else => unreachable,
            }

            return;
        }
        return VMError.TypeError;
    }
};
