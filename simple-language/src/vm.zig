const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const FixedBufferAllocator = std.heap.FixedBufferAllocator;

const debug = @import("debug.zig");

const compilation = @import("compilation.zig");
const Chunk = compilation.Chunk;
const ByteCode = compilation.ByteCode;

const objects = @import("object.zig");
const Object = objects.Object;
const Function = objects.Function;
const Value = @import("value.zig").Value;
const GC = @import("gc.zig").GC;

const StringMap = std.AutoArrayHashMap(u32, *objects.String);
const GlobalMap = std.StringArrayHashMap(Value);

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

    gc: GC,
    strings: StringMap,
    globals: GlobalMap,
    greyList: ArrayList(*Object),

    frames: ArrayList(CallFrame),
    stack: ArrayList(Value),

    const Self = @This();

    pub fn create() Self {
        return .{
            .allocator = undefined,
            .objects = null,
            .ip = 0,
            .running = true,
            .fba = undefined,
            .gc = undefined,
            .strings = undefined,
            .globals = undefined,
            .greyList = undefined,
            .frames = undefined,
            .stack = undefined,
        };
    }

    pub fn init(self: *Self, allocator: Allocator) !void {
        var fba = FixedBufferAllocator.init(&stack_buffer);
        const stackAllocator = fba.allocator();

        self.gc = GC.init(allocator, self);
        self.allocator = self.gc.allocator();

        self.stack = try ArrayList(Value).initCapacity(stackAllocator, stack_count);
        self.frames = try ArrayList(CallFrame).initCapacity(allocator, 4);
        self.greyList = ArrayList(*Object).init(std.heap.page_allocator);

        self.strings = StringMap.init(allocator);
        self.globals = GlobalMap.init(allocator);
    }

    pub fn deinit(self: *Self) void {
        self.cleanupWithGC();

        self.greyList.deinit();
        self.frames.deinit();
        self.strings.deinit();
        self.globals.deinit();
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

    fn cleanupWithGC(self: *Self) void {
        self.frames.clearRetainingCapacity();
        self.greyList.clearRetainingCapacity();
        self.strings.clearRetainingCapacity();
        self.globals.clearRetainingCapacity();

        self.gc.collectGarbage() catch unreachable;
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

    fn peek(self: *Self, depth: usize) Value {
        if (depth >= self.stack.items.len) {
            std.debug.panic("Peeking invalid index: {d}\n", .{depth});
        }
        return self.stack.items[self.stack.items.len - 1 - depth];
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
        while (self.running) {
            const instruction = @intToEnum(ByteCode, self.readByte());
            switch (instruction) {
                .ConstantByte => try self.push(self.readConstant()),

                .Add => try self.binaryOp('+'),
                .Sub => try self.binaryOp('-'),
                .Mul => try self.binaryOp('*'),
                .Div => try self.binaryOp('/'),

                .GetLocal => {
                    const index = self.readByte();

                    try self.push(self.stack.items[self.currentFrame().slot + index]);
                },

                .SetLocal => {
                    const index = self.readByte();
                    self.stack.items[self.currentFrame().slot + index] = self.peek(0);
                },

                .GetGlobal => {
                    const identifier = self.readString();
                    const value = try self.globals.getOrPutValue(identifier.chars, Value.fromNil());
                    try self.push(value.value_ptr.*);
                },
                .SetGlobal => {
                    const identifier = self.readString();
                    try self.globals.put(identifier.chars, self.peek(0));
                },

                .Print => {
                    (try self.pop()).print();
                    std.debug.print("\n", .{});
                },

                .Return => {
                    _ = self.frames.pop();
                    if (self.frames.items.len == 0) {
                        return;
                    }
                },

                .True => try self.push(Value.fromBoolean(true)),
                .False => try self.push(Value.fromBoolean(false)),
                .Nil => try self.push(Value.fromNil()),

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

    inline fn readByte(self: *Self) u8 {
        const frame = self.currentFrame();
        defer frame.ip += 1;
        return self.currentChunk().code.items[frame.ip];
    }

    inline fn readConstant(self: *Self) Value {
        return self.currentChunk().constants.items[self.readByte()];
    }

    inline fn readString(self: *Self) *objects.String {
        var val = self.readConstant();
        return val.asObject().asString();
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
