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
    InvalidCallOnValue,
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
    arena: Allocator,
    allocator: Allocator,
    objects: ?*Object,

    ip: usize,
    running: bool,

    gc: GC,
    strings: StringMap,
    globals: GlobalMap,
    greyList: ArrayList(*Object),

    frames: ArrayList(CallFrame),
    stack: ArrayList(Value),

    const Self = @This();

    pub fn create() Self {
        return .{
            .arena = undefined,
            .allocator = undefined,
            .objects = null,
            .ip = 0,
            .running = true,
            .gc = undefined,
            .strings = undefined,
            .globals = undefined,
            .greyList = undefined,
            .frames = undefined,
            .stack = undefined,
        };
    }

    pub fn init(self: *Self, arena: Allocator, allocator: Allocator) !void {
        var fba = FixedBufferAllocator.init(&stack_buffer);
        const stackAllocator = fba.allocator();

        self.arena = arena;
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

    pub fn start(self: *Self, function: *Function) !void {
        _ = self.callFunction(function, 0) catch {
            std.debug.print("Could not call function!\n", .{});
            return;
        };

        self.run() catch {
            std.debug.print("Failed to run program!\n", .{});
        };
    }

    fn cleanupWithGC(self: *Self) void {
        self.stack.clearRetainingCapacity();
        self.frames.clearRetainingCapacity();
        self.greyList.clearRetainingCapacity();
        self.strings.clearRetainingCapacity();
        self.globals.clearRetainingCapacity();

        self.gc.collectGarbage() catch unreachable;
    }

    fn runtimeError(self: *Self, msg: []const u8) void {
        @setCold(true);

        const frame = self.currentFrame();
        _ = frame;
        // FIXME: Add line numbers
        // const line = frame.function.chunk.findOpcodeLine(frame.ip);
        const line = 1;
        std.debug.print("Error: {s} [line {d}]\n", .{ msg, line });

        var idx: isize = @intCast(isize, self.frames.items.len) - 1;
        while (idx >= 0) : (idx -= 1) {
            const stackFrame = &self.frames.items[@intCast(usize, idx)];
            const function = stackFrame.function;
            const funcLine = 1;

            std.debug.print("[line {d}] in {s}()\n", .{
                funcLine,
                function.identifier.chars,
            });
        }
    }

    fn runtimeErrorAlloc(self: *Self, comptime fmt: []const u8, args: anytype) !void {
        @setCold(true);
        const msg = try std.fmt.allocPrint(self.arena, fmt, args);
        self.runtimeError(msg);
    }

    fn callValue(self: *Self, value: *Value, argCount: usize) !bool {
        if (!value.isObject()) {
            self.runtimeError("Cannot call non-function value");
            return VMError.InvalidCallOnValue;
        }

        const object = value.asObject();
        return switch (object.kind) {
            .Function => try self.callFunction(object.asFunction(), argCount),
            else => {
                self.runtimeError("Cannot call non-function object");
                return VMError.InvalidCallOnValue;
            },
        };
    }

    fn callFunction(self: *Self, function: *objects.Function, argCount: usize) !bool {
        if (function.arity != argCount) {
            try self.runtimeErrorAlloc(
                "Function '{s}' expected {d} arguments, but received {d}.",
                .{ function.identifier.chars, function.arity, argCount },
            );
            return false;
        }

        try self.pushFrame(CallFrame.create(
            function,
            self.stack.items.len - argCount,
        ));
        return true;
    }

    inline fn pushFrame(self: *Self, frame: CallFrame) !void {
        try self.frames.append(frame);
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
                .Pop => _ = try self.pop(),

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

                .Function => {
                    const value = self.readConstant();
                    try self.push(value);
                },

                .Call => {
                    const argCount = self.readByte();
                    var func = self.peek(@intCast(usize, argCount));

                    _ = try self.callValue(&func, argCount);
                },

                .Return => {
                    const result = try self.pop();
                    const oldFrame = self.frames.pop();

                    if (self.frames.items.len == 0) {
                        return;
                    }

                    try self.stack.resize(oldFrame.slot);
                    try self.push(result);
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
