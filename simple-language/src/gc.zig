const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const debug = @import("debug.zig");

const VM = @import("vm.zig").VM;
const Value = @import("value.zig").Value;
const objects = @import("object.zig");
const Object = objects.Object;
const ObjectKind = objects.ObjectKind;
const StringMap = std.AutoArrayHashMap(u32, *objects.String);

pub const Generation = struct {
    head: ?*Object,
    vm: *VM,

    const Self = @This();

    pub fn create(vm: *VM) Self {
        return .{ .head = null, .vm = vm };
    }

    pub inline fn reset(self: *Self) void {
        self.head = null;
    }

    pub inline fn resetMark(self: *Self) void {
        var maybeObject = self.head;

        while (maybeObject) |object| {
            object.marked = false;
            maybeObject = object.next;
        }
    }

    pub fn append(self: *Self, obj: *Object) void {
        obj.next = self.head;
        self.head = obj;
    }

    pub fn appendGeneration(self: *Self, other: *Self) void {
        var maybeObject = other.head;

        while (maybeObject) |object| {
            self.append(object);
            maybeObject = object.next;
        }
    }

    pub fn sweep(self: *Self) !void {
        var previous: ?*Object = null;
        var maybeObject = self.head;

        while (maybeObject) |obj| {
            if (obj.marked) {
                obj.marked = false;
                previous = obj;
                maybeObject = obj.next;
            } else {
                const unreached = obj;
                maybeObject = obj.next;
                if (previous) |prev| {
                    prev.next = maybeObject;
                } else {
                    self.head = maybeObject;
                }

                unreached.deinit(self.vm);
            }
        }
    }
};

pub const GC = struct {
    inner: Allocator,
    vm: *VM,
    bytesAllocated: usize,
    nextSweep: usize,

    generationCount: u8,
    young: Generation,
    old: Generation,

    const SWEEP_FACTOR: usize = 2;
    const GENERATION_CHECK: u8 = 3;

    const Self = @This();

    pub fn init(inner: Allocator, vm: *VM) Self {
        return .{
            .inner = inner,
            .vm = vm,
            .bytesAllocated = 0,
            .nextSweep = 1024 * 1024,
            .generationCount = 0,
            .young = Generation.create(vm),
            .old = Generation.create(vm),
        };
    }

    pub fn deinit(self: *Self) !void {
        self.generationCount = GENERATION_CHECK;
        try self.collectGarbage();
    }

    pub fn allocator(self: *Self) Allocator {
        return Allocator{
            .ptr = self,
            .vtable = &.{
                .alloc = alloc,
                .resize = resize,
                .free = free,
            },
        };
    }

    fn alloc(
        ctx: *anyopaque,
        n: usize,
        log2_ptr_align: u8,
        ret_addr: usize,
    ) ?[*]u8 {
        const self = @ptrCast(*Self, @alignCast(@alignOf(Self), ctx));
        if ((self.bytesAllocated + n > self.nextSweep) or debug.stress_gc) {
            self.collectGarbage() catch |err| {
                std.debug.print("GC Error: '{s}'\n", .{@errorName(err)});
                std.os.exit(1);
            };

            self.nextSweep = self.bytesAllocated * SWEEP_FACTOR;
        }

        var out = self.inner.rawAlloc(n, log2_ptr_align, ret_addr) orelse return null;
        self.bytesAllocated += n;

        return out;
    }

    fn resize(
        ctx: *anyopaque,
        buf: []u8,
        buf_align: u8,
        new_len: usize,
        ret_addr: usize,
    ) bool {
        const self = @ptrCast(*Self, @alignCast(@alignOf(Self), ctx));
        if (new_len > buf.len) {
            if ((self.bytesAllocated + (new_len - buf.len) > self.nextSweep) or debug.stress_gc) {
                self.collectGarbage() catch |err| {
                    std.debug.print("GC Error: '{s}'\n", .{@errorName(err)});
                    std.os.exit(1);
                };
            }
        }

        const resized = self.inner.rawResize(buf, buf_align, new_len, ret_addr);

        if (resized) {
            if (new_len > buf.len) {
                self.bytesAllocated += new_len - buf.len;
            } else {
                self.bytesAllocated -= buf.len - new_len;
            }
        }

        return resized;
    }

    fn free(
        ctx: *anyopaque,
        buf: []u8,
        buf_align: u8,
        ret_addr: usize,
    ) void {
        const self = @ptrCast(*Self, @alignCast(@alignOf(Self), ctx));
        self.inner.rawFree(buf, buf_align, ret_addr);
        self.bytesAllocated -= buf.len;

        if (debug.log_gc) {
            std.debug.print("Freeing {d} bytes\n", .{buf.len});
        }
    }

    pub fn collectGarbage(self: *Self) !void {
        if (debug.log_gc) {
            std.debug.print("-- gc begin\n", .{});
        }

        try self.markRoots();
        try self.traceReferences();

        if (debug.log_gc) {
            std.debug.print("sweeping young generation\n", .{});
        }
        try self.young.sweep();

        // Append remaining of young generation to old generation and reset
        self.old.appendGeneration(&self.young);
        self.young.reset();

        // Increment and check if old generation should be swept
        self.generationCount += 1;
        if (self.generationCount >= GENERATION_CHECK) {
            if (debug.log_gc) {
                std.debug.print("sweeping old generation\n", .{});
            }

            try self.old.sweep();
            self.old.resetMark();

            self.generationCount = 0;
        }

        self.vm.greyList.clearRetainingCapacity();

        if (debug.log_gc) {
            std.debug.print("-- gc end\n", .{});
        }
    }

    fn markObject(self: *Self, obj: *Object) !void {
        if (obj.marked) return;

        obj.marked = true;
        try self.vm.greyList.append(obj);

        if (debug.log_gc) {
            std.debug.print("marked: '", .{});
            obj.print();
            std.debug.print("'\n", .{});
        }
    }

    fn markTable(self: *Self, table: *StringMap) !void {
        for (table.values()) |entry| {
            self.markObject(&entry.object);
        }
    }

    inline fn markValue(self: *Self, value: *Value) !void {
        if (value.isObject()) {
            try self.markObject(value.asObject());
        }
    }

    fn markRoots(self: *Self) !void {
        for (self.vm.stack.items) |*item| {
            try self.markValue(item);
        }

        for (self.vm.frames.items) |frame| {
            try self.markObject(&frame.function.object);
        }

        for (self.vm.globals.values()) |*value| {
            try self.markValue(value);
        }

        for (self.vm.strings.values()) |string| {
            try self.markObject(&string.object);
        }
    }

    fn markArrayList(self: *Self, list: *ArrayList(Value)) !void {
        for (list.items) |*value| {
            try self.markValue(value);
        }
    }

    fn blackenObject(self: *Self, obj: *Object) !void {
        switch (obj.kind) {
            .String => try self.markObject(obj),
            .Function => {
                const function = obj.asFunction();
                try self.markObject(&function.identifier.object);
                try self.markArrayList(&function.chunk.constants);
            },
            .List => {
                const list = obj.asList();
                if (list.buffer) |buffer| {
                    for (buffer) |*item| {
                        try self.markValue(item);
                    }
                }
            },
            else => unreachable,
        }
    }

    fn traceReferences(self: *Self) !void {
        for (self.vm.greyList.items) |grey| {
            try self.blackenObject(grey);
        }
    }
};
