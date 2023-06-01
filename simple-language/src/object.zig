const std = @import("std");
const Allocator = std.mem.Allocator;
const assert = std.debug.assert;

const Chunk = @import("compilation.zig").Chunk;
const VM = @import("vm.zig").VM;

pub const ObjectKind = enum {
    String,
    List,
    Function,
    NativeFunction,
};

pub const Object = struct {
    kind: ObjectKind,
    next: ?*Object,

    const Self = @This();

    pub fn init(vm: *VM, comptime T: type, kind: ObjectKind) !*Self {
        const ptr = try vm.allocator.create(T);
        ptr.object = .{ .kind = kind, .next = vm.objects };

        vm.objects = &ptr.object;
        return &ptr.object;
    }

    pub fn deinit(self: *Self, vm: *VM) void {
        switch (self.kind) {
            .Function => self.asFunction().deinit(vm),
            else => unreachable,
        }
    }

    // Helpers
    pub inline fn isFunction(self: *Self) bool {
        return self.kind == .Function;
    }

    pub fn asFunction(self: *Self) *Function {
        assert(self.isFunction());
        return @fieldParentPtr(Function, "object", self);
    }
};

pub const Function = struct {
    object: Object,
    // TODO: Consider using a Obj String instead
    identifier: []const u8,
    arity: u8,
    chunk: Chunk,

    const Self = @This();

    pub fn init(vm: *VM, identifier: []const u8, arity: u8, chunk: Chunk) !*Function {
        const obj = try Object.init(vm, Function, .Function);
        const function = obj.asFunction();

        function.identifier = identifier;
        function.arity = arity;
        function.chunk = chunk;

        return function;
    }

    pub fn deinit(self: *Self, vm: *VM) void {
        self.chunk.deinit();
        vm.allocator.destroy(self);
    }
};
