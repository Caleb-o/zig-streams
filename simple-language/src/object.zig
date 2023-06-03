const std = @import("std");
const Allocator = std.mem.Allocator;
const assert = std.debug.assert;

const Chunk = @import("compilation.zig").Chunk;
const VM = @import("vm.zig").VM;
const Value = @import("value.zig").Value;

pub const ObjectKind = enum {
    String,
    List,
    Function,
    NativeFunction,
};

pub const Object = struct {
    kind: ObjectKind,
    marked: bool,
    next: ?*Object,

    const Self = @This();

    pub fn init(vm: *VM, comptime T: type, kind: ObjectKind) !*Self {
        const ptr = try vm.allocator.create(T);
        ptr.object = .{
            .kind = kind,
            .marked = false,
            .next = vm.objects,
        };

        vm.objects = &ptr.object;
        return &ptr.object;
    }

    pub fn deinit(self: *Self, vm: *VM) void {
        switch (self.kind) {
            .String => self.asString().deinit(vm),
            .List => self.asList().deinit(vm),
            .NativeFunction => self.asNativeFunction().deinit(vm),
            .Function => self.asFunction().deinit(vm),
            // else => unreachable,
        }
    }

    // Helpers
    pub inline fn isString(self: *Self) bool {
        return self.kind == .String;
    }

    pub inline fn isList(self: *Self) bool {
        return self.kind == .List;
    }

    pub inline fn isNativeFunction(self: *Self) bool {
        return self.kind == .NativeFunction;
    }

    pub inline fn isFunction(self: *Self) bool {
        return self.kind == .Function;
    }

    pub fn asString(self: *Self) *String {
        assert(self.isString());
        return @fieldParentPtr(String, "object", self);
    }

    pub fn asList(self: *Self) *List {
        std.debug.assert(self.isList());
        return @fieldParentPtr(List, "object", self);
    }

    pub fn asNativeFunction(self: *Self) *NativeFunction {
        assert(self.isNativeFunction());
        return @fieldParentPtr(NativeFunction, "object", self);
    }

    pub fn asFunction(self: *Self) *Function {
        assert(self.isFunction());
        return @fieldParentPtr(Function, "object", self);
    }

    pub fn print(self: *Self) void {
        switch (self.kind) {
            .String => std.debug.print("{s}", .{self.asString().chars}),
            .Function => std.debug.print("<fn {s}>", .{self.asFunction().identifier.chars}),
            .NativeFunction => std.debug.print("<native fn {s}>", .{self.asNativeFunction().identifier}),
            .List => {
                std.debug.print("[", .{});
                const list = self.asList();

                if (list.buffer) |buffer| {
                    for (buffer, 0..) |item, idx| {
                        item.print();
                        if (idx < buffer.len - 1) {
                            std.debug.print(" ", .{});
                        }
                    }
                }

                std.debug.print("]", .{});
            },
            // else => unreachable,
        }
    }
};

pub const String = struct {
    object: Object,
    hash: u32,
    chars: []const u8,

    pub fn create(vm: *VM, buffer: []const u8) !*String {
        const hash = getHash(buffer);

        // Find an interned string
        if (vm.strings.get(hash)) |str| {
            return str;
        }

        const object = try Object.init(vm, String, .String);
        const str = object.asString();
        str.object = object.*;
        str.chars = buffer;
        str.hash = hash;

        try vm.strings.put(hash, str);

        return str;
    }

    pub fn fromLiteral(vm: *VM, source: []const u8) !*String {
        const hash = getHash(source);

        // Find an interned string
        if (vm.strings.get(hash)) |str| {
            return str;
        }

        const buffer = try copyLiteral(vm, source);

        const object = try Object.init(vm, String, .String);
        const str = object.asString();
        str.object = object.*;
        str.chars = buffer;
        str.hash = hash;

        try vm.strings.put(hash, str);

        return str;
    }

    pub fn concat(vm: *VM, lhs: *const String, rhs: *const String) !*String {
        const buffer = try std.mem.concat(vm.allocator, u8, &[_][]const u8{
            lhs.chars,
            rhs.chars,
        });
        return try String.create(vm, buffer);
    }

    fn copyLiteral(vm: *VM, source: []const u8) ![]const u8 {
        const buffer = try vm.allocator.alloc(u8, source.len);
        std.mem.copy(u8, buffer, source);
        return buffer;
    }

    pub fn deinit(self: *String, vm: *VM) void {
        vm.allocator.free(self.chars);
        vm.allocator.destroy(self);
    }

    fn getHash(buffer: []const u8) u32 {
        var hash: u32 = 2166136261;
        for (buffer) |byte| {
            hash ^= @as(u32, byte);
            hash *%= 16777619;
        }
        return hash;
    }
};

pub const List = struct {
    object: Object,
    buffer: ?[]Value,

    pub fn create(vm: *VM, items: []const Value) !*List {
        const object = try Object.init(vm, List, .List);
        const list = object.asList();
        list.object = object.*;
        if (items.len == 0) {
            list.buffer = null;
        } else {
            list.buffer = try vm.allocator.alloc(Value, items.len);
            std.mem.copyForwards(Value, list.buffer.?, items);
        }

        return list;
    }

    pub inline fn deinit(self: *List, vm: *VM) void {
        if (self.buffer) |buffer| {
            vm.allocator.free(buffer);
        }
        vm.allocator.destroy(self);
    }
};

pub const ZigNativeFn = *const fn (args: []Value) Value;

pub const NativeFunction = struct {
    object: Object,
    identifier: []const u8,
    arity: u8,
    function: ZigNativeFn,

    const Native = @This();

    pub fn create(vm: *VM, identifier: []const u8, arity: u8, function: ZigNativeFn) !*Native {
        const object = try Object.init(vm, Native, .NativeFunction);
        const func = object.asNativeFunction();
        func.identifier = identifier;
        func.arity = arity;
        func.function = function;

        return func;
    }

    pub inline fn deinit(self: *Native, vm: *VM) void {
        vm.allocator.destroy(self);
    }
};

pub const Function = struct {
    object: Object,
    // TODO: Consider using a Obj String instead
    identifier: *String,
    arity: u8,
    chunk: Chunk,

    const Self = @This();

    pub fn init(vm: *VM, identifier: *String, arity: u8, chunk: Chunk) !*Function {
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
