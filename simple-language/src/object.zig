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
            .Function => self.asFunction().deinit(vm),
            else => unreachable,
        }
    }

    // Helpers
    pub inline fn isString(self: *Self) bool {
        return self.kind == .String;
    }

    pub inline fn isFunction(self: *Self) bool {
        return self.kind == .Function;
    }

    pub fn asString(self: *Self) *String {
        assert(self.isString());
        return @fieldParentPtr(String, "object", self);
    }

    pub fn asFunction(self: *Self) *Function {
        assert(self.isFunction());
        return @fieldParentPtr(Function, "object", self);
    }

    pub fn print(self: *Self) void {
        switch (self.kind) {
            .String => std.debug.print("{s}", .{self.asString().chars}),
            .Function => std.debug.print("<fn {s}>", .{self.asFunction().identifier.chars}),
            else => unreachable,
        }
    }

    pub fn destroy(self: *Self, vm: *VM) void {
        switch (self.kind) {
            .String => self.asString().deinit(vm),
            .Function => self.asFunction().deinit(vm),
            else => unreachable,
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

    pub fn copy(vm: *VM, source: []const u8) !*String {
        return try String.create(vm, try copyLiteral(vm, source));
    }

    pub inline fn deinit(self: *String, vm: *VM) void {
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
