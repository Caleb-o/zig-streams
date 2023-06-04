pub const compilation = @import("compilation.zig");
pub const compiler = @import("compiler.zig");
pub const gc = @import("gc.zig");
pub const lexer = @import("lexer.zig");
pub const object = @import("object.zig");
pub const value = @import("value.zig");
pub const vm = @import("vm.zig");

comptime {
    const std = @import("std");
    std.testing.refAllDecls(@This());
}
