const std = @import("std");
const Allocator = std.mem.Allocator;

const compilation = @import("compilation.zig");
const ByteCode = compilation.ByteCode;
const Chunk = compilation.Chunk;

const lex = @import("lexer.zig");
const Lexer = lex.Lexer;
const Token = lex.Token;
const TokenKind = lex.TokenKind;

const VM = @import("vm.zig").VM;
const Value = @import("value.zig").Value;
const objects = @import("object.zig");
const Function = objects.Function;

pub const CompilerErr = error{
    InvalidToken,
    InvalidExpression,
} || error{OutOfMemory} || std.fmt.ParseFloatError;

const FunctionCompiler = struct {
    enclosing: ?*FunctionCompiler,
    depth: u32,

    identifier: []const u8,
    arity: u8,
    chunk: Chunk,
    // TODO: Locals

    const Self = @This();

    pub fn create(allocator: Allocator, enclosing: ?*Self, identifier: []const u8, depth: u32) Self {
        return .{
            .enclosing = enclosing,
            .depth = depth,
            .identifier = identifier,
            .arity = 0,
            .chunk = Chunk.init(allocator),
        };
    }

    pub fn end(self: *Self, vm: *VM) !*Function {
        const function = objects.Function.init(
            vm,
            self.identifier,
            self.arity,
            self.chunk,
        );
        return function;
    }
};

pub const Compiler = struct {
    lexer: Lexer,
    previous: Token,
    current: Token,

    func: *FunctionCompiler,
    vm: *VM,

    const Self = @This();

    pub fn init(source: []const u8, vm: *VM) Self {
        var l = Lexer.init(source);
        var t = l.getToken();
        return .{
            .lexer = l,
            .previous = undefined,
            .current = t,
            .func = undefined,
            .vm = vm,
        };
    }

    pub fn compile(self: *Self) CompilerErr!*Function {
        var scope = self.openCompiler(
            null,
            try self.takeString("script"),
            0,
        );
        self.func = &scope;

        while (!self.match(.Eof)) {
            try self.declaration();
        }

        return try self.closeCompiler();
    }

    // TODO: Make this into the string object or manage strings some other way
    fn takeString(self: *Self, string: []const u8) ![]u8 {
        var str = try self.vm.allocator.alloc(u8, string.len);
        std.mem.copyForwards(u8, str, string);
        return str;
    }

    fn openCompiler(self: *Self, enclosing: ?*FunctionCompiler, identifier: []const u8, depth: u32) FunctionCompiler {
        return FunctionCompiler.create(self.vm.allocator, enclosing, identifier, depth);
    }

    fn closeCompiler(self: *Self) !*Function {
        try self.chunk().writeOp(.Return);

        const func = try self.func.end(self.vm);
        if (self.func.enclosing) |enclosing| {
            self.func = enclosing;
        }
        return func;
    }

    fn err(self: *Self, comptime msg: []const u8) void {
        std.debug.print("[Error:Compiler] {s} [{}:{}]\n", .{
            msg,
            self.current.line,
            self.current.column,
        });
    }

    inline fn chunk(self: *Self) *Chunk {
        return &self.func.chunk;
    }

    inline fn advance(self: *Self) void {
        self.previous = self.current;
        self.current = self.lexer.getToken();
    }

    fn consume(self: *Self, kind: TokenKind, comptime msg: []const u8) !void {
        if (self.current.kind == kind) {
            self.advance();
            return;
        }

        self.err(msg);
        return CompilerErr.InvalidToken;
    }

    inline fn check(self: *Self, kind: TokenKind) bool {
        return self.current.kind == kind;
    }

    fn match(self: *Self, kind: TokenKind) bool {
        if (self.current.kind == kind) {
            self.advance();
            return true;
        }

        return false;
    }

    fn declaration(self: *Self) !void {
        try self.statement();
    }

    fn statement(self: *Self) !void {
        try self.consume(.LeftParen, "Expect '(' to start statement");
        switch (self.current.kind) {
            .Print => try self.printStmt(),
            else => try self.expression(),
        }
        try self.consume(.RightParen, "Expect ')' to end statement");
    }

    fn printStmt(self: *Self) !void {
        self.advance();
        try self.groupedExpression();
        try self.chunk().writeOp(.Print);
    }

    fn groupedExpression(self: *Self) !void {
        self.advance();
        try self.term();
        try self.consume(.RightParen, "Expect ')' to end grouped expression");
    }

    fn expression(self: *Self) !void {
        try self.term();
    }

    fn term(self: *Self) !void {
        if (self.check(.Plus) or self.check(.Minus)) {
            while (self.match(.Plus) or self.match(.Minus)) {
                const op = self.previous.kind;
                try self.factor();
                try self.factor();

                switch (op) {
                    .Plus => try self.chunk().writeOp(.Add),
                    .Minus => try self.chunk().writeOp(.Sub),
                    else => unreachable,
                }
            }
        } else {
            try self.factor();
        }
    }

    fn factor(self: *Self) !void {
        if (self.check(.Star) or self.check(.Slash)) {
            while (self.match(.Star) or self.match(.Slash)) {
                const op = self.previous.kind;
                try self.primary();
                try self.primary();

                switch (op) {
                    .Star => try self.chunk().writeOp(.Mul),
                    .Slash => try self.chunk().writeOp(.Div),
                    else => unreachable,
                }
            }
        } else {
            try self.primary();
        }
    }

    fn primary(self: *Self) CompilerErr!void {
        switch (self.current.kind) {
            .Number => {
                const float = try std.fmt.parseFloat(f32, self.current.lexeme);
                self.advance();
                try self.chunk().writeConstant(Value.fromNumber(float));
            },

            .LeftParen => try self.groupedExpression(),

            else => return CompilerErr.InvalidExpression,
        }
    }
};
