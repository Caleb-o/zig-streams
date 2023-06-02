const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const compilation = @import("compilation.zig");
const ByteCode = compilation.ByteCode;
const Chunk = compilation.Chunk;

const debug = @import("debug.zig");

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
    BindingAlreadyDefined,
    UseBeforeInit,
    UndefinedLocal,
    TooManyConstants,
} || error{OutOfMemory} || std.fmt.ParseFloatError;

const Local = struct {
    identifier: Token,
    index: u8,
    depth: u8,
    initialised: bool,

    const Self = @This();

    pub fn create(identifier: Token, index: u8, depth: u8) Self {
        return .{
            .identifier = identifier,
            .index = index,
            .depth = depth,
            .initialised = false,
        };
    }
};

const FunctionCompiler = struct {
    enclosing: ?*FunctionCompiler,
    depth: u8,

    identifier: []const u8,
    arity: u8,
    chunk: Chunk,
    locals: ArrayList(Local),

    const Self = @This();

    pub fn create(allocator: Allocator, enclosing: ?*Self, identifier: []const u8, depth: u8) Self {
        return .{
            .enclosing = enclosing,
            .depth = depth,
            .identifier = identifier,
            .arity = 0,
            .chunk = Chunk.init(allocator),
            // TODO: Move to stack
            .locals = ArrayList(Local).init(allocator),
        };
    }

    pub fn end(self: *Self, vm: *VM) !*Function {
        self.locals.deinit();

        const function = try objects.Function.init(
            vm,
            try objects.String.fromLiteral(vm, self.identifier),
            self.arity,
            self.chunk,
        );
        return function;
    }

    pub fn addLocal(self: *Self, identifier: Token) !void {
        try self.locals.append(Local.create(
            identifier,
            @intCast(u8, self.locals.items.len),
            self.depth,
        ));
    }

    pub fn findLocal(self: *Self, identifier: Token) ?*Local {
        for (self.locals.items) |*local| {
            // TODO: Consider depth?
            if (std.mem.eql(u8, local.identifier.lexeme, identifier.lexeme)) {
                return local;
            }
        }
        return null;
    }

    pub fn findLocalInScope(self: *Self, identifier: Token) ?*Local {
        for (self.locals.items) |*local| {
            if (local.depth < self.depth) break;
            if (local.depth == self.depth and std.mem.eql(u8, local.identifier.lexeme, identifier.lexeme)) {
                return local;
            }
        }
        return null;
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
            "script",
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

    fn openCompiler(self: *Self, enclosing: ?*FunctionCompiler, identifier: []const u8, depth: u8) FunctionCompiler {
        return FunctionCompiler.create(
            self.vm.allocator,
            enclosing,
            identifier,
            depth,
        );
    }

    fn closeCompiler(self: *Self) !*Function {
        try self.chunk().writeOps(.Nil, .Return);

        const func = try self.func.end(self.vm);
        if (self.func.enclosing) |enclosing| {
            self.func = enclosing;
        }

        if (debug.print_chunk) {
            func.chunk.disassemble(func.identifier.chars);
        }

        return func;
    }

    fn err(self: *Self, comptime msg: []const u8) void {
        std.debug.print("[Error:Compiler] {s} '{s}' [{}:{}]\n", .{
            msg,
            self.previous.lexeme,
            self.previous.line,
            self.previous.column,
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

    inline fn makeConstant(self: *Self, value: Value) !u8 {
        return @intCast(u8, try self.chunk().addConstant(value));
    }

    fn identifierConstant(self: *Self, token: *const Token) !u8 {
        return try self.makeConstant(Value.fromObject(&(try objects.String.fromLiteral(
            self.vm,
            token.lexeme,
        )).object));
    }

    fn declareVariable(self: *Self, identifier: Token) !void {
        if (self.func.findLocalInScope(identifier)) |_| {
            return;
        }
        try self.func.addLocal(identifier);
    }

    fn defineVariable(self: *Self, identifier: Token) !void {
        if (self.func.findLocal(identifier)) |local| {
            if (local.initialised) {
                try self.chunk().writeOpByte(.SetLocal, local.index);
                return;
            }

            local.initialised = true;
            return;
        }
        return CompilerErr.UndefinedLocal;
    }

    fn declaration(self: *Self) !void {
        try self.consume(.LeftParen, "Expect '(' to start declaration");
        if (self.match(.Let)) {
            try self.letDeclaration();
            try self.consume(.RightParen, "Expect ')' to end let declaration");
            return;
        }
        if (self.match(.Global)) {
            try self.globalDeclaration();
            try self.consume(.RightParen, "Expect ')' to end global declaration");
            return;
        }
        if (self.match(.Define)) {
            try self.defineDeclaration();
            try self.consume(.RightParen, "Expect ')' to end define declaration");
            return;
        }
        try self.statement();
        try self.consume(.RightParen, "Expect ')' to end statement");
    }

    fn letDeclaration(self: *Self) !void {
        const identifier = self.current;
        try self.consume(.Identifier, "Expect identifier after let");
        try self.declareVariable(identifier);

        try self.consume(.Equal, "Expect '=' after let identifier");
        try self.expression();

        // Tell compiler the variable is ready for use
        try self.defineVariable(identifier);
    }

    fn globalDeclaration(self: *Self) !void {
        const identifier = self.current;
        try self.consume(.Identifier, "Expect identifier after global");

        try self.consume(.Equal, "Expect '=' after global identifier");
        try self.expression();

        const index = try self.identifierConstant(&identifier);
        try self.chunk().writeOpByte(.SetGlobal, index);
    }

    fn defineDeclaration(self: *Self) CompilerErr!void {
        const identifier = self.current;
        try self.consume(.Identifier, "Expect identifier after global");

        var scope = self.openCompiler(
            self.func,
            identifier.lexeme,
            self.func.depth + 1,
        );
        self.func = &scope;

        // Collect parameter list
        if (self.match(.LeftSquare)) {
            while (self.match(.Identifier)) {
                try self.declareVariable(self.previous);
                try self.defineVariable(self.previous);
            }
            try self.consume(.RightSquare, "Expect ']' after parameter list");
        }

        try self.consume(.LeftParen, "Expect '(' to start block");

        while (!self.match(.Eof) and !self.match(.RightParen)) {
            try self.declaration();
        }

        const func = try self.closeCompiler();
        const index = try self.makeConstant(Value.fromObject(&func.object));
        try self.chunk().writeOpByte(.Function, index);

        // Declare the function
        try self.declareVariable(identifier);
        try self.defineVariable(identifier);

        // HACK
        try self.defineVariable(identifier);
    }

    fn statement(self: *Self) !void {
        switch (self.current.kind) {
            .Print => try self.printStmt(),
            else => try self.expression(),
        }
    }

    fn printStmt(self: *Self) !void {
        self.advance();
        try self.expression();
        try self.chunk().writeOp(.Print);
    }

    fn groupedExpression(self: *Self) !void {
        self.advance();
        try self.term();
        try self.consume(.RightParen, "Expect ')' to end grouped expression");
    }

    inline fn getGlobalIdentifier(self: *Self) !void {
        self.advance();
        const identifier = self.current;
        try self.consume(.Identifier, "Expect identifier after '$'");

        // Globals don't expect any local to be defined or to exist

        const index = try self.identifierConstant(&identifier);
        try self.chunk().writeOpByte(.GetGlobal, index);
    }

    fn getIdentifier(self: *Self) !void {
        const identifier = self.current;
        self.advance();

        if (self.func.findLocal(identifier)) |local| {
            if (!local.initialised) {
                self.err("Use before initialisation");
                return CompilerErr.UseBeforeInit;
            }

            try self.chunk().writeOpByte(.GetLocal, local.index);
            return;
        }

        self.err("Undefined local");
        return CompilerErr.UndefinedLocal;
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

            .String => {
                try self.chunk().writeConstant(Value.fromObject(&(try objects.String.fromLiteral(
                    self.vm,
                    self.current.lexeme[1 .. self.current.lexeme.len - 1],
                )).object));
                self.advance();
            },

            .Dollar => try self.getGlobalIdentifier(),
            .Identifier => try self.getIdentifier(),
            .LeftParen => try self.groupedExpression(),

            .True => try self.chunk().writeOp(.True),
            .False => try self.chunk().writeOp(.False),
            .Nil => try self.chunk().writeOp(.Nil),

            else => {
                std.debug.print("Unknown item in expression '{s}'\n", .{self.current.lexeme});
                return CompilerErr.InvalidExpression;
            },
        }
    }
};
