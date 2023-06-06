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
    TooManyArguments,
} || error{OutOfMemory} || std.fmt.ParseFloatError;

const Local = struct {
    identifier: Token,
    index: u8,
    depth: u8,
    initialised: bool,

    const Self = @This();

    pub fn default() Self {
        return .{
            .identifier = undefined,
            .index = 0,
            .depth = 0,
            .initialised = false,
        };
    }

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
    arena: Allocator,

    const Self = @This();

    pub fn init(source: []const u8, vm: *VM, arena: Allocator) Self {
        var l = Lexer.init(source);
        var t = l.getToken();

        return .{
            .lexer = l,
            .previous = undefined,
            .current = t,
            .func = undefined,
            .vm = vm,
            .arena = arena,
        };
    }

    pub fn compile(self: *Self) CompilerErr!*Function {
        var scope = try self.openCompiler(
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

    inline fn openCompiler(self: *Self, identifier: []const u8, depth: u8) !FunctionCompiler {
        // NOTE: Using the arena resolves a strange lifetime issue
        // Chunks live from start to end anyway, so an arena is still viable
        return FunctionCompiler.create(
            self.arena,
            self.func,
            identifier,
            depth,
        );
    }

    fn closeCompiler(self: *Self) !*Function {
        if (self.func.depth > 0 and self.chunk().code.items.len > 0) {
            // _ = self.chunk().code.pop();
            try self.chunk().writeOp(.Return);
        } else {
            try self.chunk().writeReturn();
        }

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

    fn checkAny(self: *Self, kind: []const TokenKind) bool {
        for (kind) |k| {
            if (self.current.kind == k) {
                return true;
            }
        }
        return false;
    }

    fn match(self: *Self, kind: TokenKind) bool {
        if (self.current.kind == kind) {
            self.advance();
            return true;
        }

        return false;
    }

    fn matchAny(self: *Self, kind: []const TokenKind) bool {
        for (kind) |k| {
            if (self.current.kind == k) {
                self.advance();
                return true;
            }
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
        if (self.match(.Let)) {
            try self.letDeclaration();
        } else if (self.match(.Global)) {
            try self.globalDeclaration();
        } else if (self.match(.Define)) {
            try self.defineDeclaration();
            return;
        } else {
            try self.statement();
        }

        try self.consume(.Semicolon, "Expect ';' after declaration");
    }

    fn letDeclaration(self: *Self) !void {
        const identifier = self.current;
        try self.consume(.Identifier, "Expect identifier after let");
        try self.declareVariable(identifier);
        try self.expression();

        // Tell compiler the variable is ready for use
        try self.defineVariable(identifier);
    }

    fn globalDeclaration(self: *Self) !void {
        const identifier = self.current;
        try self.consume(.Identifier, "Expect identifier after global");
        try self.expression();

        const index = try self.identifierConstant(&identifier);
        try self.chunk().writeOpByte(.SetGlobal, index);
    }

    fn defineDeclaration(self: *Self) CompilerErr!void {
        const identifier = self.current;
        try self.consume(.Identifier, "Expect identifier after global");

        var scope = try self.openCompiler(
            identifier.lexeme,
            self.func.depth + 1,
        );
        self.func = &scope;

        try self.declareVariable(identifier);
        try self.defineVariable(identifier);

        // Collect parameter list
        if (self.match(.LeftSquare)) {
            while (self.match(.Identifier)) {
                try self.declareVariable(self.previous);
                try self.defineVariable(self.previous);

                self.func.arity += 1;
            }
            try self.consume(.RightSquare, "Expect ']' after parameter list");
        }

        try self.block();

        const func = try self.closeCompiler();
        const index = try self.makeConstant(Value.fromObject(&func.object));

        if (self.func.depth == 0) {
            // Place top-level functions in global table
            const id_idx = try self.identifierConstant(&identifier);
            const val = &self.func.chunk.constants.items[id_idx];
            const id = val.asObject().asString();

            try self.vm.globals.put(id.chars, Value.fromObject(&func.object));
        } else {
            try self.chunk().writeOpByte(.Function, index);
            // HACK
            try self.defineVariable(identifier);
        }
    }

    fn block(self: *Self) !void {
        try self.consume(.LeftCurly, "Expect '{' to start block");

        while (!self.match(.Eof) and !self.match(.RightCurly)) {
            try self.declaration();
        }
    }

    fn statement(self: *Self) !void {
        switch (self.current.kind) {
            .Print => try self.printStmt(),
            .If => try self.ifStmt(),
            .Return => try self.returnStatement(),
            else => try self.expression(),
        }
        // try self.chunk().writeOp(.Pop);
    }

    fn printStmt(self: *Self) !void {
        self.advance();
        try self.expression();
        try self.chunk().writeOp(.Print);
    }

    fn ifStmt(self: *Self) CompilerErr!void {
        self.advance();
        try self.groupedExpression();

        const falseLocation = try self.chunk().writeJump(.JumpNot);
        try self.block();

        if (self.match(.Else)) {
            const trueLocation = try self.chunk().writeJump(.Jump);

            self.chunk().patchJump(falseLocation);
            try self.block();

            self.chunk().patchJump(trueLocation);
        } else {
            self.chunk().patchJump(falseLocation);
        }
    }

    fn returnStatement(self: *Self) !void {
        self.advance();

        if (self.check(.Semicolon)) {
            try self.chunk().writeReturn();
        } else {
            try self.chunk().writeOp(.Return);
        }
    }

    fn groupedExpression(self: *Self) !void {
        self.advance();
        try self.expression();
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

    fn listLiteral(self: *Self) !void {
        self.advance();
        var count: usize = 0;

        if (!self.match(.RightSquare)) {
            try self.expression();
            count += 1;

            while (self.match(.Comma)) {
                try self.expression();
                count += 1;
            }

            try self.consume(
                .RightSquare,
                "Expect ']' to end list literal",
            );
        }

        if (count > std.math.maxInt(u8)) {
            self.err("Too many items in list literal");
            return CompilerErr.TooManyArguments;
        }

        try self.chunk().writeOpByte(.IntoList, @intCast(
            u8,
            count,
        ));
    }

    fn expression(self: *Self) !void {
        try self.equality();
    }

    fn equality(self: *Self) !void {
        const kind = [_]TokenKind{
            .Less,
            .LessEqual,
            .Greater,
            .GreaterEqual,
            .Equal,
        };
        try self.term();

        while (self.matchAny(&kind)) {
            const op = self.previous.kind;
            try self.term();

            switch (op) {
                .Less => try self.chunk().writeOp(.Less),
                .LessEqual => try self.chunk().writeOp(.LessEqual),
                .Greater => try self.chunk().writeOp(.Greater),
                .GreaterEqual => try self.chunk().writeOp(.GreaterEqual),
                .Equal => try self.chunk().writeOp(.Equal),
                else => unreachable,
            }
        }
    }

    fn term(self: *Self) !void {
        try self.factor();
        while (self.match(.Plus) or self.match(.Minus)) {
            const op = self.previous.kind;
            try self.factor();

            switch (op) {
                .Plus => try self.chunk().writeOp(.Add),
                .Minus => try self.chunk().writeOp(.Sub),
                else => unreachable,
            }
        }
    }

    fn factor(self: *Self) !void {
        try self.call();

        while (self.match(.Star) or self.match(.Slash)) {
            const op = self.previous.kind;
            try self.call();

            switch (op) {
                .Star => try self.chunk().writeOp(.Mul),
                .Slash => try self.chunk().writeOp(.Div),
                else => unreachable,
            }
        }
    }

    fn call(self: *Self) CompilerErr!void {
        try self.primary();

        while (self.match(.LeftParen)) {
            // Collect arguments - optional
            // NOTE: Does not use list parse, as it needs to live on the stack
            var count: u32 = 0;
            while (!self.match(.RightParen)) {
                try self.expression();
                if (count > std.math.maxInt(u8)) {
                    self.err("Provided too many arguments to function");
                    return CompilerErr.TooManyArguments;
                }
                count += 1;
            }

            try self.chunk().writeOpByte(.Call, @intCast(
                u8,
                count,
            ));
        }
    }

    fn primary(self: *Self) CompilerErr!void {
        switch (self.current.kind) {
            .Dollar => try self.getGlobalIdentifier(),
            .Identifier => try self.getIdentifier(),
            .LeftParen => try self.groupedExpression(),
            .LeftSquare => try self.listLiteral(),

            .True => try self.chunk().writeOp(.True),
            .False => try self.chunk().writeOp(.False),
            .Nil => try self.chunk().writeOp(.Nil),

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

            else => {
                std.debug.print(
                    "Unknown item in expression '{s}'\n",
                    .{self.current.lexeme},
                );
                return CompilerErr.InvalidExpression;
            },
        }
    }
};

// ====== TESTS
const expect = std.testing.expect;

test "Simple Expressions" {}
