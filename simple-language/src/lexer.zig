const std = @import("std");
const mem = std.mem;

pub const TokenKind = enum {
    // Single-character tokens.
    LeftParen,
    RightParen,
    LeftSquare,
    RightSquare,

    Comma,
    Dollar,
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Equal,
    StarStar,

    Bang,
    EqualEqual,
    Greater,
    GreaterGreater,
    GreaterEqual,
    Less,
    LessLess,
    LessEqual,

    // Literals.
    Identifier,
    String,
    Number,

    // Keywords.
    Define,
    Else,
    False,
    Global,
    True,
    If,
    Let,
    Nil,
    Not,
    Print,
    Return,

    Error,
    Eof,
};

pub const Token = struct {
    kind: TokenKind,
    line: u32,
    column: u32,
    lexeme: []const u8,

    const Self = @This();

    pub fn artificial() Self {
        return .{
            .kind = .Error,
            .line = 0,
            .column = 0,
            .lexeme = undefined,
        };
    }
};

pub const Lexer = struct {
    start: []const u8,
    current: usize,
    line: u32,

    const Self = @This();

    pub fn init(source: []const u8) Self {
        return .{
            .start = source,
            .current = 0,
            .line = 1,
        };
    }

    pub fn getToken(self: *Self) Token {
        self.skipWhitespace();
        self.setStart();

        if (self.isAtEnd()) {
            return self.makeToken(.Eof);
        }

        var c = self.advance();

        if (isAlpha(c)) return self.identifier();
        if (isDigit(c)) return self.number();

        return switch (c) {
            '(' => self.makeToken(.LeftParen),
            ')' => self.makeToken(.RightParen),
            '[' => self.makeToken(.LeftSquare),
            ']' => self.makeToken(.RightSquare),
            ',' => self.makeToken(.Comma),

            '+' => self.makeToken(.Plus),
            '-' => self.makeToken(.Minus),
            '*' => self.makeToken(if (self.match('*')) .StarStar else .Star),
            '/' => self.makeToken(.Slash),
            '%' => self.makeToken(.Percent),

            '$' => self.makeToken(.Dollar),
            '!' => self.makeToken(.Bang),
            '=' => self.makeToken(if (self.match('=')) .EqualEqual else .Equal),
            '<' => self.makeToken(if (self.match('=')) .LessEqual else .Less),
            '>' => self.makeToken(if (self.match('=')) .GreaterEqual else .Greater),

            '\'' => self.string('\''),
            '"' => self.string('"'),
            else => self.errorToken("Unexpected character."),
        };
    }

    inline fn setStart(self: *Self) void {
        self.start = self.start[self.current..];
        self.current = 0;
    }

    fn skipWhitespace(self: *Self) void {
        while (!self.isAtEnd()) {
            switch (self.peek()) {
                // Comments
                '/' => if (self.peekNext() == '/') {
                    _ = self.advance();
                    _ = self.advance();

                    while (self.peek() != '\n' and !self.isAtEnd()) : (_ = self.advance()) {}
                } else return,
                '\n' => {
                    _ = self.advance();
                    self.line += 1;
                },
                ' ', '\r', '\t' => _ = self.advance(),
                else => return,
            }
        }
    }

    inline fn peek(self: *Self) u8 {
        if (self.isAtEnd()) return 0;
        return self.start[self.current];
    }

    inline fn peekNext(self: *Self) u8 {
        if (self.isAtEnd()) return 0;
        return self.start[self.current + 1];
    }

    fn match(self: *Self, expected: u8) bool {
        if (self.isAtEnd()) return false;
        if (self.start[1] != expected) return false;
        self.current += 1;
        return true;
    }

    fn isAlpha(c: u8) bool {
        return (c >= 'a' and c <= 'z') or
            (c >= 'A' and c <= 'Z') or
            c == '_';
    }

    fn isDigit(c: u8) bool {
        return c >= '0' and c <= '9';
    }

    fn isIdentifier(c: u8) bool {
        return isAlpha(c) or isDigit(c);
    }

    fn advance(self: *Self) u8 {
        defer self.current += 1;
        return self.start[self.current];
    }

    fn isAtEnd(self: *Self) bool {
        return self.current >= self.start.len;
    }

    fn makeToken(self: *Self, kind: TokenKind) Token {
        return .{
            .kind = kind,
            .lexeme = self.start[0..self.current],
            .line = self.line,
            .column = @intCast(u32, self.current + 1),
        };
    }

    fn errorToken(self: *Self, msg: []const u8) Token {
        return .{
            .kind = .Error,
            .lexeme = msg,
            .line = self.line,
            .column = @intCast(u32, self.current + 1),
        };
    }

    fn string(self: *Self, end: u8) Token {
        while (self.peek() != end and !self.isAtEnd()) {
            if (self.peek() == '\n') {
                self.line += 1;
            }
            _ = self.advance();
        }

        if (self.isAtEnd()) {
            return self.errorToken("Unterminated string.");
        }

        // Closing quote
        _ = self.advance();
        return self.makeToken(.String);
    }

    fn number(self: *Self) Token {
        while (isDigit(self.peek())) : (_ = self.advance()) {}

        if (self.peek() == '.' and isDigit(self.peekNext())) {
            _ = self.advance();
            while (isDigit(self.peek())) : (_ = self.advance()) {}
        }

        return self.makeToken(.Number);
    }

    fn identifier(self: *Self) Token {
        while (isIdentifier(self.peek())) : (_ = self.advance()) {}
        return self.makeToken(self.identifierType());
    }

    fn identifierType(self: *Self) TokenKind {
        return switch (self.start[0]) {
            'd' => self.checkKeyword(1, "efine", .Define),
            'e' => self.checkKeyword(1, "lse", .Else),
            'f' => self.checkKeyword(1, "alse", .False),
            'g' => self.checkKeyword(1, "lobal", .Global),
            'i' => self.checkKeyword(1, "f", .If),
            'l' => self.checkKeyword(1, "et", .Let),
            'n' => if (self.start.len == 1) .Identifier else return switch (self.start[1]) {
                'i' => self.checkKeyword(2, "l", .Nil),
                'o' => self.checkKeyword(2, "t", .Not),
                else => .Identifier,
            },
            'p' => self.checkKeyword(1, "rint", .Print),
            'r' => self.checkKeyword(1, "eturn", .Return),
            't' => self.checkKeyword(1, "rue", .True),
            else => .Identifier,
        };
    }

    fn checkKeyword(self: *Self, start: usize, rest: []const u8, kind: TokenKind) TokenKind {
        if (self.current != start + rest.len) return .Identifier;

        if (mem.eql(u8, self.start[start..self.current], rest)) {
            return kind;
        }

        return .Identifier;
    }
};
