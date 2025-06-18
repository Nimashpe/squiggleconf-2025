const std = @import("std");
const Allocator = std.mem.Allocator;

// Result type for parsers
const ParseResult = struct {
    success: bool,
    value: []const u8,
    remaining: []const u8,
};

// Parser combinator framework
const Parser = struct {
    // Parser function type
    const ParserFn = fn([]const u8) ParseResult;
    
    // Core function
    fn_ptr: ParserFn,
    
    // Constructor
    pub fn init(func: ParserFn) Parser {
        return Parser{ .fn_ptr = func };
    }
    
    // Run the parser
    pub fn parse(self: Parser, input: []const u8) ParseResult {
        return self.fn_ptr(input);
    }
    
    // Combinators
    pub fn map(self: Parser, comptime func: anytype) Parser {
        const mapFn = struct {
            fn parser(input: []const u8) ParseResult {
                const result = self.parse(input);
                if (!result.success) return result;
                
                return ParseResult{
                    .success = true,
                    .value = func(result.value),
                    .remaining = result.remaining,
                };
            }
        }.parser;
        
        return Parser.init(mapFn);
    }
    
    pub fn then(self: Parser, next: Parser) Parser {
        const thenFn = struct {
            fn parser(input: []const u8) ParseResult {
                const result1 = self.parse(input);
                if (!result1.success) return result1;
                
                const result2 = next.parse(result1.remaining);
                if (!result2.success) return result2;
                
                return ParseResult{
                    .success = true,
                    .value = result2.value,
                    .remaining = result2.remaining,
                };
            }
        }.parser;
        
        return Parser.init(thenFn);
    }
    
    pub fn or(self: Parser, alternative: Parser) Parser {
        const orFn = struct {
            fn parser(input: []const u8) ParseResult {
                const result = self.parse(input);
                if (result.success) return result;
                
                return alternative.parse(input);
            }
        }.parser;
        
        return Parser.init(orFn);
    }
    
    pub fn many(self: Parser) Parser {
        const manyFn = struct {
            fn parser(input: []const u8) ParseResult {
                var current_input = input;
                var matched = true;
                
                // Try to match as many times as possible
                while (matched) {
                    const result = self.parse(current_input);
                    if (!result.success) {
                        matched = false;
                    } else {
                        current_input = result.remaining;
                    }
                }
                
                // Calculate what was consumed
                const consumed_len = input.len - current_input.len;
                
                return ParseResult{
                    .success = true,
                    .value = input[0..consumed_len],
                    .remaining = current_input,
                };
            }
        }.parser;
        
        return Parser.init(manyFn);
    }
};

// Basic parsers
fn charP(expected: u8) Parser {
    const charFn = struct {
        fn parser(input: []const u8) ParseResult {
            if (input.len == 0) {
                return ParseResult{
                    .success = false,
                    .value = "",
                    .remaining = input,
                };
            }
            
            if (input[0] == expected) {
                return ParseResult{
                    .success = true,
                    .value = input[0..1],
                    .remaining = input[1..],
                };
            }
            
            return ParseResult{
                .success = false,
                .value = "",
                .remaining = input,
            };
        }
    }.parser;
    
    return Parser.init(charFn);
}

fn stringP(expected: []const u8) Parser {
    const stringFn = struct {
        fn parser(input: []const u8) ParseResult {
            if (input.len < expected.len) {
                return ParseResult{
                    .success = false,
                    .value = "",
                    .remaining = input,
                };
            }
            
            const potential_match = input[0..expected.len];
            
            if (std.mem.eql(u8, potential_match, expected)) {
                return ParseResult{
                    .success = true,
                    .value = potential_match,
                    .remaining = input[expected.len..],
                };
            }
            
            return ParseResult{
                .success = false,
                .value = "",
                .remaining = input,
            };
        }
    }.parser;
    
    return Parser.init(stringFn);
}

fn digitP() Parser {
    const digitFn = struct {
        fn parser(input: []const u8) ParseResult {
            if (input.len == 0) {
                return ParseResult{
                    .success = false,
                    .value = "",
                    .remaining = input,
                };
            }
            
            if (input[0] >= '0' and input[0] <= '9') {
                return ParseResult{
                    .success = true,
                    .value = input[0..1],
                    .remaining = input[1..],
                };
            }
            
            return ParseResult{
                .success = false,
                .value = "",
                .remaining = input,
            };
        }
    }.parser;
    
    return Parser.init(digitFn);
}

// Demo of parser combinators
pub fn main() !void {
    // Basic parsers
    const digit = digitP();
    const hello = stringP("hello");
    const world = stringP("world");
    const comma = charP(',');
    const space = charP(' ');
    
    // Combined parsers
    const digits = digit.many();
    const hello_world = hello.then(space).then(world);
    const hello_or_world = hello.or(world);
    
    // Test some inputs
    const tests = [_]struct {
        name: []const u8,
        parser: Parser,
        input: []const u8,
    }{
        .{ .name = "digit", .parser = digit, .input = "5abc" },
        .{ .name = "digits", .parser = digits, .input = "12345abc" },
        .{ .name = "hello", .parser = hello, .input = "hello world" },
        .{ .name = "hello_world", .parser = hello_world, .input = "hello world!" },
        .{ .name = "hello_or_world (hello)", .parser = hello_or_world, .input = "hello!" },
        .{ .name = "hello_or_world (world)", .parser = hello_or_world, .input = "world!" },
    };
    
    std.debug.print("=== Parser Combinator Demo ===\n", .{});
    
    for (tests) |test| {
        const result = test.parser.parse(test.input);
        std.debug.print("Parser: {s}\n", .{test.name});
        std.debug.print("  Input: \"{s}\"\n", .{test.input});
        std.debug.print("  Success: {}\n", .{result.success});
        if (result.success) {
            std.debug.print("  Matched: \"{s}\"\n", .{result.value});
            std.debug.print("  Remaining: \"{s}\"\n", .{result.remaining});
        }
        std.debug.print("\n", .{});
    }
}
