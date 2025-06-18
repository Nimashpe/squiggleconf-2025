const std = @import("std");

// Type-level function that generates array types
fn Array(comptime T: type, comptime size: usize) type {
    return struct {
        data: [size]T,
        len: usize,

        const Self = @This();

        pub fn init() Self {
            return Self{
                .data = undefined,
                .len = 0,
            };
        }

        pub fn push(self: *Self, item: T) !void {
            if (self.len >= size) return error.OutOfMemory;
            self.data[self.len] = item;
            self.len += 1;
        }

        pub fn get(self: *const Self, index: usize) ?T {
            if (index >= self.len) return null;
            return self.data[index];
        }
    };
}

pub fn main() !void {
    // Create a specialized integer array with 10 elements
    var intArray = Array(i32, 10).init();
    try intArray.push(42);
    try intArray.push(100);
    
    // Create a specialized float array with 5 elements
    var floatArray = Array(f64, 5).init();
    try floatArray.push(3.14);
    
    // Print the first element of each array
    if (intArray.get(0)) |value| {
        std.debug.print("First integer: {}\n", .{value});
    }
    
    if (floatArray.get(0)) |value| {
        std.debug.print("First float: {}\n", .{value});
    }
}
