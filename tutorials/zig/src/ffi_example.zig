const std = @import("std");

// Define C function interfaces
const c = struct {
    extern "c" fn printf(format: [*:0]const u8, ...) c_int;
    
    // Imagined C library functions
    extern "c" fn add(a: c_int, b: c_int) c_int;
    extern "c" fn create_buffer(size: c_int) ?[*]u8;
    extern "c" fn free_buffer(ptr: [*]u8) void;
};

// Safer Zig wrapper around C function
fn safeAdd(a: i32, b: i32) i32 {
    return c.add(@intCast(a), @intCast(b));
}

// Higher-level abstraction over C buffer management
const Buffer = struct {
    ptr: [*]u8,
    size: usize,
    
    pub fn create(size: usize) !Buffer {
        const ptr = c.create_buffer(@intCast(size)) orelse return error.OutOfMemory;
        return Buffer{
            .ptr = ptr,
            .size = size,
        };
    }
    
    pub fn deinit(self: *Buffer) void {
        c.free_buffer(self.ptr);
        self.ptr = undefined;
        self.size = 0;
    }
    
    pub fn write(self: *Buffer, data: []const u8) !void {
        if (data.len > self.size) return error.BufferTooSmall;
        @memcpy(self.ptr[0..data.len], data);
    }
    
    pub fn asSlice(self: Buffer) []u8 {
        return self.ptr[0..self.size];
    }
};

// Export a Zig function to be called from C
export fn zig_process_data(data: [*]const u8, len: c_int) c_int {
    const slice = data[0..@intCast(len)];
    var sum: i32 = 0;
    
    for (slice) |byte| {
        sum += byte;
    }
    
    // Demonstrate calling back into C
    _ = c.printf("Processed %d bytes with sum: %d\n".ptr, len, sum);
    
    return @intCast(sum);
}

// This function exists only to demonstrate the code, as we can't actually call C functions
pub fn simulateFFIDemo() !void {
    std.debug.print("=== FFI Demo ===\n", .{});
    
    // Call C function through wrapper
    const result = safeAdd(5, 10);
    std.debug.print("5 + 10 = {}\n", .{result});
    
    // Use the buffer abstraction
    var buffer = try Buffer.create(256);
    defer buffer.deinit();
    
    try buffer.write("Hello from Zig!");
    const slice = buffer.asSlice();
    
    std.debug.print("Buffer contains {} bytes\n", .{slice.len});
    
    // Demonstrate how C would call back into Zig
    std.debug.print("C would call zig_process_data() and get result\n", .{});
}

pub fn main() !void {
    // In a real program, we'd call actual C functions
    // Here we just show the structure since we don't have actual C code
    try simulateFFIDemo();
    
    std.debug.print("\nNote: This is a demonstration of the FFI pattern.\n", .{});
    std.debug.print("In a real program, the C functions would be from actual libraries.\n", .{});
}
