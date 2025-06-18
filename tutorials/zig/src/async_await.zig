const std = @import("std");

// A frame is an allocated stack and other small state for an async function
fn simpleAsyncFn() callconv(.Async) void {
    std.debug.print("Started async function\n", .{});
    suspend {
        // Execution is suspended here, callsite can resume later
        std.debug.print("Suspended\n", .{});
    }
    std.debug.print("Resumed and completed\n", .{});
}

// A more complex async function that returns a value
fn fetchValue(value: u32, delay_ms: u32) callconv(.Async) u32 {
    std.debug.print("Fetching value: {}\n", .{value});
    
    // Simulate work with a suspend point
    suspend {
        std.debug.print("Suspended fetch for {}ms\n", .{delay_ms});
    }
    
    // Compute result after resuming
    const result = value * 2;
    std.debug.print("Fetch complete, result: {}\n", .{result});
    return result;
}

// Async function that awaits multiple operations
fn processValues() callconv(.Async) !void {
    std.debug.print("=== Processing Values ===\n", .{});
    
    // Start multiple async operations (these would normally use real I/O)
    var frame1 = async fetchValue(42, 100);
    var frame2 = async fetchValue(100, 50);
    
    // Await their results
    const result1 = await frame1;
    const result2 = await frame2;
    
    const total = result1 + result2;
    std.debug.print("Total: {}\n", .{total});
}

// A simple event loop simulator for demonstration
fn simulateEventLoop() !void {
    std.debug.print("=== Event Loop Simulation ===\n", .{});
    
    // Create async frames
    var simple_frame = async simpleAsyncFn();
    var process_frame = async processValues();
    
    // Immediately resume all suspended functions
    // In a real event loop, we'd wait for events and resume selectively
    resume simple_frame;
    resume process_frame;
    
    // In a real program, we'd wait for async operations to complete
    // But since we can't actually wait here, we just demonstrate the concept
    std.debug.print("\nNote: This is a simplified event loop simulation.\n", .{});
    std.debug.print("In a real program, we'd wait for I/O events and resume frames accordingly.\n", .{});
}

pub fn main() !void {
    try simulateEventLoop();
}
