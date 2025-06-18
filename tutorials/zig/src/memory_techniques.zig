const std = @import("std");

// Structure with custom alignment requirements
const AlignedStruct = struct {
    // Aligned to cache line size for better performance
    x: u64 align(64),
    y: u64,
    z: u64,
};

// Structure with packed layout for memory efficiency
const PackedStruct = packed struct {
    // Use exact bit widths to minimize memory usage
    flag1: bool,
    flag2: bool,
    flag3: bool,
    mode: u5,
    reserved: u8,
};

// Custom allocator that logs allocations
const LoggingAllocator = struct {
    parent_allocator: std.mem.Allocator,
    allocation_count: usize,
    
    pub fn init(parent_allocator: std.mem.Allocator) LoggingAllocator {
        return LoggingAllocator{
            .parent_allocator = parent_allocator,
            .allocation_count = 0,
        };
    }
    
    pub fn allocator(self: *LoggingAllocator) std.mem.Allocator {
        return std.mem.Allocator.init(
            self,
            alloc,
            resize,
            free,
        );
    }
    
    fn alloc(
        self: *LoggingAllocator,
        len: usize,
        ptr_align: u8,
        len_align: usize,
        ret_addr: usize,
    ) ![]u8 {
        const result = try self.parent_allocator.rawAlloc(len, ptr_align, len_align, ret_addr);
        self.allocation_count += 1;
        std.debug.print("Allocation #{}: {} bytes at {*}\n", .{
            self.allocation_count,
            len,
            result.ptr,
        });
        return result;
    }
    
    fn resize(
        self: *LoggingAllocator,
        buf: []u8,
        buf_align: u8,
        new_len: usize,
        len_align: usize,
        ret_addr: usize,
    ) ?usize {
        std.debug.print("Resize: {} bytes to {} bytes at {*}\n", .{
            buf.len,
            new_len,
            buf.ptr,
        });
        return self.parent_allocator.rawResize(buf, buf_align, new_len, len_align, ret_addr);
    }
    
    fn free(
        self: *LoggingAllocator,
        buf: []u8,
        buf_align: u8,
        ret_addr: usize,
    ) void {
        std.debug.print("Free: {} bytes at {*}\n", .{
            buf.len,
            buf.ptr,
        });
        self.parent_allocator.rawFree(buf, buf_align, ret_addr);
    }
};

// Demo of advanced memory techniques
pub fn main() !void {
    std.debug.print("=== Advanced Memory Management Demo ===\n\n", .{});
    
    // 1. Alignment demonstration
    std.debug.print("Memory Alignment:\n", .{});
    var aligned = AlignedStruct{
        .x = 1,
        .y = 2,
        .z = 3,
    };
    
    std.debug.print("AlignedStruct size: {} bytes\n", .{@sizeOf(AlignedStruct)});
    std.debug.print("x address: {*} (alignment: {})\n", .{&aligned.x, @alignOf(@TypeOf(aligned.x))});
    std.debug.print("y address: {*} (alignment: {})\n", .{&aligned.y, @alignOf(@TypeOf(aligned.y))});
    std.debug.print("z address: {*} (alignment: {})\n\n", .{&aligned.z, @alignOf(@TypeOf(aligned.z))});
    
    // 2. Packed struct demonstration
    std.debug.print("Packed Structures:\n", .{});
    var packed = PackedStruct{
        .flag1 = true,
        .flag2 = false,
        .flag3 = true,
        .mode = 16,
        .reserved = 0,
    };
    
    std.debug.print("PackedStruct size: {} bytes\n", .{@sizeOf(PackedStruct)});
    std.debug.print("Individual bits stored: {}\n", .{1 + 1 + 1 + 5 + 8});
    std.debug.print("Values: flag1={}, flag2={}, flag3={}, mode={}\n\n", 
        .{packed.flag1, packed.flag2, packed.flag3, packed.mode});
    
    // 3. Custom allocator demonstration
    std.debug.print("Custom Allocator:\n", .{});
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    
    var logging_allocator = LoggingAllocator.init(gpa.allocator());
    const allocator = logging_allocator.allocator();
    
    // Allocate memory
    const buffer1 = try allocator.alloc(u8, 128);
    defer allocator.free(buffer1);
    
    const buffer2 = try allocator.alloc(u32, 64);
    defer allocator.free(buffer2);
    
    // Resize
    const resized = try allocator.realloc(buffer1, 256);
    
    std.debug.print("\nTotal allocations: {}\n", .{logging_allocator.allocation_count});
}
