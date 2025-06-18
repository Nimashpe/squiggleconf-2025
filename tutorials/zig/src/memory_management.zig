const std = @import("std");
const Allocator = std.mem.Allocator;

// A custom type that requires allocation
const CustomList = struct {
    items: []i32,
    allocator: Allocator,

    // Constructor that takes an allocator
    pub fn init(allocator: Allocator) !CustomList {
        return CustomList{
            .items = try allocator.alloc(i32, 0),
            .allocator = allocator,
        };
    }

    // Always free memory
    pub fn deinit(self: *CustomList) void {
        self.allocator.free(self.items);
        self.items = undefined;
    }

    // Add an item, reallocating as needed
    pub fn append(self: *CustomList, value: i32) !void {
        const new_items = try self.allocator.realloc(self.items, self.items.len + 1);
        self.items = new_items;
        self.items[self.items.len - 1] = value;
    }

    // Print all items
    pub fn print(self: CustomList) void {
        std.debug.print("List contents: ", .{});
        for (self.items) |item| {
            std.debug.print("{} ", .{item});
        }
        std.debug.print("\n", .{});
    }
};

// Demonstrates different allocators
pub fn main() !void {
    // General purpose allocator - good for applications
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const gpa_allocator = gpa.allocator();

    // Use GPA for a list
    var list1 = try CustomList.init(gpa_allocator);
    defer list1.deinit();
    try list1.append(1);
    try list1.append(2);
    try list1.append(3);
    list1.print();

    // Arena allocator - efficient for temporary allocations
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const arena_allocator = arena.allocator();

    // Use arena for another list
    var list2 = try CustomList.init(arena_allocator);
    // No need to call list2.deinit() individually since arena.deinit() frees everything at once
    try list2.append(10);
    try list2.append(20);
    list2.print();

    // Fixed buffer allocator - no heap allocation
    var buffer: [1024]u8 = undefined;
    var fba = std.heap.FixedBufferAllocator.init(&buffer);
    const fba_allocator = fba.allocator();

    // Use fixed buffer for a third list
    var list3 = try CustomList.init(fba_allocator);
    defer list3.deinit();
    try list3.append(100);
    try list3.append(200);
    try list3.append(300);
    list3.print();
}
