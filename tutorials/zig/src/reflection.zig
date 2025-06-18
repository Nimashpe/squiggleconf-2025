const std = @import("std");

// Sample struct to reflect on
const Person = struct {
    name: []const u8,
    age: u32,
    active: bool,
    
    pub fn init(name: []const u8, age: u32) Person {
        return Person{
            .name = name,
            .age = age,
            .active = true,
        };
    }
};

// Generic function to print all fields of any struct
fn debugPrint(value: anytype) void {
    const T = @TypeOf(value);
    const info = @typeInfo(T);
    
    // Ensure we're dealing with a struct
    if (info != .Struct) {
        std.debug.print("Not a struct: {}\n", .{@typeName(T)});
        return;
    }
    
    const struct_info = info.Struct;
    
    std.debug.print("Type: {s}\n", .{@typeName(T)});
    std.debug.print("Fields:\n", .{});
    
    // Iterate over all fields in the struct at compile time
    inline for (struct_info.fields) |field| {
        // Get the field value using @field
        const field_value = @field(value, field.name);
        
        // Handle different field types
        switch (@TypeOf(field_value)) {
            []const u8 => std.debug.print("  {s}: \"{s}\"\n", .{field.name, field_value}),
            u32, i32, usize, isize => std.debug.print("  {s}: {}\n", .{field.name, field_value}),
            bool => std.debug.print("  {s}: {}\n", .{field.name, field_value}),
            else => std.debug.print("  {s}: (complex type)\n", .{field.name}),
        }
    }
}

// Generate a JSON serializer for any struct type
fn Serializer(comptime T: type) type {
    return struct {
        // Static function to serialize a value of type T to JSON
        pub fn toJson(value: T, allocator: std.mem.Allocator) ![]const u8 {
            const info = @typeInfo(T);
            if (info != .Struct) @compileError("Can only serialize structs");
            
            var json = std.ArrayList(u8).init(allocator);
            defer json.deinit();
            
            try json.append('{');
            
            const fields = info.Struct.fields;
            comptime var first = true;
            
            // Process each field at compile time
            inline for (fields) |field| {
                if (!first) {
                    try json.appendSlice(", ");
                }
                first = false;
                
                // Add field name
                try json.appendSlice("\"");
                try json.appendSlice(field.name);
                try json.appendSlice("\": ");
                
                // Get field value
                const field_value = @field(value, field.name);
                
                // Handle different field types
                switch (@TypeOf(field_value)) {
                    []const u8 => {
                        try json.appendSlice("\"");
                        try json.appendSlice(field_value);
                        try json.appendSlice("\"");
                    },
                    u32, i32, usize, isize => {
                        var buf: [20]u8 = undefined;
                        const slice = try std.fmt.bufPrint(&buf, "{}", .{field_value});
                        try json.appendSlice(slice);
                    },
                    bool => {
                        if (field_value) {
                            try json.appendSlice("true");
                        } else {
                            try json.appendSlice("false");
                        }
                    },
                    else => @compileError("Unsupported field type"),
                }
            }
            
            try json.append('}');
            
            return json.toOwnedSlice();
        }
    };
}

pub fn main() !void {
    // Create a person instance
    const person = Person.init("Alan Turing", 41);
    
    // Print all fields using reflection
    debugPrint(person);
    
    // Use the generated serializer
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    
    const PersonSerializer = Serializer(Person);
    const json = try PersonSerializer.toJson(person, gpa.allocator());
    defer gpa.allocator().free(json);
    
    std.debug.print("\nJSON: {s}\n", .{json});
}
