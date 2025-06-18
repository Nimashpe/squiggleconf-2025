const std = @import("std");

// Define custom error sets
const FileError = error{
    NotFound,
    PermissionDenied,
    Corrupted,
};

const NetworkError = error{
    ConnectionFailed,
    Timeout,
    InvalidResponse,
};

// Combine error sets
const AppError = FileError || NetworkError;

// Function that returns a specific error set
fn readConfig(path: []const u8) FileError![]const u8 {
    if (std.mem.eql(u8, path, "/nonexistent")) {
        return FileError.NotFound;
    }
    if (std.mem.eql(u8, path, "/protected")) {
        return FileError.PermissionDenied;
    }
    return "config_data";
}

// Function that uses another error set
fn fetchData(url: []const u8) NetworkError![]const u8 {
    if (std.mem.eql(u8, url, "https://invalid")) {
        return NetworkError.ConnectionFailed;
    }
    return "response_data";
}

// Function that combines error sets
fn loadData(config_path: []const u8, api_url: []const u8) AppError!void {
    // Errors from readConfig are automatically promoted to AppError
    const config = try readConfig(config_path);
    std.debug.print("Config loaded: {s}\n", .{config});
    
    // Errors from fetchData are automatically promoted to AppError
    const data = try fetchData(api_url);
    std.debug.print("Data fetched: {s}\n", .{data});
}

pub fn main() void {
    // Success case
    loadData("/valid_config", "https://valid") catch |err| {
        std.debug.print("Error: {}\n", .{err});
        return;
    };
    
    // File error case
    loadData("/nonexistent", "https://valid") catch |err| {
        std.debug.print("Error: {}\n", .{err});
    };
    
    // Network error case
    loadData("/valid_config", "https://invalid") catch |err| {
        std.debug.print("Error: {}\n", .{err});
    };
}
