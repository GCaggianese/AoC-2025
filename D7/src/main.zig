const std = @import("std");
const D7 = @import("D7");

var counter: usize = 0;

fn step(grid: [][]u8, rows: usize, cols: usize) void {
    var i: usize = rows - 1;
    while (i > 0) : (i -= 1) {
        const row_above = i - 1;
        for (0..cols) |j| {
            if ((grid[row_above][j] == 'S') or (grid[row_above][j] == '|')) {
                const below = grid[i][j];
                if (below == '.') {
                    grid[i][j] = '|';
                } else if (below == '^') {
                    counter += 1;
                    grid[i][j] = '-';
                    if (j > 0) grid[i][j - 1] = '|';
                    if (j + 1 < cols) grid[i][j + 1] = '|';
                }
            }
        }
    }
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Read file
    const file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    const content = try file.readToEndAlloc(allocator, 1024 * 1024);
    defer allocator.free(content);

    // Get dimensions
    var rows: usize = 0;
    var cols: usize = 0;
    var lines = std.mem.splitScalar(u8, content, '\n');
    while (lines.next()) |line| {
        if (line.len == 0) continue;
        if (cols == 0) cols = line.len;
        rows += 1;
    }

    // Allocate memory for the matrix
    const grid = try allocator.alloc([]u8, rows);
    defer {
        for (grid) |row| allocator.free(row);
        allocator.free(grid);
    }

    // Fill the matrix
    lines = std.mem.splitScalar(u8, content, '\n');
    var row_idx: usize = 0;
    while (lines.next()) |line| {
        if (line.len == 0) continue;
        grid[row_idx] = try allocator.dupe(u8, line);
        row_idx += 1;
    }

    // Print input
    std.debug.print("Grid: {}x{}\n", .{ rows, cols });
    for (grid, 0..) |row, i| {
        std.debug.print("Row {}: {s}\n", .{ i, row });
    }

    // Run propagation until bottom
    for (0..rows) |_| {
        step(grid, rows, cols);
    }

    // Print result
    for (grid) |row| {
        std.debug.print("{s}\n", .{row});
    }
    std.debug.print("Result: {}\n", .{counter});
}
