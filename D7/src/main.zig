// SPDX-FileCopyrightText: 2026 Germán Caggianese <german.caggianese@pm.me>
//
// SPDX-License-Identifier: Apache-2.0

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

const State = struct {
    row: usize,
    col: usize,

    pub fn hash(self: State) u64 {
        return (@as(u64, self.row) << 32) | @as(u64, self.col);
    }

    pub fn eql(self: State, other: State) bool {
        return self.row == other.row and self.col == other.col;
    }
};

const StateContext = struct {
    pub fn hash(_: StateContext, s: State) u64 {
        return s.hash();
    }
    pub fn eql(_: StateContext, a: State, b: State) bool {
        return a.eql(b);
    }
};

var call_count: usize = 0;
var cache_hits: usize = 0;

fn countPaths(
    grid: [][]u8,
    row: usize,
    col: usize,
    rows: usize,
    cols: usize,
    memo: *std.HashMap(State, usize, StateContext, std.hash_map.default_max_load_percentage),
) !usize {
    call_count += 1;

    // Base case
    if (col >= cols) {
        return 1;
    }

    // Check hash
    const state = State{ .row = row, .col = col };
    if (memo.get(state)) |cached| {
        cache_hits += 1;
        if (call_count % 1000 == 0) {
            // std.debug.print("[calls={}, hits={}, efficiency={d:.1}%]\n", .{
            //     call_count,
            //     cache_hits,
            //     @as(f64, @floatFromInt(cache_hits)) / @as(f64, @floatFromInt(call_count)) * 100.0,
            // });
        }
        return cached;
    }

    var r = row;
    while (r < rows) : (r += 1) {
        if (grid[r][col] == '^') {
            // Split: sum left + right branches
            const next_row = r + 1;

            const left = if (col == 0) 1 else try countPaths(grid, next_row, col - 1, rows, cols, memo);
            const right = try countPaths(grid, next_row, col + 1, rows, cols, memo);

            const total = left + right;

            // Save result
            try memo.put(state, total);
            return total;
        }
    }

    // Base case completed path
    try memo.put(state, 1);
    return 1;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    const content = try file.readToEndAlloc(allocator, 1024 * 1024);
    defer allocator.free(content);

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

    std.debug.print("--------- Part Two ---------\n", .{});

    // std.debug.print("Grid: {}x{}\n", .{ rows, cols });

    // Find S position
    var start_row: usize = 0;
    var start_col: usize = 0;
    for (grid, 0..) |row, r| {
        for (row, 0..) |cell, c| {
            if (cell == 'S') {
                start_row = r;
                start_col = c;
            }
        }
    }

    // std.debug.print("Starting from S at row={}, col={}\n\n", .{ start_row, start_col });

    // Creating hash to avoid recalculations
    var memo = std.HashMap(State, usize, StateContext, std.hash_map.default_max_load_percentage).init(allocator);
    defer memo.deinit();

    const result = try countPaths(grid, start_row, start_col, rows, cols, &memo);

    // std.debug.print("\n=== RESULT ===\n", .{});
    // std.debug.print("Total function calls: {}\n", .{call_count});
    // std.debug.print("Cache hits: {}\n", .{cache_hits});
    // std.debug.print("Cache efficiency: {d:.1}%\n", .{
    //     @as(f64, @floatFromInt(cache_hits)) / @as(f64, @floatFromInt(call_count)) * 100.0,
    // });
    // std.debug.print("Unique states computed: {}\n", .{memo.count()});
    std.debug.print("Timelines: {}\n", .{result});

    std.debug.print("--------- Part One ---------\n", .{});

    // Print input
    // std.debug.print("Grid: {}x{}\n", .{ rows, cols });
    // for (grid, 0..) |row, i| {
    //     std.debug.print("Row {}: {s}\n", .{ i, row });
    // }

    // Run propagation until bottom
    for (0..rows) |_| {
        step(grid, rows, cols);
    }

    // Print result
    // for (grid) |row| {
    //     std.debug.print("{s}\n", .{row});
    // }
    std.debug.print("Result: {}\n", .{counter});
}
