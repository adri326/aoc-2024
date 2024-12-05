const std = @import("std");

const Grid = struct {
    bytes: []u8,
    width: usize,
    height: usize,
    allocator: std.mem.Allocator,

    fn parse(raw: []u8, allocator: std.mem.Allocator) std.mem.Allocator.Error!Grid {
        var length: usize = 0;
        var first_line = true;
        var width: usize = 0;
        for (raw) |c| {
            if (c != '\n') {
                length += 1;
                if (first_line) {
                    width += 1;
                }
            } else {
                first_line = false;
            }
        }

        const bytes = try allocator.alloc(u8, length);
        var offset: usize = 0;
        for (raw) |c| {
            if (c != '\n') {
                bytes[offset] = c;
                offset += 1;
            }
        }

        return Grid{
            .bytes = bytes,
            .width = width,
            .height = length / width,
            .allocator = allocator,
        };
    }

    fn free(self: *const Grid) void {
        self.allocator.free(self.bytes);
    }

    fn get(self: *const Grid, x: usize, y: usize) u8 {
        if (x >= self.width or y >= self.height) {
            return 0;
        }

        return self.bytes[x + y * self.width];
    }
};

const XMAS = "XMAS";
const MAS = "MMSS";
const Dir = struct { x: i32, y: i32 };
const DIRS = [_]Dir{
    .{ .x = 0, .y = 1 },
    .{ .x = 1, .y = 1 },
    .{ .x = -1, .y = 1 },
    .{ .x = 1, .y = 0 },
    .{ .x = -1, .y = 0 },
    .{ .x = 0, .y = -1 },
    .{ .x = 1, .y = -1 },
    .{ .x = -1, .y = -1 },
};

/// Returns the number of `XMAS` whose `X` originate at `(x, y)`.
fn count_xmas(grid: *const Grid, x: usize, y: usize) usize {
    if (grid.get(x, y) != XMAS[0]) return 0;

    var res: usize = 0;
    for (DIRS) |dir| {
        const xmax = @as(i32, @intCast(x)) + dir.x * @as(i32, XMAS.len - 1);
        const ymax = @as(i32, @intCast(y)) + dir.y * @as(i32, XMAS.len - 1);
        if (xmax < 0 or ymax < 0 or xmax >= @as(i32, @intCast(grid.width)) or ymax >= @as(i32, @intCast(grid.height))) {
            continue;
        }

        var correct = true;
        for (1..XMAS.len) |dist| {
            const x2 = @as(i32, @intCast(x)) + dir.x * @as(i32, @intCast(dist));
            const y2 = @as(i32, @intCast(y)) + dir.y * @as(i32, @intCast(dist));
            if (grid.get(@as(usize, @intCast(x2)), @as(usize, @intCast(y2))) != XMAS[dist]) {
                correct = false;
                break;
            }
        }

        if (correct) {
            res += 1;
        }
    }

    return res;
}

/// Returns true if `(x, y)` is the center of two "MAS" in an "X" shape.
fn is_x_mas(grid: *const Grid, x: usize, y: usize) bool {
    if (x == 0 or y == 0 or x == grid.width - 1 or y == grid.height - 1 or grid.get(x, y) != 'A') {
        return 0;
    }

    const diagonals = [4]u8{
        grid.get(x - 1, y - 1),
        grid.get(x + 1, y - 1),
        grid.get(x + 1, y + 1),
        grid.get(x - 1, y + 1),
    };

    for (0..4) |rot| {
        var matches = true;
        for (0..4) |i| {
            if (diagonals[(i + rot) % 4] != MAS[i]) {
                matches = false;
                break;
            }
        }
        if (matches) return true;
    }
    return false;
}

pub fn main() !void {
    var input = try std.fs.cwd().openFile("./input/day4.txt", .{});

    const raw_grid = try input.readToEndAlloc(std.heap.page_allocator, 65535);
    defer std.heap.page_allocator.free(raw_grid);
    const grid = try Grid.parse(raw_grid, std.heap.page_allocator);
    defer grid.free();

    var crossword_sum: usize = 0;
    var cross_sum: usize = 0;
    for (0..grid.height) |y| {
        for (0..grid.width) |x| {
            crossword_sum += count_xmas(&grid, x, y);
            if (is_x_mas(&grid, x, y)) {
                cross_sum += 1;
            }
        }
    }
    std.debug.print("{}\n", .{crossword_sum});
    std.debug.print("{}\n", .{cross_sum});
}
