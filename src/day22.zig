const std = @import("std");

const modulo: u32 = 16777216;

fn hash(seed: u32, cycles: u32) u32 {
    var res: u32 = seed;
    for (0..cycles) |_| {
        res ^= res << 6;
        res %= modulo;

        res ^= res >> 5;
        res %= modulo;

        res ^= res << 11;
        res %= modulo;
    }
    return res;
}

const MAP_SIZE: usize = 19 * 19 * 19 * 19;

fn sequenceToIndex(prev: *const [4]u32, current: u32) usize {
    const diff: [4]i32 = .{
        @as(i32, @intCast(prev[1] % 10)) - @as(i32, @intCast(prev[0] % 10)),
        @as(i32, @intCast(prev[2] % 10)) - @as(i32, @intCast(prev[1] % 10)),
        @as(i32, @intCast(prev[3] % 10)) - @as(i32, @intCast(prev[2] % 10)),
        @as(i32, @intCast(current % 10)) - @as(i32, @intCast(prev[3] % 10)),
    };
    // std.debug.print(":: {},{},{},{},{}\n", .{ prev[0], prev[1], prev[2], prev[3], current });
    // std.debug.print("{},{},{},{}\n", .{ diff[0], diff[1], diff[2], diff[3] });

    var sum: usize = 0;
    for (0..4) |x| {
        sum *= 19;
        sum += @intCast(9 + diff[x]);
    }
    return sum;
}

fn indexToSequence(index: usize) [4]i32 {
    var diff: [4]i32 = undefined;
    var current = index;
    for (0..4) |x| {
        diff[3 - x] = @as(i32, @intCast(current % 19)) - 9;
        current /= 19;
    }
    return diff;
}

fn shiftSequence(sequence: *[4]u32, current: u32) void {
    for (0..3) |x| {
        sequence[x] = sequence[x + 1];
    }
    sequence[3] = current;
}

fn computeChanges(seed: u32, cycles: u32, current_map: []u8) void {
    var prev: [4]u32 = .{ 0, 0, 0, 0 };
    for (0..4) |x| {
        prev[x] = hash(seed, @intCast(x));
    }

    for (4..cycles) |_| {
        const next = hash(prev[3], 1);
        const index = sequenceToIndex(&prev, next);
        shiftSequence(&prev, next);
        if (current_map[index] == 255) {
            current_map[index] = @intCast(next % 10);
        }
    }
}

fn fill(comptime T: type, map: []T, value: T) void {
    for (0..map.len) |x| {
        map[x] = value;
    }
}

fn addMap(score_map: []u32, current_map: []u8) void {
    if (score_map.len != current_map.len) return;

    for (0..score_map.len) |x| {
        if (current_map[x] != 255) score_map[x] += current_map[x];
    }
}

fn argmax(score_map: []u32) struct { usize, u32 } {
    var res: struct { usize, u32 } = .{ 0, score_map[0] };

    for (0..score_map.len) |x| {
        if (score_map[x] > res.@"1") {
            res = .{ x, score_map[x] };
        }
    }

    return res;
}

pub fn main() !void {
    const input = try std.fs.cwd().openFile("./input/day22.txt", .{});
    const raw = try input.readToEndAlloc(std.heap.page_allocator, 65535);
    defer std.heap.page_allocator.free(raw);

    var lines = std.mem.splitScalar(u8, raw, '\n');
    var part1: u64 = 0;

    const score_map = try std.heap.page_allocator.alloc(u32, MAP_SIZE);
    defer std.heap.page_allocator.free(score_map);
    fill(u32, score_map, 0);

    const current_map = try std.heap.page_allocator.alloc(u8, MAP_SIZE);
    defer std.heap.page_allocator.free(current_map);

    while (lines.next()) |line| {
        if (line.len == 0) {
            continue;
        }
        const seed = try std.fmt.parseInt(u32, line, 10);
        part1 += hash(seed, 2000);

        fill(u8, current_map, 255);
        computeChanges(seed, 2000, current_map);
        addMap(score_map, current_map);
    }
    std.debug.print("{}\n", .{part1});

    const best = argmax(score_map);
    const best_sequence = indexToSequence(best.@"0");
    std.debug.print("{},{},{},{}: ", .{ best_sequence[0], best_sequence[1], best_sequence[2], best_sequence[3] });
    std.debug.print("{}\n", .{best.@"1"});
}
