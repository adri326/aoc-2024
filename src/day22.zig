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

pub fn main() !void {
    const input = try std.fs.cwd().openFile("./input/day22.txt", .{});
    const raw = try input.readToEndAlloc(std.heap.page_allocator, 65535);
    defer std.heap.page_allocator.free(raw);

    var lines = std.mem.splitScalar(u8, raw, '\n');
    var res: u64 = 0;
    while (lines.next()) |line| {
        if (line.len == 0) {
            continue;
        }
        const seed = try std.fmt.parseInt(u32, line, 10);
        res += hash(seed, 2000);
    }

    std.debug.print("{}\n", .{res});
}
