const std = @import("std");

const FIRST_PART: bool = false;
const MAX_RULE: usize = 100;

const Rule = struct {
    lhs: u32,
    rhs: u32,

    fn is_satisfied(self: *const Rule, list: []const u32) bool {
        if (std.mem.indexOf(u32, list, &[1]u32{self.lhs})) |lhs_pos| {
            if (std.mem.indexOf(u32, list, &[1]u32{self.rhs})) |rhs_pos| {
                return lhs_pos < rhs_pos;
            }
        }

        return true;
    }

    fn satisfy(self: *const Rule, list: []u32) bool {
        if (std.mem.indexOf(u32, list, &[1]u32{self.lhs})) |lhs_pos| {
            if (std.mem.indexOf(u32, list, &[1]u32{self.rhs})) |rhs_pos| {
                if (lhs_pos > rhs_pos) {
                    std.mem.swap(u32, &list[lhs_pos], &list[rhs_pos]);
                    return true;
                }
            }
        }
        return false;
    }
};

const Ruleset = struct {
    switch_if: [MAX_RULE * MAX_RULE]u8,

    fn from_rules(rules: []const Rule) Ruleset {
        var switch_if: [MAX_RULE * MAX_RULE]u8 = undefined;
        for (0..switch_if.len) |i| {
            switch_if[i] = 0;
        }

        for (rules) |rule| {
            switch_if[rule.rhs + rule.lhs * MAX_RULE] = 2;
            switch_if[rule.lhs + rule.rhs * MAX_RULE] = 1;
        }

        return Ruleset{ .switch_if = switch_if };
    }

    fn satisfy_rules(self: *const Ruleset, list: []u32) void {
        for (0..list.len) |current_index| {
            for (0..list.len) |cmp_index| {
                const state: u8 = if (cmp_index < current_index) 2 else 1;
                if (cmp_index == current_index) {
                    continue;
                }

                if (self.switch_if[list[current_index] * MAX_RULE + list[cmp_index]] == state) {
                    std.mem.swap(u32, &list[current_index], &list[cmp_index]);
                }
            }
        }
    }

    fn are_rules_satisfied(self: *const Ruleset, list: []const u32) bool {
        for (0..list.len) |current_index| {
            for (0..list.len) |cmp_index| {
                const state: u8 = if (cmp_index < current_index) 2 else 1;
                if (self.switch_if[list[current_index] * MAX_RULE + list[cmp_index]] == state) {
                    return false;
                }
            }
        }
        return true;
    }
};

fn are_rules_satisfied(rules: []const Rule, list: []const u32) bool {
    for (rules) |rule| {
        if (!rule.is_satisfied(list)) return false;
    }
    return true;
}

fn satisfy_rules(rules: []const Rule, list: []u32) !void {
    while (true) {
        var keep_running = false;
        for (rules) |rule| {
            if (rule.satisfy(list)) {
                keep_running = true;
            }
        }
        if (!keep_running) {
            break;
        }
    }

    try std.testing.expect(are_rules_satisfied(rules, list));
    // if (!rule.is_satisfied(list)) {

    // }
}

pub fn main() !void {
    const input = try std.fs.cwd().openFile("./input/day5.txt", .{});
    const raw = try input.readToEndAlloc(std.heap.page_allocator, 65535);
    defer std.heap.page_allocator.free(raw);

    var lines = std.mem.splitScalar(u8, raw, '\n');
    var rules = std.ArrayList(Rule).init(std.heap.page_allocator);
    defer rules.deinit();

    var current_list = std.ArrayList(u32).init(std.heap.page_allocator);
    defer current_list.deinit();

    var base_sum: usize = 0;
    var corrected_sum: usize = 0;

    while (lines.next()) |line| {
        if (std.mem.indexOf(u8, line, "|")) |bar| {
            try rules.append(Rule{
                .lhs = try std.fmt.parseInt(u32, line[0..bar], 10),
                .rhs = try std.fmt.parseInt(u32, line[(bar + 1)..], 10),
            });
        } else break;
    }

    const ruleset = Ruleset.from_rules(rules.items);

    while (lines.next()) |line| {
        if (line.len == 0) continue;

        current_list.clearRetainingCapacity();
        var numbers = std.mem.splitScalar(u8, line, ',');
        while (numbers.next()) |num| {
            try current_list.append(try std.fmt.parseInt(u32, num, 10));
        }

        if (ruleset.are_rules_satisfied(current_list.items)) {
            const middle = current_list.items[(current_list.items.len - 1) / 2];
            base_sum += middle;
        } else {
            ruleset.satisfy_rules(current_list.items);

            const middle = current_list.items[(current_list.items.len - 1) / 2];
            corrected_sum += middle;
        }
    }

    std.debug.print("{}\n", .{base_sum});
    std.debug.print("{}\n", .{corrected_sum});
}
