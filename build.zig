const std = @import("std");

// Although this function looks imperative, note that its job is to
// declaratively construct a build graph that will be executed by an external
// runner.
pub fn build(b: *std.Build) !void {
    // Standard target options allows the person running `zig build` to choose
    // what target to build for. Here we do not override the defaults, which
    // means any target is allowed, and the default is native. Other options
    // for restricting supported target set are available.
    const target = b.standardTargetOptions(.{});

    // Standard optimization options allow the person running `zig build` to select
    // between Debug, ReleaseSafe, ReleaseFast, and ReleaseSmall. Here we do not
    // set a preferred release mode, allowing the user to decide how to optimize.
    const optimize = b.standardOptimizeOption(.{});

    const days: [3][]const u8 = [_][]const u8{ "day4", "day5", "day22" };
    for (days) |day| {
        const path = try std.mem.concatWithSentinel(std.heap.page_allocator, u8, &[_][]const u8{ "src/", day, ".zig" }, 0);
        const day_target = b.addExecutable(.{
            .name = day,
            .root_source_file = b.path(path),
            .target = target,
            .optimize = optimize,
        });
        b.installArtifact(day_target);
        const day_step = b.step(day, "Build day");
        const install_day = b.addInstallArtifact(day_target, .{});
        day_step.dependOn(&install_day.step);
    }
}
