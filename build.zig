const std = @import("std");

// Although this function looks imperative, note that its job is to
// declaratively construct a build graph that will be executed by an external
// runner.
pub fn build(b: *std.Build) void {
    // Standard target options allows the person running `zig build` to choose
    // what target to build for. Here we do not override the defaults, which
    // means any target is allowed, and the default is native. Other options
    // for restricting supported target set are available.
    const target = b.standardTargetOptions(.{});

    // Standard optimization options allow the person running `zig build` to select
    // between Debug, ReleaseSafe, ReleaseFast, and ReleaseSmall. Here we do not
    // set a preferred release mode, allowing the user to decide how to optimize.
    const optimize = b.standardOptimizeOption(.{});

    const day4 = b.addExecutable(.{
        .name = "day4",
        .root_source_file = b.path("src/day4.zig"),
        .target = target,
        .optimize = optimize,
    });
    b.installArtifact(day4);
    const day4_step = b.step("day4", "Build day4");
    const install_day4 = b.addInstallArtifact(day4, .{});
    day4_step.dependOn(&install_day4.step);

    const day5 = b.addExecutable(.{
        .name = "day5",
        .root_source_file = b.path("src/day5.zig"),
        .target = target,
        .optimize = optimize,
    });
    b.installArtifact(day5);
    const day5_step = b.step("day5", "Build day5");
    const install_day5 = b.addInstallArtifact(day5, .{});
    day5_step.dependOn(&install_day5.step);
}
