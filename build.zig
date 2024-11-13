const Builder = @import("std").Build;

pub fn build(builder: *Builder) void {
    const target = builder.standardTargetOptions(.{});
    const optimize = builder.standardOptimizeOption(.{});

    const executable = builder.addExecutable(.{
        .name = "compiler",
        .root_source_file = builder.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    const collections = builder.dependency("collections", .{
        .target = target,
        .optimize = optimize,
    });

    const util = builder.dependency("util", .{
        .target = target,
        .optimize = optimize,
    });

    const mem = builder.dependency("mem", .{
        .target = target,
        .optimize = optimize,
    });

    executable.root_module.addImport("mem", mem.module("mem"));
    executable.root_module.addImport("util", util.module("util"));
    executable.root_module.addImport("collections", collections.module("collections"));

    builder.installArtifact(executable);

    const run_executable = builder.addRunArtifact(executable);
    const run_step = builder.step("run", "Run unit tests");

    if (builder.args) |args| {
        run_executable.addArgs(args);
    }

    run_step.dependOn(&run_executable.step);
    run_step.dependOn(builder.getInstallStep());

    const executable_test = builder.addTest(.{
        .root_source_file = builder.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    executable_test.root_module.addImport("mem", mem.module("mem"));
    executable_test.root_module.addImport("util", util.module("util"));
    executable_test.root_module.addImport("collections", collections.module("collections"));

    const run_tests = builder.addRunArtifact(executable_test);

    const test_step = builder.step("test", "Run unit tests");

    test_step.dependOn(&run_tests.step);
}
