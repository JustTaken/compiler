const Builder = @import("std").Build;

pub fn build(builder: *Builder) void {
    const target = builder.standardTargetOptions(.{});
    const optimize = builder.standardOptimizeOption(.{});

    const main = builder.addExecutable(.{
        .name = "compiler",
        .root_source_file = builder.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    builder.installArtifact(main);

    const run_cmd = builder.addRunArtifact(main);

    run_cmd.step.dependOn(builder.getInstallStep());
    const run_step = builder.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    const unit_test = builder.addTest(.{
        .root_source_file = builder.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    const run_test = builder.addRunArtifact(unit_test);
    const test_step = builder.step("test", "Run unit tests");
    test_step.dependOn(&run_test.step);
}
