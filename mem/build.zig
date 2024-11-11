const Builder = @import("std").Build;

pub fn build(builder: *Builder) void {
    const target = builder.standardTargetOptions(.{});
    const optimize = builder.standardOptimizeOption(.{});

    const util = builder.dependency("util", .{
        .target = target,
        .optimize = optimize,
    });

    const mem = builder.addModule("mem", .{
        .root_source_file = builder.path("src/lib.zig"),
    });

    mem.addImport("util", util.module("util"));
}
