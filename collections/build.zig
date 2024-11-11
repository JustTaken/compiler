const Builder = @import("std").Build;

pub fn build(builder: *Builder) void {
    const target = builder.standardTargetOptions(.{});
    const optimize = builder.standardOptimizeOption(.{});

    const util = builder.dependency("util", .{
        .target = target,
        .optimize = optimize,
    });

    const mem = builder.dependency("mem", .{
        .target = target,
        .optimize = optimize,
    });

    const collections = builder.addModule("collections", .{
        .root_source_file = builder.path("src/lib.zig"),
    });

    collections.addImport("util", util.module("util"));
    collections.addImport("mem", mem.module("mem"));
}
