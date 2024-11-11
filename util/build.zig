const Builder = @import("std").Build;

pub fn build(builder: *Builder) void {
    _ = builder.standardTargetOptions(.{});
    _ = builder.standardOptimizeOption(.{});

    _ = builder.addModule("util", .{
        .root_source_file = builder.path("src/lib.zig"),
    });
}
