const Builder = @import("std").Build;

pub fn build(builder: *Builder) void {
    _ = builder.standardTargetOptions(.{});
    _ = builder.standardOptimizeOption(.{});

    const collections = builder.addModule("collections", .{
        .root_source_file = builder.path("collections/lib.zig"),
    });

    const mem = builder.addModule("mem", .{
        .root_source_file = builder.path("mem/lib.zig"),
    });

    const util = builder.addModule("util", .{
        .root_source_file = builder.path("util/lib.zig"),
    });

    collections.addImport("mem", mem);
    collections.addImport("util", util);
    mem.addImport("util", util);
    util.addImport("collections", collections);
    util.addImport("mem", mem);
}
