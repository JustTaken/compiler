const std = @import("std");
const builtin = @import("builtin");

const String = @import("collections").String;

const PROGRAM_START: usize = 0x400_000;
const ARCH_SIZE: usize = @sizeOf(usize);

pub const Header = extern struct {
    ident: [16]u8,
    kind: u16,
    machine: u16,
    version: u32,
    entry: usize,
    phoff: usize,
    shoff: usize,
    flags: u32,
    ehsize: u16,
    pehntsize: u16,
    phnum: u16,
    shentsize: u16,
    shnum: u16,
    shstrndx: u16,

    const Kind = enum(u16) { None, Rel, Exec, Dyn, Core, Loos, Hios, Loproc, Hiproc };

    const CLASS = switch (ARCH_SIZE) {
        4 => 0x01,
        8 => 0x02,
        else => @compileError("Unknow integer size"),
    };

    const ENDIAN = switch (builtin.cpu.arch.endian()) {
        .little => 0x01,
        .big => 0x02,
    };

    const MACHINE = switch (builtin.cpu.arch) {
        .x86_64 => 0x3E,
        else => @compileError("Unknow archtecture"),
    };

    const OS_ABI = switch (builtin.abi) {
        .gnu => 0x00,
        else => @compileError("Unknow abi"),
    };

    const ELF_VERSION: u8 = 0x01;
    const ABI_VERSION: u8 = 0x00;
    const PADDING: u8 = 0x00;

    pub fn new(file_kind: Kind, program_header_count: u16, code_len: usize) Header {
        return Header{
            .ident = .{ 0x7F, 'E', 'L', 'F', CLASS, ENDIAN, ELF_VERSION, OS_ABI, ABI_VERSION, PADDING, PADDING, PADDING, PADDING, PADDING, PADDING, PADDING },
            .kind = @intFromEnum(file_kind),
            .machine = MACHINE,
            .version = ELF_VERSION,
            .entry = @sizeOf(ProgramHeader) * program_header_count + @sizeOf(Header) + PROGRAM_START + code_len,
            .phoff = @sizeOf(Header),
            .shoff = 0x00,
            .flags = 0x00,
            .ehsize = @sizeOf(Header),
            .pehntsize = @sizeOf(ProgramHeader),
            .phnum = program_header_count,
            .shentsize = 0x00,
            .shnum = 0x00,
            .shstrndx = 0x00,
        };
    }
};

pub const ProgramHeader = extern struct {
    kind: u32,
    flags: u32,
    offset: usize,
    vaddr: usize,
    paddr: usize,
    file_size: usize,
    mem_size: usize,
    alignment: usize,

    const Kind = enum(u8) { Null, Load, Dynamic, Interp, Note, ShLib, Phdr, Tls };
    const Flag = enum(u8) {
        Executable = 0x01,
        Writable = 0x02,
        Readable = 0x04,
    };

    pub fn new(kind: Kind, flags: []const Flag, len: usize) ProgramHeader {
        var flag_mask: u32 = 0;

        for (flags) |flag| {
            flag_mask |= @intFromEnum(flag);
        }

        const offset = @sizeOf(ProgramHeader) + @sizeOf(Header);

        return ProgramHeader{
            .kind = @intFromEnum(kind),
            .flags = flag_mask,
            .offset = offset,
            .vaddr = offset + PROGRAM_START,
            .paddr = offset + PROGRAM_START,
            .file_size = len,
            .mem_size = len,
            .alignment = 0x1000,
        };
    }
};

const SectionHeader = extern struct {
    name: [4]u8,
    kind: u32,
    flags: usize,
    addr: usize,
    offset: usize,
    size: usize,
    link: u32,
    info: u32,
    addralign: usize,
    entsize: usize,

    const Kind = enum(u32) { Null, ProgBits, SymTab, StrTab, Rela, Hash, Dynamic, Note, NoBits, Rel, ShLib, DynSym, InitArray, FiniArray, PreInitArray, Group, SymbTabShndx, Num };
    const Flag = enum(usize) { Write, Alloc, ExecInstr, Merge, Strings, InfoLink, Order, NonConforming, Group, Tls };
};
