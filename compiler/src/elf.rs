pub const EXECUTABLE: u32 = 0x1;
pub const WRITEABLE: u32 = 0x2;
pub const READABLE: u32 = 0x3;

pub const PROGRAM_HEADER_COUNT: usize = 0x1;
pub const START_ADDRESS: usize = 0x400000;

#[repr(packed)]
#[allow(dead_code)]
pub struct ElfHeader {
    pub ident: [u8; 16],
    pub kind: u16,
    pub machine: u16,
    pub version: u32,
    pub entry: u64,
    pub phoff: u64,
    pub shoff: u64,
    pub flags: u32,
    pub ehsize: u16,
    pub pehntsize: u16,
    pub phnum: u16,
    pub shentsize: u16,
    pub shnum: u16,
    pub shstrndx: u16,
}

#[repr(packed)]
#[allow(dead_code)]
pub struct ProgramHeader {
    pub kind: u32,
    pub flags: u32,
    pub offset: u64,
    pub vaddr: u64,
    pub paddr: u64,
    pub file_size: u64,
    pub mem_size: u64,
    pub align: u64,
}
