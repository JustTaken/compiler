use crate::checker::{Constant, ConstantBinary, ConstantKind};
use collections::Vector;
use mem::Arena;
use std::io::Write;
use util::{Index, Range};

#[repr(packed)]
#[allow(dead_code)]
struct ElfHeader {
    ident: [u8; 16],
    kind: u16,
    machine: u16,
    version: u32,
    entry: u64,
    phoff: u64,
    shoff: u64,
    flags: u32,
    ehsize: u16,
    pehntsize: u16,
    phnum: u16,
    shentsize: u16,
    shnum: u16,
    shstrndx: u16,
}

#[repr(packed)]
#[allow(dead_code)]
struct ProgramHeader {
    kind: u32,
    flags: u32,
    offset: u64,
    vaddr: u64,
    paddr: u64,
    file_size: u64,
    mem_size: u64,
    align: u64,
}

struct Number(usize);

enum Register {
    Rax,
    Rbx,
    Rcx,
    Rdi,
    Rdx,
    Rsi,
    Rsp,
}

enum Source {
    Register(Register),
    Memory(Register, Number),
    Number(Number),
}

enum Destination {
    Register(Register),
    Memory(Register, Number),
}

enum Operation {
    Mov(Destination, Source),
    Add(Destination, Source),
    Sub(Destination, Source),
    Mul(Destination, Source),
    Syscall,
}

pub struct Generator {
    buffer: Vector<u8>,
    operations: Vector<Operation>,
    stack: Index,
    code_len: Index,
    arena: Arena,
    file: std::fs::File,
}

trait AsBytes {
    type Item;
    fn write(&self, arg: Self::Item, buffer: &mut Vector<u8>);
    fn size(&self) -> Index;
}

impl AsBytes for Register {
    type Item = u8;

    fn write(&self, offset: Self::Item, buffer: &mut Vector<u8>) {
        let value = match self {
            Register::Rax => 0x00,
            Register::Rbx => 0x03,
            Register::Rcx => 0x01,
            Register::Rdi => 0x07,
            Register::Rdx => 0x02,
            Register::Rsi => 0x06,
            Register::Rsp => 0x04,
        };

        buffer.push(offset + value);
    }

    fn size(&self) -> Index {
        1
    }
}

impl AsBytes for Number {
    type Item = usize;

    fn write(&self, bytes: Self::Item, buffer: &mut Vector<u8>) {
        for i in 0..bytes {
            buffer.push((self.0 >> (8 * i)) as u8);
        }
    }

    fn size(&self) -> Index {
        0
    }
}

impl AsBytes for Destination {
    type Item = u8;

    fn write(&self, offset: Self::Item, buffer: &mut Vector<u8>) {
        match self {
            Destination::Register(r) => r.write(offset, buffer),
            Destination::Memory(_, _) => todo!(),
        }
    }

    fn size(&self) -> Index {
        match self {
            Destination::Register(r) => r.size(),
            Destination::Memory(_, _) => todo!(),
        }
    }
}

impl AsBytes for Source {
    type Item = usize;

    fn write(&self, bytes: Self::Item, buffer: &mut Vector<u8>) {
        match self {
            Source::Number(n) => n.write(bytes, buffer),
            Source::Register(_) => todo!(),
            Source::Memory(r, n) => todo!(),
        }
    }

    fn size(&self) -> Index {
        0
    }
}

impl Operation {
    fn new(i: usize, destination: Destination, value: Source) -> Operation {
        match i {
            0 => Operation::Mov(destination, value),
            1 => Operation::Add(destination, value),
            2 => Operation::Sub(destination, value),
            3 => Operation::Mul(destination, value),
            _ => panic!("Should not happen"),
        }
    }
}

impl std::fmt::Debug for Register {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Register::Rax => write!(f, "Regsiter(Rax)"),
            Register::Rbx => write!(f, "Regsiter(Rbx)"),
            Register::Rcx => write!(f, "Regsiter(Rcx)"),
            Register::Rdi => write!(f, "Regsiter(Rdi)"),
            Register::Rsi => write!(f, "Regsiter(Rsi)"),
            Register::Rsp => write!(f, "Regsiter(Rsp)"),
            Register::Rdx => write!(f, "Regsiter(Rdx)"),
        }
    }
}

impl std::fmt::Debug for Destination {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Destination::Register(r) => write!(f, "Destination({:?})", r),
            Destination::Memory(_, _) => panic!("Should not happen"),
        }
    }
}

impl std::fmt::Debug for Source {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Source::Register(r) => write!(f, "Source({:?})", r),
            Source::Number(Number(n)) => write!(f, "Source({})", n),
            Source::Memory(r, Number(n)) => write!(f, "Source(Memory({:?}, {}))", r, n),
        }
    }
}

impl std::fmt::Debug for Operation {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Operation::Mov(destination, value) => write!(f, "Mov({:?}, {:?})", destination, value),
            Operation::Add(destination, value) => write!(f, "Add({:?}, {:?})", destination, value),
            Operation::Sub(destination, value) => write!(f, "Sub({:?}, {:?})", destination, value),
            Operation::Mul(destination, value) => write!(f, "Mul({:?}, {:?})", destination, value),
            Operation::Syscall => write!(f, "Syscall"),
            _ => write!(f, ""),
        }
    }
}

struct ProgramFlag;
impl ProgramFlag {
    const EXECUTABLE: u32 = 0x1;
    const WRITEABLE: u32 = 0x2;
    const READABLE: u32 = 0x3;
}

const PROGRAM_HEADER_COUNT: usize = 0x1;
const START_ADDRESS: usize = 0x400000;

impl Generator {
    pub fn new(path: &str, arena: &mut Arena) -> Generator {
        let mut self_arena = Arena::new(arena.bytes(4096));

        Generator {
            buffer: Vector::new(1024, &mut self_arena),
            operations: Vector::new(50, &mut self_arena),
            code_len: 0,
            stack: 0,
            arena: self_arena,
            file: std::fs::File::create(path).unwrap(),
        }
    }

    fn operation_size(&self, operation: &Operation) -> Index {
        match operation {
            Operation::Syscall => 0x02,
            Operation::Add(destin, _) => destin.size() + 0x02,
            Operation::Sub(destin, _) => destin.size() + 0x02,
            Operation::Mov(destin, _) => destin.size() + 0x04,
            Operation::Mul(destin, _) => destin.size() + 0x04,
        }
    }

    fn write_operation(&mut self, operation: &Operation) {
        match operation {
            Operation::Syscall => self.buffer.extend(&[0x0F, 0x05]),
            Operation::Add(destin, source) => {
                self.buffer.push(0x83);

                destin.write(0xC0, &mut self.buffer);
                source.write(1, &mut self.buffer);
            }
            Operation::Sub(destin, source) => {
                self.buffer.push(0x83);

                destin.write(0xE8, &mut self.buffer);
                source.write(1, &mut self.buffer);
            }
            Operation::Mul(destin, source) => {
                self.buffer.push(0x6B);

                destin.write(0xC0, &mut self.buffer);
                source.write(1, &mut self.buffer);
            }
            Operation::Mov(destin, source) => match (destin, source) {
                (Destination::Register(rd), Source::Number(n)) => {
                    rd.write(0xB8, &mut self.buffer);
                    n.write(4, &mut self.buffer);
                }
                (Destination::Memory(rd, n), Source::Register(rs)) => {
                    // This just works if the source is RAX and the destination memory is base on the RSP register
                    self.buffer.push(0x89);
                    rs.write(0x44, &mut self.buffer);

                    self.buffer.push(0x24);

                    n.write(1, &mut self.buffer);
                }
                _ => panic!("Should not happen"),
            },
        }
    }

    fn write_bytes<T>(&mut self, t: &T) {
        self.buffer.extend(unsafe {
            std::slice::from_raw_parts(t as *const T as *const u8, std::mem::size_of::<T>())
        });
    }

    pub fn stack_pointer(&self) -> Index {
        self.stack
    }

    fn write_binary(&mut self, op: usize, binary: &ConstantBinary, words: &Vector<u8>) {
        let left = binary.get_left();
        let right = binary.get_right();

        self.write_constant(op, left.get(), words);
        self.write_constant(binary.op(), right.get(), words);
    }

    pub fn write_constant(&mut self, op: usize, constant: &Constant, words: &Vector<u8>) {
        match constant {
            Constant::Binary(binary) => self.write_binary(op, &binary, words),
            Constant::Raw(r) => match r.value() {
                ConstantKind::Identifier(_) => {}
                ConstantKind::Number(range) => {
                    let value = words.range(*range);
                    let number = util::parse_string(value);
                    let operation = Operation::new(
                        op,
                        Destination::Register(Register::Rax),
                        Source::Number(Number(number)),
                    );

                    self.stack += constant.get_type().get().get_size();
                    self.code_len += self.operation_size(&operation);
                    self.operations.push(operation);
                }
                _ => {}
            },
            _ => {}
        }
    }

    pub fn bind_constant(&mut self, constant: &Constant, stack_pointer: Index, words: &Vector<u8>) {
        self.write_constant(0, constant, words);
        // self.operations.push(Operation::Mov(
        //     Destination::Memory(Number(0x00)),
        //     Source::Register(Register::Rax),
        // ));
    }

    pub fn write_procedure(&mut self) {
        // self.write_operation(&Operation::Sub(
        //     Destination::Register(Register::Rsp),
        //     Source::Number(Number(self.stack as usize)),
        // ));
        // for i in 0..self.operations.len() {
        //     let op = self.operations.value(i);

        //     self.write_operation(&op);
        // }

        println!("{:?}", self.operations);

        self.operations.clear();
    }

    pub fn generate(&mut self) {
        let start = self.buffer.len();
        let elf_header_size = std::mem::size_of::<ElfHeader>();
        let program_header_size = std::mem::size_of::<ProgramHeader>();
        let code_start_offset = program_header_size * PROGRAM_HEADER_COUNT + elf_header_size;
        let code_virtual_position = code_start_offset + START_ADDRESS;

        let program_headers: &[ProgramHeader; PROGRAM_HEADER_COUNT] = &[ProgramHeader {
            kind: 0x01,
            flags: ProgramFlag::READABLE | ProgramFlag::EXECUTABLE,
            offset: code_start_offset as u64,
            vaddr: code_virtual_position as u64,
            paddr: code_virtual_position as u64,
            file_size: self.code_len as u64,
            mem_size: self.code_len as u64,
            align: 0x1000,
        }];

        let elf_header = ElfHeader {
            ident: [
                0x7F, b'E', b'L', b'F', 0x02, 0x01, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
                0x00, 0x00,
            ],
            kind: 0x02,
            machine: 0x3e,
            version: 0x01,
            entry: code_virtual_position as u64,
            phoff: elf_header_size as u64,
            shoff: 0x00,
            flags: 0x00,
            ehsize: elf_header_size as u16,
            pehntsize: program_header_size as u16,
            phnum: program_headers.len() as u16,
            shentsize: 0x00,
            shnum: 0x00,
            shstrndx: 0x00,
        };

        self.write_bytes(&elf_header);

        for program_header in program_headers {
            self.write_bytes(program_header);
        }

        _ = self
            .file
            .write_all(self.buffer.offset(start as usize))
            .unwrap();
        _ = self
            .file
            .write_all(self.buffer.range(Range::new(0, start)))
            .unwrap();
    }
}
