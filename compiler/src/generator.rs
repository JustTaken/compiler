use crate::checker::{Constant, ConstantBinary};
use collections::{Buffer, Vector};
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

pub struct Immediate(pub usize);

pub enum Register {
    Rax,
    Rbx,
    Rcx,
    Rdi,
    Rdx,
    Rsi,
    Rsp,
    Rbp,
}

pub enum Source {
    Register(Register),
    Memory(Register, Immediate),
    Immediate(Immediate),
    Pop,
}

pub enum Destination {
    Register(Register),
    Memory(Register, Immediate),
    Push,
}

pub enum BinaryOperation {
    Mov,
    Add,
    Sub,
    Mul,
}

enum Operation {
    Binary(BinaryOperation, Destination, Source),
    Call(Immediate),
    Ret,
    Syscall,
}

pub struct Generator {
    buffer: Vector<u8>,
    operations: Vector<Operation>,
    code_len: Index,
    stack: Index,
    arena: Arena,
    file: std::fs::File,
}

impl Immediate {
    fn write(&self, bytes: usize, buffer: &mut Vector<u8>) {
        for i in 0..bytes {
            buffer.push((self.0 >> (8 * i)) as u8);
        }
    }

    fn value(&self) -> usize {
        self.0
    }

    fn clone(&self) -> Immediate {
        Immediate(self.0)
    }
}

impl Destination {
    fn clone(&self) -> Destination {
        match self {
            Destination::Register(r) => Destination::Register(r.clone()),
            Destination::Memory(r, imm) => Destination::Memory(r.clone(), imm.clone()),
            Destination::Push => Destination::Push,
        }
    }
}

impl Operation {
    fn write(&self, buffer: &mut Vector<u8>) {
        println!("{:?}", self);

        match self {
            Operation::Syscall => buffer.extend(&[0x0F, 0x05]),
            Operation::Ret => buffer.push(0xC3),
            Operation::Call(i) => {
                let offset =
                    Immediate((i.value() as isize - buffer.len() as isize) as usize - 0x05);
                buffer.push(0xE8);
                offset.write(4, buffer);
            }
            Operation::Binary(binary, destin, source) => match (binary, destin, source) {
                (BinaryOperation::Mov, Destination::Push, Source::Register(r)) => {
                    buffer.push(0x50 + r.value());
                }
                (BinaryOperation::Mov, Destination::Push, Source::Memory(r, imm)) => {
                    buffer.extend(&[0xFF, 0b01110000 + r.value()]);
                    imm.write(1, buffer);
                }
                (BinaryOperation::Mov, Destination::Push, Source::Immediate(imm)) => {
                    buffer.push(0x68);
                    imm.write(4, buffer);
                }
                (BinaryOperation::Mov, Destination::Register(rd), Source::Register(rs)) => {
                    buffer.extend(&[0x48, 0x89, 0b11000000 + (rs.value() << 3) + rd.value()]);
                }
                (BinaryOperation::Mov, Destination::Register(rd), Source::Memory(rs, imm)) => {
                    buffer.extend(&[0x8B, 0b01000000 + (rd.value() << 3) + rs.value()]);
                    imm.write(1, buffer);
                }
                (BinaryOperation::Mov, Destination::Memory(rd, imm), Source::Register(rs)) => {
                    buffer.extend(&[0x89, 0b01000000 + (rs.value() << 3) + rd.value()]);
                    imm.write(1, buffer);
                }
                (BinaryOperation::Mov, Destination::Register(r), Source::Immediate(imm)) => {
                    buffer.push(0xB8 + r.value());
                    imm.write(4, buffer);
                }
                (BinaryOperation::Add, Destination::Register(rd), Source::Register(rs)) => {
                    buffer.extend(&[0x01, 0b11000000 + (rs.value() << 3) + rd.value()])
                }
                (BinaryOperation::Add, Destination::Memory(rd, imm), Source::Register(rs)) => {
                    buffer.extend(&[0x01, 0b01000000 + (rs.value() << 3) + rd.value()]);
                    imm.write(1, buffer);
                }
                (BinaryOperation::Add, Destination::Register(r), Source::Immediate(imm)) => {
                    buffer.extend(&[0x83, 0b11000000 + r.value()]);
                    imm.write(1, buffer);
                }
                (BinaryOperation::Sub, Destination::Register(rd), Source::Register(rs)) => {
                    buffer.extend(&[0x29, 0b11000000 + (rs.value() << 3) + rd.value()])
                }
                (BinaryOperation::Sub, Destination::Memory(rd, imm), Source::Register(rs)) => {
                    buffer.extend(&[0x29, 0b01000000 + (rs.value() << 3) + rd.value()]);
                    imm.write(1, buffer);
                }
                (BinaryOperation::Sub, Destination::Register(r), Source::Immediate(imm)) => {
                    buffer.extend(&[0x48, 0x83, 0b11101000 + r.value()]);
                    imm.write(1, buffer);
                }

                _ => {}
            },
        }
    }
}

impl Register {
    fn value(&self) -> u8 {
        match self {
            Register::Rax => 0x00,
            Register::Rbx => 0x03,
            Register::Rcx => 0x01,
            Register::Rdi => 0x07,
            Register::Rdx => 0x02,
            Register::Rsi => 0x06,
            Register::Rsp => 0x04,
            Register::Rbp => 0x05,
        }
    }

    pub fn clone(&self) -> Register {
        match self {
            Register::Rax => Register::Rax,
            Register::Rbx => Register::Rbx,
            Register::Rcx => Register::Rcx,
            Register::Rdi => Register::Rdi,
            Register::Rdx => Register::Rdx,
            Register::Rsi => Register::Rsi,
            Register::Rsp => Register::Rsp,
            Register::Rbp => Register::Rbp,
        }
    }
}

impl Source {
    pub fn clone(&self) -> Source {
        match self {
            Source::Register(r) => Source::Register(r.clone()),
            Source::Memory(r, imm) => Source::Memory(r.clone(), imm.clone()),
            Source::Immediate(imm) => Source::Immediate(imm.clone()),
            Source::Pop => Source::Pop,
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
            Register::Rbp => write!(f, "Regsiter(Rbp)"),
        }
    }
}

impl std::fmt::Debug for Destination {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Destination::Register(r) => write!(f, "Destination::Register({:?})", r),
            Destination::Push => write!(f, "Destination::Push"),
            Destination::Memory(r, Immediate(i)) => {
                write!(f, "Destination::Memory({:?}, {:#010x})", r, i)
            }
        }
    }
}

impl std::fmt::Debug for Source {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Source::Register(r) => write!(f, "Source::Register({:?})", r),
            Source::Immediate(Immediate(n)) => write!(f, "Source::Immediate({:#010x})", n),
            Source::Memory(r, Immediate(n)) => write!(f, "Source::Memory({:?}, {:#010x})", r, n),
            Source::Pop => write!(f, "Source::Pop"),
        }
    }
}

impl std::fmt::Debug for Operation {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Operation::Syscall => write!(f, "Syscall"),
            Operation::Ret => write!(f, "Ret"),
            Operation::Call(Immediate(i)) => write!(f, "Call({:#010x})", i),
            Operation::Binary(binary, destination, source) => match binary {
                BinaryOperation::Mov => write!(f, "Mov({:?}, {:?})", destination, source),
                BinaryOperation::Add => write!(f, "Add({:?}, {:?})", destination, source),
                BinaryOperation::Sub => write!(f, "Sub({:?}, {:?})", destination, source),
                BinaryOperation::Mul => write!(f, "Mul({:?}, {:?})", destination, source),
            },
        }
    }
}

mod program_flag {
    pub const EXECUTABLE: u32 = 0x1;
    pub const WRITEABLE: u32 = 0x2;
    pub const READABLE: u32 = 0x3;
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

    fn write_bytes<T>(&mut self, t: &T) {
        self.buffer.extend(unsafe {
            std::slice::from_raw_parts(t as *const T as *const u8, std::mem::size_of::<T>())
        });
    }

    fn write_binary(
        &mut self,
        op: BinaryOperation,
        binary: &ConstantBinary,
        destination: &Destination,
        words: &Vector<u8>,
    ) {
        let left = binary.get_left();
        let right = binary.get_right();

        self.write_constant(op, &destination, left.get(), words);
        self.write_constant(binary.op(), &destination, right.get(), words);
    }

    fn write_constant(
        &mut self,
        op: BinaryOperation,
        destination: &Destination,
        constant: &Constant,
        words: &Vector<u8>,
    ) {
        match constant {
            Constant::Binary(binary) => self.write_binary(op, &binary, destination, words),
            Constant::Raw(_) => self.operations.push(Operation::Binary(
                op,
                destination.clone(),
                constant.get_value(),
            )),
            _ => {}
        }
    }

    pub fn bind_constant(&mut self, constant: &Constant, words: &Vector<u8>) -> usize {
        self.stack += constant.get_type().get().get_size();
        self.write_constant(
            BinaryOperation::Mov,
            &Destination::Register(Register::Rbx),
            constant,
            words,
        );

        let offset = -(self.stack as isize) as usize;

        self.operations.push(Operation::Binary(
            BinaryOperation::Mov,
            Destination::Memory(Register::Rbp, Immediate(offset)),
            Source::Register(Register::Rbx),
        ));

        offset
    }

    pub fn write_call(
        &mut self,
        args: Buffer<Constant>,
        len: Index,
        offset: Index,
        words: &Vector<u8>,
    ) {
        let arguments = args.slice(0, len as usize);

        for arg in arguments {
            if let Constant::Raw(_) = arg {
                self.write_constant(BinaryOperation::Mov, &Destination::Push, arg, words);
            } else {
                self.write_constant(
                    BinaryOperation::Mov,
                    &Destination::Register(Register::Rbx),
                    arg,
                    words,
                );

                self.operations.push(Operation::Binary(
                    BinaryOperation::Mov,
                    Destination::Push,
                    Source::Register(Register::Rbx),
                ));
            }
        }

        self.operations
            .push(Operation::Call(Immediate(offset as usize)));
    }

    pub fn write_procedure(&mut self, constant: Option<&Constant>) -> Index {
        let non_zero_stack = self.stack != 0;

        if non_zero_stack {
            let procedure_stack_init = &[
                Operation::Binary(
                    BinaryOperation::Mov,
                    Destination::Register(Register::Rbp),
                    Source::Register(Register::Rsp),
                ),
                Operation::Binary(
                    BinaryOperation::Sub,
                    Destination::Register(Register::Rsp),
                    Source::Immediate(Immediate(self.stack as usize)),
                ),
            ];

            for op in procedure_stack_init {
                op.write(&mut self.buffer);
            }
        }

        if let Some(c) = constant {
            let source = c.get_value();

            if let Source::Register(Register::Rax) = source {
            } else {
                self.operations.push(Operation::Binary(
                    BinaryOperation::Mov,
                    Destination::Register(Register::Rax),
                    source,
                ))
            }
        }

        if non_zero_stack {
            self.operations.push(Operation::Binary(
                BinaryOperation::Mov,
                Destination::Register(Register::Rsp),
                Source::Register(Register::Rbp),
            ));
        }

        self.operations.push(Operation::Ret);

        for op in self.operations.offset(0) {
            op.write(&mut self.buffer);
        }

        let code_len = self.code_len;

        self.code_len = self.buffer.len() as Index;
        self.stack = 0;
        self.operations.clear();

        code_len
    }

    pub fn generate(&mut self, main_procedure_offset: Index) {
        let instructions_end = self.buffer.len();
        let program_end = &[
            Operation::Call(Immediate(main_procedure_offset as usize)),
            Operation::Binary(
                BinaryOperation::Mov,
                Destination::Register(Register::Rdi),
                Source::Register(Register::Rax),
            ),
            Operation::Binary(
                BinaryOperation::Mov,
                Destination::Register(Register::Rax),
                Source::Immediate(Immediate(0x3C)),
            ),
            Operation::Syscall,
        ];

        for op in program_end {
            op.write(&mut self.buffer);
        }

        let code_len = self.buffer.len();
        let elf_header_size = std::mem::size_of::<ElfHeader>();
        let program_header_size = std::mem::size_of::<ProgramHeader>();
        let code_start_offset = program_header_size * PROGRAM_HEADER_COUNT + elf_header_size;
        let code_virtual_position = code_start_offset + START_ADDRESS;

        let program_headers: &[ProgramHeader; PROGRAM_HEADER_COUNT] = &[ProgramHeader {
            kind: 0x01,
            flags: program_flag::READABLE | program_flag::EXECUTABLE,
            offset: code_start_offset as u64,
            vaddr: code_virtual_position as u64,
            paddr: code_virtual_position as u64,
            file_size: code_len as u64,
            mem_size: code_len as u64,
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
            entry: code_virtual_position as u64 + instructions_end as u64,
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
            .write_all(self.buffer.offset(code_len as usize))
            .unwrap();
        _ = self
            .file
            .write_all(self.buffer.range(Range::new(0, code_len)))
            .unwrap();
    }
}
