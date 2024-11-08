use crate::checker::{Constant, ConstantBinary};
use crate::elf::{self, ElfHeader, ProgramHeader};
use crate::x86_64::{
    BinaryOperation, Destination, Immediate, Operation, Register, RegisterManager, Source,
};
use collections::Vector;
use mem::{Arena, Container};
use std::io::Write;
use util::{Index, Range};

pub struct Generator {
    buffer: Vector<u8>,
    operations: Vector<Operation>,
    code_len: Index,
    stack: Index,
    register_manager: RegisterManager,
    _arena: Arena,
    path: String,
}

impl Generator {
    pub fn new(path: String, arena: &mut Arena) -> Generator {
        let mut self_arena = Arena::new(arena.bytes(4096));

        Generator {
            buffer: Vector::new(1024, &mut self_arena),
            operations: Vector::new(50, &mut self_arena),
            code_len: 0,
            stack: 0,
            register_manager: RegisterManager::new(),
            _arena: self_arena,
            path,
        }
    }

    fn write_bytes<T>(&mut self, t: &T) {
        self.buffer.extend(unsafe {
            std::slice::from_raw_parts(t as *const T as *const u8, std::mem::size_of::<T>())
        });
    }

    fn evaluate_binary(
        &mut self,
        op: BinaryOperation,
        binary: &ConstantBinary,
        destination: &Destination,
    ) {
        let left = binary.get_left();
        let right = binary.get_right();

        self.evaluate_constant(op, left.get(), &destination);
        self.evaluate_constant(binary.op(), right.get(), &destination);
    }

    fn evaluate_constant(
        &mut self,
        op: BinaryOperation,
        constant: &Constant,
        destination: &Destination,
    ) {
        match constant {
            Constant::Binary(binary) => self.evaluate_binary(op, binary.get(), destination),
            Constant::Ref(c) => self.evaluate_constant(op, c.get(), destination),
            Constant::Call(call) => {
                let len = call.get().len();
                let mut size = 0;

                for i in 0..len {
                    let arg = call.get().arg(i);
                    size += arg.get_type().get().get_size();

                    self.evaluate_expression(arg);

                    self.operations.push(Operation::Binary(
                        BinaryOperation::Mov,
                        Destination::Push,
                        arg.get_source().unwrap(),
                    ));
                }

                self.operations
                    .push(Operation::Call(Immediate(call.get().offset())));

                self.operations.push(Operation::Binary(
                    BinaryOperation::Add,
                    Destination::Register(Register::Rbx),
                    Source::Register(Register::Rbp),
                ));
            }
            Constant::Raw(raw) => self.operations.push(Operation::Binary(
                op,
                destination.clone(),
                raw.get().source(),
            )),
            _ => {}
        }
    }

    fn evaluate_expression(&mut self, constant: &mut Constant) {
        if let Some(_) = constant.get_source() {
        } else {
            let size = constant.get_type().get().get_size();

            let (source, destination) = if size > 8 {
                let register = Register::Rbp;
                let offset = Immediate(self.pos());

                self.stack += size;

                (
                    Source::Memory(register.clone(), offset.clone()),
                    Destination::Memory(register, offset),
                )
            } else {
                let register = self.register_manager.get();
                (
                    Source::Register(register.clone()),
                    Destination::Register(register),
                )
            };

            constant.set_source(source);

            self.evaluate_constant(BinaryOperation::Mov, constant, &destination);
        }
    }

    pub fn write_procedure(&mut self, constant: Container<Constant>) -> Index {
        let non_zero_stack = self.stack != 0;
        Operation::Binary(
            BinaryOperation::Mov,
            Destination::Register(Register::Rbp),
            Source::Register(Register::Rsp),
        )
        .write(&mut self.buffer);

        if non_zero_stack {
            Operation::Binary(
                BinaryOperation::Sub,
                Destination::Register(Register::Rsp),
                Source::Immediate(Immediate(self.stack as usize)),
            )
            .write(&mut self.buffer);
        }

        if constant.is_some() {
            self.evaluate_expression(constant.get());
            let source = constant.get().get_source().unwrap();

            if let Source::Register(r) = source.clone() {
                if let Register::Rax = r {
                } else {
                    self.operations.push(Operation::Binary(
                        BinaryOperation::Mov,
                        Destination::Register(Register::Rax),
                        source.clone(),
                    ));
                }

                self.register_manager.unuse(r);
            } else {
                self.operations.push(Operation::Binary(
                    BinaryOperation::Mov,
                    Destination::Register(Register::Rax),
                    source.clone(),
                ));
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

        self.operations.clear();
        self.code_len = self.buffer.len() as Index;
        self.stack = 0;

        code_len
    }

    pub fn pos(&self) -> usize {
        -(self.stack as isize) as usize
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
        let code_start_offset = program_header_size * elf::PROGRAM_HEADER_COUNT + elf_header_size;
        let code_virtual_position = code_start_offset + elf::START_ADDRESS;

        let program_headers: &[ProgramHeader; elf::PROGRAM_HEADER_COUNT] = &[ProgramHeader {
            kind: 0x01,
            flags: elf::READABLE | elf::EXECUTABLE,
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

        let mut file = std::fs::File::create(self.path.as_str()).unwrap();

        _ = file
            .write_all(self.buffer.offset(code_len as usize))
            .unwrap();
        _ = file
            .write_all(self.buffer.range(Range::new(0, code_len)))
            .unwrap();
    }
}
