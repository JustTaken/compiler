use collections::Vector;

pub const BASE_SIZE: usize = 4;
pub struct Immediate(pub usize);

#[repr(usize)]
pub enum Register {
    Rax = 0,
    Rbx,
    Rcx,
    Rdi,
    Rdx,
    Rsi,
    Rsp,
    Rbp,
}

pub struct RegisterManager {
    registers: [Register; 8],
    uses: [bool; 8],
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

pub enum Operation {
    Binary(BinaryOperation, Destination, Source),
    Call(Immediate),
    Ret,
    Syscall,
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

    pub fn clone(&self) -> Immediate {
        Immediate(self.0)
    }
}

impl std::clone::Clone for Destination {
    fn clone(&self) -> Destination {
        match self {
            Destination::Register(r) => Destination::Register(r.clone()),
            Destination::Memory(r, imm) => Destination::Memory(r.clone(), imm.clone()),
            Destination::Push => Destination::Push,
        }
    }
}

impl Operation {
    pub fn write(&self, buffer: &mut Vector<u8>) {
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
                    imm.write(BASE_SIZE, buffer);
                }
                (BinaryOperation::Mov, Destination::Register(rd), Source::Register(rs)) => {
                    buffer.extend(&[0x48, 0x89, 0b11000000 + (rs.value() << 3) + rd.value()]);
                }
                (BinaryOperation::Mov, Destination::Register(rd), Source::Memory(rs, imm)) => {
                    buffer.extend(&[0x8B, 0b01000000 + (rd.value() << 3) + rs.value()]);
                    imm.write(1, buffer);
                }
                (BinaryOperation::Mov, Destination::Register(r), Source::Pop) => {
                    buffer.push(0x58 + r.value());
                }
                (BinaryOperation::Mov, Destination::Memory(rd, imm), Source::Register(rs)) => {
                    buffer.extend(&[0x89, 0b01000000 + (rs.value() << 3) + rd.value()]);
                    imm.write(1, buffer);
                }
                (
                    BinaryOperation::Mov,
                    Destination::Memory(r, m_offset),
                    Source::Immediate(imm),
                ) => {
                    buffer.extend(&[0xC7, 0b01000000 + r.value()]);
                    m_offset.write(1, buffer);
                    imm.write(BASE_SIZE, buffer);
                }
                (BinaryOperation::Mov, Destination::Register(r), Source::Immediate(imm)) => {
                    buffer.push(0xB8 + r.value());
                    imm.write(BASE_SIZE, buffer);
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

                _ => todo!(),
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

impl std::clone::Clone for Source {
    fn clone(&self) -> Source {
        match self {
            Source::Register(r) => Source::Register(r.clone()),
            Source::Memory(r, imm) => Source::Memory(r.clone(), imm.clone()),
            Source::Immediate(imm) => Source::Immediate(imm.clone()),
            Source::Pop => Source::Pop,
        }
    }
}

impl RegisterManager {
    pub fn new() -> RegisterManager {
        RegisterManager {
            registers: [
                Register::Rax,
                Register::Rbx,
                Register::Rcx,
                Register::Rdi,
                Register::Rdx,
                Register::Rsi,
                Register::Rsp,
                Register::Rbp,
            ],
            uses: [false; 8],
        }
    }

    pub fn get(&mut self) -> Register {
        for i in 0..self.registers.len() {
            if !self.uses[i] {
                self.uses[i] = true;

                return self.registers[i].clone();
            }
        }

        panic!("No unused register");
    }

    pub fn unuse(&mut self, register: Register) {
        let index = register as usize;
        self.uses[index] = false;
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
                write!(f, "Destination::Memory({:?}, Immediate({:#010x}))", r, i)
            }
        }
    }
}

impl std::fmt::Debug for Source {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Source::Register(r) => write!(f, "Source::Register({:?})", r),
            Source::Immediate(Immediate(n)) => write!(f, "Source::Immediate({:#010x})", n),
            Source::Memory(r, Immediate(n)) => {
                write!(f, "Source::Memory({:?}, Immediate({:#010x}))", r, n)
            }
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
