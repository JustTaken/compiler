use crate::generator::{BinaryOperation, Generator, Immediate, Register, Source};
use collections::{Buffer, RangeMap, Vector};
use mem::{Arena, Container};
use util::{Index, Range};

pub enum ConstantKind {
    Number(usize),
    Identifier(Source),
    Boolean(bool),
}

pub struct ConstantRaw {
    kind: ConstantKind,
    inner: Container<Type>,
}

#[derive(Clone)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mul,
    Div,
    Gt,
    Lt,
    Eq,
}

#[derive(Clone)]
pub enum UnaryOperator {
    Negation,
    Oposite,
}

pub struct ConstantBinary {
    left: Container<Constant>,
    right: Container<Constant>,
    inner: Container<Type>,
    operator: BinaryOperator,
}

pub struct ConstantUnary {
    constant: Container<Constant>,
    operator: UnaryOperator,
}

struct Of {
    matcher: ConstantRaw,
    expression: Constant,
}

pub struct ConstantCase {
    inner: Container<Type>,
    ofs: Buffer<Of>,
    len: Index,
    expression: Container<Constant>,
}

pub struct Scope {
    parent: Container<Scope>,
    constant: Option<Constant>,
    constant_count: Index,
    signature: Index,
    variables: Buffer<Container<Range>>,
    variable_count: Index,
    inner: Container<Type>,
}

pub enum Constant {
    Raw(ConstantRaw),
    Binary(ConstantBinary),
    Case(ConstantCase),
    Unary(ConstantUnary),
}

pub enum Statement {
    Let(Index, Constant),
    Expression(Constant),
}

struct Variable {
    signature: Index,
    constant: Constant,
}

struct Field {
    name: Range,
    inner: Container<Type>,
}

pub struct Type {
    fields: Buffer<Field>,
    len: Index,
    size: Index,
    align: Index,
}

pub struct Procedure {
    parameters: Buffer<Container<Type>>,
    len: Index,
    offset: Index,
    inner: Container<Type>,
}

pub struct TypeChecker {
    constants: Vector<Constant>,
    ranges: Vector<Range>,

    types: RangeMap<Type>,
    fields: Vector<Field>,
    construct_fields: RangeMap<Constant>,

    procedures: RangeMap<Procedure>,

    ofs: Vector<Of>,
    parameters: Vector<Container<Type>>,
    arguments: Vector<Constant>,

    variables: RangeMap<Variable>,
    variable_ranges: Vector<Container<Range>>,

    generator: Generator,
    last_scope: Container<Scope>,
    scope_count: Index,
    parameters_size: Index,

    arena: Arena,
}

impl BinaryOperator {
    fn is_comparison(&self) -> bool {
        match self {
            BinaryOperator::Lt | BinaryOperator::Gt | BinaryOperator::Eq => true,
            _ => false,
        }
    }

    fn to_operation(&self) -> BinaryOperation {
        match self {
            BinaryOperator::Add => BinaryOperation::Add,
            BinaryOperator::Sub => BinaryOperation::Sub,
            BinaryOperator::Mul => BinaryOperation::Mul,
            _ => todo!(),
        }
    }
}

impl PartialEq for ConstantKind {
    fn eq(&self, other: &ConstantKind) -> bool {
        match (self, other) {
            (ConstantKind::Identifier(_), ConstantKind::Identifier(_)) => true,
            (ConstantKind::Number(_), ConstantKind::Number(_)) => true,
            (ConstantKind::Boolean(a), ConstantKind::Boolean(b)) => a == b,
            _ => false,
        }
    }
}

impl ConstantBinary {
    fn set_type(&mut self, inner: Container<Type>) -> bool {
        if self.inner.is_some() {
            self.inner.eql(&inner)
        } else {
            self.inner = Container::new(inner.pointer());
            self.left.get().set_type(Container::new(inner.pointer()))
                && self.right.get().set_type(inner)
        }
    }

    fn clone(&self) -> ConstantBinary {
        ConstantBinary {
            left: Container::new(self.left.pointer()),
            right: Container::new(self.right.pointer()),
            operator: self.operator.clone(),
            inner: Container::new(self.inner.pointer()),
        }
    }

    pub fn get_left(&self) -> &Container<Constant> {
        &self.left
    }

    pub fn get_right(&self) -> &Container<Constant> {
        &self.right
    }

    pub fn op(&self) -> BinaryOperation {
        self.operator.to_operation()
    }
}

impl ConstantRaw {
    pub fn value(&self) -> &ConstantKind {
        &self.kind
    }

    fn clone(&self) -> ConstantRaw {
        let kind = match &self.kind {
            ConstantKind::Number(n) => ConstantKind::Number(*n),
            ConstantKind::Identifier(src) => ConstantKind::Identifier(src.clone()),
            ConstantKind::Boolean(b) => ConstantKind::Boolean(*b),
        };

        ConstantRaw {
            kind,
            inner: Container::new(self.inner.pointer()),
        }
    }
}

impl ConstantCase {
    fn set_type(&mut self, inner: Container<Type>) -> bool {
        for exp in self.ofs.slice_mut(0, self.len as usize) {
            if !exp.expression.set_type(Container::new(inner.pointer())) {
                return false;
            }
        }

        true
    }

    fn clone(&self) -> ConstantCase {
        ConstantCase {
            inner: Container::new(self.inner.pointer()),
            ofs: Buffer::new(self.ofs.pointer()),
            len: self.len,
            expression: Container::new(self.expression.pointer()),
        }
    }
}

impl ConstantUnary {
    fn clone(&self) -> ConstantUnary {
        ConstantUnary {
            operator: self.operator.clone(),
            constant: Container::new(self.constant.pointer()),
        }
    }
}

impl Type {
    pub fn get_size(&self) -> Index {
        self.size
    }

    fn offset(&self, range: Range, words: &Vector<u8>) -> (Container<Type>, usize) {
        let mut o: usize = 0;
        let fields = self.fields.slice(0, self.len as usize);

        for field in fields {
            o += field.inner.get().size as usize;

            if util::compare(words.range(field.name), words.range(range)) {
                return (Container::new(field.inner.pointer()), o);
            }
        }

        panic!("Should not happen");
    }
}

impl Constant {
    fn set_type(&mut self, inner: Container<Type>) -> bool {
        match self {
            Constant::Binary(ref mut binary) => binary.set_type(inner),
            Constant::Case(case) => case.set_type(inner),
            Constant::Unary(unary) => unary.constant.get().set_type(inner),
            Constant::Raw(ref mut raw) => {
                if raw.inner.is_some() {
                    raw.inner.eql(&inner)
                } else {
                    raw.inner = Container::new(inner.pointer());
                    true
                }
            }
        }
    }

    pub fn get_type(&self) -> Container<Type> {
        match self {
            Constant::Raw(raw) => Container::new(raw.inner.pointer()),
            Constant::Case(case) => Container::new(case.inner.pointer()),
            Constant::Binary(binary) => Container::new(binary.inner.pointer()),
            Constant::Unary(unary) => unary.constant.get().get_type(),
        }
    }

    pub fn get_value(&self) -> Source {
        match self {
            Constant::Case(_) => todo!(),
            Constant::Binary(_) => Source::Register(Register::Rbx),
            Constant::Unary(_) => Source::Register(Register::Rbx),
            Constant::Raw(raw) => match &raw.kind {
                ConstantKind::Identifier(source) => source.clone(),
                ConstantKind::Number(value) => Source::Immediate(Immediate(*value as usize)),
                _ => panic!("Should not happen"),
            },
        }
    }

    fn clone(&self) -> Constant {
        match self {
            Constant::Raw(raw) => Constant::Raw(raw.clone()),
            Constant::Binary(binary) => Constant::Binary(binary.clone()),
            Constant::Case(case) => Constant::Case(case.clone()),
            Constant::Unary(unary) => Constant::Unary(unary.clone()),
        }
    }

    fn deinit(&self, arena: &mut Arena) {
        match self {
            Constant::Raw(_) => {}
            Constant::Binary(binary) => {
                binary.left.get().deinit(arena);
                binary.right.get().deinit(arena);
                arena.dealloc::<Constant>(2);
            }
            Constant::Unary(unary) => {
                unary.constant.get().deinit(arena);
                arena.dealloc::<Constant>(1);
            }
            Constant::Case(case) => {
                for of in case.ofs.slice(0, case.len as usize) {
                    of.expression.deinit(arena);
                }

                case.expression.get().deinit(arena);
            }
        }
    }
}

impl TypeChecker {
    pub fn new(path: &str, arena: &mut Arena) -> TypeChecker {
        let mut self_arena = Arena::new(arena.bytes(8196));

        TypeChecker {
            ranges: Vector::new(10, &mut self_arena),
            constants: Vector::new(10, &mut self_arena),

            types: RangeMap::new(10, &mut self_arena),
            fields: Vector::new(10, &mut self_arena),
            construct_fields: RangeMap::new(10, &mut self_arena),

            procedures: RangeMap::new(10, &mut self_arena),

            variables: RangeMap::new(10, &mut self_arena),
            variable_ranges: Vector::new(10, &mut self_arena),

            parameters: Vector::new(10, &mut self_arena),
            arguments: Vector::new(10, &mut self_arena),
            ofs: Vector::new(10, &mut self_arena),

            generator: Generator::new(path, &mut self_arena),
            last_scope: Container::null(),
            scope_count: 0,
            parameters_size: 0,

            arena: self_arena,
        }
    }

    fn push_constant(&mut self, constant: Constant) {
        self.last_scope.get().constant_count += 1;
        self.constants.push(constant);
    }

    fn get_type(&mut self, range: Range, words: &Vector<u8>) -> Container<Type> {
        if let Some(t) = self.types.addr_of(words.range(range), words) {
            t
        } else {
            panic!("Should not happen");
        }
    }

    fn get_type_from_str(&mut self, type_name: &[u8], words: &Vector<u8>) -> Container<Type> {
        if let Some(t) = self.types.addr_of(type_name, words) {
            t
        } else {
            panic!("Should not happen");
        }
    }

    fn register_variable(&mut self, name_range: Range, constant: Constant, words: &Vector<u8>) {
        let index = self.variables.put(
            name_range,
            Variable {
                signature: self.last_scope.get().signature,
                constant,
            },
            words,
        );

        self.variable_ranges.push(self.variables.key_addr_at(index));
        self.last_scope.get().variable_count += 1;
    }

    fn pop_constant(&mut self) -> Constant {
        self.last_scope.get().constant_count -= 1;
        self.constants.pop()
    }

    pub fn push_identifier(&mut self, range: Range, words: &Vector<u8>) {
        if let Some(variable) = self.variables.get(range, words) {
            let mut s = &self.last_scope;

            while s.is_some() {
                if s.get().signature == variable.signature {
                    self.push_constant(variable.constant.clone());

                    return;
                }

                s = &s.get().parent;
            }
        }

        println!(
            "varaible: {}",
            std::str::from_utf8(words.range(range)).unwrap()
        );

        panic!("undeclared variable");
    }

    pub fn push_boolean(&mut self, b: bool, words: &Vector<u8>) {
        let inner = self.get_type_from_str(b"bool", words);

        self.push_constant(Constant::Raw(ConstantRaw {
            kind: ConstantKind::Boolean(b),
            inner,
        }));
    }

    pub fn push_number(&mut self, range: Range, words: &Vector<u8>) {
        let string = words.range(range);
        let number = util::parse_string(string);
        self.push_constant(Constant::Raw(ConstantRaw {
            kind: ConstantKind::Number(number),
            inner: Container::null(),
        }));
    }

    pub fn push_parameter(&mut self, words: &Vector<u8>) {
        let type_range = self.ranges.pop();
        let name_range = self.ranges.pop();
        let inner = self.get_type(type_range, words);
        let size = inner.get().get_size();

        self.parameters.push(Container::new(inner.pointer()));
        self.register_variable(
            name_range,
            Constant::Raw(ConstantRaw {
                kind: ConstantKind::Identifier(Source::Memory(
                    Register::Rbp,
                    Immediate(self.parameters_size as usize),
                )),
                inner,
            }),
            words,
        );

        self.parameters_size += size;
    }

    pub fn push_procedure(&mut self, parameter_count: usize, words: &Vector<u8>) {
        let type_range = self.ranges.pop();
        let name_range = self.ranges.pop();
        let inner = self.get_type(type_range, words);

        let constant = if self.constants.len() > 0 {
            let mut c = self.pop_constant();
            if !c.set_type(Container::new(inner.pointer())) {
                panic!("Could not set the scope last expression type");
            }

            Some(c)
        } else {
            None
        };

        let pointer = self
            .parameters
            .pointer(self.parameters.len() - parameter_count as u32);

        self.procedures.push(
            name_range,
            Procedure {
                inner,
                len: parameter_count as Index,
                parameters: Buffer::new(pointer),
                offset: self.generator.write_procedure(constant.as_ref()),
            },
            words,
        );

        if let Some(c) = constant {
            c.deinit(&mut self.arena);
        }

        self.parameters_size = 0;
    }

    pub fn push_case(&mut self, count: usize) {
        let buffer = self.ofs.pointer(self.ofs.len());
        let mut inner: Container<Type> = Container::null();
        let mut matcher_inner: Container<Type> = Container::null();

        for _ in 0..count {
            let expression = self.pop_constant();
            let Constant::Raw(mut matcher) = self.pop_constant() else {
                panic!("should not happen");
            };

            let expression_inner = expression.get_type();

            if !inner.eql(&expression_inner) {
                if !inner.is_some() {
                    inner = expression_inner;
                } else {
                    panic!("expression type must match the case type");
                }
            }

            if !matcher_inner.eql(&matcher.inner) {
                if !matcher_inner.is_some() {
                    matcher_inner = Container::new(matcher.inner.pointer());
                } else if !matcher.inner.is_some() {
                    matcher.inner = Container::new(matcher_inner.pointer());
                } else {
                    panic!("Could not set the matcher type");
                }
            }

            self.ofs.push(Of {
                expression,
                matcher,
            });
        }

        let mut constant = self.pop_constant();

        if !constant.set_type(matcher_inner) {
            panic!("Could not set the match on expression type");
        }

        let expression = self.arena.create(constant);

        self.push_constant(Constant::Case(ConstantCase {
            ofs: Buffer::new(buffer),
            len: count as Index,
            expression,
            inner,
        }));

        self.ofs.clear();
    }

    pub fn push_let(&mut self, words: &Vector<u8>) {
        let type_range = self.ranges.pop();
        let name_range = self.ranges.pop();
        let inner = self.get_type(type_range, words);

        let mut constant = self.pop_constant();

        if !constant.set_type(Container::new(inner.pointer())) {
            panic!("Could not set type of let declaration");
        }

        if let Constant::Raw(_) = constant {
            self.register_variable(name_range, constant, words);
        } else {
            let pointer = self.generator.bind_constant(&constant);
            let constant = Constant::Raw(ConstantRaw {
                kind: ConstantKind::Identifier(Source::Memory(Register::Rbp, Immediate(pointer))),
                inner,
            });

            self.register_variable(name_range, constant, words);
        };

        self.arguments.clear();
    }

    pub fn push_call(&mut self, len: usize, words: &Vector<u8>) {
        let name_range = self.ranges.pop();
        let Some(procedure) = self.procedures.addr_of(words.range(name_range), words) else {
            panic!("Should not happen");
        };

        if len != procedure.get().len as usize {
            panic!("Passing the wrog number of arguments");
        }

        for i in 0..len {
            let mut constant = self.pop_constant();
            let parameter_inner = procedure.get().parameters.get(len - i - 1);

            if !constant.set_type(Container::new(parameter_inner.pointer())) {
                panic!("Could not set the argument type");
            }

            self.arguments.push(constant);
        }

        self.generator.write_call(
            Buffer::new(self.arguments.pointer(0)),
            procedure.get().len,
            procedure.get().offset,
        );

        self.push_constant(Constant::Raw(ConstantRaw {
            kind: ConstantKind::Identifier(Source::Register(Register::Rax)),
            inner: Container::new(procedure.get().inner.pointer()),
        }));

        self.arguments.clear();
    }

    pub fn push_type(&mut self, field_count: usize, annotated_size: usize, words: &Vector<u8>) {
        let name_range = self.ranges.pop();
        let start = self.fields.len();

        let mut size: Index = annotated_size as Index;
        let mut align: Index = annotated_size as Index;

        for _ in 0..field_count {
            let field_type_range = self.ranges.pop();
            let field_name_range = self.ranges.pop();
            let inner = self.get_type(field_type_range, words);

            size += inner.get().size;
            align = util::max(align as usize, inner.get().align as usize) as Index;

            self.fields.push(Field {
                name: field_name_range,
                inner,
            });
        }

        self.types.push(
            name_range,
            Type {
                fields: Buffer::new(self.fields.pointer(start)),
                len: field_count as Index,
                size,
                align,
            },
            words,
        );
    }

    pub fn push_binary(&mut self, operator: BinaryOperator, words: &Vector<u8>) {
        let mut first = self.pop_constant();
        let mut second = self.pop_constant();

        let first_inner = first.get_type();
        let second_inner = second.get_type();

        let inner = if operator.is_comparison() {
            self.get_type_from_str(b"bool", words)
        } else {
            if first_inner.is_some() {
                if !second.set_type(Container::new(first_inner.pointer())) {
                    panic!("Could not set variable type");
                } else {
                    first_inner
                }
            } else if second_inner.is_some() {
                if !first.set_type(Container::new(second_inner.pointer())) {
                    panic!("Could not set variable type");
                } else {
                    second_inner
                }
            } else {
                Container::null()
            }
        };

        let left = self.arena.create(first);
        let right = self.arena.create(second);

        self.push_constant(Constant::Binary(ConstantBinary {
            left,
            right,
            inner,
            operator,
        }));
    }

    pub fn push_construct(&mut self, len: usize, words: &Vector<u8>) {
        let name_range = self.ranges.pop();
        let typ = self.get_type(name_range, words);

        if len != typ.get().len as usize {
            panic!("Passing the wrogn number of arguments");
        }

        for _ in 0..len {
            let constant = self.pop_constant();
            let name = self.ranges.pop();

            self.construct_fields.push(name, constant, words);
        }

        let offset = self.generator.pos();

        for field in typ.get().fields.slice(0, typ.get().len as usize) {
            if let Some(i) = self.construct_fields.index(words.range(field.name), words) {
                let constant = self.construct_fields.value_addr_at(i);

                constant
                    .get()
                    .set_type(Container::new(field.inner.pointer()));

                self.generator.bind_constant(constant.get());
                self.construct_fields.key_addr_at(i).get().reset();
            } else {
                panic!("Could not find field");
            }
        }

        self.push_constant(Constant::Raw(ConstantRaw {
            kind: ConstantKind::Identifier(Source::Memory(Register::Rbp, Immediate(offset))),
            inner: Container::new(typ.pointer()),
        }));
    }

    pub fn push_dot(&mut self, words: &Vector<u8>) {
        let name_range = self.ranges.pop();
        let constant = self.pop_constant();

        let Constant::Raw(raw) = constant else {
            panic!("Should not happen");
        };

        let ConstantKind::Identifier(Source::Memory(Register::Rbp, Immediate(offset))) = raw.kind
        else {
            panic!("Should not happen");
        };

        let (inner, relative_position) = raw.inner.get().offset(name_range, words);

        self.push_constant(Constant::Raw(ConstantRaw {
            kind: ConstantKind::Identifier(Source::Memory(
                Register::Rbp,
                Immediate((offset as isize - relative_position as isize) as usize),
            )),
            inner,
        }))
    }

    pub fn push_unary(&mut self, operator: UnaryOperator) {
        let constant = self.arena.create(self.constants.pop());
        self.constants
            .push(Constant::Unary(ConstantUnary { operator, constant }))
    }

    pub fn push_range(&mut self, range: Range) {
        self.ranges.push(range);
    }

    pub fn start_scope(&mut self) {
        self.last_scope = self.arena.create(Scope {
            parent: Container::new(self.last_scope.pointer()),
            signature: self.scope_count,
            constant: None,
            constant_count: 0,
            variables: Buffer::new(self.variable_ranges.pointer(self.variable_ranges.len())),
            variable_count: 0,
            inner: Container::null(),
        });
    }

    pub fn end_scope(&mut self) {
        let constant_count = self.last_scope.get().constant_count;

        if constant_count > 1 {
            panic!("Should not have more than one constant value ranging");
        }

        let c = if constant_count > 0 {
            Some(self.pop_constant())
        } else {
            None
        };

        {
            let count = self.last_scope.get().variable_count as usize;
            let variables_to_reset = self.last_scope.get().variables.slice(0, count);

            for v in variables_to_reset {
                v.get().reset();
            }

            self.variable_ranges
                .set_len(self.variable_ranges.len() - count as u32);
        }

        {
            self.last_scope = Container::new(self.last_scope.get().parent.pointer());
            if let Some(constant) = c {
                self.push_constant(constant);
            }
        }
    }

    pub fn deinit(&mut self, words: &Vector<u8>) {
        if let Some(main_procedure) = self.procedures.addr_of(b"main", words) {
            if main_procedure.get().inner.get().get_size() != 4 {
                panic!("program return value should be of size 4");
            } else {
                self.generator.generate(main_procedure.get().offset);
            }
        } else {
            panic!("program entry point not found");
        }
    }
}
