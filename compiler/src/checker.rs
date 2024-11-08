use crate::generator::Generator;
use crate::x86_64::{self, BinaryOperation, Immediate, Register, Source};
use collections::{Buffer, RangeMap, Vector};
use mem::{Arena, Container};
use util::{Index, Range};

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

pub enum ConstantRawKind {
    Number(usize),
    Identifier(Source),
    Boolean(bool),
}

pub struct ConstantRaw {
    kind: ConstantRawKind,
    inner: Container<Type>,
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
    _matcher: Constant,
    expression: Constant,
}

pub struct ConstantCase {
    inner: Container<Type>,
    ofs: Buffer<Of>,
    len: Index,
    expression: Container<Constant>,
}

pub struct ConstantCall {
    arguments: Buffer<Constant>,
    procedure: Container<Procedure>,
    source: Option<Source>,
}

pub struct ConstantDot {
    constant: Container<Constant>,
    inner: Container<Type>,
}

pub struct ConstantConstruct {
    fields: Buffer<Constant>,
    inner: Container<Type>,
}

pub struct Scope {
    parent: Container<Scope>,
    _constant: Option<Constant>,
    constant_count: Index,
    signature: Index,
    variables: Buffer<Container<Range>>,
    variable_count: Index,
    _inner: Container<Type>,
}

pub enum Constant {
    Raw(Container<ConstantRaw>),
    Binary(Container<ConstantBinary>),
    Case(Container<ConstantCase>),
    Unary(Container<ConstantUnary>),
    Call(Container<ConstantCall>),
    Construct(Container<ConstantConstruct>),
    Ref(Container<Constant>),
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
    parameters: Buffer<Constant>,
    len: Index,
    offset: Index,
    inner: Container<Type>,
}

pub struct TypeChecker {
    constants: Vector<Constant>,
    ranges: Vector<Range>,

    types: RangeMap<Type>,
    procedures: RangeMap<Procedure>,

    variables: RangeMap<Variable>,
    variable_ranges: Vector<Container<Range>>,

    generator: Generator,
    last_scope: Container<Scope>,
    scope_count: Index,

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

impl PartialEq for ConstantRawKind {
    fn eq(&self, other: &ConstantRawKind) -> bool {
        match (self, other) {
            (ConstantRawKind::Identifier(_), ConstantRawKind::Identifier(_)) => true,
            (ConstantRawKind::Number(_), ConstantRawKind::Number(_)) => true,
            (ConstantRawKind::Boolean(a), ConstantRawKind::Boolean(b)) => a == b,
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
    pub fn value(&self) -> &ConstantRawKind {
        &self.kind
    }

    pub fn source(&self) -> Source {
        match &self.kind {
            ConstantRawKind::Identifier(source) => source.clone(),
            ConstantRawKind::Number(usize) => Source::Immediate(Immediate(*usize)),
            ConstantRawKind::Boolean(b) => Source::Immediate(Immediate(*b as usize)),
        }
    }
}

impl ConstantCall {
    pub fn arg(&mut self, index: usize) -> &mut Constant {
        self.arguments.get_mut(index)
    }

    pub fn len(&self) -> usize {
        self.procedure.get().len as usize
    }

    pub fn offset(&self) -> usize {
        self.procedure.get().offset as usize
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
}

impl ConstantConstruct {
    fn field(&self, name_range: Range, words: &Vector<u8>) -> Container<Constant> {
        let pos = self.inner.get().field_index(name_range, words);
        Container::new(self.fields.offset(pos))
    }
}

impl Type {
    pub fn get_size(&self) -> Index {
        self.size
    }

    fn field_index(&self, range: Range, words: &Vector<u8>) -> usize {
        let fields = self.fields.slice(0, self.len as usize);

        for (i, field) in fields.iter().enumerate() {
            if util::compare(words.range(field.name), words.range(range)) {
                return i;
            }
        }

        panic!("Should not happen");
    }
}

impl Constant {
    pub fn get_type(&self) -> Container<Type> {
        match self {
            Constant::Raw(raw) => Container::new(raw.get().inner.pointer()),
            Constant::Case(case) => Container::new(case.get().inner.pointer()),
            Constant::Binary(binary) => Container::new(binary.get().inner.pointer()),
            Constant::Unary(unary) => unary.get().constant.get().get_type(),
            Constant::Call(call) => Container::new(call.get().procedure.get().inner.pointer()),
            Constant::Construct(construct) => Container::new(construct.get().inner.pointer()),
            Constant::Ref(c) => c.get().get_type(),
        }
    }

    pub fn get_source(&self) -> Option<Source> {
        match self {
            Constant::Raw(raw) => Some(raw.get().source()),
            Constant::Ref(r) => r.get().get_source(),
            Constant::Call(call) => call.get().source.clone(),
            _ => panic!("Should not happen"),
        }
    }

    pub fn set_source(&self, s: Source) {
        match self {
            Constant::Raw(_) => panic!("Cannot dot that"),
            Constant::Ref(r) => r.get().set_source(s),
            Constant::Call(call) => call.get().source = Some(s),
            _ => todo!(),
        }
    }

    fn set_type(&mut self, inner: Container<Type>) -> bool {
        match self {
            Constant::Binary(binary) => binary.get().set_type(inner),
            Constant::Case(case) => case.get().set_type(inner),
            Constant::Unary(unary) => unary.get().constant.get().set_type(inner),
            Constant::Call(call) => call.get().procedure.get().inner.eql(&inner),
            Constant::Construct(construct) => construct.get().inner.eql(&inner),
            Constant::Ref(c) => c.get().set_type(inner),
            Constant::Raw(raw) => {
                if raw.get().inner.is_some() {
                    raw.get().inner.eql(&inner)
                } else {
                    raw.get().inner = Container::new(inner.pointer());
                    true
                }
            }
        }
    }

    fn clone(&self) -> Constant {
        match self {
            Constant::Raw(raw) => Constant::Raw(Container::new(raw.get())),
            Constant::Case(case) => Constant::Case(Container::new(case.get())),
            Constant::Binary(binary) => Constant::Binary(Container::new(binary.get())),
            Constant::Unary(unary) => Constant::Unary(Container::new(unary.get())),
            Constant::Call(call) => Constant::Call(Container::new(call.get())),
            Constant::Construct(construct) => Constant::Construct(Container::new(construct.get())),
            Constant::Ref(c) => Constant::Ref(Container::new(c.get())),
        }
    }

    fn deinit(&self, arena: &mut Arena) {
        match self {
            Constant::Raw(_) => arena.dealloc::<ConstantRaw>(1),
            Constant::Case(case) => {
                arena.dealloc::<ConstantCase>(1);

                for i in 0..case.get().len {
                    let of = case.get().ofs.get(case.get().len as usize - i as usize - 1);

                    of._matcher.deinit(arena);
                    of.expression.deinit(arena);
                }

                arena.dealloc::<Of>(case.get().len as usize);
            }
            Constant::Binary(_) => arena.dealloc::<ConstantBinary>(1),
            Constant::Unary(_) => arena.dealloc::<ConstantUnary>(1),
            Constant::Call(call) => {
                arena.dealloc::<ConstantCall>(1);
                Constant::deinit_buffer(
                    &call.get().arguments,
                    call.get().procedure.get().len as usize,
                    arena,
                );
            }
            Constant::Construct(construct) => {
                arena.dealloc::<ConstantConstruct>(1);
                Constant::deinit_buffer(
                    &construct.get().fields,
                    construct.get().inner.get().len as usize,
                    arena,
                );
            }
            Constant::Ref(_) => {}
        }
    }

    fn deinit_buffer(buffer: &Buffer<Constant>, len: usize, arena: &mut Arena) {
        for i in 0..len {
            buffer.get(len - i - 1).deinit(arena);
        }

        arena.dealloc::<Constant>(len);
    }
}

impl TypeChecker {
    pub fn new(path: String, arena: &mut Arena) -> TypeChecker {
        let mut self_arena = Arena::new(arena.bytes(8196));

        TypeChecker {
            ranges: Vector::new(10, &mut self_arena),
            constants: Vector::new(10, &mut self_arena),

            types: RangeMap::new(10, &mut self_arena),
            procedures: RangeMap::new(10, &mut self_arena),

            variables: RangeMap::new(10, &mut self_arena),
            variable_ranges: Vector::new(10, &mut self_arena),

            generator: Generator::new(path, &mut self_arena),
            last_scope: Container::null(),
            scope_count: 0,

            arena: self_arena,
        }
    }

    pub fn push_identifier(&mut self, range: Range, words: &Vector<u8>) {
        if let Some(variable) = self.variables.get(range, words) {
            self.push_constant(variable.constant.clone());

            return;
        }

        panic!("undeclared variable");
    }

    pub fn push_boolean(&mut self, b: bool, words: &Vector<u8>) {
        let inner = self.get_type_from_str(b"bool", words);
        let constant = Constant::Raw(self.arena.create(ConstantRaw {
            kind: ConstantRawKind::Boolean(b),
            inner,
        }));

        self.push_constant(constant);
    }

    pub fn push_number(&mut self, range: Range, words: &Vector<u8>) {
        let string = words.range(range);
        let number = util::parse_string(string);
        let constant = Constant::Raw(self.arena.create(ConstantRaw {
            kind: ConstantRawKind::Number(number),
            inner: Container::null(),
        }));

        self.push_constant(constant);
    }

    pub fn push_range(&mut self, range: Range) {
        self.ranges.push(range);
    }

    pub fn push_type(&mut self, field_count: usize, annotated_size: usize, words: &Vector<u8>) {
        let name_range = self.ranges.pop();
        let mut fields = Buffer::new(self.arena.allocate::<Field>(field_count));

        let mut size: Index = annotated_size as Index;
        let mut align: Index = annotated_size as Index;

        for i in 0..field_count {
            let field_type_range = self.ranges.pop();
            let field_name_range = self.ranges.pop();
            let inner = self.get_type(field_type_range, words);

            size += inner.get().size;
            align = util::max(align as usize, inner.get().align as usize) as Index;

            fields.set(
                Field {
                    name: field_name_range,
                    inner,
                },
                i,
            );
        }

        self.types.push(
            name_range,
            Type {
                fields,
                len: field_count as Index,
                size,
                align,
            },
            words,
        );
    }

    pub fn push_let(&mut self, words: &Vector<u8>) {
        let type_range = self.ranges.pop();
        let name_range = self.ranges.pop();
        let inner = self.get_type(type_range, words);
        let mut constant = self.pop_constant();

        if !constant.set_type(Container::new(inner.pointer())) {
            panic!("Could not set type of let declaration");
        }

        self.register_variable(name_range, constant, words);
    }

    pub fn push_parameter(&mut self, current_size: usize, words: &Vector<u8>) -> usize {
        let type_range = self.ranges.pop();
        let name_range = self.ranges.pop();
        let inner = self.get_type(type_range, words);
        let size = inner.get().get_size() as usize;

        let constant = Constant::Raw(self.arena.create(ConstantRaw {
            kind: ConstantRawKind::Identifier(Source::Memory(
                Register::Rbp,
                Immediate(current_size),
            )),
            inner,
        }));

        self.push_constant(constant);

        let constant_ref = Container::new(self.constants.pointer(self.constants.len() - 1));

        self.register_variable(name_range, Constant::Ref(constant_ref), words);

        size
    }

    pub fn push_procedure(&mut self, parameter_count: usize, words: &Vector<u8>) {
        let type_range = self.ranges.pop();
        let name_range = self.ranges.pop();
        let inner = self.get_type(type_range, words);

        let offset = if self.last_scope.get().constant_count > parameter_count as Index {
            let c = Container::new(self.constants.pointer(self.constants.len() - 1));

            if !c.get().set_type(Container::new(inner.pointer())) {
                panic!("Could not set the scope last expression type");
            }

            let off = self.generator.write_procedure(c);
            let constant = self.pop_constant();

            constant.deinit(&mut self.arena);

            off
        } else {
            self.generator.write_procedure(Container::null())
        };

        let mut parameters = Buffer::new(self.arena.allocate::<Constant>(parameter_count));

        for i in 0..parameter_count {
            let parameter = self.pop_constant();

            parameters.set(parameter, i);
        }

        self.procedures.push(
            name_range,
            Procedure {
                inner,
                len: parameter_count as Index,
                parameters,
                offset,
            },
            words,
        );
    }

    pub fn push_case(&mut self, count: usize) {
        let mut ofs = Buffer::new(self.arena.allocate::<Of>(count));

        let mut inner: Container<Type> = Container::null();
        let mut matcher_inner: Container<Type> = Container::null();

        for i in 0..count {
            let expression = self.pop_constant();
            let mut matcher = self.pop_constant();

            let expression_inner = expression.get_type();

            if !inner.eql(&expression_inner) {
                if !inner.is_some() {
                    inner = expression_inner;
                } else {
                    panic!("expression type must match the case type");
                }
            }

            let typ = matcher.get_type();

            if !matcher_inner.eql(&typ) {
                if !matcher_inner.is_some() {
                    matcher_inner = matcher.get_type();
                } else if !typ.is_some() {
                    matcher.set_type(Container::new(matcher_inner.pointer()));
                } else {
                    panic!("Could not set the matcher type");
                }
            }

            ofs.set(
                Of {
                    expression,
                    _matcher: matcher,
                },
                i,
            );
        }

        if !self.constants.last_mut().set_type(matcher_inner) {
            panic!("Could not set the match on expression type");
        }

        let new_constant = Constant::Case(self.arena.create(ConstantCase {
            ofs,
            len: count as Index,
            expression: Container::new(self.constants.pointer(self.constants.len() - 1)),
            inner,
        }));

        self.push_constant(new_constant);
    }

    pub fn push_call(&mut self, len: usize, words: &Vector<u8>) {
        let name_range = self.ranges.pop();
        let mut arguments = Buffer::new(self.arena.allocate::<Constant>(len));

        let Some(procedure) = self.procedures.addr_of(words.range(name_range), words) else {
            panic!("Should not happen");
        };

        if len != procedure.get().len as usize {
            panic!("Passing the wrog number of arguments");
        }

        for i in 0..len {
            let mut constant = self.pop_constant();
            let parameter_inner = procedure.get().parameters.get(len - i - 1);

            if !constant.set_type(parameter_inner.get_type()) {
                panic!("Could not set the argument type");
            }

            arguments.set(constant, i);
        }

        let constant = Constant::Call(self.arena.create(ConstantCall {
            arguments,
            procedure,
            source: None,
        }));

        self.push_constant(constant);
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
        let constant = Constant::Binary(self.arena.create(ConstantBinary {
            left,
            right,
            inner,
            operator,
        }));

        self.push_constant(constant);
    }

    pub fn push_construct(&mut self, len: u32, words: &Vector<u8>) {
        let name_range = self.ranges.pop();
        let inner = self.get_type(name_range, words);
        let mut fields = Buffer::new(self.arena.allocate::<Constant>(len as usize));

        if len != inner.get().len as u32 {
            panic!("Passing the wrogn number of arguments");
        }

        for _ in 0..len {
            let name = self.ranges.pop();
            let mut constant = self.pop_constant();
            let type_index = inner.get().field_index(name, words);

            constant.set_type(Container::new(
                inner.get().fields.get(type_index).inner.pointer(),
            ));

            fields.set(constant, type_index);
        }

        let constant = Constant::Construct(self.arena.create(ConstantConstruct { fields, inner }));

        self.push_constant(constant);
    }

    pub fn push_dot(&mut self, words: &Vector<u8>) {
        let name_range = self.ranges.pop();
        let constant_construct = self.pop_constant();

        if let Constant::Construct(c) = constant_construct {
            let constant = c.get().field(name_range, words);

            self.push_constant(Constant::Ref(constant));
        } else {
            panic!("Should not happen");
        }
    }

    pub fn push_unary(&mut self, operator: UnaryOperator) {
        let constant = Container::new(self.constants.pointer(self.constants.len() - 1));
        let new_unary = Constant::Unary(self.arena.create(ConstantUnary { operator, constant }));

        self.push_constant(new_unary)
    }

    pub fn start_scope(&mut self) {
        self.last_scope = self.arena.create(Scope {
            parent: Container::new(self.last_scope.pointer()),
            signature: self.scope_count,
            _constant: None,
            constant_count: 0,
            variables: Buffer::new(self.variable_ranges.pointer(self.variable_ranges.len())),
            variable_count: 0,
            _inner: Container::null(),
        });

        self.scope_count += 1;
    }

    pub fn end_scope(&mut self) {
        let constant_count = self.last_scope.get().constant_count;

        if constant_count > 1 {
            panic!("Should not have more than one constant value ranging");
        }

        let constant = if constant_count > 0 {
            Some(self.pop_constant())
        } else {
            None
        };

        self.reset_scope_variables();
        self.last_scope = Container::new(self.last_scope.get().parent.pointer());
        self.scope_count -= 1;

        if let Some(c) = constant {
            self.push_constant(c);
        }
    }

    fn push_constant(&mut self, constant: Constant) {
        self.last_scope.get().constant_count += 1;
        self.constants.push(constant);
    }

    fn pop_constant(&mut self) -> Constant {
        self.last_scope.get().constant_count -= 1;
        self.constants.pop()
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

    fn reset_scope_variables(&mut self) {
        let variable_count = self.last_scope.get().variable_count as u32;
        let constant_count = self.last_scope.get().constant_count as u32;

        for _ in 0..variable_count {
            self.variable_ranges.pop().get().reset();
        }

        for _ in 0..constant_count {
            self.pop_constant().deinit(&mut self.arena);
        }
    }

    pub fn deinit(&mut self, words: &Vector<u8>) {
        if let Some(main_procedure) = self.procedures.addr_of(b"main", words) {
            if main_procedure.get().inner.get().get_size() != x86_64::BASE_SIZE as Index {
                panic!("program return value should be of size 4");
            } else {
                self.generator.generate(main_procedure.get().offset);
            }
        } else {
            panic!("program entry point not found");
        }
    }
}
