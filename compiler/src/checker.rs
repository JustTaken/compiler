use crate::generator::Generator;
use collections::{Buffer, RangeMap, Vector};
use mem::{Arena, Container};
use util::{Index, Range};

pub enum ConstantKind {
    Number(Range),
    Identifier(Range),
    Boolean(bool),
}

pub struct ConstantRaw {
    kind: ConstantKind,
    inner: Container<Type>,
}

pub enum BinaryOperator {
    Sum,
    Sub,
    Mul,
    Div,
    Gt,
    Lt,
    Eq,
}

pub struct ConstantBinary {
    left: Container<Constant>,
    right: Container<Constant>,
    inner: Container<Type>,
    operator: BinaryOperator,
}

pub struct ConstantCall {
    inner: Container<Type>,
    arguments: Buffer<Constant>,
    range: Range,
    len: Index,
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

pub struct ConstantScope {
    parent: Container<ConstantScope>,
    statements: Buffer<Statement>,
    signature: Index,
    len: Index,
    constant_count: Index,
    inner: Container<Type>,
}

pub enum Constant {
    Raw(ConstantRaw),
    Binary(ConstantBinary),
    Call(ConstantCall),
    Case(ConstantCase),
    Scope(Container<ConstantScope>),
}

pub enum Statement {
    Let(Index, Constant),
    Expression(Constant),
}

struct Variable {
    signature: Index,
    inner: Container<Type>,
}

struct Field {
    name: Range,
    inner: Container<Type>,
}

pub struct Type {
    fields: Buffer<Field>,
    len: Index,
    size: Index,
}

pub struct Procedure {
    pub parameters: Buffer<Container<Type>>,
    pub len: Index,
    pub inner: Container<Type>,
}

pub struct TypeChecker {
    constants: Vector<Constant>,
    ranges: Vector<Range>,

    types: RangeMap<Type>,
    fields: Vector<Field>,

    procedures: RangeMap<Procedure>,

    ofs: Vector<Of>,
    parameters: Vector<Container<Type>>,
    statements: Vector<Statement>,
    arguments: Vector<Constant>,

    variables: RangeMap<Variable>,

    generator: Generator,
    last_scope: Container<ConstantScope>,
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

    fn to_int(&self) -> usize {
        match self {
            BinaryOperator::Sum => 1,
            BinaryOperator::Sub => 2,
            BinaryOperator::Mul => 3,
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

    pub fn get_left(&self) -> &Container<Constant> {
        &self.left
    }

    pub fn get_right(&self) -> &Container<Constant> {
        &self.right
    }

    pub fn op(&self) -> usize {
        self.operator.to_int()
    }
}

impl ConstantRaw {
    pub fn range(&self) -> Range {
        match self.kind {
            ConstantKind::Identifier(r) | ConstantKind::Number(r) => r,
            _ => panic!("Shold not happen"),
        }
    }

    pub fn value(&self) -> &ConstantKind {
        &self.kind
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

impl ConstantScope {
    fn set_type(&mut self, inner: Container<Type>) -> bool {
        if self.inner.is_some() {
            self.inner.eql(&inner)
        } else {
            if self.len == 0 {
                !inner.is_some()
            } else if let Statement::Expression(ref mut exp) =
                self.statements.get_mut(self.len as usize - 1)
            {
                self.inner = Container::new(inner.pointer());
                exp.set_type(inner)
            } else {
                !inner.is_some()
            }
        }
    }

    pub fn get_statements(&self) -> &[Statement] {
        self.statements.slice(0, self.len as usize)
    }
}

impl Constant {
    fn set_type(&mut self, inner: Container<Type>) -> bool {
        match self {
            Constant::Binary(ref mut binary) => binary.set_type(inner),
            Constant::Call(call) => call.inner.eql(&inner),
            Constant::Case(case) => case.set_type(inner),
            Constant::Scope(scope) => scope.get().set_type(inner),
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

    pub fn range(&self) -> Range {
        match self {
            Constant::Raw(raw) => raw.range(),
            Constant::Call(call) => call.range,
            _ => panic!("should not happen"),
        }
    }

    pub fn get_type(&self) -> Container<Type> {
        match self {
            Constant::Raw(raw) => Container::new(raw.inner.pointer()),
            Constant::Call(call) => Container::new(call.inner.pointer()),
            Constant::Case(case) => Container::new(case.inner.pointer()),
            Constant::Binary(binary) => Container::new(binary.inner.pointer()),
            Constant::Scope(scope) => Container::new(scope.get().inner.pointer()),
        }
    }
}

impl Type {
    pub fn get_size(&self) -> Index {
        self.size
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

            procedures: RangeMap::new(10, &mut self_arena),

            variables: RangeMap::new(10, &mut self_arena),

            statements: Vector::new(10, &mut self_arena),
            parameters: Vector::new(10, &mut self_arena),
            arguments: Vector::new(10, &mut self_arena),
            ofs: Vector::new(10, &mut self_arena),

            generator: Generator::new(path, &mut self_arena),
            last_scope: Container::null(),
            scope_count: 0,

            arena: self_arena,
        }
    }

    fn push_constant(&mut self, constant: Constant) {
        self.last_scope.get().constant_count += 1;
        self.constants.push(constant);
    }

    pub fn pop_constant(&mut self) -> Constant {
        self.last_scope.get().constant_count -= 1;
        self.constants.pop()
    }

    pub fn push_identifier(&mut self, range: Range, words: &Vector<u8>) {
        if let Some(variable) = self.variables.get(range, words) {
            let mut s = &self.last_scope;

            while s.is_some() {
                if s.get().signature == variable.signature {
                    self.push_constant(Constant::Raw(ConstantRaw {
                        kind: ConstantKind::Identifier(range),
                        inner: Container::new(variable.inner.pointer()),
                    }));

                    return;
                }

                s = &s.get().parent;
            }
        }

        panic!("undeclared variable");
    }

    pub fn push_boolean(&mut self, b: bool, words: &Vector<u8>) {
        let inner = self.get_type_from_str(b"bool", words);
        self.push_constant(Constant::Raw(ConstantRaw {
            kind: ConstantKind::Boolean(b),
            inner,
        }));
    }

    pub fn push_number(&mut self, range: Range) {
        self.push_constant(Constant::Raw(ConstantRaw {
            kind: ConstantKind::Number(range),
            inner: Container::null(),
        }));
    }

    pub fn push_parameter(&mut self, words: &Vector<u8>) {
        let type_range = self.ranges.pop();
        let name_range = self.ranges.pop();
        let inner = self.get_type(type_range, words);

        self.parameters.push(Container::new(inner.pointer()));
        self.register_variable(name_range, inner, words);
    }

    fn push_statement(&mut self, statement: Statement) {
        self.statements.push(statement);
        self.last_scope.get().len += 1;
    }

    pub fn start_scope(&mut self) {
        let statements = Buffer::new(self.statements.pointer(self.statements.len()));

        self.last_scope = self.arena.create(ConstantScope {
            parent: Container::new(self.last_scope.pointer()),
            statements,
            signature: self.scope_count,
            len: 0,
            constant_count: 0,
            inner: Container::null(),
        });
    }

    pub fn end_scope(&mut self) {
        if self.last_scope.get().constant_count > 0 {
            let constant = self.pop_constant();
            let inner = constant.get_type();

            self.push_statement(Statement::Expression(constant));
            self.last_scope.get().inner = inner;
        }

        let ptr = self.last_scope.pointer();
        self.last_scope = Container::new(self.last_scope.get().parent.pointer());
        self.push_constant(Constant::Scope(Container::new(ptr)));
    }

    pub fn push_procedure(&mut self, parameter_count: usize, words: &Vector<u8>) {
        let type_range = self.ranges.pop();
        let name_range = self.ranges.pop();
        let inner = self.get_type(type_range, words);

        if !self
            .constants
            .last_mut()
            .set_type(Container::new(inner.pointer()))
        {
            panic!("Could not set the scope last expression type");
        }

        let pointer = self
            .parameters
            .pointer(self.parameters.len() - parameter_count as u32);

        self.procedures.push(
            name_range,
            Procedure {
                inner,
                len: parameter_count as Index,
                parameters: Buffer::new(pointer),
            },
            words,
        );

        self.generator.write_procedure();
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
    }

    pub fn push_let(&mut self, words: &Vector<u8>) {
        let type_range = self.ranges.pop();
        let name_range = self.ranges.pop();
        let inner = self.get_type(type_range, words);

        let mut constant = self.pop_constant();

        if !constant.set_type(Container::new(inner.pointer())) {
            panic!("Could not set type of let declaration");
        }

        let pointer = self.generator.stack_pointer();

        self.generator.bind_constant(&constant, pointer, words);
        self.push_statement(Statement::Let(pointer, constant));
        self.register_variable(name_range, inner, words);
    }

    pub fn push_call(&mut self, words: &Vector<u8>) {
        let name_range = self.ranges.pop();
        let Some(procedure) = self.procedures.get(name_range, words) else {
            panic!("Should not happen");
        };

        let start = self.constants.len() - procedure.len as u32;
        let argument_start = self.arguments.len();

        for i in 0..procedure.len as usize {
            let mut constant = self.constants.value(start + i as u32);
            let parameter_inner = procedure.parameters.get(i);

            if !constant.set_type(Container::new(parameter_inner.pointer())) {
                panic!("Could not set the argument type");
            }

            self.arguments.push(constant);
        }

        let buffer = Buffer::new(self.arguments.pointer(argument_start));

        self.constants.set_len(start);
        self.push_constant(Constant::Call(ConstantCall {
            arguments: buffer,
            range: name_range,
            inner: Container::new(procedure.inner.pointer()),
            len: procedure.len,
        }));
    }

    pub fn push_type(&mut self, field_count: usize, words: &Vector<u8>) {
        let name_range = self.ranges.pop();
        let start = self.fields.len();
        let mut size = 0;

        for i in 0..field_count {
            let field_type_range = self.ranges.pop();
            let field_name_range = self.ranges.pop();
            let inner = self.get_type(field_type_range, words);

            size += inner.get().size;

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

    pub fn push_unary(&mut self, op: BinaryOperator) {}

    pub fn push_range(&mut self, range: Range) {
        self.ranges.push(range);
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

    fn register_variable(&mut self, name_range: Range, inner: Container<Type>, words: &Vector<u8>) {
        self.variables.push(
            name_range,
            Variable {
                signature: self.last_scope.get().signature,
                inner,
            },
            words,
        );
    }

    pub fn clear(&mut self) {
        self.statements.clear();
        self.variables.clear();
    }
}
