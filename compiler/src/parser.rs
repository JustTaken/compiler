use mem::Arena;

use crate::checker::{BinaryOperator, TypeChecker, UnaryOperator};
use crate::lexer::{Keyword, Lexer, Operator, Symbol, Token};

#[repr(u32)]
#[derive(Clone, Copy)]
enum Precedence {
    Nothing,
    Assignment,
    Or,
    And,
    Equality,
    Comparison,
    Term,
    Factor,
    Unary,
    Call,
    Primary,
}

type Fn = fn(&mut Parser);
struct Rule {
    prefix: Option<Fn>,
    infix: Option<Fn>,
    precedence: Precedence,
}

pub struct Parser {
    previous: Token,
    current: Token,
    lexer: Lexer,
    checker: TypeChecker,
}

impl Precedence {
    fn from_integer(integer: u32) -> Precedence {
        unsafe { std::mem::transmute(integer) }
    }

    fn to_int(&self) -> u32 {
        *self as u32
    }

    fn inc(&self) -> Precedence {
        Precedence::from_integer(self.to_int() + 1)
    }
}

impl Rule {
    fn new(prefix: Option<Fn>, infix: Option<Fn>, precedence: Precedence) -> Rule {
        Rule {
            prefix,
            infix,
            precedence,
        }
    }

    fn from_token(token: Token) -> Rule {
        match token {
            Token::Symbol(symbol) => {
                if let Symbol::ParentesisLeft = symbol {
                    Rule::new(Some(grouping), None, Precedence::Assignment)
                } else {
                    Rule::new(None, None, Precedence::Nothing)
                }
            }
            Token::Operator(operator) => match operator {
                Operator::Minus => Rule::new(Some(unary), Some(binary), Precedence::Term),
                Operator::Plus => Rule::new(None, Some(binary), Precedence::Term),
                Operator::Bang => Rule::new(Some(unary), None, Precedence::Equality),
                Operator::BangEqual | Operator::EqualEqual => {
                    Rule::new(None, Some(binary), Precedence::Equality)
                }
                Operator::Slash | Operator::Star => {
                    Rule::new(None, Some(binary), Precedence::Factor)
                }
                Operator::Greater
                | Operator::GreaterEqual
                | Operator::Less
                | Operator::LessEqual => Rule::new(None, Some(binary), Precedence::Comparison),
            },
            Token::Keyword(keyword) => match keyword {
                Keyword::False | Keyword::True => {
                    Rule::new(Some(literal), None, Precedence::Nothing)
                }
                _ => Rule::new(None, None, Precedence::Nothing),
            },
            Token::Number(_) => Rule::new(Some(number), None, Precedence::Nothing),
            Token::Identifier(_) => Rule::new(Some(identifier), None, Precedence::Nothing),
            Token::String(_) => Rule::new(Some(string), None, Precedence::Nothing),
            _ => Rule::new(None, None, Precedence::Nothing),
        }
    }
}

impl Parser {
    pub fn new(input: &str, output: &str, arena: &mut Arena) -> Parser {
        let mut lexer = Lexer::new(input, arena);
        let mut checker = TypeChecker::new(output, arena);

        checker.start_scope();

        Parser {
            previous: Token::Eof,
            current: lexer.next(),
            lexer,
            checker,
        }
    }

    fn advance(&mut self) {
        self.previous = self.current;
        self.current = self.lexer.next();
    }

    fn consume(&mut self, token: Token) {
        if self.current == token {
            self.advance();
        } else {
            panic!("Failed to consume token");
        }
    }

    fn assert(&mut self, token: Token) -> bool {
        if self.current == token {
            self.advance();
            true
        } else {
            false
        }
    }

    fn parse(&mut self, precedence: Precedence) {
        self.advance();

        if let Some(f) = Rule::from_token(self.previous).prefix {
            f(self);
        } else {
            panic!("should not happen");
        }

        let mut rule = Rule::from_token(self.current);

        while precedence.to_int() <= rule.precedence.to_int() {
            if let Some(f) = rule.infix {
                self.advance();
                f(self);
            } else {
                break;
            }

            rule = Rule::from_token(self.current);
        }
    }

    pub fn next(&mut self) -> bool {
        if !self.assert(Token::EOF) {
            statement(self);
            true
        } else {
            false
        }
    }

    pub fn deinit(&mut self) {
        self.checker.deinit(&self.lexer.words);
    }
}

fn statement(parser: &mut Parser) {
    match parser.current {
        Token::Keyword(keyword) => match keyword {
            Keyword::Let => variable(parser),
            Keyword::Procedure => procedure(parser),
            Keyword::Type => typ(parser),
            Keyword::Case => case(parser),
            _ => {}
        },
        _ => expression(parser),
    }
}

fn expression(parser: &mut Parser) {
    parser.parse(Precedence::Assignment);
}

fn grouping(parser: &mut Parser) {
    expression(parser);
    parser.consume(Token::PARENTESISRIGHT);
}

fn unary(parser: &mut Parser) {
    let Token::Operator(operator) = parser.previous else {
        panic!("Should not happen")
    };

    parser.parse(Precedence::Unary);

    match operator {
        Operator::Bang => parser.checker.push_unary(UnaryOperator::Negation),
        Operator::Minus => parser.checker.push_unary(UnaryOperator::Oposite),
        _ => {}
    }
}

fn binary(parser: &mut Parser) {
    let rule = Rule::from_token(parser.previous);
    let Token::Operator(operator) = parser.previous else {
        panic!("Should not happen")
    };

    parser.parse(rule.precedence.inc());

    match operator {
        Operator::BangEqual => {
            parser
                .checker
                .push_binary(BinaryOperator::Eq, &parser.lexer.words);
            parser.checker.push_unary(UnaryOperator::Negation);
        }
        Operator::GreaterEqual => {
            parser
                .checker
                .push_binary(BinaryOperator::Lt, &parser.lexer.words);
            parser.checker.push_unary(UnaryOperator::Negation);
        }
        Operator::LessEqual => {
            parser
                .checker
                .push_binary(BinaryOperator::Gt, &parser.lexer.words);
            parser.checker.push_unary(UnaryOperator::Negation);
        }
        Operator::EqualEqual => parser
            .checker
            .push_binary(BinaryOperator::Eq, &parser.lexer.words),
        Operator::Greater => parser
            .checker
            .push_binary(BinaryOperator::Gt, &parser.lexer.words),
        Operator::Less => parser
            .checker
            .push_binary(BinaryOperator::Lt, &parser.lexer.words),
        Operator::Plus => parser
            .checker
            .push_binary(BinaryOperator::Add, &parser.lexer.words),
        Operator::Minus => parser
            .checker
            .push_binary(BinaryOperator::Sub, &parser.lexer.words),
        Operator::Star => parser
            .checker
            .push_binary(BinaryOperator::Mul, &parser.lexer.words),
        Operator::Slash => parser
            .checker
            .push_binary(BinaryOperator::Div, &parser.lexer.words),
        _ => {}
    }
}

fn literal(parser: &mut Parser) {
    let keyword = parser.previous.keyword();

    match keyword {
        Keyword::False => parser.checker.push_boolean(false, &parser.lexer.words),
        Keyword::True => parser.checker.push_boolean(true, &parser.lexer.words),
        _ => {}
    }
}

fn identifier(parser: &mut Parser) {
    let Token::Identifier(range) = parser.previous else {
        panic!("Should not happen")
    };

    if parser.assert(Token::PARENTESISLEFT) {
        call(parser);
        parser.checker.push_range(range);
        parser.checker.push_call(&parser.lexer.words);
    } else if parser.assert(Token::BRACELEFT) {
        construct(parser);
        parser.checker.push_range(range);
        parser.checker.push_construct(&parser.lexer.words);
    } else {
        parser.checker.push_identifier(range, &parser.lexer.words);
    }
}

fn construct(parser: &mut Parser) {
    while !parser.assert(Token::BRACERIGHT) {
        let Token::Identifier(range) = parser.current else {
            panic!("Should not happen");
        };

        parser.advance();
        parser.consume(Token::DOUBLECOLON);

        expression(parser);

        parser.consume(Token::COMMA);
        parser.checker.push_range(range);
    }
}

fn number(parser: &mut Parser) {
    let Token::Number(range) = parser.previous else {
        panic!("Should not happen")
    };

    parser.checker.push_number(range, &parser.lexer.words);
}

fn procedure(parser: &mut Parser) {
    parser.advance();

    let Token::Identifier(iden) = parser.current else {
        panic!("Should not happen")
    };

    parser.advance();
    parser.consume(Token::PARENTESISLEFT);

    parser.checker.start_scope();

    let mut parameter_count: usize = 0;

    while !parser.assert(Token::PARENTESISRIGHT) {
        let Token::Identifier(param) = parser.current else {
            panic!("Should not happen")
        };

        parser.advance();
        parser.consume(Token::DOUBLECOLON);

        let Token::Identifier(kind) = parser.current else {
            panic!("Should not happen")
        };

        parser.advance();
        parser.checker.push_range(param);
        parser.checker.push_range(kind);

        _ = parser.assert(Token::COMMA);

        parser.checker.push_parameter(&parser.lexer.words);
        parameter_count += 1;
    }

    parser.consume(Token::DOUBLECOLON);

    let Token::Identifier(ret) = parser.current else {
        panic!("Should not happen")
    };

    parser.advance();
    parser.consume(Token::BRACELEFT);

    scope(parser);

    parser.checker.push_range(iden);
    parser.checker.push_range(ret);

    parser
        .checker
        .push_procedure(parameter_count, &parser.lexer.words);
}

fn variable(parser: &mut Parser) {
    parser.advance();

    let mutable = parser.assert(Token::MUT);
    let Token::Identifier(iden) = parser.current else {
        panic!("Should not happen")
    };

    parser.advance();
    parser.consume(Token::DOUBLECOLON);

    let Token::Identifier(kind) = parser.current else {
        panic!("Should not happen")
    };

    parser.advance();
    parser.consume(Token::EQUAL);

    expression(parser);

    parser.consume(Token::SEMICOLON);

    parser.checker.push_range(iden);
    parser.checker.push_range(kind);
    parser.checker.push_let(&parser.lexer.words);
}

fn call(parser: &mut Parser) {
    while !parser.assert(Token::PARENTESISRIGHT) {
        _ = parser.assert(Token::COMMA);

        expression(parser);
    }
}

fn case(parser: &mut Parser) {
    parser.advance();

    expression(parser);

    parser.consume(Token::BRACELEFT);

    let mut of_count: usize = 0;
    while !parser.assert(Token::BRACERIGHT) {
        parser.consume(Token::OF);

        match parser.current {
            Token::Identifier(r) => parser.checker.push_identifier(r, &parser.lexer.words),
            Token::Number(r) => parser.checker.push_number(r, &parser.lexer.words),
            Token::Keyword(k) => match k {
                Keyword::True => parser.checker.push_boolean(true, &parser.lexer.words),
                Keyword::False => parser.checker.push_boolean(false, &parser.lexer.words),
                _ => panic!("Expected value, found keyword"),
            },
            _ => panic!("Matcher just accept identifiers, numbers, or booleans"),
        }

        parser.advance();
        parser.consume(Token::ARROW);

        if parser.assert(Token::BRACELEFT) {
            parser.checker.start_scope();
            scope(parser);
        } else {
            expression(parser);
        }

        parser.consume(Token::COMMA);
        of_count += 1;
    }

    parser.checker.push_case(of_count);
}

fn typ(parser: &mut Parser) {
    parser.advance();

    let Token::Identifier(iden) = parser.current else {
        panic!("Should not happen");
    };

    parser.advance();

    let mut field_count: usize = 0;
    let mut size: usize = 0;

    if parser.assert(Token::BRACELEFT) {
        while !parser.assert(Token::BRACERIGHT) {
            let Token::Identifier(field_name) = parser.current else {
                panic!("Should not happen");
            };

            parser.advance();
            parser.consume(Token::DOUBLECOLON);

            let Token::Identifier(field_type) = parser.current else {
                panic!("Should not happen");
            };

            parser.advance();
            parser.consume(Token::COMMA);

            parser.checker.push_range(field_name);
            parser.checker.push_range(field_type);

            field_count += 1;
        }
    } else {
        let Token::Number(size_range) = parser.current else {
            panic!("Should not happen");
        };

        parser.advance();
        parser.consume(Token::SEMICOLON);

        size = util::parse_string(parser.lexer.words.range(size_range));
    }

    parser.checker.push_range(iden);
    parser
        .checker
        .push_type(field_count, size, &parser.lexer.words);
}

fn scope(parser: &mut Parser) {
    while !parser.assert(Token::BRACERIGHT) {
        statement(parser);
    }

    parser.checker.end_scope();
}

fn string(parser: &mut Parser) {
    parser.advance();
}
