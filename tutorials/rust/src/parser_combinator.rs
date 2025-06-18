//! Advanced parser combinator library for building high-performance parsers
//! Designed for systems that need to parse complex data formats efficiently

use std::cell::RefCell;
use std::fmt;
use std::iter::Peekable;
use std::marker::PhantomData;
use std::rc::Rc;
use std::str::Chars;

// Type alias for parser results
type ParseResult<'a, T> = Result<(T, &'a str), ParseError>;

// Error type for parsing
#[derive(Debug, Clone, PartialEq)]
pub struct ParseError {
    pub message: String,
    pub position: usize,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Parse error at position {}: {}", self.position, self.message)
    }
}

// Parser trait - the core of our combinator library
pub trait Parser<'a, T> {
    fn parse(&self, input: &'a str) -> ParseResult<'a, T>;
    
    // Combinators
    fn map<F, U>(self, f: F) -> Map<Self, F, T, U>
    where
        Self: Sized,
        F: Fn(T) -> U,
    {
        Map {
            parser: self,
            map_fn: f,
            _input: PhantomData,
            _output: PhantomData,
        }
    }
    
    fn and_then<F, U, P>(self, f: F) -> AndThen<Self, F, T, U>
    where
        Self: Sized,
        P: Parser<'a, U>,
        F: Fn(T) -> P,
    {
        AndThen {
            parser: self,
            f,
            _input: PhantomData,
            _output: PhantomData,
        }
    }
    
    fn or<P>(self, other: P) -> Or<Self, P, T>
    where
        Self: Sized,
        P: Parser<'a, T>,
    {
        Or {
            left: self,
            right: other,
            _output: PhantomData,
        }
    }
    
    fn pair<P, U>(self, other: P) -> Pair<Self, P, T, U>
    where
        Self: Sized,
        P: Parser<'a, U>,
    {
        Pair {
            left: self,
            right: other,
            _left_output: PhantomData,
            _right_output: PhantomData,
        }
    }
    
    fn left<P, U>(self, other: P) -> Left<Self, P, T, U>
    where
        Self: Sized,
        P: Parser<'a, U>,
    {
        Left {
            left: self,
            right: other,
            _left_output: PhantomData,
            _right_output: PhantomData,
        }
    }
    
    fn right<P, U>(self, other: P) -> Right<Self, P, T, U>
    where
        Self: Sized,
        P: Parser<'a, U>,
    {
        Right {
            left: self,
            right: other,
            _left_output: PhantomData,
            _right_output: PhantomData,
        }
    }
    
    fn many(self) -> Many<Self, T>
    where
        Self: Sized,
        T: Clone,
    {
        Many {
            parser: self,
            _output: PhantomData,
        }
    }
    
    fn many1(self) -> Many1<Self, T>
    where
        Self: Sized,
        T: Clone,
    {
        Many1 {
            parser: self,
            _output: PhantomData,
        }
    }
    
    fn opt(self) -> Opt<Self, T>
    where
        Self: Sized,
        T: Clone,
    {
        Opt {
            parser: self,
            _output: PhantomData,
        }
    }
    
    fn between<P1, P2, U, V>(self, open: P1, close: P2) -> Between<Self, P1, P2, T, U, V>
    where
        Self: Sized,
        P1: Parser<'a, U>,
        P2: Parser<'a, V>,
    {
        Between {
            parser: self,
            open,
            close,
            _output: PhantomData,
            _open_output: PhantomData,
            _close_output: PhantomData,
        }
    }
    
    fn with_context<F>(self, context: F) -> WithContext<Self, F, T>
    where
        Self: Sized,
        F: Fn() -> String,
    {
        WithContext {
            parser: self,
            context,
            _output: PhantomData,
        }
    }
}

// Basic parsers

// Parser for matching a specific character
pub struct Char {
    c: char,
}

impl Char {
    pub fn new(c: char) -> Self {
        Self { c }
    }
}

impl<'a> Parser<'a, char> for Char {
    fn parse(&self, input: &'a str) -> ParseResult<'a, char> {
        if let Some(first) = input.chars().next() {
            if first == self.c {
                let new_input = &input[first.len_utf8()..];
                Ok((first, new_input))
            } else {
                Err(ParseError {
                    message: format!("Expected '{}', found '{}'", self.c, first),
                    position: 0,
                })
            }
        } else {
            Err(ParseError {
                message: format!("Expected '{}', found end of input", self.c),
                position: 0,
            })
        }
    }
}

// Parser for matching a specific string
pub struct Str<'s> {
    s: &'s str,
}

impl<'s> Str<'s> {
    pub fn new(s: &'s str) -> Self {
        Self { s }
    }
}

impl<'a, 's> Parser<'a, &'a str> for Str<'s> {
    fn parse(&self, input: &'a str) -> ParseResult<'a, &'a str> {
        if input.starts_with(self.s) {
            let (matched, rest) = input.split_at(self.s.len());
            Ok((matched, rest))
        } else {
            Err(ParseError {
                message: format!("Expected '{}', found '{}'", self.s, input.chars().next().unwrap_or('\0')),
                position: 0,
            })
        }
    }
}

// Parser for matching a character based on a predicate
pub struct Satisfy<F> {
    predicate: F,
    description: String,
}

impl<F> Satisfy<F>
where
    F: Fn(char) -> bool,
{
    pub fn new(predicate: F, description: &str) -> Self {
        Self {
            predicate,
            description: description.to_string(),
        }
    }
}

impl<'a, F> Parser<'a, char> for Satisfy<F>
where
    F: Fn(char) -> bool,
{
    fn parse(&self, input: &'a str) -> ParseResult<'a, char> {
        if let Some(first) = input.chars().next() {
            if (self.predicate)(first) {
                let new_input = &input[first.len_utf8()..];
                Ok((first, new_input))
            } else {
                Err(ParseError {
                    message: format!("Expected {}, found '{}'", self.description, first),
                    position: 0,
                })
            }
        } else {
            Err(ParseError {
                message: format!("Expected {}, found end of input", self.description),
                position: 0,
            })
        }
    }
}

// Combinator parsers

// Map parser: applies a function to the result of another parser
pub struct Map<P, F, T, U> {
    parser: P,
    map_fn: F,
    _input: PhantomData<T>,
    _output: PhantomData<U>,
}

impl<'a, P, F, T, U> Parser<'a, U> for Map<P, F, T, U>
where
    P: Parser<'a, T>,
    F: Fn(T) -> U,
{
    fn parse(&self, input: &'a str) -> ParseResult<'a, U> {
        match self.parser.parse(input) {
            Ok((result, rest)) => Ok(((self.map_fn)(result), rest)),
            Err(err) => Err(err),
        }
    }
}

// AndThen parser: applies a function to the result of another parser, returning a new parser
pub struct AndThen<P, F, T, U> {
    parser: P,
    f: F,
    _input: PhantomData<T>,
    _output: PhantomData<U>,
}

impl<'a, P, F, T, U, Q> Parser<'a, U> for AndThen<P, F, T, U>
where
    P: Parser<'a, T>,
    Q: Parser<'a, U>,
    F: Fn(T) -> Q,
{
    fn parse(&self, input: &'a str) -> ParseResult<'a, U> {
        match self.parser.parse(input) {
            Ok((result, rest)) => (self.f)(result).parse(rest),
            Err(err) => Err(err),
        }
    }
}

// Or parser: tries one parser, then another if the first fails
pub struct Or<P1, P2, T> {
    left: P1,
    right: P2,
    _output: PhantomData<T>,
}

impl<'a, P1, P2, T> Parser<'a, T> for Or<P1, P2, T>
where
    P1: Parser<'a, T>,
    P2: Parser<'a, T>,
{
    fn parse(&self, input: &'a str) -> ParseResult<'a, T> {
        match self.left.parse(input) {
            Ok(result) => Ok(result),
            Err(_) => self.right.parse(input),
        }
    }
}

// Pair parser: runs two parsers in sequence and returns both results
pub struct Pair<P1, P2, T, U> {
    left: P1,
    right: P2,
    _left_output: PhantomData<T>,
    _right_output: PhantomData<U>,
}

impl<'a, P1, P2, T, U> Parser<'a, (T, U)> for Pair<P1, P2, T, U>
where
    P1: Parser<'a, T>,
    P2: Parser<'a, U>,
{
    fn parse(&self, input: &'a str) -> ParseResult<'a, (T, U)> {
        match self.left.parse(input) {
            Ok((left_result, rest)) => {
                match self.right.parse(rest) {
                    Ok((right_result, rest)) => Ok(((left_result, right_result), rest)),
                    Err(err) => Err(err),
                }
            }
            Err(err) => Err(err),
        }
    }
}

// Left parser: runs two parsers in sequence and returns the left result
pub struct Left<P1, P2, T, U> {
    left: P1,
    right: P2,
    _left_output: PhantomData<T>,
    _right_output: PhantomData<U>,
}

impl<'a, P1, P2, T, U> Parser<'a, T> for Left<P1, P2, T, U>
where
    P1: Parser<'a, T>,
    P2: Parser<'a, U>,
{
    fn parse(&self, input: &'a str) -> ParseResult<'a, T> {
        match self.left.parse(input) {
            Ok((left_result, rest)) => {
                match self.right.parse(rest) {
                    Ok((_, rest)) => Ok((left_result, rest)),
                    Err(err) => Err(err),
                }
            }
            Err(err) => Err(err),
        }
    }
}

// Right parser: runs two parsers in sequence and returns the right result
pub struct Right<P1, P2, T, U> {
    left: P1,
    right: P2,
    _left_output: PhantomData<T>,
    _right_output: PhantomData<U>,
}

impl<'a, P1, P2, T, U> Parser<'a, U> for Right<P1, P2, T, U>
where
    P1: Parser<'a, T>,
    P2: Parser<'a, U>,
{
    fn parse(&self, input: &'a str) -> ParseResult<'a, U> {
        match self.left.parse(input) {
            Ok((_, rest)) => self.right.parse(rest),
            Err(err) => Err(err),
        }
    }
}

// Many parser: applies a parser zero or more times
pub struct Many<P, T> {
    parser: P,
    _output: PhantomData<T>,
}

impl<'a, P, T> Parser<'a, Vec<T>> for Many<P, T>
where
    P: Parser<'a, T>,
    T: Clone,
{
    fn parse(&self, input: &'a str) -> ParseResult<'a, Vec<T>> {
        let mut results = Vec::new();
        let mut rest = input;
        
        loop {
            match self.parser.parse(rest) {
                Ok((result, new_rest)) => {
                    results.push(result);
                    rest = new_rest;
                }
                Err(_) => break,
            }
        }
        
        Ok((results, rest))
    }
}

// Many1 parser: applies a parser one or more times
pub struct Many1<P, T> {
    parser: P,
    _output: PhantomData<T>,
}

impl<'a, P, T> Parser<'a, Vec<T>> for Many1<P, T>
where
    P: Parser<'a, T>,
    T: Clone,
{
    fn parse(&self, input: &'a str) -> ParseResult<'a, Vec<T>> {
        match self.parser.parse(input) {
            Ok((result, rest)) => {
                let mut results = vec![result];
                let mut current_rest = rest;
                
                loop {
                    match self.parser.parse(current_rest) {
                        Ok((result, new_rest)) => {
                            results.push(result);
                            current_rest = new_rest;
                        }
                        Err(_) => break,
                    }
                }
                
                Ok((results, current_rest))
            }
            Err(err) => Err(err),
        }
    }
}

// Opt parser: makes a parser optional
pub struct Opt<P, T> {
    parser: P,
    _output: PhantomData<T>,
}

impl<'a, P, T> Parser<'a, Option<T>> for Opt<P, T>
where
    P: Parser<'a, T>,
    T: Clone,
{
    fn parse(&self, input: &'a str) -> ParseResult<'a, Option<T>> {
        match self.parser.parse(input) {
            Ok((result, rest)) => Ok((Some(result), rest)),
            Err(_) => Ok((None, input)),
        }
    }
}

// Between parser: runs a parser between two other parsers
pub struct Between<P, P1, P2, T, U, V> {
    parser: P,
    open: P1,
    close: P2,
    _output: PhantomData<T>,
    _open_output: PhantomData<U>,
    _close_output: PhantomData<V>,
}

impl<'a, P, P1, P2, T, U, V> Parser<'a, T> for Between<P, P1, P2, T, U, V>
where
    P: Parser<'a, T>,
    P1: Parser<'a, U>,
    P2: Parser<'a, V>,
{
    fn parse(&self, input: &'a str) -> ParseResult<'a, T> {
        match self.open.parse(input) {
            Ok((_, rest1)) => {
                match self.parser.parse(rest1) {
                    Ok((result, rest2)) => {
                        match self.close.parse(rest2) {
                            Ok((_, rest3)) => Ok((result, rest3)),
                            Err(err) => Err(err),
                        }
                    }
                    Err(err) => Err(err),
                }
            }
            Err(err) => Err(err),
        }
    }
}

// WithContext parser: adds context to error messages
pub struct WithContext<P, F, T> {
    parser: P,
    context: F,
    _output: PhantomData<T>,
}

impl<'a, P, F, T> Parser<'a, T> for WithContext<P, F, T>
where
    P: Parser<'a, T>,
    F: Fn() -> String,
{
    fn parse(&self, input: &'a str) -> ParseResult<'a, T> {
        match self.parser.parse(input) {
            Ok(result) => Ok(result),
            Err(err) => {
                Err(ParseError {
                    message: format!("{}: {}", (self.context)(), err.message),
                    position: err.position,
                })
            }
        }
    }
}

// Helper functions to create parsers

// Match a specific character
pub fn char(c: char) -> Char {
    Char::new(c)
}

// Match a specific string
pub fn string<'s>(s: &'s str) -> Str<'s> {
    Str::new(s)
}

// Match a digit character
pub fn digit() -> Satisfy<impl Fn(char) -> bool> {
    Satisfy::new(|c| c.is_ascii_digit(), "digit")
}

// Match a letter character
pub fn letter() -> Satisfy<impl Fn(char) -> bool> {
    Satisfy::new(|c| c.is_ascii_alphabetic(), "letter")
}

// Match an alphanumeric character
pub fn alphanumeric() -> Satisfy<impl Fn(char) -> bool> {
    Satisfy::new(|c| c.is_ascii_alphanumeric(), "alphanumeric")
}

// Match whitespace
pub fn whitespace() -> Satisfy<impl Fn(char) -> bool> {
    Satisfy::new(|c| c.is_whitespace(), "whitespace")
}

// Parse an integer
pub fn integer() -> impl Parser<'static, i64> {
    let digits = digit().many1();
    
    digits.map(|digits| {
        let s: String = digits.into_iter().collect();
        s.parse::<i64>().unwrap()
    })
}

// Example parser for a simple expression language
fn parse_expr<'a>() -> impl Parser<'a, i64> {
    // Forward reference for recursive parsers
    let expr = Rc::new(RefCell::new(None));
    
    // Parse a number
    let number = integer().with_context(|| "parsing number".to_string());
    
    // Parse parenthesized expressions
    let lparen = char('(');
    let rparen = char(')');
    let parens = expr.clone()
        .with_context(|| "parsing parenthesized expression".to_string())
        .between(lparen, rparen);
    
    // Parse a term (number or parenthesized expression)
    let term = number.or(parens);
    
    // Parse operators
    let plus = char('+');
    let minus = char('-');
    let times = char('*');
    let divide = char('/');
    
    // Parse a factor (term followed by * or / and another factor, or just a term)
    let factor_parser = Rc::new(RefCell::new(None));
    
    let factor_op = term.pair(
        (times.or(divide)).pair(factor_parser.clone())
    ).map(|(left, (op, right))| {
        match op {
            '*' => left * right,
            '/' => left / right,
            _ => unreachable!(),
        }
    });
    
    let factor = factor_op.or(term);
    *factor_parser.borrow_mut() = Some(factor.clone());
    
    // Parse an expression (factor followed by + or - and another expression, or just a factor)
    let expr_op = factor.pair(
        (plus.or(minus)).pair(expr.clone())
    ).map(|(left, (op, right))| {
        match op {
            '+' => left + right,
            '-' => left - right,
            _ => unreachable!(),
        }
    });
    
    let expr_parser = expr_op.or(factor);
    *expr.borrow_mut() = Some(expr_parser.clone());
    
    expr_parser
}

// Example of a JSON parser
mod json {
    use super::*;
    
    #[derive(Debug, Clone)]
    pub enum JsonValue {
        Null,
        Boolean(bool),
        Number(f64),
        String(String),
        Array(Vec<JsonValue>),
        Object(Vec<(String, JsonValue)>),
    }
    
    // Parse JSON null
    pub fn null<'a>() -> impl Parser<'a, JsonValue> {
        string("null").map(|_| JsonValue::Null)
    }
    
    // Parse JSON boolean
    pub fn boolean<'a>() -> impl Parser<'a, JsonValue> {
        string("true").map(|_| JsonValue::Boolean(true))
            .or(string("false").map(|_| JsonValue::Boolean(false)))
    }
    
    // Parse JSON number
    pub fn number<'a>() -> impl Parser<'a, JsonValue> {
        // This is a simplified number parser
        let integer_part = digit().many1();
        let fraction_part = char('.').right(digit().many1()).opt();
        
        integer_part.pair(fraction_part).map(|(int_digits, frac_digits)| {
            let mut s = int_digits.iter().collect::<String>();
            
            if let Some(frac) = frac_digits {
                s.push('.');
                s.push_str(&frac.iter().collect::<String>());
            }
            
            JsonValue::Number(s.parse::<f64>().unwrap())
        })
    }
    
    // Parse JSON string
    pub fn string_literal<'a>() -> impl Parser<'a, String> {
        // This is a simplified string parser that doesn't handle all escapes
        let quote = char('"');
        let escape = char('\\').right(
            char('"').or(char('\\')).or(char('/')).or(char('b')).or(char('f'))
                .or(char('n')).or(char('r')).or(char('t'))
        );
        
        let string_char = escape.or(
            Satisfy::new(|c| c != '"' && c != '\\', "string character")
        );
        
        string_char.many().between(quote, quote)
            .map(|chars| chars.iter().collect::<String>())
    }
    
    // Parse JSON string value
    pub fn string<'a>() -> impl Parser<'a, JsonValue> {
        string_literal().map(JsonValue::String)
    }
    
    // Forward declarations for recursive types
    pub fn value<'a>() -> impl Parser<'a, JsonValue> {
        let value_parser = Rc::new(RefCell::new(None));
        
        // JSON array
        let array = {
            let lbracket = char('[');
            let rbracket = char(']');
            let comma = char(',');
            let ws = whitespace().many();
            
            let elements = value_parser.clone()
                .pair(
                    ws.right(comma)
                        .right(ws)
                        .right(value_parser.clone())
                        .many()
                )
                .opt()
                .map(|opt| {
                    match opt {
                        Some((first, rest)) => {
                            let mut elements = vec![first];
                            elements.extend(rest);
                            elements
                        }
                        None => vec![],
                    }
                });
            
            ws.right(elements).left(ws)
                .between(lbracket, rbracket)
                .map(JsonValue::Array)
        };
        
        // JSON object
        let object = {
            let lbrace = char('{');
            let rbrace = char('}');
            let comma = char(',');
            let colon = char(':');
            let ws = whitespace().many();
            
            let pair = string_literal()
                .left(ws)
                .left(colon)
                .left(ws)
                .pair(value_parser.clone());
            
            let pairs = pair
                .pair(
                    ws.right(comma)
                        .right(ws)
                        .right(pair)
                        .many()
                )
                .opt()
                .map(|opt| {
                    match opt {
                        Some((first, rest)) => {
                            let mut pairs = vec![first];
                            pairs.extend(rest);
                            pairs
                        }
                        None => vec![],
                    }
                });
            
            ws.right(pairs).left(ws)
                .between(lbrace, rbrace)
                .map(JsonValue::Object)
        };
        
        // JSON value (any valid JSON value)
        let parser = null()
            .or(boolean())
            .or(number())
            .or(string())
            .or(array)
            .or(object);
        
        *value_parser.borrow_mut() = Some(parser.clone());
        
        parser
    }
}

fn main() {
    println!("=== Advanced Parser Combinators ===");
    
    // Example 1: Basic parsers
    println!("\n--- Basic Parsers ---");
    
    let parser = char('a');
    match parser.parse("abc") {
        Ok((c, rest)) => println!("Parsed: '{}', Rest: '{}'", c, rest),
        Err(e) => println!("Error: {}", e),
    }
    
    let parser = string("hello");
    match parser.parse("hello world") {
        Ok((s, rest)) => println!("Parsed: '{}', Rest: '{}'", s, rest),
        Err(e) => println!("Error: {}", e),
    }
    
    // Example 2: Combinators
    println!("\n--- Combinators ---");
    
    let parser = char('a').or(char('b'));
    match parser.parse("abc") {
        Ok((c, rest)) => println!("Parsed one of 'a' or 'b': '{}', Rest: '{}'", c, rest),
        Err(e) => println!("Error: {}", e),
    }
    
    let parser = char('a').pair(char('b'));
    match parser.parse("abc") {
        Ok(((a, b), rest)) => println!("Parsed pair: '{}' and '{}', Rest: '{}'", a, b, rest),
        Err(e) => println!("Error: {}", e),
    }
    
    // Example 3: Many and Many1
    println!("\n--- Many and Many1 ---");
    
    let parser = digit().many();
    match parser.parse("123abc") {
        Ok((digits, rest)) => {
            let s: String = digits.iter().collect();
            println!("Parsed many digits: '{}', Rest: '{}'", s, rest);
        }
        Err(e) => println!("Error: {}", e),
    }
    
    let parser = letter().many1();
    match parser.parse("abc123") {
        Ok((letters, rest)) => {
            let s: String = letters.iter().collect();
            println!("Parsed many1 letters: '{}', Rest: '{}'", s, rest);
        }
        Err(e) => println!("Error: {}", e),
    }
    
    // Example 4: Integer parser
    println!("\n--- Integer Parser ---");
    
    let parser = integer();
    match parser.parse("123abc") {
        Ok((n, rest)) => println!("Parsed integer: {}, Rest: '{}'", n, rest),
        Err(e) => println!("Error: {}", e),
    }
    
    // Example 5: Expression parser
    println!("\n--- Expression Parser ---");
    
    let parser = parse_expr();
    
    let inputs = [
        "123",
        "123+456",
        "123+456*789",
        "(123+456)*789",
        "1+2*3+4",
    ];
    
    for input in inputs {
        match parser.parse(input) {
            Ok((result, rest)) => {
                if rest.is_empty() {
                    println!("Parsed expression '{}' = {}", input, result);
                } else {
                    println!("Parsed expression '{}' = {}, but had leftover input: '{}'", input, result, rest);
                }
            }
            Err(e) => println!("Error parsing '{}': {}", input, e),
        }
    }
    
    // Example 6: JSON parser
    println!("\n--- JSON Parser ---");
    
    let parser = json::value();
    
    let json_inputs = [
        "null",
        "true",
        "123",
        "\"hello\"",
        "[1, 2, 3]",
        "{\"name\": \"John\", \"age\": 30}",
        "{\"person\": {\"name\": \"John\", \"hobbies\": [\"reading\", \"coding\"]}}",
    ];
    
    for input in json_inputs {
        match parser.parse(input) {
            Ok((result, rest)) => {
                if rest.is_empty() {
                    println!("Parsed JSON: {:?}", result);
                } else {
                    println!("Parsed JSON: {:?}, but had leftover input: '{}'", result, rest);
                }
            }
            Err(e) => println!("Error parsing JSON '{}': {}", input, e),
        }
    }
}
