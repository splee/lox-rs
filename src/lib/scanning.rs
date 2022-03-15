use anyhow::{bail, Result};
use std::str::Chars;
use phf::phf_map;

#[derive(Debug, Clone)]
pub enum TokenType {
    // Single Character Tokens
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    // One or two character tokens
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals
    Identifier,
    Str,
    Numeric,
    Comment,

    // Keywords
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    // Internal tokens that should be ignored
    IdentifierOrKeyword,

    Whitespace,
    Newline,

    Eof,
}

static KEYWORDS: phf::Map<&'static str, TokenType> = phf_map! {
    "and" => TokenType::And,
    "class" => TokenType::Class,
    "else" => TokenType::Else,
    "false" => TokenType::False,
    "fun" => TokenType::Fun,
    "for" => TokenType::For,
    "if" => TokenType::If,
    "nil" => TokenType::Nil,
    "or" => TokenType::Or,
    "print" => TokenType::Print,
    "return" => TokenType::Return,
    "super" => TokenType::Super,
    "this" => TokenType::This,
    "true" => TokenType::True,
    "var" => TokenType::Var,
    "while" => TokenType::While,
};

#[derive(Debug)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,
    pub line: usize,
    pub line_pos: usize,
}

pub struct Scanner<'a> {
    chars: std::iter::Peekable<Chars<'a>>,
    current: usize,
    line: usize,
    line_pos: usize,
    tokens: Vec<Token>,
}

impl<'a> Scanner<'a> {
    fn new(source: &'a str) -> Self {
        let chars: std::iter::Peekable<Chars<'a>> = source.chars().peekable();
        Scanner {
            chars: chars,
            current: 0,
            line_pos: 0,
            line: 1,
            tokens: Vec::new(),
        }
    }

    fn is_finished(&mut self) -> bool {
        match self.chars.peek() {
            Some(_) => false,
            None => true,
        }
    }

    fn advance(&mut self) -> Result<char> {
        match self.chars.next() {
            Some(v) => {
                self.current += 1;
                self.line_pos += 1;
                Ok(v)
            },
            None => bail!("Source consumed."),
        }
    }

    fn peek_match(&mut self, val: char) -> bool {
        match self.chars.peek() {
            Some(v) => {
                if val == *v {
                    true
                } else {
                    false
                }
            },
            None => false,
        }
    }

    fn scan_next(&mut self) -> Result<()> {
        let c = self.advance()?;
        // collect metadata about our position in the source script.
        let line = self.line;
        let line_pos = self.line_pos;
        // Mutable to allow us to refine TokenType::IdentifierOrKeyword
        let mut token_type = match c {
            '(' => TokenType::LeftParen,
            ')' => TokenType::RightParen,
            '{' => TokenType::LeftBrace,
            '}' => TokenType::RightBrace,
            ',' => TokenType::Comma,
            '.' => TokenType::Dot,
            '-' => TokenType::Minus,
            '+' => TokenType::Plus,
            ';' => TokenType::Semicolon,
            '*' => TokenType::Star,
            // 
            '!' => if self.peek_match('=') { TokenType::BangEqual } else { TokenType::Bang },
            '=' => if self.peek_match('=') { TokenType::EqualEqual } else { TokenType::Equal },
            '<' => if self.peek_match('=') { TokenType::LessEqual } else { TokenType::Less },
            '>' => if self.peek_match('=') { TokenType::GreaterEqual } else { TokenType::Greater },
            '/' => if self.peek_match('/') { TokenType::Comment } else { TokenType::Slash },
            ' ' => TokenType::Whitespace,
            '\r' => TokenType::Whitespace,
            '\t' => TokenType::Whitespace,
            '\n' => {
                self.line += 1;
                self.line_pos = 0;
                TokenType::Newline
            },
            '"' => TokenType::Str,
            default => {
                if is_numeric(default) {
                    TokenType::Numeric
                } else if is_alpha(default) {
                    TokenType::IdentifierOrKeyword
                } else {
                    bail!("Unknown token: {}", c);
                }
            }
        };
        let lexeme: String = {
            match &token_type {
               TokenType::Str => self.consume_string()?,
               TokenType::Comment => self.consume_comment()?,
               TokenType::IdentifierOrKeyword => self.consume_identifier_or_keyword(c)?,
               TokenType::Numeric => String::from(self.consume_numeric(c)?),
               TokenType::BangEqual | TokenType::EqualEqual | TokenType::LessEqual | TokenType::GreaterEqual => String::from_iter(vec![c, self.advance()?]),
               _ => String::from(c),           }
        };
        // Now we have the lexeme, we can resolve IdentifierOrKeyword if needed.
        let should_resolve = match &token_type {
            TokenType::IdentifierOrKeyword => true,
            _ => false,
        };
        if should_resolve {
            token_type = match KEYWORDS.get(&lexeme).cloned() {
                Some(t) => t,
                None => TokenType::Identifier,
            }
        }

        let token = Token {
            token_type,
            lexeme,
            line,
            line_pos,
        };
        self.tokens.push(token);
        Ok(())
    }

    fn consume_until(&mut self, breaker: char) -> Result<String> {
        let mut content: Vec<char> = Vec::new();
        loop {
            match self.chars.peek() {
                Some(v) => {
                    if *v == breaker {
                        break;
                    }
                    content.push(self.advance()?);
                },
                None => { break; }
            }
        }
        Ok(String::from_iter(content))
    }

    fn consume_comment(&mut self) -> Result<String> {
        // Consume the rest of the line.
        let content = self.consume_until('\n')?;
        // Update line specific info
        self.line += 1;
        self.line_pos = 0;
        Ok(content)
    }

    fn consume_string(&mut self) -> Result<String> {
        let content = self.consume_until('"')?;
        // Also consume the closing double-quote
        let _ = self.advance()?;
        Ok(content)
    }

    fn consume_numeric(&mut self, first_char: char) -> Result<String> {
        let mut content: Vec<char> = vec!(first_char);
        loop {
            match self.chars.peek() {
                Some(vref) => {
                    let v = *vref;
                    if is_numeric(v) || v == '.' {
                        content.push(self.advance()?);
                    } else {
                        break;
                    }
                },
                None => { break; }
            }
        }
        let str_val = String::from_iter(content);
        // Ensure we have a valid number
        let _: f64 = match str_val.parse::<f64>() {
            Ok(v) => v,
            Err(why) => bail!("Failed to parse numeric: {:#?}", why),
        };
        Ok(str_val)
    }

    fn consume_identifier_or_keyword(&mut self, first_char: char) -> Result<String> {
        let mut content: Vec<char> = vec!(first_char);
        loop {
            match self.chars.peek() {
                Some(vref) => {
                    let v = *vref;
                    if is_alpha_numeric(v) {
                        content.push(self.advance()?)
                    } else {
                        break;
                    }
                },
                None => { break; },
            }
        }
        Ok(String::from_iter(content))
    }
}


pub fn scan(source: &str) -> Result<Vec<Token>> {
    let mut scanner = Scanner::new(source);
    loop {
        match scanner.scan_next() {
            Ok(()) => continue,
            Err(why) => {
                println!("Scanner finished: {:#?}", why);
                break;
            }
        }
    }
    Ok(scanner.tokens)
}

fn is_numeric(c: char) -> bool {
    c.is_digit(10)
}

fn is_alpha(c: char) -> bool {
    (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'
}

fn is_alpha_numeric(c: char) -> bool {
    is_alpha(c) || is_numeric(c)
}