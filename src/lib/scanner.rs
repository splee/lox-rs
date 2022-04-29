use crate::lib::err::LoxError;
use phf::phf_map;
use std::iter::Peekable;
use std::str::Chars;
use tracing::{debug, error, info, warn, instrument};

#[derive(Debug, Clone, PartialEq)]
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
}

impl TokenType {

    pub fn matches(&self, t: &TokenType) -> bool {
        let pair = (self, t);
        matches!(pair, (this, that) if this == that)
    }
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

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,
    pub line: usize,
    pub line_pos: usize,
}

#[derive(Debug)]
struct Scanner<'a> {
    chars: Peekable<Chars<'a>>,
    current: usize,
    line: usize,
    line_pos: usize,
    tokens: Vec<Token>,
}

impl<'a> Scanner<'a> {
    fn new(source: &'a str) -> Self {
        let chars: Peekable<Chars<'a>> = source.chars().peekable();
        Scanner {
            chars,
            current: 0,
            line_pos: 0,
            line: 1,
            tokens: Vec::new(),
        }
    }

    fn advance(&mut self) -> Option<char> {
        match self.chars.next() {
            Some(v) => {
                self.current += 1;
                self.line_pos += 1;
                Some(v)
            }
            None => None,
        }
    }

    fn peek_match(&mut self, val: char) -> bool {
        match self.chars.peek() {
            Some(v) => {
                val == *v
            }
            None => false,
        }
    }

    #[instrument(skip(self))]
    fn scan_next(&mut self) -> Result<(), LoxError> {
        let c = match self.advance() {
            Some(v) => v,
            None => return Err(LoxError::Syntax {
                line: self.line,
                line_pos: self.line_pos,
                message: "Source is fully consumed; expected more input.".to_owned(),
            }),
        };
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
            '!' => {
                if self.peek_match('=') {
                    TokenType::BangEqual
                } else {
                    TokenType::Bang
                }
            }
            '=' => {
                if self.peek_match('=') {
                    TokenType::EqualEqual
                } else {
                    TokenType::Equal
                }
            }
            '<' => {
                if self.peek_match('=') {
                    TokenType::LessEqual
                } else {
                    TokenType::Less
                }
            }
            '>' => {
                if self.peek_match('=') {
                    TokenType::GreaterEqual
                } else {
                    TokenType::Greater
                }
            }
            '/' => {
                if self.peek_match('/') {
                    TokenType::Comment
                } else {
                    TokenType::Slash
                }
            }
            ' ' => TokenType::Whitespace,
            '\r' => TokenType::Whitespace,
            '\t' => TokenType::Whitespace,
            '\n' => {
                self.line += 1;
                self.line_pos = 0;
                TokenType::Newline
            }
            '"' => TokenType::Str,
            default => {
                if is_numeric(default) {
                    TokenType::Numeric
                } else if is_alpha(default) {
                    TokenType::IdentifierOrKeyword
                } else {
                    // Strings and chars and byte-buffers, oh my!
                    let mut b = [0; 4];
                    let charstr = c.encode_utf8(&mut b);
                    error!(character = charstr, line_pos = line_pos, line = line, "Unknown Token");
                    return Err(LoxError::Syntax { line, line_pos, message: format!("Unknown token: {}", c) });
                }
            }
        };
        let lexeme: String = {
            match &token_type {
                TokenType::Str => self.consume_string()?,
                TokenType::Comment => self.consume_comment()?,
                TokenType::IdentifierOrKeyword => self.consume_identifier_or_keyword(c)?,
                TokenType::Numeric => self.consume_numeric(c)?,
                // Unwrap should not fail as it has been peek()'d.
                TokenType::BangEqual
                | TokenType::EqualEqual
                | TokenType::LessEqual
                | TokenType::GreaterEqual => String::from_iter(vec![c, self.advance().unwrap()]),
                _ => String::from(c),
            }
        };
        // Now we have the lexeme, we can resolve IdentifierOrKeyword if needed.
        let should_resolve = matches!(&token_type, TokenType::IdentifierOrKeyword);
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
        debug!(?token.token_type, token.line_pos, token.lexeme = token.lexeme.as_str());
        self.tokens.push(token);
        Ok(())
    }

    fn consume_until(&mut self, breaker: char) -> Result<String, LoxError> {
        let mut content: Vec<char> = Vec::new();
        while let Some(v) = self.chars.peek() {
            if *v == breaker {
                break;
            }
            // Should not fail, guarded by peek()
            content.push(self.advance().unwrap());
        }
        Ok(String::from_iter(content))
    }

    fn consume_comment(&mut self) -> Result<String, LoxError> {
        // Skip the second slash
        let _ = self.advance();
        // Consume the rest of the line.
        let content = self.consume_until('\n')?;
        // Update line specific info
        self.line += 1;
        self.line_pos = 0;
        Ok(content)
    }

    fn consume_string(&mut self) -> Result<String, LoxError> {
        let content = self.consume_until('"')?;
        // Also consume the closing double-quote
        let _ = self.advance();
        Ok(content)
    }

    fn consume_numeric(&mut self, first_char: char) -> Result<String, LoxError> {
        let mut content: Vec<char> = vec![first_char];
        while let Some(vref) = self.chars.peek() {
            let v = *vref;
            if is_numeric(v) || v == '.' {
                // As chars.peek() gave a result, this should not fail.
                content.push(self.advance().unwrap());
            } else {
                break;
            }
        }
        let str_val = String::from_iter(content);
        // Ensure we have a valid number
        let _: f64 = match str_val.parse::<f64>() {
            Ok(v) => v,
            Err(why) => return Err(LoxError::Syntax { line: self.line, line_pos: self.line_pos, message: format!("Failed to parse numeric: {:#?}", why) }),
        };
        Ok(str_val)
    }

    fn consume_identifier_or_keyword(&mut self, first_char: char) -> Result<String, LoxError> {
        let mut content: Vec<char> = vec![first_char];
        while let Some(vref) = self.chars.peek() {
            let v = *vref;
            if is_alpha_numeric(v) || v == '.' {
                content.push(self.advance().unwrap());
            } else {
                break;
            }
        }
        Ok(String::from_iter(content))
    }
}

#[instrument(skip(source))]
pub fn scan(source: &str) -> Result<Vec<Token>, LoxError> {
    let mut scanner = Scanner::new(source);
    while let Ok(()) = scanner.scan_next() {
        // just consume the whole thing
        continue
    }
    info!("Finished scanning tokens");
    // Do the final whitespace filtering
    let filtered_tokens: Vec<Token> = scanner
        .tokens
        .into_iter()
        .filter(|t| !matches!(t.token_type, TokenType::Whitespace | TokenType::Newline))
        .collect();
    debug!("Filtered whitespace");

    Ok(filtered_tokens)
}

fn is_numeric(c: char) -> bool {
    c.is_digit(10)
}

fn is_alpha(c: char) -> bool {
    ('a'..='z').contains(&c) || ('A'..='Z').contains(&c) || c == '_'
}

fn is_alpha_numeric(c: char) -> bool {
    is_alpha(c) || is_numeric(c)
}

#[cfg(test)]
mod tests {
    use super::*;
    use anyhow::{bail, Result};

    #[test]
    fn test_whitespace_is_filtered() -> Result<()> {
        let src = "\r\t ";
        let tokens = match scan(src) {
            Ok(v) => v,
            Err(why) => bail!(why),
        };
        assert_eq!(tokens.len(), 0);
        Ok(())
    }

    #[test]
    fn test_keywords_are_identified() -> Result<()> {
        let keyword_soup = KEYWORDS.keys()
            .map(|k| &**k)
            .collect::<Vec<&str>>()
            .join(" ");

        let tokens = match scan(&keyword_soup) {
            Ok(v) => v,
            Err(why) => bail!(why),
        };
        assert_eq!(tokens.len(), KEYWORDS.len());
        for t in tokens {
            let expected = match KEYWORDS.get(&t.lexeme) {
                Some(t) => t,
                None => bail!("Unable to find expected token type for keyword '{}'", t.lexeme),
            };
            assert_eq!(&t.token_type, expected);
        }
        Ok(())
    }

    #[test]
    fn test_single_and_double_char_tokens() -> Result<()> {
        let token_types = [
            ("(", &TokenType::LeftParen),
            (")", &TokenType::RightParen),
            ("{", &TokenType::LeftBrace),
            ("}", &TokenType::RightBrace),
            (",", &TokenType::Comma),
            (".", &TokenType::Dot),
            ("-", &TokenType::Minus),
            ("+", &TokenType::Plus),
            (";", &TokenType::Semicolon),
            ("/", &TokenType::Slash),
            ("*", &TokenType::Star),
            ("!", &TokenType::Bang),
            ("!=", &TokenType::BangEqual),
            ("=", &TokenType::Equal),
            ("==", &TokenType::EqualEqual),
            (">", &TokenType::Greater),
            (">=", &TokenType::GreaterEqual),
            ("<", &TokenType::Less),
            ("<=", &TokenType::LessEqual),
        ];
        let token_soup = &token_types.into_iter()
            .map(|(l, _t)| l)
            .collect::<Vec<&str>>()
            .join(" ");
        
        let tokens = match scan(&token_soup) {
            Ok(v) => v,
            Err(why) => bail!(why),
        };

        // TODO: Figure out enumerate for iterators of tuples.
        let mut i: usize = 0;
        for (lexeme, ttype) in &token_types {
            let token = match tokens.get(i) {
                Some(t) => t,
                None => bail!("Expecting {:?} at idx {}", ttype, i),
            };
            assert_eq!(*lexeme, &token.lexeme);
            assert_eq!(*ttype, &token.token_type);

            i += 1;
        }
        Ok(())
    }

    #[test]
    fn test_str() -> Result<()> {
        let str_str = "\"This is a string.\"";
        let tokens = match scan(str_str) {
            Ok(v) => v,
            Err(why) => bail!(why),
        };
        let t = match tokens.first() {
            Some(t) => t,
            None => bail!("No tokens returned!"),
        };
        assert_eq!(&t.token_type, &TokenType::Str);
        assert_eq!(&t.lexeme, "This is a string.");
        Ok(())
    }
     
    #[test]
    fn test_numerics() -> Result<()> {
        let nums = ["12345", "123.45"];
        let num_str = nums.join(" ");
        let tokens = match scan(&num_str) {
            Ok(v) => v,
            Err(why) => bail!(why),
        };
        let mut i = 0usize;
        for expected_num in nums {
            let t = match tokens.get(i) {
                Some(t) => t,
                None => bail!("Expected token at idx {}", i),
            };
            assert_eq!(expected_num, &t.lexeme);
            assert_eq!(&TokenType::Numeric, &t.token_type);
            i += 1;
        }
        Ok(())
    }

    #[test]
    fn test_comments() -> Result<()> {
        let mixed_str = "12345 // This is a test comment\n//Full line comment.";
        let tokens = match scan(mixed_str) {
            Ok(v) => v,
            Err(why) => bail!(why),
        };
        let expected = [
            ("12345", &TokenType::Numeric),
            // TODO: Should we be trimming the leading whitespace?
            (" This is a test comment", &TokenType::Comment),
            ("Full line comment.", &TokenType::Comment),
        ];

        let mut i = 0;
        for (lexeme, ttype) in expected {
            let t = match tokens.get(i) {
                Some(t) => t,
                None => bail!("Failed to find expected token at idx {}", i)
            };
            assert_eq!(&t.lexeme, lexeme);
            assert_eq!(&t.token_type, ttype);
            i += 1;
        }
        Ok(())
    }
}
