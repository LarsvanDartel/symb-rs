use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{anychar, digit0, digit1, multispace0, one_of},
    combinator::{peek, verify},
    sequence::{preceded, separated_pair, terminated},
    IResult, Parser as _,
};

use std::str::FromStr;

use super::{literals, Action, Expression, Number, Function};

type ParseResult<'a, T> = IResult<&'a str, T>;

#[derive(Debug, Clone, PartialEq)]
enum TokenType {
    EndOfFile,
    Error,
    Number,
    Variable,
    Function,
    Constant,
    Add,
    Sub,
    Mul,
    Div,
    Pow,
    LeftParenthesis,
    RightParenthesis,
    Comma,
    Equals,
}

#[derive(Clone, PartialEq)]
struct Token {
    token: TokenType,
    lexeme: Option<String>,
}

impl Token {
    fn new<T: ToString>(token: TokenType, lexeme: T) -> Self {
        Self {
            token,
            lexeme: Some(lexeme.to_string()),
        }
    }

    fn parse(input: &str) -> ParseResult<Self> {
        alt((
            Self::parse_number,
            Self::parse_function,
            Self::parse_constant,
            verify(anychar, |c: &char| c.is_ascii_lowercase())
                .map(|c| Token::new(TokenType::Variable, c)),
            tag(literals::ADD).map(|_| TokenType::Add.into()),
            tag(literals::SUB).map(|_| TokenType::Sub.into()),
            tag(literals::MUL).map(|_| TokenType::Mul.into()),
            tag(literals::DIV).map(|_| TokenType::Div.into()),
            tag(literals::POW).map(|_| TokenType::Pow.into()),
            tag(literals::COMMA).map(|_| TokenType::Comma.into()),
            tag(literals::EQUALS).map(|_| TokenType::Equals.into()),
            one_of(literals::LEFT_PARENTHESES).map(|c| Token::new(TokenType::LeftParenthesis, c)),
            one_of(literals::RIGHT_PARENTHESES).map(|c| Token::new(TokenType::RightParenthesis, c)),
            anychar
                .map(|c| Token::new(TokenType::Error, format!("Unrecognized character: '{}'", c))),
        ))
        .parse(input)
    }

    fn parse_number(input: &str) -> ParseResult<Self> {
        alt((
            separated_pair(digit0, tag("."), digit1)
                .map(|(a, b)| Token::new(TokenType::Number, format!("{}.{}", a, b))),
            digit1.map(|a: &str| Token::new(TokenType::Number, a)),
        ))
        .parse(input)
    }

    fn parse_function(input: &str) -> ParseResult<Self> {
        for &f in literals::FUNCTIONS.iter() {
            if let Ok((i, _)) = terminated(
                tag::<&str, &str, nom::error::Error<&str>>(f),
                peek(one_of(literals::LEFT_PARENTHESES)),
            )
            .parse(input)
            {
                return Ok((i, Token::new(TokenType::Function, f)));
            }
        }

        Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Tag,
        )))
    }

    fn parse_constant(input: &str) -> ParseResult<Token> {
        for &c in literals::CONSTANTS.iter() {
            if let Ok((i, _)) = tag::<&str, &str, nom::error::Error<&str>>(c).parse(input) {
                return Ok((i, Token::new(TokenType::Constant, c)));
            }
        }

        Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Tag,
        )))
    }
}

impl From<TokenType> for Token {
    fn from(token: TokenType) -> Self {
        Self {
            token,
            lexeme: None,
        }
    }
}

impl std::fmt::Debug for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(lexeme) = &self.lexeme {
            f.write_fmt(format_args!("{:?}({})", self.token, lexeme))
        } else {
            f.write_fmt(format_args!("{:?}", self.token))
        }
    }
}

pub(crate) struct Parser {
    text: String,
    current_token: Option<Token>,
}

impl Parser {
    pub fn new<T: ToString>(text: T) -> Self {
        Parser {
            text: text.to_string(),
            current_token: None,
        }
    }

    pub fn parse(mut self) -> Result<Expression, String> {
        self.advance()?;

        let mut expr = self.parse_expression()?;
        if let Some(token) = &self.current_token {
            if token.token == TokenType::Equals {
                self.eat(TokenType::Equals)?;
                expr = Expression::new_binary(expr, self.parse_expression()?, Action::Equals);
            }
        }
        if let Some(token) = &self.current_token {
            if token.token != TokenType::EndOfFile {
                return Err(format!(
                    "Expected EOF, found {:?}",
                    self.current_token.clone().unwrap()
                ));
            }
        }
        Ok(expr)
    }

    fn parse_expression(&mut self) -> Result<Expression, String> {
        let mut expr = self.parse_term()?;
        while let Some(token) = &self.current_token {
            match token.token {
                TokenType::Add => {
                    self.eat(TokenType::Add)?;
                    expr = Expression::new_binary(expr, self.parse_term()?, Action::Add);
                }
                TokenType::Sub => {
                    self.eat(TokenType::Sub)?;
                    expr = Expression::new_binary(expr, self.parse_term()?, Action::Sub);
                }
                _ => break,
            }
        }
        Ok(expr)
    }

    fn parse_term(&mut self) -> Result<Expression, String> {
        let mut expr = self.parse_factor()?;
        while let Some(token) = &self.current_token {
            match token.token {
                TokenType::Mul => {
                    self.eat(TokenType::Mul)?;
                    expr = Expression::new_binary(expr, self.parse_factor()?, Action::Mul);
                }
                TokenType::Div => {
                    self.eat(TokenType::Div)?;
                    expr = Expression::new_binary(expr, self.parse_factor()?, Action::Div);
                }
                TokenType::Function
                | TokenType::Variable
                | TokenType::Constant
                | TokenType::LeftParenthesis => {
                    expr = Expression::new_binary(expr, self.parse_factor()?, Action::Mul);
                }
                _ => break,
            }
        }
        Ok(expr)
    }

    fn parse_factor(&mut self) -> Result<Expression, String> {
        let mut expr = self.parse_primary()?;
        while let Some(token) = &self.current_token {
            if token.token != TokenType::Pow {
                break;
            }
            self.eat(TokenType::Pow)?;
            expr = Expression::new_binary(expr, self.parse_factor()?, Action::Pow);
        }
        Ok(expr)
    }

    fn parse_primary(&mut self) -> Result<Expression, String> {
        if self.current_token.is_none() {
            Err("Expected primary, found None")?;
        }
        let token = self.current_token.clone().unwrap();

        match token.token {
            TokenType::Variable => {
                let cur = self.eat(TokenType::Variable)?;
                Ok(Expression::create_variable(cur.lexeme.unwrap()))
            }
            TokenType::Number => {
                let cur = self.eat(TokenType::Number)?;
                Ok(Expression::create_value(
                    Number::from_str(&cur.lexeme.unwrap()).unwrap(),
                ))
            }
            TokenType::Function => {
                let fun = self.eat(TokenType::Function)?;
                let mut args = Vec::new();
                let left_paren = self.eat(TokenType::LeftParenthesis)?;
                args.push(self.parse_expression()?);
                while self.eat(TokenType::Comma).is_ok() {
                    args.push(self.parse_expression()?);
                }
                let right_paren = self.eat(TokenType::RightParenthesis)?;
                Self::matching_parentheses(left_paren, right_paren)?;
                let fun = Function::from_str(&fun.lexeme.unwrap())?;
                if fun.arity() != args.len() {
                    Err(format!(
                        "Expected {} arguments, found {}",
                        fun.arity(),
                        args.len()
                    ))?;
                }
                Ok(Expression::create_function(fun, args))
            }
            TokenType::Constant => {
                let c = self.eat(TokenType::Constant)?;
                Ok(Expression::create_constant(c.lexeme.unwrap()))
            }
            TokenType::LeftParenthesis => {
                let left_paren = self.eat(TokenType::LeftParenthesis)?;
                let expr = self.parse_expression()?;
                let right_paren = self.eat(TokenType::RightParenthesis)?;
                Self::matching_parentheses(left_paren, right_paren)?;
                Ok(expr)
            }
            TokenType::Sub => {
                self.eat(TokenType::Sub)?;
                if let Some(token) = &self.current_token {
                    if token.token == TokenType::Number {
                        let cur = self.eat(TokenType::Number)?;
                        return Ok(Expression::create_value(
                            Number::from_str(&format!("-{}", cur.lexeme.unwrap())).unwrap(),
                        ));
                    }
                }
                Ok(-self.parse_primary()?)
            }
            TokenType::Error => Err(token.lexeme.unwrap()),
            _ => Err(format!("Unexpected token: {:?}", self.current_token)),
        }
    }

    fn advance(&mut self) -> Result<Token, String> {
        let token = self.next_token()?;
        self.current_token = Some(token.clone());
        Ok(token)
    }

    fn eat(&mut self, token: TokenType) -> Result<Token, String> {
        if self.current_token.is_none() {
            return Err(format!("Expected {:?}, found None", token));
        }
        if self.current_token.as_ref().unwrap().token == token {
            let cur = self.current_token.clone().unwrap();
            self.advance()?;
            Ok(cur)
        } else {
            Err(format!(
                "Expected {:?}, found {:?}",
                token,
                self.current_token.clone().unwrap()
            ))
        }
    }

    fn next_token(&mut self) -> Result<Token, String> {
        if self.text.trim().is_empty() {
            return Ok(Token::from(TokenType::EndOfFile));
        }

        let text = self.text.clone();
        let r = preceded(multispace0, Token::parse)
            .parse(&text)
            .map_err(|e| e.to_string())?;

        self.text = r.0.to_string();
        Ok(r.1)
    }

    fn matching_parentheses(l: Token, r: Token) -> Result<(), String> {
        let l = l.lexeme.unwrap().chars().next().unwrap();
        let r = r.lexeme.unwrap().chars().next().unwrap();

        let m = literals::matching_parentheses(l).unwrap();

        if m != r {
            return Err(format!("Expected {:?}, found {:?}", m, r));
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn test_parser_tokens_number() {
        let mut p = Parser::new("123 0.1 .1 .");
        let t = p.next_token().unwrap();
        assert_eq!(t.token, TokenType::Number);
        assert_eq!(t.lexeme, Some(String::from("123")));

        let t = p.next_token().unwrap();
        assert_eq!(t.token, TokenType::Number);
        assert_eq!(t.lexeme, Some(String::from("0.1")));

        let t = p.next_token().unwrap();
        assert_eq!(t.token, TokenType::Number);
        assert_eq!(t.lexeme, Some(String::from(".1")));

        let t = p.next_token().unwrap();
        assert_eq!(t.token, TokenType::Error);
        assert_eq!(t.lexeme, Some(String::from("Unrecognized character: '.'")));
    }

    #[test]
    pub fn test_parser_tokens_function() {
        let mut p = Parser::new(
            literals::FUNCTIONS
                .iter()
                .map(|f| format!("{}()", f))
                .collect::<Vec<String>>()
                .join(" "),
        );

        for f in literals::FUNCTIONS.iter() {
            let t = p.next_token().unwrap();
            assert_eq!(t.token, TokenType::Function);
            assert_eq!(t.lexeme, Some(f.to_string()));

            let t = p.next_token().unwrap();
            assert_eq!(t.token, TokenType::LeftParenthesis);
            assert_eq!(t.lexeme, Some('('.to_string()));

            let t = p.next_token().unwrap();
            assert_eq!(t.token, TokenType::RightParenthesis);
            assert_eq!(t.lexeme, Some(')'.to_string()));
        }
    }

    #[test]
    pub fn test_parser_tokens_constant() {
        let mut p = Parser::new(
            literals::CONSTANTS
                .iter()
                .map(|c| format!("{}", c))
                .collect::<Vec<String>>()
                .join(" "),
        );

        for c in literals::CONSTANTS.iter() {
            let t = p.next_token().unwrap();
            assert_eq!(t.token, TokenType::Constant);
            assert_eq!(t.lexeme, Some(c.to_string()));
        }
    }

    #[test]
    pub fn test_parser_tokens_variable() {
        let mut p = Parser::new("x y z");
        let t = p.next_token().unwrap();
        assert_eq!(t.token, TokenType::Variable);
        assert_eq!(t.lexeme, Some(String::from("x")));

        let t = p.next_token().unwrap();
        assert_eq!(t.token, TokenType::Variable);
        assert_eq!(t.lexeme, Some(String::from("y")));

        let t = p.next_token().unwrap();
        assert_eq!(t.token, TokenType::Variable);
        assert_eq!(t.lexeme, Some(String::from("z")));
    }

    #[test]
    pub fn test_parser_tokens_operator() {
        let mut p = Parser::new(
            literals::OPERATORS
                .iter()
                .map(|o| format!("{} ", o))
                .collect::<Vec<String>>()
                .join(" "),
        );

        for &o in literals::OPERATORS.iter() {
            match o {
                literals::ADD => {
                    let t = p.next_token().unwrap();
                    assert_eq!(t.token, TokenType::Add);
                    assert_eq!(t.lexeme, None);
                }
                literals::SUB => {
                    let t = p.next_token().unwrap();
                    assert_eq!(t.token, TokenType::Sub);
                    assert_eq!(t.lexeme, None);
                }
                literals::MUL => {
                    let t = p.next_token().unwrap();
                    assert_eq!(t.token, TokenType::Mul);
                    assert_eq!(t.lexeme, None);
                }
                literals::DIV => {
                    let t = p.next_token().unwrap();
                    assert_eq!(t.token, TokenType::Div);
                    assert_eq!(t.lexeme, None);
                }
                literals::POW => {
                    let t = p.next_token().unwrap();
                    assert_eq!(t.token, TokenType::Pow);
                    assert_eq!(t.lexeme, None);
                }
                _ => unreachable!(),
            }
        }
    }

    #[test]
    pub fn test_parser_tokens_parenthesis() {
        let mut p = Parser::new("() [] {}");

        let t = p.next_token().unwrap();
        assert_eq!(t.token, TokenType::LeftParenthesis);
        assert_eq!(t.lexeme, Some(String::from("(")));

        let t = p.next_token().unwrap();
        assert_eq!(t.token, TokenType::RightParenthesis);
        assert_eq!(t.lexeme, Some(String::from(")")));

        let t = p.next_token().unwrap();
        assert_eq!(t.token, TokenType::LeftParenthesis);
        assert_eq!(t.lexeme, Some(String::from("[")));

        let t = p.next_token().unwrap();
        assert_eq!(t.token, TokenType::RightParenthesis);
        assert_eq!(t.lexeme, Some(String::from("]")));

        let t = p.next_token().unwrap();
        assert_eq!(t.token, TokenType::LeftParenthesis);
        assert_eq!(t.lexeme, Some(String::from("{")));

        let t = p.next_token().unwrap();
        assert_eq!(t.token, TokenType::RightParenthesis);
        assert_eq!(t.lexeme, Some(String::from("}")));
    }

    #[test]
    pub fn test_parser_tokens_comma() {
        let mut p = Parser::new(literals::COMMA);

        let t = p.next_token().unwrap();
        assert_eq!(t.token, TokenType::Comma);
        assert_eq!(t.lexeme, None);
    }

    #[test]
    pub fn test_parser_tokens_equals() {
        let mut p = Parser::new(literals::EQUALS);

        let t = p.next_token().unwrap();
        assert_eq!(t.token, TokenType::Equals);
        assert_eq!(t.lexeme, None);
    }

    #[test]
    pub fn test_parser_tokens() {
        let mut p = Parser::new("1 + 2 * 3 - 4 / 5 ^ 6 = Sin(2Pi) - 1");
        let mut tokens = Vec::new();
        loop {
            let t = p.next_token().unwrap();
            if t.token == TokenType::EOF {
                break;
            }
            tokens.push(t);
        }

        assert_eq!(
            tokens,
            vec![
                Token::new(TokenType::Number, "1"),
                Token::from(TokenType::Add),
                Token::new(TokenType::Number, "2"),
                Token::from(TokenType::Mul),
                Token::new(TokenType::Number, "3"),
                Token::from(TokenType::Sub),
                Token::new(TokenType::Number, "4"),
                Token::from(TokenType::Div),
                Token::new(TokenType::Number, "5"),
                Token::from(TokenType::Pow),
                Token::new(TokenType::Number, "6"),
                Token::from(TokenType::Equals),
                Token::new(TokenType::Function, "Sin"),
                Token::new(TokenType::LeftParenthesis, "("),
                Token::new(TokenType::Number, "2"),
                Token::new(TokenType::Constant, "Pi"),
                Token::new(TokenType::RightParenthesis, ")"),
                Token::from(TokenType::Sub),
                Token::new(TokenType::Number, "1"),
            ]
        );
    }
}
