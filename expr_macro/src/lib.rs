extern crate expr;
extern crate proc_macro;

use std::iter::Peekable;

use expr::literals;
use proc_macro2::{token_stream::IntoIter, Group, Ident, TokenStream, TokenTree};
use quote::{quote, ToTokens};

#[proc_macro]
pub fn symb(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = TokenStream::from(input);

    let mut input = input.into_iter().peekable();

    let mut symbols = Vec::new();

    while let Some(token) = input.next() {
        if let TokenTree::Ident(ident) = token {
            symbols.push(ident);
            if input.peek().is_some() {
                eat(',', &mut input);
            }
        } else {
            panic!("Expected identifier")
        }
    }

    let mut output = TokenStream::new();

    for symbol in symbols {
        let s = symbol.to_string();
        output.extend(quote! {
            let #symbol = expr::Expression::create_variable(#s);
        });
    }

    proc_macro::TokenStream::from(output)
}

fn parse_expr(input: &mut Peekable<IntoIter>, expect_group: bool) -> TokenStream {
    let token = input.next();
    if token.is_none() {
        return TokenStream::new();
    }
    let token = token.unwrap();

    if let TokenTree::Group(group) = token {
        if !expect_group {
            panic!("Unexpected group");
        }
        return parse_group(group);
    } else if expect_group {
        panic!("Expected group")
    }

    match token {
        TokenTree::Group(_) => unreachable!(),
        TokenTree::Ident(ident) => parse_ident(ident, input),
        TokenTree::Punct(punct) => {
            if punct.as_char() == '~' {
                return parse_pattern(input);
            }

            if punct.as_char() == '-' {
                if let Some(TokenTree::Literal(literal)) = input.peek() {
                    let output = quote! {
                        expr::Expression::create_value(-#literal)
                    };
                    input.next();
                    return output;
                }
            }

            let action = match punct.as_char() {
                '+' => quote! { expr::Action::Add },
                '-' => quote! { expr::Action::Sub },
                '*' => quote! { expr::Action::Mul },
                '/' => quote! { expr::Action::Div },
                '^' => quote! { expr::Action::Pow },
                c => panic!("Unexpected operator '{}'", c),
            };

            let children = parse_expr(input, true);
            if children.is_empty() {
                return action;
            }

            quote! {
                expr::Expression::new(#children, #action)
            }
        }
        TokenTree::Literal(literal) => quote! {
            expr::Expression::create_value(#literal)
        },
    }
}

fn eat(punct: char, input: &mut Peekable<IntoIter>) {
    let token = input
        .next()
        .unwrap_or_else(|| panic!("Expected '{}'", punct));
    if let TokenTree::Punct(p) = token {
        if p.as_char() != punct {
            panic!("Expected '{}', found '{}'", punct, p.as_char())
        }
    } else {
        panic!("Expected '{}', found {}", punct, token)
    }
}

fn parse_pattern(input: &mut Peekable<IntoIter>) -> TokenStream {
    let token = input.next().expect("Expected identifier after '~'");
    let name;
    let is_segment;

    if let TokenTree::Ident(ident) = token {
        name = ident.to_string();
        is_segment = false;
    } else if let TokenTree::Punct(punct) = token {
        if punct.as_char() == '~' {
            let token = input.next().expect("Expected identifier after '~'");
            if let TokenTree::Ident(ident) = token {
                name = ident.to_string();
                is_segment = true;
            } else {
                panic!("Expected identifier after '~'")
            }
        } else {
            panic!("Unexpected token: '{}'", punct.as_char())
        }
    } else {
        panic!("Expected identifier or '~'")
    }

    let mut matcher = TokenStream::new();
    if let Some(TokenTree::Punct(punct)) = input.peek() {
        if punct.as_char() == ':' {
            input.next();
            matcher.extend(quote! { &|e| });
            let predicate = parse_predicate(input);
            if !predicate.is_empty() {
                matcher.extend(quote! { #predicate });
            } else {
                matcher.extend(quote! { true });
            }
        }
    }

    if matcher.is_empty() {
        matcher = quote! { &|_| true }
    }

    let mut predicate = TokenStream::new();
    if let Some(TokenTree::Punct(punct)) = input.peek() {
        if punct.as_char() == ':' {
            input.next();
            predicate.extend(quote! { &|e| });
            predicate.extend(parse_predicate(input));
        }
    }

    if predicate.is_empty() {
        predicate = quote! { &|_| true }
    }

    if is_segment {
        quote! {
            expr::Expression::new_empty(
                expr::Action::Segment {
                    name: String::from(#name),
                    matcher: #matcher,
                    predicate: #predicate
                }
            )
        }
    } else {
        quote! {
            expr::Expression::new_empty(
                expr::Action::Slot {
                    name: String::from(#name),
                    matcher: #matcher,
                    predicate: #predicate
                }
            )
        }
    }
}

fn parse_predicate(input: &mut Peekable<IntoIter>) -> TokenStream {
    if let Some(TokenTree::Punct(punct)) = input.peek() {
        if punct.as_char() == ':' {
            return TokenStream::new();
        }
    }
    let mut token = input.next().expect("Expected identifier after ':'");
    let mut predicate = TokenStream::new();

    if let TokenTree::Group(group) = token {
        let mut group = group.stream().into_iter().peekable();
        let mut i = 0;
        while group.peek().is_some() {
            if i > 0 {
                predicate.extend(quote! { && });
            }
            predicate.extend(parse_predicate(&mut group));
            if group.peek().is_some() {
                eat(',', &mut group);
            }
            i += 1;
        }

        if i == 0 {
            panic!("Expected identifier after ':'")
        }

        return predicate;
    }

    if let TokenTree::Punct(punct) = token {
        if punct.as_char() == '!' {
            predicate.extend(quote! { ! });
            token = input.next().expect("Expected identifier after '!'");
        } else {
            panic!("Expected identifier after ':'")
        }
    }

    if let TokenTree::Ident(ident) = token {
        predicate.extend(quote! { #ident(e) });
    } else {
        panic!("Expected identifier after ':'")
    }

    predicate
}

fn parse_ident(ident: Ident, input: &mut Peekable<IntoIter>) -> TokenStream {
    if literals::FUNCTIONS.contains(&ident.to_string().as_str()) {
        let children = parse_expr(input, true);
        quote! {
            expr::Expression::create_function(expr::Function::#ident, #children)
        }
    } else if literals::CONSTANTS.contains(&ident.to_string().as_str()) {
        quote! {
            expr::Expression::create_constant(expr::Constant::#ident)
        }
    } else if ::expr::maps::MAPS.contains(&ident.to_string().as_str()) {
        if let Some(TokenTree::Group(group)) = input.peek() {
            let mut group = group.stream().into_iter().peekable();
            input.next();

            let expr = parse_expr(&mut group, false);

            let mut extra_args = TokenStream::new();

            if group.peek().is_some() {
                eat(',', &mut group);
                let mut i = 0;
                while group.peek().is_some() {
                    let expr = parse_expr(&mut group, false);
                    if i > 0 {
                        extra_args.extend(quote! {, });
                    }
                    extra_args.extend(quote! { #expr });
                    if group.peek().is_some() {
                        eat(',', &mut group);
                    }
                    i += 1;
                }
            }

            let name = ident.to_string();
            quote! {
                expr::Expression::new(vec![#expr], expr::Action::Map {
                    name: String::from(#name),
                    map: &|e, p| #ident(e, p, #extra_args)
                })
            }
        } else {
            panic!("Expected group")
        }
    } else {
        ident.to_token_stream()
    }
}

fn parse_group(group: Group) -> TokenStream {
    let mut group = group.stream().into_iter().peekable();
    let mut children = Vec::new();

    while group.peek().is_some() {
        children.push(parse_expr(&mut group, false));
        if group.peek().is_some() {
            eat(',', &mut group);
        }
    }

    quote! {
        vec![#(#children),*]
    }
}

#[proc_macro]
pub fn expr(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = TokenStream::from(input);
    let mut input = input.into_iter().peekable();
    let expr = parse_expr(&mut input, false);
    if let Some(token) = input.next() {
        panic!("Unexpected token: {}", token);
    }
    proc_macro::TokenStream::from(expr)
}

#[proc_macro]
pub fn rule(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = TokenStream::from(input);
    let mut input = input.into_iter().peekable();

    let name;
    if let Some(TokenTree::Literal(literal)) = input.peek() {
        name = literal.clone();
        input.next();
        eat(',', &mut input);
    } else {
        name = proc_macro2::Literal::string("Unnamed rule");
    }

    let pattern = parse_expr(&mut input, false);

    eat('=', &mut input);
    eat('>', &mut input);

    let replacement = parse_expr(&mut input, false);

    proc_macro::TokenStream::from(quote! {
        &expr::MatchRule::new(#name, #pattern, #replacement)
    })
}
