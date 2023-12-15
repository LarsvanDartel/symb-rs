extern crate expr;
extern crate proc_macro;

use std::iter::Peekable;

use expr::literals;
use proc_macro2::{token_stream::IntoIter, Group, Ident, TokenStream, TokenTree};
use quote::quote;

#[proc_macro]
pub fn symb(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = TokenStream::from(input);

    let mut input = input.into_iter();

    let mut symbols = Vec::new();

    while let Some(token) = input.next() {
        match token {
            TokenTree::Ident(ident) => {
                if let Some(next) = input.next() {
                    match next {
                        TokenTree::Punct(punct) => {
                            if punct.as_char() == ',' {
                                symbols.push(ident);
                            } else {
                                panic!("Expected ',' after identifier")
                            }
                        }
                        _ => panic!("Expected ',' after identifier"),
                    }
                } else {
                    symbols.push(ident);
                }
            }
            _ => panic!("Expected identifier"),
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
    if let Some(token) = input.next() {
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
                    return prase_pattern(input);
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
                    _ => panic!("Unexpected operator"),
                };

                let children = parse_expr(input, true);

                quote! {
                    expr::Expression::new(#children, #action)
                }
            }
            TokenTree::Literal(literal) => quote! {
                expr::Expression::create_value(#literal)
            },
        }
    } else {
        TokenStream::new()
    }
}

fn prase_pattern(input: &mut Peekable<IntoIter>) -> TokenStream {
    if let Some(token) = input.next() {
        let name;
        let is_segment;
        if let TokenTree::Ident(ident) = token {
            name = ident.to_string();
            is_segment = false;
        } else if let TokenTree::Punct(punct) = token {
            if punct.as_char() == '~' {
                if let Some(token) = input.next() {
                    if let TokenTree::Ident(ident) = token {
                        name = ident.to_string();
                        is_segment = true;
                    } else {
                        panic!("Expected identifier after '~'")
                    }
                } else {
                    panic!("Expected identifier after '~'")
                }
            } else {
                panic!("Expected identifier or '~'")
            }
        } else {
            panic!("Expected identifier or '~'")
        }

        let mut predicates = Vec::new();
        if let Some(TokenTree::Punct(punct)) = input.peek() {
            if punct.as_char() == ':' {
                input.next();
                if let Some(token) = input.next() {
                    if let TokenTree::Ident(ident) = token {
                        predicates.push(ident);
                    } else if let TokenTree::Group(group) = token {
                        let mut group = group.stream().into_iter().peekable();
                        if let Some(TokenTree::Ident(ident)) = group.next() {
                            predicates.push(ident);
                        } else {
                            panic!("Expected identifier after ':'")
                        }
                        while let Some(TokenTree::Punct(punct)) = group.next() {
                            if punct.as_char() == ',' {
                                if let Some(TokenTree::Ident(ident)) = group.next() {
                                    predicates.push(ident);
                                } else {
                                    panic!("Expected identifier after ','")
                                }
                            } else {
                                panic!("Expected ',' after identifier")
                            }
                        }
                    } else {
                        panic!("Expected identifier after ':'")
                    }
                } else {
                    panic!("Expected predicate after ':'")
                }
            }
        }

        let predicates = predicates
            .iter()
            .map(|ident| match ident.to_string().as_str() {
                "number" => quote! { expr::predicates::number },
                "integer" => quote! { expr::predicates::integer },
                "positive" => quote! { expr::predicates::positive },
                "nonnegative" => quote! { expr::predicates::nonnegative },
                "negative" => quote! { expr::predicates::negative },
                "nonpositive" => quote! { expr::predicates::nonpositive },
                "constant" => quote! { expr::predicates::constant },
                "variable" => quote! { expr::predicates::variable },
                "value" => quote! { expr::predicates::value },
                _ => quote! { #ident },
            })
            .collect::<Vec<_>>();

        let predicate = if predicates.is_empty() {
            quote! { &|_| true }
        } else {
            let mut predicate = quote!(&|e|);
            for i in 0..predicates.len() {
                let ident = &predicates[i];
                if i == 0 {
                    predicate.extend(quote! { #ident(e) });
                } else {
                    predicate.extend(quote! { && #ident(e) });
                }
            }
            predicate
        };

        if is_segment {
            quote! {
                expr::Expression::new_empty(expr::Action::Segment { name: String::from(#name), predicate: #predicate })
            }
        } else {
            quote! {
                expr::Expression::new_empty(expr::Action::Slot { name: String::from(#name), predicate: #predicate })
            }
        }
    } else {
        panic!("Expected identifier after '~'")
    }
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
    } else {
        quote! {
            #ident.clone()
        }
    }
}

fn parse_group(group: Group) -> TokenStream {
    let mut group = group.stream().into_iter().peekable();
    let mut children = Vec::new();

    children.push(parse_expr(&mut group, false));

    while let Some(TokenTree::Punct(punct)) = group.next() {
        if punct.as_char() == ',' {
            children.push(parse_expr(&mut group, false));
        } else {
            panic!("Expected ',' after expression")
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
