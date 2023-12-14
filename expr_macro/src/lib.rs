extern crate expr;
extern crate proc_macro;

use expr::literals;
use proc_macro2::{token_stream::IntoIter, TokenStream, TokenTree};
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

fn parse_expr(input: &mut IntoIter, expect_group: bool) -> TokenStream {
    if let Some(token) = input.next() {
        if let TokenTree::Group(group) = token {
            if !expect_group {
                panic!("Unexpected group");
            }
            let mut group = group.stream().into_iter();
            let mut children = Vec::new();

            children.push(parse_expr(&mut group, false));

            while let Some(TokenTree::Punct(punct)) = group.next() {
                if punct.as_char() == ',' {
                    children.push(parse_expr(&mut group, false));
                } else {
                    panic!("Expected ',' after expression")
                }
            }

            return quote! {
                vec![#(#children),*]
            };
        } else if expect_group {
            panic!("Expected group")
        }

        match token {
            TokenTree::Group(_) => {
                unreachable!()
            }
            TokenTree::Ident(ident) => {
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
            TokenTree::Punct(punct) => {
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
            TokenTree::Literal(literal) => {
                quote! {
                    expr::Expression::create_value(#literal)
                }
            }
        }
    } else {
        TokenStream::new()
    }
}
#[proc_macro]
pub fn expr(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = TokenStream::from(input);
    let mut input = input.into_iter();
    let output = parse_expr(&mut input, false);
    proc_macro::TokenStream::from(output)
}
