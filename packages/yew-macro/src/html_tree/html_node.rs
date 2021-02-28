use super::ToNodeIterator;
use crate::stringify::Stringify;
use proc_macro2::TokenStream;
use quote::{quote_spanned, ToTokens};
use syn::{
    parse::{Parse, ParseStream, Result},
    spanned::Spanned,
    Expr, Lit,
};

pub enum HtmlNode {
    Literal(Box<Lit>),
    Expression(Box<Expr>),
}

impl Parse for HtmlNode {
    fn parse(input: ParseStream) -> Result<Self> {
        let node = if input.peek(Lit) {
            let lit: Lit = input.parse()?;
            match lit {
                Lit::Str(_) | Lit::Char(_) | Lit::Int(_) | Lit::Float(_) | Lit::Bool(_) => {}
                _ => return Err(syn::Error::new(lit.span(), "unsupported type")),
            }
            HtmlNode::Literal(Box::new(lit))
        } else {
            HtmlNode::Expression(Box::new(input.parse()?))
        };

        Ok(node)
    }
}

impl ToTokens for HtmlNode {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.extend(match &self {
            HtmlNode::Literal(lit) => {
                let sr = lit.stringify();
                quote_spanned! {lit.span()=>
                    ::yew::virtual_dom::VText::new(#sr)
                }
            }
            HtmlNode::Expression(expr) => quote_spanned! {expr.span()=>
                #expr
            },
        });
    }
}

impl ToNodeIterator for HtmlNode {
    fn to_node_iterator_stream(&self) -> Option<TokenStream> {
        match self {
            HtmlNode::Literal(_) => None,
            HtmlNode::Expression(expr) => {
                // NodeSeq turns both Into<T> and Vec<Into<T>> into IntoIterator<Item = T>
                Some(quote_spanned! {expr.span()=>
                    ::std::convert::Into::<::yew::utils::NodeSeq<_, _>>::into(#expr)
                })
            }
        }
    }
}
