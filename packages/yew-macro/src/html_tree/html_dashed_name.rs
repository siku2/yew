use crate::stringify::Stringify;
use proc_macro2::{Ident, TokenStream};
use quote::{quote, ToTokens};
use std::fmt;
use syn::{
    ext::IdentExt,
    parse::{Parse, ParseStream},
    spanned::Spanned,
    LitStr, Token,
};

#[derive(Clone, PartialEq)]
pub struct HtmlDashedName {
    pub name: Ident,
    pub extended: Vec<(Token![-], Ident)>,
}

impl HtmlDashedName {
    pub fn to_ascii_lowercase_string(&self) -> String {
        let mut s = self.to_string();
        s.make_ascii_lowercase();
        s
    }

    pub fn to_lit_str(&self) -> LitStr {
        LitStr::new(&self.to_string(), self.span())
    }
}

impl fmt::Display for HtmlDashedName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.name)?;
        for (_, ident) in &self.extended {
            write!(f, "-{}", ident)?;
        }
        Ok(())
    }
}

impl Parse for HtmlDashedName {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let name = input.call(Ident::parse_any)?;
        let mut extended = Vec::new();
        while input.peek(Token![-]) {
            extended.push((input.parse::<Token![-]>()?, input.parse::<Ident>()?));
        }

        Ok(HtmlDashedName { name, extended })
    }
}

impl ToTokens for HtmlDashedName {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let HtmlDashedName { name, extended } = self;
        let dashes = extended.iter().map(|(dash, _)| quote! {#dash});
        let idents = extended.iter().map(|(_, ident)| quote! {#ident});
        let extended = quote! { #(#dashes#idents)* };
        tokens.extend(quote! { #name#extended });
    }
}
impl Stringify for HtmlDashedName {
    fn try_into_lit(&self) -> Option<LitStr> {
        Some(self.to_lit_str())
    }

    fn stringify(&self) -> TokenStream {
        self.to_lit_str().stringify()
    }
}
